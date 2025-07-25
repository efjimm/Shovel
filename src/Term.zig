// Copyright © 2021 - 2022 Leon Henrik Plickat
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 3 as published
// by the Free Software Foundation.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const posix = std.posix;
const builtin = @import("builtin");
const mode = @import("builtin").mode;

const zg = @import("zg");

const TerminalCellWriter = @import("TerminalCellWriter.zig");
const GraphemeClusteringMode = @import("main.zig").GraphemeClusteringMode;
const input = @import("input.zig");
const InputParser = input.InputParser;
const log = @import("log.zig");
const spells = @import("spells.zig");
pub const CursorShape = spells.CursorShape;
const Style = @import("Style.zig");
const TermInfo = @import("TermInfo.zig");

const Term = @This();

const UncookOptions = struct {
    request_kitty_keyboard_protocol: bool = true,
    request_mouse_tracking: bool = false,
    request_mode_2027: bool = true,
};

const TermConfig = struct {
    use_terminfo: bool = true,
    terminfo_inputs: bool = true,

    /// How to handle 24 bit color support
    truecolour: enum {
        /// Disable 24 bit color
        disable,
        /// Check for 24 bit color and enable or disable it based on these checks. May result in
        /// false negatives, as many terminal emulators don't correctly advertise truecolor support.
        check,
        /// Force enable 24 bit color. Most terminals support truecolor, and most terminals that
        /// don't support it will still accept the truecolor escape sequences and display an
        /// approximation of that color instead. A notable exception to this is Terminal.app, the
        /// default terminal on MacOS, which apparently does not support truecolor at all.
        force,
    } = .check,
};

/// The original termios configuration saved when entering raw mode. null if in cooked mode,
/// otherwise we are uncooked.
cooked_termios: ?posix.termios = null,

/// Size of the terminal, updated when `fetchSize()` is called.
width: u16 = 0,
height: u16 = 0,

/// Are we currently rendering?
currently_rendering: bool = false,

tty: posix.fd_t,

cursor_visible: bool = true,
cursor_shape: CursorShape = .unknown,

codepoint: [4]u8 = undefined,
codepoint_len: u3 = 0,

terminfo: ?*TermInfo = null,

/// True if the kitty keyboard protocol is active.
kitty_enabled: bool = false,

grapheme_clustering_mode: GraphemeClusteringMode = .codepoint,

truecolor_enabled: bool = false,

/// See `input.inputParser`
pub fn inputParser(term: *Term, bytes: []const u8) InputParser {
    return input.inputParser(bytes, term);
}

pub fn writer(term: *Term, buffer: []u8) std.fs.File.Writer {
    return .init(.{ .handle = term.tty }, buffer);
}

// NotATerminal + a subset of posix.OpenError, removing all errors which aren't reachable due to how
// we call posix.open
pub const InitError = error{
    AccessDenied,
    BadPathName,
    FileBusy,
    FileNotFound,
    FileTooBig,
    InvalidUtf8,
    IsDir,
    NameTooLong,
    NoDevice,
    NotATerminal,
    ProcessFdQuotaExceeded,
    SymLinkLoop,
    SystemFdQuotaExceeded,
    SystemResources,
    PermissionDenied,
    Unexpected,
} || Allocator.Error;

pub fn init(allocator: Allocator, term_config: TermConfig) InitError!Term {
    var ret = Term{
        .tty = posix.open("/dev/tty", .{ .ACCMODE = .RDWR }, 0) catch |err| switch (err) {
            // None of these are reachable with the flags we pass to posix.open
            error.DeviceBusy,
            error.FileLocksNotSupported,
            error.NoSpaceLeft,
            error.NotDir,
            error.PathAlreadyExists,
            error.WouldBlock,
            error.NetworkNotFound,
            error.InvalidWtf8,
            error.ProcessNotFound,
            => unreachable,

            error.AccessDenied,
            error.BadPathName,
            error.FileBusy,
            error.FileNotFound,
            error.FileTooBig,
            error.InvalidUtf8,
            error.IsDir,
            error.NameTooLong,
            error.NoDevice,
            error.ProcessFdQuotaExceeded,
            error.SymLinkLoop,
            error.SystemFdQuotaExceeded,
            error.SystemResources,
            error.PermissionDenied,
            error.Unexpected,
            => |e| return e,
        },
        .terminfo = null,
    };
    errdefer posix.close(ret.tty);

    if (!posix.isatty(ret.tty))
        return error.NotATerminal;
    errdefer ret.deinit(allocator);

    if (term_config.use_terminfo) {
        ret.terminfo = getTermInfo(allocator) catch |err| switch (err) {
            error.OutOfMemory => |e| return e,
            error.NoTermInfo => blk: {
                log.warn("Proceeding without terminfo definitions", .{});
                break :blk null;
            },
        };

        if (term_config.terminfo_inputs) {
            try ret.useTermInfoInputs(allocator);
        }
    }

    ret.truecolor_enabled = sw: switch (term_config.truecolour) {
        .disable => false,
        .check => {
            if (posix.getenv("COLORTERM")) |colorterm| {
                if (std.mem.eql(u8, colorterm, "truecolor") or
                    std.mem.eql(u8, colorterm, "24bit"))
                {
                    break :sw true;
                }
            }

            if (ret.terminfo) |ti| {
                if (ti.getNumberCapability(.max_colors)) |colors| {
                    if (colors >= comptime std.math.pow(u32, 2, 24))
                        break :sw true;
                }
            }

            break :sw false;
        },
        .force => true,
    };

    return ret;
}

pub fn deinit(term: *Term, allocator: Allocator) void {
    assert(!term.currently_rendering);

    // It's probably a good idea to cook the terminal on exit.
    if (!term.isCooked())
        term.cook() catch {};

    if (term.terminfo) |ti|
        ti.destroy(allocator);

    posix.close(term.tty);
    term.* = undefined;
}

fn getTermInfo(allocator: Allocator) !*TermInfo {
    const term_var = posix.getenv("TERM") orelse {
        log.info("No TERM variable defined", .{});
        return error.NoTermInfo;
    };

    var buf: [TermInfo.max_file_length]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buf);

    var iter: TermInfo.FileIter = .{ .term = term_var };
    while (iter.next()) |file| {
        defer file.close();

        fba.reset();
        const bytes = file.readToEndAlloc(fba.allocator(), TermInfo.max_file_length) catch |err| switch (err) {
            error.OutOfMemory => |e| return e,
            else => {
                log.info("{} when reading terminfo file, skipping", .{err});
                continue;
            },
        };

        return TermInfo.parse(allocator, bytes) catch |err| switch (err) {
            error.OutOfMemory => |e| return e,
            else => {
                log.info("Could not parse terminfo file, skipping {}", .{err});
                continue;
            },
        };
    }

    log.info("Could not find terminfo description", .{});
    return error.NoTermInfo;
}

pub fn useTermInfoInputs(term: *Term, allocator: Allocator) !void {
    if (term.terminfo) |ti| {
        _ = try ti.createInputMap(allocator);
    }
}

const SetBlockingReadError = posix.TermiosGetError || posix.TermiosSetError;

pub fn setBlockingRead(term: Term, enabled: bool) SetBlockingReadError!void {
    const termios = blk: {
        var raw = try posix.tcgetattr(term.tty);

        if (enabled) {
            raw.cc[@intFromEnum(posix.system.V.TIME)] = 0;
            raw.cc[@intFromEnum(posix.system.V.MIN)] = 1;
        } else {
            raw.cc[@intFromEnum(posix.system.V.TIME)] = 0;
            raw.cc[@intFromEnum(posix.system.V.MIN)] = 0;
        }

        break :blk raw;
    };

    try posix.tcsetattr(term.tty, .FLUSH, termios);
}

// Reads from stdin to the supplied buffer. Asserts that `buf.len >= 8`.
pub fn readInput(term: *Term, buf: []u8) ![]u8 {
    assert(buf.len >= 8); // Ensures that at least one full escape sequence can be handled
    assert(!term.currently_rendering);
    assert(!term.isCooked());

    // If we have a partial codepoint from the last read, append it to the buffer
    const buffer = blk: {
        const len = term.codepoint_len;
        if (len > 0) {
            @memcpy(buf[0..len], term.codepoint[0..len]);
            term.codepoint_len = 0;
            break :blk buf[len..];
        }
        break :blk buf;
    };

    // Use system.read instead of posix.read so it won't restart on signals.
    const rc = posix.system.read(term.tty, buffer.ptr, buffer.len);

    const bytes_read: usize = switch (posix.errno(rc)) {
        .SUCCESS => @intCast(rc),
        .INTR => 0,
        .INVAL => unreachable,
        .FAULT => unreachable,
        .AGAIN => return error.WouldBlock,
        .BADF => return error.NotOpenForReading, // Can be a race condition.
        .IO => return error.InputOutput,
        .ISDIR => return error.IsDir,
        .NOBUFS => return error.SystemResources,
        .NOMEM => return error.SystemResources,
        .CONNRESET => return error.ConnectionResetByPeer,
        .TIMEDOUT => return error.ConnectionTimedOut,
        else => |err| return posix.unexpectedErrno(err),
    };

    const slice = buffer[0..bytes_read];
    // Check if last part of the buffer is a partial utf-8 codepoint. If this happens, we write the
    // partial codepoint to an internal buffer, and complete it on the next call to readInput.
    if (bytes_read == buffer.len and buffer[buffer.len - 1] > 0x7F) {
        var i: usize = buffer.len;
        while (i > 0) {
            i -= 1;
            if (buffer[i] & 0xC0 == 0xC0) break;
        } else return slice; // Have invalid utf-8, pass it through

        const codepoint_len = std.unicode.utf8ByteSequenceLength(buffer[i]) catch return slice;
        const len = buffer.len - i; // Length of unfinished codepoint
        if (codepoint_len <= len) return slice; // Have invalid utf-8

        // Copy unfinished codepoint into internal buffer
        @memcpy(term.codepoint[0..len], buffer[i..]);
        term.codepoint_len = @intCast(len);
        // Return the buffer without the trailing unfinished codepoint
        return buffer[0..i];
    }

    return slice;
}

// TODO: Make this ?bool

pub fn getExtendedFlag(term: *const Term, name: []const u8) bool {
    return if (term.terminfo) |ti|
        ti.getExtendedFlag(name)
    else
        false;
}

pub fn getExtendedNumber(term: *const Term, name: []const u8) ?u31 {
    return if (term.terminfo) |ti|
        ti.getExtendedNumber(name)
    else
        null;
}

pub fn getExtendedString(term: *const Term, name: []const u8) ?[:0]const u8 {
    return if (term.terminfo) |ti|
        ti.getExtendedString(name)
    else
        null;
}

pub fn getFlagCapability(
    term: *const Term,
    comptime tag: TermInfo.Flag,
) bool {
    return if (term.terminfo) |ti|
        ti.getFlagCapability(tag)
    else
        null;
}

pub fn getNumberCapability(
    term: *const Term,
    comptime tag: TermInfo.Number,
) ?u31 {
    return if (term.terminfo) |ti|
        ti.getNumberCapability(tag)
    else
        null;
}

pub fn getStringCapability(
    term: *const Term,
    comptime tag: TermInfo.String,
) ?[:0]const u8 {
    return if (term.terminfo) |ti|
        ti.getStringCapability(tag)
    else
        null;
}

pub inline fn isCooked(term: *const Term) bool {
    return term.cooked_termios == null;
}

pub const UncookError =
    std.mem.Allocator.Error ||
    posix.TermiosGetError ||
    posix.TermiosSetError ||
    posix.WriteError ||
    posix.FcntlError ||
    posix.ReadError ||
    posix.PollError;

/// Enter raw mode.
pub fn uncook(
    term: *Term,
    allocator: std.mem.Allocator,
    options: UncookOptions,
) UncookError!void {
    if (!term.isCooked())
        return;

    // The information on the various flags and escape sequences is pieced
    // together from various sources, including termios(3) and
    // https://viewsourcecode.org/snaptoken/kilo/02.enteringRawMode.html.

    term.cooked_termios = try posix.tcgetattr(term.tty);
    errdefer term.cook() catch {};

    const raw_termios = blk: {
        var raw = term.cooked_termios.?;

        //   ECHO: Stop the terminal from displaying pressed keys.
        // ICANON: Disable canonical ("cooked") mode. Allows us to read inputs
        //         byte-wise instead of line-wise.
        //   ISIG: Disable signals for Ctrl-C (SIGINT) and Ctrl-Z (SIGTSTP), so we
        //         can handle them as normal escape sequences.
        // IEXTEN: Disable input preprocessing. This allows us to handle Ctrl-V,
        //         which would otherwise be intercepted by some terminals.
        raw.lflag.ECHO = false;
        raw.lflag.ICANON = false;
        raw.lflag.ISIG = false;
        raw.lflag.IEXTEN = false;

        //   IXON: Disable software control flow. This allows us to handle Ctrl-S
        //         and Ctrl-Q.
        //  ICRNL: Disable converting carriage returns to newlines. Allows us to
        //         handle Ctrl-J and Ctrl-M.
        // BRKINT: Disable converting sending SIGINT on break conditions. Likely has
        //         no effect on anything remotely modern.
        //  INPCK: Disable parity checking. Likely has no effect on anything
        //         remotely modern.
        // ISTRIP: Disable stripping the 8th bit of characters. Likely has no effect
        //         on anything remotely modern.
        raw.iflag.IXON = false;
        raw.iflag.ICRNL = false;
        raw.iflag.BRKINT = false;
        raw.iflag.INPCK = false;
        raw.iflag.ISTRIP = false;

        // IUTF8: (Linux only)
        if (builtin.os.tag == .linux)
            raw.iflag.IUTF8 = true;

        // Disable output processing. Common output processing includes prefixing
        // newline with a carriage return.
        raw.oflag.OPOST = false;

        // Set the character size to 8 bits per byte. Likely has no efffect on
        // anything remotely modern.
        raw.cflag.CSIZE = .CS8;

        break :blk raw;
    };

    try posix.tcsetattr(term.tty, .FLUSH, raw_termios);

    var buf: [256]u8 = undefined;
    var bw = term.writer(&buf);
    inline for (.{
        term.getStringCapability(.save_cursor) orelse spells.save_cursor_position,
        term.getStringCapability(.enter_ca_mode) orelse spells.enter_alt_buffer,
        term.getStringCapability(.exit_insert_mode) orelse spells.overwrite_mode,
        term.getStringCapability(.exit_am_mode) orelse spells.reset_auto_wrap,
        term.getStringCapability(.cursor_invisible) orelse spells.hide_cursor,
    }) |str| bw.interface.writeAll(str) catch return bw.err.?;

    if (options.request_kitty_keyboard_protocol) {
        term.enableKittyKeyboard(&bw.interface) catch return bw.err.?;
    }

    if (options.request_mode_2027) {
        term.enableMode2027(allocator, &bw.interface) catch return bw.err.?;
    }

    if (options.request_mouse_tracking) {
        bw.interface.writeAll(spells.enable_mouse_tracking) catch return bw.err.?;
    }
    bw.interface.flush() catch return bw.err.?;
}

/// Attempts to enable mode 2027. If successful, also initializes the unicode data required to do
/// proper grapheme cluster segmentation.
///
/// https://github.com/contour-terminal/terminal-unicode-core
pub fn enableMode2027(term: *Term, allocator: std.mem.Allocator, wr: *std.io.Writer) !void {
    try wr.writeAll(spells.enable_mode_2027);
    try wr.writeAll("\x1B[?2027$p");
    try wr.flush();
    var poll_fds: [1]posix.pollfd = .{
        .{
            .fd = term.tty,
            .events = posix.POLL.IN,
            .revents = 0,
        },
    };
    _ = posix.poll(&poll_fds, 5) catch {};
    if (poll_fds[0].revents & posix.POLL.IN != 0) {
        var buf: [16]u8 = undefined;
        if (posix.read(term.tty, &buf)) |len| {
            if (std.mem.eql(u8, buf[0..len], "\x1b[?2027;1$y") or
                std.mem.eql(u8, buf[0..len], "\x1b[?2027;3$y"))
            {
                if (!zg.isInitialized(.graphemes))
                    try zg.initData(allocator, &.{.graphemes});
                term.grapheme_clustering_mode = .grapheme;
                log.info("Mode 2027 enabled", .{});
            }
        } else |err| {
            log.warn("Could not read mode 2027 query response from terminal: {}", .{err});
        }
    } else {
        log.info("Mode 2027 not found, disabling", .{});
    }
}

fn enableKittyKeyboard(term: *Term, wr: *std.io.Writer) !void {
    try wr.writeAll(spells.enable_kitty_keyboard);
    try wr.writeAll("\x1B[?u");
    try wr.flush();
    var poll_fds: [1]posix.pollfd = .{
        .{
            .fd = term.tty,
            .events = posix.POLL.IN,
            .revents = 0,
        },
    };
    _ = posix.poll(&poll_fds, 5) catch {};
    if (poll_fds[0].revents & posix.POLL.IN != 0) {
        var buf: [16]u8 = undefined;
        if (posix.read(term.tty, &buf)) |len| {
            if (std.mem.eql(u8, buf[0..len], "\x1b[?1u")) {
                // Got the correct response from the terminal, kitty keyboard is enabled
                term.kitty_enabled = true;
                log.info("Kitty keyboard enabled", .{});
            }
        } else |err| {
            log.warn("Could not read kitty keyboard query response from terminal: {}", .{err});
        }
    } else {
        log.info("Kitty keyboard not found, disabling", .{});
    }
}

pub const CookError = posix.WriteError || posix.TermiosSetError;

/// Enter cooked mode.
pub fn cook(term: *Term) CookError!void {
    if (term.isCooked())
        return;

    try posix.tcsetattr(term.tty, .FLUSH, term.cooked_termios.?);
    term.cooked_termios = null;

    var buf: [128]u8 = undefined;
    var bw = term.writer(&buf);

    if (term.kitty_enabled) {
        bw.interface.writeAll(spells.disable_kitty_keyboard) catch return bw.err.?;
        term.kitty_enabled = false;
    }

    inline for (.{
        spells.disable_mouse_tracking,
        term.getStringCapability(.clear_screen) orelse "",
        term.getStringCapability(.exit_ca_mode) orelse "",
        term.getStringCapability(.cursor_visible) orelse "",
        term.getStringCapability(.exit_attribute_mode) orelse "",
        term.getStringCapability(.enter_am_mode) orelse spells.enable_auto_wrap,
    }) |str| bw.interface.writeAll(str) catch return bw.err.?;
    bw.interface.flush() catch return bw.err.?;
}

pub fn fetchSize(term: *Term) posix.UnexpectedError!void {
    if (term.isCooked())
        return;

    var size = std.mem.zeroes(std.posix.winsize);
    const err = posix.system.ioctl(term.tty, posix.system.T.IOCGWINSZ, @intFromPtr(&size));
    if (posix.errno(err) != .SUCCESS) {
        return posix.unexpectedErrno(@enumFromInt(err));
    }
    term.height = size.row;
    term.width = size.col;
}

pub const WriteError = std.posix.WriteError;

/// Set window title using OSC 2. Shall not be called while rendering.
pub fn setWindowTitle(term: *Term, comptime fmt: []const u8, args: anytype) WriteError!void {
    assert(!term.currently_rendering);
    var buf: [1024]u8 = undefined;
    var bw = term.writer(&buf);
    bw.interface.print("\x1b]2;" ++ fmt ++ "\x1b\\", args) catch return bw.err.?;
    bw.interface.flush() catch return bw.err.?;
}

pub fn graphemeWidth(term: *Term, bytes: []const u8) u32 {
    return @import("main.zig").graphemeWidth(bytes, term.grapheme_clustering_mode);
}

pub fn getRenderContext(term: *Term, buf: []u8) WriteError!RenderContext {
    assert(!term.currently_rendering);
    assert(!term.isCooked());

    var rc: RenderContext = .{
        .term = term,
        .writer = .initMode(.{ .handle = term.tty }, buf, .streaming),
    };

    if (rc.term.getExtendedString("Sync")) |sync|
        TermInfo.writeParamSequence(sync, &rc.writer.interface, .{1}) catch {
            return rc.writer.err.?;
        };

    if (term.getStringCapability(.exit_attribute_mode)) |srg0|
        rc.writer.interface.writeAll(srg0) catch return rc.writer.err.?;

    term.currently_rendering = true;
    return rc;
}

pub const RenderContext = struct {
    term: *Term,
    writer: std.fs.File.Writer,

    /// Finishes the render operation. The render context may not be used any further.
    pub fn done(rc: *RenderContext) WriteError!void {
        assert(rc.term.currently_rendering);
        assert(!rc.term.isCooked());
        defer rc.term.currently_rendering = false;
        if (rc.term.getExtendedString("Sync")) |sync| {
            TermInfo.writeParamSequence(sync, &rc.writer.interface, .{2}) catch return rc.writer.err.?;
        }
        rc.writer.interface.flush() catch return rc.writer.err.?;
    }

    /// Clears all content. Avoid calling often, as fully clearing and redrawing the screen can
    /// cause flicker on some terminals (such as the Linux tty). Prefer more granular clearing
    /// functions like `clearToEol` and `clearToBot`.
    pub fn clear(rc: *RenderContext) WriteError!void {
        assert(rc.term.currently_rendering);
        const spell = rc.term.getStringCapability(.clear_screen) orelse spells.clear;
        rc.writer.interface.writeAll(spell) catch return rc.writer.err.?;
    }

    /// Clears the screen from the current line to the bottom.
    pub fn clearToBot(rc: *RenderContext) WriteError!void {
        assert(rc.term.currently_rendering);
        const wr = &rc.writer.interface;
        const spell = rc.term.getStringCapability(.clr_eos) orelse spells.clear_to_bot;
        wr.writeAll(spell) catch return rc.writer.err.?;
    }

    /// Clears from the cursor position to the end of the line.
    pub fn clearToEol(rc: *RenderContext) WriteError!void {
        assert(rc.term.currently_rendering);
        const wr = &rc.writer.interface;
        const spell = rc.term.getStringCapability(.clr_eol) orelse spells.clear_to_eol;
        wr.writeAll(spell) catch return rc.writer.err.?;
    }

    /// Clears the screen from the cursor to the beginning of the line.
    pub fn clearToBol(rc: *RenderContext) WriteError!void {
        assert(rc.term.currently_rendering);
        const wr = &rc.writer.interface;
        const spell = rc.term.getStringCapability(.clr_bol) orelse spells.clear_to_bol;
        wr.writeAll(spell) catch return rc.writer.err.?;
    }

    /// Move the cursor to the specified cell.
    pub fn moveCursorTo(rc: *RenderContext, row: u16, col: u16) WriteError!void {
        assert(rc.term.currently_rendering);
        const spell = rc.term.getStringCapability(.cursor_address) orelse spells.move_cursor_fmt;
        TermInfo.writeParamSequence(spell, &rc.writer.interface, .{ row, col }) catch return rc.writer.err.?;
    }

    pub fn saveCursor(rc: *RenderContext) WriteError!void {
        assert(rc.term.currently_rendering);
        const spell = rc.term.getStringCapability(.save_cursor) orelse spells.save_cursor_position;
        TermInfo.writeParamSequence(spell, &rc.writer.interface, .{}) catch return rc.writer.err.?;
    }

    pub fn restoreCursor(rc: *RenderContext) WriteError!void {
        assert(rc.term.currently_rendering);
        const spell = rc.term.getStringCapability(.restore_cursor) orelse spells.restore_cursor_position;
        TermInfo.writeParamSequence(spell, &rc.writer.interface, .{}) catch return rc.writer.err.?;
    }

    /// Hide the cursor.
    pub fn hideCursor(rc: *RenderContext) WriteError!void {
        assert(rc.term.currently_rendering);
        if (!rc.term.cursor_visible) return;
        const wr = &rc.writer.interface;
        const spell = rc.term.getStringCapability(.cursor_invisible) orelse spells.hide_cursor;
        wr.writeAll(spell) catch return rc.writer.err.?;
        rc.term.cursor_visible = false;
    }

    /// Show the cursor.
    pub fn showCursor(rc: *RenderContext) WriteError!void {
        assert(rc.term.currently_rendering);
        if (rc.term.cursor_visible) return;
        const wr = &rc.writer.interface;
        const spell = rc.term.getStringCapability(.cursor_normal) orelse spells.show_cursor;
        wr.writeAll(spell) catch return rc.writer.err.?;
        rc.term.cursor_visible = true;
    }

    /// Set the text attributes for all following writes.
    pub fn setStyle(rc: *RenderContext, attr: Style) WriteError!void {
        assert(rc.term.currently_rendering);
        attr.dump(&rc.writer.interface, .{
            .terminfo = rc.term.terminfo,
            .truecolor = rc.term.truecolor_enabled,
        }) catch return rc.writer.err.?;
    }

    pub fn cellWriter(rc: *RenderContext, width: u16) TerminalCellWriter {
        assert(rc.term.currently_rendering);
        return .init(
            &rc.writer.interface,
            rc.term.grapheme_clustering_mode,
            width,
        );
    }

    /// Write all bytes, wrapping at the end of the line.
    pub fn writeAllWrapping(rc: *RenderContext, bytes: []const u8) WriteError!void {
        assert(rc.term.currently_rendering);
        const wr = &rc.writer.interface;
        const enable = rc.term.getStringCapability(.enter_am_mode) orelse spells.enable_auto_wrap;
        const disable = rc.term.getStringCapability(.exit_am_mode) orelse spells.reset_auto_wrap;
        wr.writeAll(enable) catch return rc.writer.err.?;
        wr.writeAll(bytes) catch return rc.writer.err.?;
        wr.writeAll(disable) catch return rc.writer.err.?;
    }

    pub fn setCursorShape(rc: *RenderContext, shape: CursorShape) WriteError!void {
        assert(rc.term.currently_rendering);
        assert(shape != .unknown);

        if (rc.term.cursor_shape == shape) return;

        const spell = rc.term.getExtendedString("Ss") orelse spells.change_cursor;
        TermInfo.writeParamSequence(spell, &rc.writer.interface, .{@intFromEnum(shape)}) catch
            return rc.writer.err.?;
        rc.term.cursor_shape = shape;
    }
};
