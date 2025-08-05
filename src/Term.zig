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
pub const WriteError = std.posix.WriteError;
const builtin = @import("builtin");
const mode = @import("builtin").mode;

const zg = @import("zg");

const GraphemeClusteringMode = @import("main.zig").GraphemeClusteringMode;
const input = @import("input.zig");
const InputParser = input.InputParser;
const log = @import("log.zig");
const Screen = @import("Screen.zig");
const spells = @import("spells.zig");
pub const CursorShape = spells.CursorShape;
const Style = @import("Style.zig");
const TerminalCellWriter = @import("TerminalCellWriter.zig");
const TermInfo = @import("TermInfo.zig");

const Term = @This();

const UncookOptions = struct {
    request_kitty_keyboard_protocol: bool = true,
    request_mouse_tracking: bool = false,
    request_mode_2027: bool = true,
};

pub const TermInfoConfig = struct {
    /// The terminfo definition to fall back to depending on the fallback mode.
    fallback: TermInfo.Fallback,
    fallback_mode: TermInfo.FallbackMode,

    pub const no_fallback: TermInfoConfig = .{
        .fallback = .dumb,
        .fallback_mode = .last_resort,
    };

    pub const disable: TermInfoConfig = .{
        .fallback = .dumb,
        .fallback_mode = .always,
    };
};

pub const TermConfig = struct {
    terminfo: TermInfoConfig,

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

terminfo: *TermInfo,

/// True if the kitty keyboard protocol is active.
kitty_enabled: bool = false,

grapheme_clustering_mode: GraphemeClusteringMode = .codepoint,

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

pub fn init(gpa: Allocator, conf: TermConfig) InitError!Term {
    const tty = posix.open("/dev/tty", .{ .ACCMODE = .RDWR }, 0) catch |err| switch (err) {
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
    };
    errdefer posix.close(tty);

    if (!posix.isatty(tty))
        return error.NotATerminal;

    const terminfo = try gpa.create(TermInfo);
    errdefer gpa.destroy(terminfo);
    terminfo.* = getTermInfo(gpa, conf.terminfo) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidFormat, error.NoTermInfo => blk: {
            std.log.err("Failed to load fallback terminfo, proceeding without terminfo definitions", .{});
            break :blk .{};
        },
    };

    try terminfo.populateInputMap(gpa);

    switch (conf.truecolour) {
        .disable => terminfo.truecolour = .none,
        .check => terminfo.queryTrueColour(),
        .force => {
            terminfo.queryTrueColour();
            if (terminfo.truecolour == .none)
                terminfo.truecolour = .hardcoded;
        },
    }

    return .{
        .tty = tty,
        .terminfo = terminfo,
    };
}

pub fn deinit(term: *Term, gpa: Allocator) void {
    assert(!term.currently_rendering);

    // It's probably a good idea to cook the terminal on exit.
    if (!term.isCooked())
        term.cook() catch {};

    if (term.grapheme_clustering_mode == .grapheme)
        zg.deinitData(gpa, &.{.graphemes});

    term.terminfo.deinit(gpa);
    gpa.destroy(term.terminfo);

    posix.close(term.tty);
    term.* = undefined;
}

/// Returns a terminfo defintion based on the configuration. Returns `error.NoTermInfo` if a valid
/// terminfo file cannot be found.
fn getTermInfo(gpa: Allocator, opts: TermInfoConfig) !TermInfo {
    switch (opts.fallback_mode) {
        .always => return opts.fallback.getTermInfo(gpa),
        .last_resort => {
            const term_var = posix.getenv("TERM") orelse {
                log.info("No TERM variable defined", .{});
                return error.NoTermInfo;
            };

            return TermInfo.getTermInfoForTerm(gpa, term_var) catch |err| switch (err) {
                error.OutOfMemory => error.OutOfMemory,
                error.NoTermInfo => opts.fallback.getTermInfo(gpa),
            };
        },
        .merge => {
            const term_var = posix.getenv("TERM") orelse {
                log.info("No TERM variable defined", .{});
                return error.NoTermInfo;
            };

            var terminfo: TermInfo = TermInfo.getTermInfoForTerm(gpa, term_var) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.NoTermInfo => .{},
            };
            errdefer terminfo.deinit(gpa);

            var fallback: TermInfo = opts.fallback.getTermInfo(gpa) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.NoTermInfo, error.InvalidFormat => .{},
            };
            defer fallback.deinit(gpa);

            try TermInfo.merge(gpa, &terminfo, &fallback);
            return terminfo;
        },
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
        .save_cursor,
        .enter_ca_mode,
        .exit_insert_mode,
        .exit_am_mode,
        .cursor_invisible,
    }) |str| term.terminfo.write(&bw.interface, str, .{}) catch return bw.err.?;

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
        term.terminfo.getStringCapability(.clear_screen) orelse "",
        term.terminfo.getStringCapability(.exit_ca_mode) orelse "",
        term.terminfo.getStringCapability(.cursor_visible) orelse "",
        term.terminfo.getStringCapability(.exit_attribute_mode) orelse "",
        term.terminfo.getStringCapability(.enter_am_mode) orelse "",
    }) |str| bw.interface.writeAll(str) catch return bw.err.?;
    bw.interface.flush() catch return bw.err.?;
}

pub fn fetchSize(term: *Term) posix.UnexpectedError!void {
    var size = std.mem.zeroes(std.posix.winsize);
    const err = posix.system.ioctl(term.tty, posix.system.T.IOCGWINSZ, @intFromPtr(&size));
    if (posix.errno(err) != .SUCCESS) {
        return posix.unexpectedErrno(@enumFromInt(err));
    }
    term.height = size.row;
    term.width = size.col;
}

/// Set window title using OSC 2. Shall not be called while rendering.
pub fn setWindowTitle(term: *Term, comptime fmt: []const u8, args: anytype) WriteError!void {
    assert(!term.currently_rendering);
    var buf: [1024]u8 = undefined;
    var bw = term.writer(&buf);
    bw.interface.print("\x1b]2;" ++ fmt ++ "\x1b\\", args) catch return bw.err.?;
    bw.interface.flush() catch return bw.err.?;
}

pub fn stringWidth(term: *Term, bytes: []const u8) u32 {
    return @intCast(@import("main.zig").stringWidth(bytes, term.grapheme_clustering_mode, .{}).width);
}

/// Returns a double buffer using the terminfo and grapheme clustering mode of the terminal.
pub fn doubleBuffer(term: *const Term, gpa: std.mem.Allocator) Screen.DoubleBuffer {
    return .init(gpa, term.terminfo, term.grapheme_clustering_mode);
}

pub fn getRenderContext(term: *Term, buf: []u8) WriteError!RenderContext {
    assert(!term.currently_rendering);
    assert(!term.isCooked());

    var rc: RenderContext = .{
        .term = term,
        .writer = .initMode(.{ .handle = term.tty }, buf, .streaming),
    };

    term.terminfo.writeExt(&rc.writer.interface, "Sync", .{1}) catch
        return rc.writer.err.?;

    term.terminfo.write(&rc.writer.interface, .exit_attribute_mode, .{}) catch {
        term.terminfo.writeExt(&rc.writer.interface, "Sync", .{2}) catch
            return rc.writer.err.?;

        return rc.writer.err.?;
    };

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
        rc.term.terminfo.writeExt(&rc.writer.interface, "Sync", .{2}) catch
            return rc.writer.err.?;
        rc.writer.interface.flush() catch return rc.writer.err.?;
    }

    /// Clears all content. Avoid calling often, as fully clearing and redrawing the screen can
    /// cause flicker on some terminals (such as the Linux tty). Prefer more granular clearing
    /// functions like `clearToEol` and `clearToBot`.
    pub fn clear(rc: *RenderContext) WriteError!void {
        assert(rc.term.currently_rendering);
        rc.term.terminfo.write(
            &rc.writer.interface,
            .clear_screen,
            .{},
        ) catch return rc.writer.err.?;
    }

    pub fn write(rc: *RenderContext, str: TermInfo.String, args: anytype) WriteError!void {
        assert(rc.term.currently_rendering);
        rc.term.terminfo.write(&rc.writer.interface, str, args) catch return rc.writer.err.?;
    }

    pub fn writeExt(rc: *RenderContext, str: []const u8, args: anytype) WriteError!void {
        assert(rc.term.currently_rendering);
        rc.term.terminfo.writeExt(&rc.writer.interface, str, args) catch return rc.writer.err.?;
    }

    /// Clears the screen from the current line to the bottom.
    pub fn clearToBot(rc: *RenderContext) WriteError!void {
        try rc.write(.clr_eos, .{});
    }

    /// Clears from the cursor position to the end of the line.
    pub fn clearToEol(rc: *RenderContext) WriteError!void {
        try rc.write(.clr_eol, .{});
    }

    /// Clears the screen from the cursor to the beginning of the line.
    pub fn clearToBol(rc: *RenderContext) WriteError!void {
        try rc.write(.clr_bol, .{});
    }

    /// Move the cursor to the specified cell.
    pub fn moveCursorTo(rc: *RenderContext, row: u16, col: u16) WriteError!void {
        try rc.write(.cursor_address, .{ row, col });
    }

    pub fn saveCursor(rc: *RenderContext) WriteError!void {
        try rc.write(.save_cursor, .{});
    }

    pub fn restoreCursor(rc: *RenderContext) WriteError!void {
        try rc.write(.restore_cursor, .{});
    }

    /// Hide the cursor.
    pub fn hideCursor(rc: *RenderContext) WriteError!void {
        assert(rc.term.currently_rendering);
        if (!rc.term.cursor_visible) return;
        try rc.write(.cursor_invisible, .{});
        rc.term.cursor_visible = false;
    }

    /// Show the cursor.
    pub fn showCursor(rc: *RenderContext) WriteError!void {
        assert(rc.term.currently_rendering);
        if (rc.term.cursor_visible) return;
        try rc.write(.cursor_normal, .{});
        rc.term.cursor_visible = true;
    }

    /// Set the text attributes for all following writes.
    pub fn setStyle(rc: *RenderContext, attr: Style) WriteError!void {
        assert(rc.term.currently_rendering);
        attr.dump(&rc.writer.interface, .{
            .terminfo = rc.term.terminfo,
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
        const enable = rc.term.terminfo.getStringCapability(.enter_am_mode) orelse return;
        const disable = rc.term.terminfo.getStringCapability(.exit_am_mode) orelse return;
        wr.writeAll(enable) catch return rc.writer.err.?;
        wr.writeAll(bytes) catch return rc.writer.err.?;
        wr.writeAll(disable) catch return rc.writer.err.?;
    }

    pub fn setCursorShape(rc: *RenderContext, shape: CursorShape) WriteError!void {
        assert(rc.term.currently_rendering);
        assert(shape != .unknown);

        if (rc.term.cursor_shape == shape) return;

        try rc.writeExt("Ss", .{@intFromEnum(shape)});
        rc.term.cursor_shape = shape;
    }
};

pub fn setCursorShape(term: *Term, w: *std.io.Writer, shape: CursorShape) !void {
    assert(shape != .unknown);
    if (term.cursor_shape == shape) return;

    try term.terminfo.writeExt(w, "Ss", .{@intFromEnum(shape)});
    term.cursor_shape = shape;
}
