// This file is part of zig-spoon, a TUI library for the zig language.
//
// Copyright © 2021 - 2022 Leon Henrik Plickat
// Copyright © 2023 Evan Bonner
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

const builtin = @import("builtin");
const std = @import("std");
const log = @import("log.zig");
const Allocator = std.mem.Allocator;
const ascii = std.ascii;
const io = std.io;
const mem = std.mem;
const os = std.os;
const unicode = std.unicode;
const debug = std.debug;
const math = std.math;
const assert = debug.assert;

// Workaround for bad libc integration of zigs std.
const constants = if (builtin.link_libc and builtin.os.tag == .linux) os.linux else os.system;

const Style = @import("Style.zig");
const spells = @import("spells.zig");
const rpw = @import("restricted_padding_writer.zig");
const TermInfo = @import("terminfo.zig");

const Term = @This();

const UncookOptions = struct {
    request_kitty_keyboard_protocol: bool = true,
    request_mouse_tracking: bool = false,
};

const TermConfig = struct {
    tty_name: []const u8 = "/dev/tty",
};

/// The original termios configuration saved when entering raw mode. null if in cooked mode,
/// otherwise we are uncooked.
cooked_termios: ?os.termios = null,

/// Size of the terminal, updated when `fetchSize()` is called.
width: u16 = 0,
height: u16 = 0,

/// Are we currently rendering?
currently_rendering: bool = false,

/// Descriptor of opened file.
tty: os.fd_t,

cursor_visible: bool = true,
cursor_shape: CursorShape = .unknown,

codepoint: [4]u8 = undefined,
codepoint_len: u3 = 0,

terminfo: ?*TermInfo = null,

pub const CursorShape = spells.CursorShape;

pub const WriteError = os.WriteError;

/// Dumb writer. Don't use.
const Writer = io.Writer(os.fd_t, os.WriteError, os.write);
fn writer(self: Term) Writer {
    return .{ .context = self.tty };
}

/// Buffered writer. Use.
fn bufferedWriter(
    self: Term,
    comptime buffer_size: usize,
) io.BufferedWriter(buffer_size, Writer) {
    return io.BufferedWriter(buffer_size, Writer){
        .unbuffered_writer = self.writer(),
    };
}

// NotATerminal + a subset of os.OpenError, removing all errors which aren't reachable due to how
// we call os.open
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
    Unexpected,
};

pub fn init(allocator: Allocator, term_config: TermConfig) (InitError || Allocator.Error)!Term {
    var ret = try initInternal(term_config);
    errdefer ret.deinit(allocator);

    ret.terminfo = getTermInfo(allocator) catch |err| switch (err) {
        error.OutOfMemory => |e| return e,
        error.NoTermInfo => blk: {
            log.warn("Proceeding without terminfo definitions", .{});
            break :blk null;
        },
    };

    return ret;
}

pub fn initNoTermInfo(term_config: TermConfig) InitError!Term {
    const ret = try initInternal(term_config);
    log.info("spoon initialized without terminfo definitions", .{});
    return ret;
}

fn initInternal(term_config: TermConfig) InitError!Term {
    const ret = Term{
        .tty = os.open(term_config.tty_name, constants.O.RDWR, 0) catch |err| switch (err) {
            // None of these are reachable with the flags we pass to os.open
            error.DeviceBusy,
            error.FileLocksNotSupported,
            error.InvalidHandle,
            error.NoSpaceLeft,
            error.NotDir,
            error.PathAlreadyExists,
            error.WouldBlock,
            error.NetworkNotFound,
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
            error.Unexpected,
            => |new_err| return new_err,
        },
        .terminfo = null,
    };
    errdefer os.close(ret.tty);

    if (!os.isatty(ret.tty))
        return error.NotATerminal;

    return ret;
}

fn getTermInfo(allocator: Allocator) !*TermInfo {
    const term_var = std.os.getenv("TERM") orelse {
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

pub fn deinit(self: *Term, allocator: Allocator) void {
    assert(!self.currently_rendering);

    // It's probably a good idea to cook the terminal on exit.
    if (!self.isCooked())
        self.cook() catch {};

    if (self.terminfo) |ti|
        ti.destroy(allocator);

    os.close(self.tty);
    self.* = undefined;
}

const SetBlockingReadError = os.TermiosGetError || os.TermiosSetError;

pub fn setBlockingRead(self: Term, enabled: bool) SetBlockingReadError!void {
    const termios = blk: {
        var raw = try os.tcgetattr(self.tty);

        if (enabled) {
            raw.cc[constants.V.TIME] = 0;
            raw.cc[constants.V.MIN] = 1;
        } else {
            raw.cc[constants.V.TIME] = 0;
            raw.cc[constants.V.MIN] = 0;
        }

        break :blk raw;
    };

    try os.tcsetattr(self.tty, .FLUSH, termios);
}

// Reads from stdin to the supplied buffer. Asserts that `buf.len >= 8`.
pub fn readInput(self: *Term, buf: []u8) ![]u8 {
    assert(buf.len >= 8); // Ensures that at least one full escape sequence can be handled
    assert(!self.currently_rendering);
    assert(!self.isCooked());

    // If we have a partial codepoint from the last read, append it to the buffer
    const buffer = blk: {
        const len = self.codepoint_len;
        if (len > 0) {
            @memcpy(buf[0..len], self.codepoint[0..len]);
            self.codepoint_len = 0;
            break :blk buf[len..];
        }
        break :blk buf;
    };

    // Use system.read instead of os.read so it won't restart on signals.
    const rc = os.system.read(self.tty, buffer.ptr, buffer.len);

    const bytes_read: usize = switch (os.errno(rc)) {
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
        else => |err| return os.unexpectedErrno(err),
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
        @memcpy(self.codepoint[0..len], buffer[i..]);
        self.codepoint_len = @intCast(len);
        // Return the buffer without the trailing unfinished codepoint
        return buffer[0..i];
    }

    return slice;
}

pub fn getExtendedFlag(self: *const Term, name: []const u8) bool {
    return if (self.terminfo) |ti|
        ti.getExtendedFlag(name)
    else
        null;
}

pub fn getExtendedNumber(self: *const Term, name: []const u8) ?u31 {
    return if (self.terminfo) |ti|
        ti.getExtendedNumber(name)
    else
        null;
}

pub fn getExtendedString(self: *const Term, name: []const u8) ?[:0]const u8 {
    return if (self.terminfo) |ti|
        ti.getExtendedString(name)
    else
        null;
}

pub fn getFlagCapability(
    self: *const Term,
    comptime tag: TermInfo.FlagTag,
) bool {
    return if (self.terminfo) |ti|
        ti.getFlagCapability(tag)
    else
        null;
}

pub fn getNumberCapability(
    self: *const Term,
    comptime tag: TermInfo.NumberTag,
) ?u31 {
    return if (self.terminfo) |ti|
        ti.getNumberCapability(tag)
    else
        null;
}

pub fn getStringCapability(
    self: *const Term,
    comptime tag: TermInfo.StringTag,
) ?[:0]const u8 {
    return if (self.terminfo) |ti|
        ti.getStringCapability(tag)
    else
        null;
}

pub inline fn isCooked(self: *const Term) bool {
    return self.cooked_termios == null;
}

pub const UncookError = os.TermiosGetError || os.TermiosSetError || os.WriteError;

/// Enter raw mode.
pub fn uncook(self: *Term, options: UncookOptions) UncookError!void {
    if (!self.isCooked())
        return;

    // The information on the various flags and escape sequences is pieced
    // together from various sources, including termios(3) and
    // https://viewsourcecode.org/snaptoken/kilo/02.enteringRawMode.html.

    self.cooked_termios = try os.tcgetattr(self.tty);
    errdefer self.cook() catch {};

    const raw_termios = blk: {
        var raw = self.cooked_termios.?;

        //   ECHO: Stop the terminal from displaying pressed keys.
        // ICANON: Disable canonical ("cooked") mode. Allows us to read inputs
        //		 byte-wise instead of line-wise.
        //   ISIG: Disable signals for Ctrl-C (SIGINT) and Ctrl-Z (SIGTSTP), so we
        //		 can handle them as normal escape sequences.
        // IEXTEN: Disable input preprocessing. This allows us to handle Ctrl-V,
        //		 which would otherwise be intercepted by some terminals.
        raw.lflag &= ~@as(
            constants.tcflag_t,
            constants.ECHO | constants.ICANON | constants.ISIG | constants.IEXTEN,
        );

        //   IXON: Disable software control flow. This allows us to handle Ctrl-S
        //		 and Ctrl-Q.
        //  ICRNL: Disable converting carriage returns to newlines. Allows us to
        //		 handle Ctrl-J and Ctrl-M.
        // BRKINT: Disable converting sending SIGINT on break conditions. Likely has
        //		 no effect on anything remotely modern.
        //  INPCK: Disable parity checking. Likely has no effect on anything
        //		 remotely modern.
        // ISTRIP: Disable stripping the 8th bit of characters. Likely has no effect
        //		 on anything remotely modern.
        raw.iflag &= ~@as(
            constants.tcflag_t,
            constants.IXON | constants.ICRNL | constants.BRKINT | constants.INPCK | constants.ISTRIP,
        );

        // IUTF8: (Linux only)
        if (builtin.os.tag == .linux)
            raw.iflag &= ~@as(constants.tcflag_t, constants.IUTF8);

        // Disable output processing. Common output processing includes prefixing
        // newline with a carriage return.
        raw.oflag &= ~@as(constants.tcflag_t, constants.OPOST);

        // Set the character size to 8 bits per byte. Likely has no efffect on
        // anything remotely modern.
        raw.cflag |= constants.CS8;

        break :blk raw;
    };

    try os.tcsetattr(self.tty, .FLUSH, raw_termios);

    var buffered_writer = self.bufferedWriter(256);
    const _writer = buffered_writer.writer();
    inline for (.{
        self.getStringCapability(.save_cursor) orelse spells.save_cursor_position,
        self.getStringCapability(.enter_ca_mode) orelse spells.enter_alt_buffer,
        self.getStringCapability(.exit_insert_mode) orelse spells.overwrite_mode,
        self.getStringCapability(.exit_am_mode) orelse spells.reset_auto_wrap,
        self.getStringCapability(.cursor_invisible) orelse spells.hide_cursor,
    }) |str| try _writer.writeAll(str);

    if (options.request_kitty_keyboard_protocol) {
        try _writer.writeAll(spells.enable_kitty_keyboard);
    }
    if (options.request_mouse_tracking) {
        try _writer.writeAll(spells.enable_mouse_tracking);
    }
    try buffered_writer.flush();
}

pub const CookError = os.WriteError || os.TermiosSetError;

/// Enter cooked mode.
pub fn cook(self: *Term) CookError!void {
    if (self.isCooked())
        return;

    try os.tcsetattr(self.tty, .FLUSH, self.cooked_termios.?);
    self.cooked_termios = null;

    var buffered_writer = self.bufferedWriter(128);
    const _writer = buffered_writer.writer();
    inline for (.{
        spells.disable_kitty_keyboard,
        spells.disable_mouse_tracking,
        self.getStringCapability(.clear_screen) orelse "",
        self.getStringCapability(.exit_ca_mode) orelse "",
        self.getStringCapability(.cursor_visible) orelse "",
        self.getStringCapability(.exit_attribute_mode) orelse "",
    }) |str| try _writer.writeAll(str);
    try buffered_writer.flush();
}

pub fn fetchSize(self: *Term) os.UnexpectedError!void {
    if (self.isCooked())
        return;

    var size = mem.zeroes(constants.winsize);
    const err = os.system.ioctl(self.tty, constants.T.IOCGWINSZ, @intFromPtr(&size));
    if (os.errno(err) != .SUCCESS) {
        return os.unexpectedErrno(@enumFromInt(err));
    }
    self.height = size.ws_row;
    self.width = size.ws_col;
}

/// Set window title using OSC 2. Shall not be called while rendering.
pub fn setWindowTitle(self: *Term, comptime fmt: []const u8, args: anytype) WriteError!void {
    assert(!self.currently_rendering);
    const _writer = self.writer();
    try _writer.print("\x1b]2;" ++ fmt ++ "\x1b\\", args);
}

pub fn getRenderContextSafe(
    self: *Term,
    comptime buffer_size: usize,
) WriteError!?RenderContext(buffer_size) {
    if (self.currently_rendering or self.isCooked())
        return null;

    self.currently_rendering = true;
    errdefer self.currently_rendering = false;

    var rc = RenderContext(buffer_size){
        .term = self,
        .buffer = self.bufferedWriter(buffer_size),
    };

    const _writer = rc.buffer.writer();
    if (rc.term.getExtendedString("Sync")) |sync| {
        try TermInfo.writeParamSequence(sync, _writer, .{1});
    }
    if (self.getStringCapability(.exit_attribute_mode)) |srg0|
        try _writer.writeAll(srg0);

    return rc;
}

pub fn getRenderContext(
    self: *Term,
    comptime buffer_size: usize,
) WriteError!RenderContext(buffer_size) {
    assert(!self.currently_rendering);
    assert(!self.isCooked());
    return (try self.getRenderContextSafe(buffer_size)) orelse unreachable;
}

pub fn RenderContext(comptime buffer_size: usize) type {
    return struct {
        term: *Term,
        buffer: BufferedWriter,

        const Self = @This();
        const BufferedWriter = io.BufferedWriter(buffer_size, Writer);
        const RestrictedPaddingWriter = rpw.RestrictedPaddingWriter(BufferedWriter.Writer);

        /// Finishes the render operation. The render context may not be used any
        /// further.
        pub fn done(rc: *Self) WriteError!void {
            assert(rc.term.currently_rendering);
            assert(!rc.term.isCooked());
            defer rc.term.currently_rendering = false;
            const _writer = rc.buffer.writer();
            if (rc.term.getExtendedString("Sync")) |sync| {
                try TermInfo.writeParamSequence(sync, _writer, .{2});
            }
            try rc.buffer.flush();
        }

        /// Clears all content. Avoid calling often, as fully clearing and redrawing the screen can
        /// cause flicker on some terminals (such as the Linux tty). Prefer more granular clearing
        /// functions like `clearToEol` and `clearToBot`.
        pub fn clear(rc: *Self) WriteError!void {
            assert(rc.term.currently_rendering);
            const _writer = rc.buffer.writer();
            try _writer.writeAll(rc.term.getStringCapability(.clear_screen) orelse spells.clear);
        }

        /// Clears the screen from the current line to the bottom.
        pub fn clearToBot(rc: *Self) WriteError!void {
            assert(rc.term.curerntly_rendering);
            const _writer = rc.buffer.writer();
            try _writer.writeAll(rc.term.getStringCapability(.clr_eos) orelse spells.clear_to_bot);
        }

        /// Clears from the cursor position to the end of the line.
        pub fn clearToEol(rc: *Self) WriteError!void {
            assert(rc.term.currently_rendering);
            const _writer = rc.buffer.writer();
            const spell = rc.term.getStringCapability(.clr_eol) orelse spells.clear_to_eol;
            try _writer.writeAll(spell);
        }

        /// Clears the screen from the cursor to the beginning of the line.
        pub fn clearToBol(rc: *Self) WriteError!void {
            assert(rc.term.currently_rendering);
            const _writer = rc.buffer.writer();
            const spell = rc.term.getStringCapability(.clr_bol) orelse spells.clear_to_bol;
            try _writer.writeAll(spell);
        }

        /// Move the cursor to the specified cell.
        pub fn moveCursorTo(rc: *Self, row: u16, col: u16) WriteError!void {
            assert(rc.term.currently_rendering);
            const _writer = rc.buffer.writer();
            const spell = rc.term.getStringCapability(.cursor_address) orelse spells.move_cursor_fmt;
            try TermInfo.writeParamSequence(spell, _writer, .{ row, col });
        }

        /// Hide the cursor.
        pub fn hideCursor(rc: *Self) WriteError!void {
            assert(rc.term.currently_rendering);
            if (!rc.term.cursor_visible) return;
            const _writer = rc.buffer.writer();
            const spell = rc.term.getStringCapability(.cursor_invisible) orelse spells.hide_cursor;
            try _writer.writeAll(spell);
            rc.term.cursor_visible = false;
        }

        /// Show the cursor.
        pub fn showCursor(rc: *Self) WriteError!void {
            assert(rc.term.currently_rendering);
            if (rc.term.cursor_visible) return;
            const _writer = rc.buffer.writer();
            const spell = rc.term.getStringCapability(.cursor_normal) orelse spells.show_cursor;
            try _writer.writeAll(spell);
            rc.term.cursor_visible = true;
        }

        /// Set the text attributes for all following writes.
        pub fn setStyle(rc: *Self, attr: Style) WriteError!void {
            assert(rc.term.currently_rendering);
            const _writer = rc.buffer.writer();
            try attr.dump(rc.term.terminfo, _writer);
        }

        pub fn restrictedPaddingWriter(rc: *Self, width: u16) RestrictedPaddingWriter {
            assert(rc.term.currently_rendering);
            return rpw.restrictedPaddingWriter(rc.buffer.writer(), width);
        }

        /// Write all bytes, wrapping at the end of the line.
        pub fn writeAllWrapping(rc: *Self, bytes: []const u8) WriteError!void {
            assert(rc.term.currently_rendering);
            const _writer = rc.buffer.writer();
            const enable = rc.term.getStringCapability(.enter_am_mode) orelse spells.enable_auto_wrap;
            const disable = rc.term.getStringCapability(.exit_am_mode) orelse spells.reset_auto_wrap;
            try _writer.writeAll(enable);
            try _writer.writeAll(bytes);
            try _writer.writeAll(disable);
        }

        pub fn setCursorShape(rc: *Self, shape: CursorShape) WriteError!void {
            assert(rc.term.currently_rendering);
            assert(shape != .unknown);

            if (rc.term.cursor_shape == shape) return;
            const _writer = rc.buffer.writer();

            if (rc.term.terminfo.?.getExtendedString("Ss")) |ss| {
                try TermInfo.writeParamSequence(ss, _writer, .{@intFromEnum(shape)});
            }
            rc.term.cursor_shape = shape;
        }
    };
}
