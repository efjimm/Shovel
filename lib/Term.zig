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
const ascii = std.ascii;
const io = std.io;
const mem = std.mem;
const os = std.os;
const unicode = std.unicode;
const debug = std.debug;
const math = std.math;

// Workaround for bad libc integration of zigs std.
const constants = if (builtin.link_libc and builtin.os.tag == .linux) os.linux else os.system;

const Style = @import("Style.zig");
const spells = @import("spells.zig");
const rpw = @import("restricted_padding_writer.zig");

const Self = @This();

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

/// Dumb writer. Don't use.
const Writer = io.Writer(os.fd_t, os.WriteError, os.write);
fn writer(self: Self) Writer {
	return .{ .context = self.tty };
}

/// Buffered writer. Use.
const BufferedWriter = io.BufferedWriter(4096, Writer);
fn bufferedWriter(self: Self) BufferedWriter {
	return io.bufferedWriter(self.writer());
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

pub fn init(term_config: TermConfig) InitError!Self {
	const ret = Self{
		.tty = os.open(term_config.tty_name, constants.O.RDWR, 0) catch |err| switch (err) {
			// None of these are reachable with the flags we pass to os.open
			error.DeviceBusy,
			error.FileLocksNotSupported,
			error.InvalidHandle,
			error.NoSpaceLeft,
			error.NotDir,
			error.PathAlreadyExists,
			error.WouldBlock,
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
				=> return @errSetCast(InitError, err), // Always safe cast
		},
	};
	errdefer os.close(ret.tty);

	if (!os.isatty(ret.tty))
		return error.NotATerminal;

	return ret;
}

pub fn deinit(self: *Self) void {
	debug.assert(!self.currently_rendering);

	// It's probably a good idea to cook the terminal on exit.
	if (!self.isCooked())
		self.cook() catch {};

	os.close(self.tty);
	self.* = undefined;
}

const SetBlockingReadError = os.TermiosGetError || os.TermiosSetError;

pub fn setBlockingRead(self: Self, enabled: bool) SetBlockingReadError!void {
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

pub fn readInput(self: *Self, buffer: []u8) os.ReadError!usize {
	debug.assert(!self.currently_rendering);
	debug.assert(!self.isCooked());
	return try os.read(self.tty, buffer);
}

pub inline fn isCooked(self: Self) bool {
	return self.cooked_termios == null;
}

pub const UncookError = os.TermiosGetError || os.TermiosSetError || os.WriteError;

/// Enter raw mode.
pub fn uncook(self: *Self, options: UncookOptions) UncookError!void {
	if (!self.isCooked())
		return;

	// The information on the various flags and escape sequences is pieced
	// together from various sources, including termios(3) and
	// https://viewsourcecode.org/snaptoken/kilo/02.enteringRawMode.html.
	// TODO: IUTF8 ?

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

		// Disable output processing. Common output processing includes prefixing
		// newline with a carriage return.
		raw.oflag &= ~@as(constants.tcflag_t, constants.OPOST);

		// Set the character size to 8 bits per byte. Likely has no efffect on
		// anything remotely modern.
		raw.cflag |= constants.CS8;

		break :blk raw;
	};

	try os.tcsetattr(self.tty, .FLUSH, raw_termios);

	var buffered_writer = self.bufferedWriter();
	const _writer = buffered_writer.writer();
	try _writer.writeAll(
		spells.save_cursor_position ++
			spells.save_cursor_position ++
			spells.enter_alt_buffer ++
			spells.overwrite_mode ++
			spells.reset_auto_wrap ++
			spells.reset_auto_repeat ++
			spells.reset_auto_interlace ++
			spells.hide_cursor,
	);
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
pub fn cook(self: *Self) CookError!void {
	if (self.isCooked())
		return;

	var buffered_writer = self.bufferedWriter();
	const _writer = buffered_writer.writer();
	try _writer.writeAll(
		// Even if we did not request the kitty keyboard protocol or mouse
		// tracking, asking the terminal to disable it should have no effect.
		spells.disable_kitty_keyboard ++
			spells.disable_mouse_tracking ++
			spells.clear ++
			spells.leave_alt_buffer ++
			spells.restore_screen ++
			spells.restore_cursor_position ++
			spells.show_cursor ++
			spells.reset_attributes ++
			spells.reset_attributes,
	);
	try buffered_writer.flush();

	try os.tcsetattr(self.tty, .FLUSH, self.cooked_termios.?);
	self.cooked_termios = null;
}

pub fn fetchSize(self: *Self) os.UnexpectedError!void {
	if (self.isCooked())
		return;

	var size = mem.zeroes(constants.winsize);
	const err = os.system.ioctl(self.tty, constants.T.IOCGWINSZ, @ptrToInt(&size));
	if (os.errno(err) != .SUCCESS) {
		return os.unexpectedErrno(@intToEnum(os.system.E, err));
	}
	self.height = size.ws_row;
	self.width = size.ws_col;
}

/// Set window title using OSC 2. Shall not be called while rendering.
pub fn setWindowTitle(self: *Self, comptime fmt: []const u8, args: anytype) !void {
	debug.assert(!self.currently_rendering);
	const _writer = self.writer();
	try _writer.print("\x1b]2;" ++ fmt ++ "\x1b\\", args);
}

pub fn getRenderContextSafe(self: *Self) !?RenderContext {
	if (self.currently_rendering or self.isCooked())
		return null;

	self.currently_rendering = true;
	errdefer self.currently_rendering = false;

	var rc = RenderContext{
		.term = self,
		.buffer = self.bufferedWriter(),
	};

	const _writer = rc.buffer.writer();
	try _writer.writeAll(spells.start_sync);
	try _writer.writeAll(spells.reset_attributes);

	return rc;
}

pub fn getRenderContext(self: *Self) !RenderContext {
	debug.assert(!self.currently_rendering);
	debug.assert(!self.isCooked());
	return (try self.getRenderContextSafe()) orelse unreachable;
}

pub const RenderContext = struct {
	term: *Self,
	buffer: BufferedWriter,

	const RestrictedPaddingWriter = rpw.RestrictedPaddingWriter(BufferedWriter.Writer);

	/// Finishes the render operation. The render context may not be used any
	/// further.
	pub fn done(rc: *RenderContext) !void {
		debug.assert(rc.term.currently_rendering);
		debug.assert(!rc.term.isCooked());
		defer rc.term.currently_rendering = false;
		const _writer = rc.buffer.writer();
		try _writer.writeAll(spells.end_sync);
		try rc.buffer.flush();
	}

	/// Clears all content.
	pub fn clear(rc: *RenderContext) !void {
		debug.assert(rc.term.currently_rendering);
		const _writer = rc.buffer.writer();
		try _writer.writeAll(spells.clear);
	}

	/// Move the cursor to the specified cell.
	pub fn moveCursorTo(rc: *RenderContext, row: u16, col: u16) !void {
		debug.assert(rc.term.currently_rendering);
		const _writer = rc.buffer.writer();
		try _writer.print(spells.move_cursor_fmt, .{ row + 1, col + 1 });
	}

	/// Hide the cursor.
	pub fn hideCursor(rc: *RenderContext) !void {
		debug.assert(rc.term.currently_rendering);
		const _writer = rc.buffer.writer();
		try _writer.writeAll(spells.hide_cursor);
	}

	/// Show the cursor.
	pub fn showCursor(rc: *RenderContext) !void {
		debug.assert(rc.term.currently_rendering);
		const _writer = rc.buffer.writer();
		try _writer.writeAll(spells.show_cursor);
	}

	/// Set the text attributes for all following writes.
	pub fn setStyle(rc: *RenderContext, attr: Style) !void {
		debug.assert(rc.term.currently_rendering);
		const _writer = rc.buffer.writer();
		try attr.dump(_writer);
	}

	pub fn restrictedPaddingWriter(rc: *RenderContext, width: u16) RestrictedPaddingWriter {
		debug.assert(rc.term.currently_rendering);
		return rpw.restrictedPaddingWriter(rc.buffer.writer(), width);
	}

	/// Write all bytes, wrapping at the end of the line.
	pub fn writeAllWrapping(rc: *RenderContext, bytes: []const u8) !void {
		debug.assert(rc.term.currently_rendering);
		const _writer = rc.buffer.writer();
		try _writer.writeAll(spells.enable_auto_wrap);
		try _writer.writeAll(bytes);
		try _writer.writeAll(spells.reset_auto_wrap);
	}
};
