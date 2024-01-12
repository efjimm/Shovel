const std = @import("std");
const wcWidth = @import("wcwidth").wcWidth;
const io = std.io;
const assert = std.debug.assert;

pub fn restrictedPaddingWriter(writer: anytype, width: u32) RestrictedPaddingWriter(@TypeOf(writer)) {
    assert(width > 0);
    return .{
        .underlying_writer = writer,
        .width_left = width,
    };
}

pub fn RestrictedPaddingWriter(comptime UnderlyingWriter: type) type {
    return struct {
        underlying_writer: UnderlyingWriter,

        width_left: u32,
        buf: union(enum) {
            codepoint: u21,
            byte: u8,
            none,
        } = .none,
        finished: bool = false,

        const Self = @This();
        const WriteError = UnderlyingWriter.Error;
        const Writer = std.io.Writer(*Self, WriteError, write);

        pub fn writer(self: *Self) Writer {
            return .{
                .context = self,
            };
        }

        pub fn write(self: *Self, bytes: []const u8) WriteError!usize {
            if (self.finished) {
                if (self.buf != .none) {
                    try self.underlying_writer.writeAll("…");
                    self.buf = .none;
                }
                return bytes.len;
            }

            var i: usize = 0;
            while (i < bytes.len) {
                if (std.ascii.isControl(bytes[i])) {
                    try self.writeByte(bytes[i], i == bytes.len - 1);
                    i += 1;
                    continue;
                }

                const cp_len = std.unicode.utf8ByteSequenceLength(bytes[i]) catch {
                    try self.writeByte(bytes[i], i == bytes.len - 1);
                    i += 1;
                    continue;
                };

                if (i + cp_len > bytes.len) {
                    try self.writeByte(bytes[i], i == bytes.len - 1);
                    i += 1;
                    continue;
                }

                const cp = std.unicode.utf8Decode(bytes[i..][0..cp_len]) catch {
                    try self.writeByte(bytes[i], i == bytes.len - 1);
                    i += 1;
                    continue;
                };

                const width = wcWidth(cp);
                if (width == self.width_left) {
                    self.finished = true;
                    if (i + cp_len >= bytes.len) {
                        // Have no more input after this - buffer the codepoint
                        self.buf = .{ .codepoint = cp };
                        self.width_left -= width;
                    } else {
                        // Have more input after this - truncate
                        self.buf = .none;
                        self.width_left -= 1;
                        try self.underlying_writer.writeAll("…");
                    }

                    break;
                } else if (width > self.width_left) {
                    assert(self.width_left > 0);
                    self.buf = .none;
                    self.width_left -= 1;
                    self.finished = true;
                    try self.underlying_writer.writeAll("…");
                    break;
                }

                try self.underlying_writer.writeAll(bytes[i..][0..cp_len]);
                self.width_left -= width;

                i += cp_len;
            }

            return bytes.len;
        }

        pub fn finish(self: *Self) WriteError!void {
            switch (self.buf) {
                .codepoint => |cp| try std.fmt.formatUnicodeCodepoint(cp, .{}, self.underlying_writer),
                .byte => |b| try self.underlying_writer.print("{}", .{
                    std.fmt.fmtSliceEscapeUpper(&.{b}),
                }),
                .none => {},
            }
        }

        pub fn pad(self: *Self) WriteError!void {
            try self.finish();
            try self.underlying_writer.writeByteNTimes(' ', self.width_left);
        }

        fn writeByte(self: *Self, byte: u8, last: bool) WriteError!void {
            if (std.ascii.isPrint(byte)) {
                return self.underlying_writer.writeByte(byte);
            }

            if (self.width_left == 3) {
                self.finished = true;
                if (last) {
                    self.buf = .{ .byte = byte };
                    self.width_left -= 3;
                } else {
                    try self.underlying_writer.writeAll("…");
                    self.buf = .none;
                    self.width_left -= 1;
                }
                return;
            }

            if (self.width_left < 3) {
                try self.underlying_writer.writeAll("…");
                self.buf = .none;
                self.width_left -= 1;
                self.finished = true;
                return;
            }

            try self.underlying_writer.print("{}", .{
                std.fmt.fmtSliceEscapeUpper(&.{byte}),
            });
            self.width_left -= 3;
        }
    };
}

fn testWriter(
    comptime finish_type: enum { pad, finish },
    width: u32,
    input: []const u8,
    expected: []const u8,
) !void {
    var buf = std.BoundedArray(u8, 128){};
    var rpw = restrictedPaddingWriter(buf.writer(), width);
    try rpw.writer().writeAll(input);
    switch (finish_type) {
        .finish => try rpw.finish(),
        .pad => try rpw.pad(),
    }
    try std.testing.expectEqualStrings(expected, buf.slice());
}

test "RestrictedPaddingWriter" {
    const data = .{
        .{ .finish, 8, "12345678", "12345678" },
        .{ .finish, 7, "12345678", "123456…" },
        .{ .pad, 20, "12345678", "12345678            " },
        .{ .pad, 10, "漢字漢字漢字漢字", "漢字漢字… " },
        .{ .pad, 16, "漢字漢字漢字漢字", "漢字漢字漢字漢字" },
        .{ .pad, 20, "漢字漢字漢字漢字", "漢字漢字漢字漢字    " },
        .{ .pad, 6, "漢字漢字", "漢字… " },
        .{ .pad, 2, "\x01", "… " },
        .{ .finish, 3, "\x01", "\\x01" },
        .{ .pad, 5, "\xff", "\\xFF  " },
    };

    inline for (data) |d| {
        const end, const width, const input, const expected = d;
        try testWriter(end, width, input, expected);
    }
}
