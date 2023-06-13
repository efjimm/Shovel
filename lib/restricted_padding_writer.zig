const std = @import("std");
const wcWidth = @import("wcwidth").wcWidth;
const io = std.io;
const assert = std.debug.assert;

pub fn restrictedPaddingWriter(writer: anytype, width: u32) RestrictedPaddingWriter(@TypeOf(writer)) {
    return .{
        .underlying_writer = writer,
        .width_left = width,
    };
}

pub fn RestrictedPaddingWriter(comptime UnderlyingWriter: type) type {
    return struct {
        underlying_writer: UnderlyingWriter,

        width_left: u32,
        codepoint_buf: ?u21 = null,
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
                if (self.codepoint_buf) |_| {
                    self.codepoint_buf = null;
                    try self.underlying_writer.writeAll("…");
                }
                return bytes.len;
            }

            var iter = std.unicode.Utf8Iterator{
                .bytes = bytes,
                .i = 0,
            };

            while (iter.nextCodepointSlice()) |slice| {
                const cp = std.unicode.utf8Decode(slice) catch unreachable;
                const width = wcWidth(cp);
                if (width == self.width_left) {
                    if (iter.i >= bytes.len) {
                        // Have no more input after this - buffer the codepoint
                        self.codepoint_buf = cp;
                        self.width_left = 0;
                    } else {
                        // Have more input after this - truncate
                        self.codepoint_buf = null;
                        self.width_left -= 1;
                        self.finished = true;
                        try self.underlying_writer.writeAll("…");
                    }

                    break;
                } else if (width > self.width_left) {
                    assert(self.width_left > 0);
                    self.codepoint_buf = null;
                    self.width_left -= 1;
                    self.finished = true;
                    try self.underlying_writer.writeAll("…");
                    break;
                }

                self.width_left -= width;
                try self.underlying_writer.writeAll(slice);
            }

            return bytes.len;
        }

        pub fn finish(self: *Self) WriteError!void {
            if (self.codepoint_buf) |cp| {
                try std.fmt.formatUnicodeCodepoint(cp, .{}, self.underlying_writer);
            }
        }

        pub fn pad(self: *Self) WriteError!void {
            try self.finish();
            try self.underlying_writer.writeByteNTimes(' ', self.width_left);
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
    };

    inline for (data) |d| try testWriter(d[0], d[1], d[2], d[3]);
}
