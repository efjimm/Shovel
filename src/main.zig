// Copyright © 2022 Leon Henrik Plickat
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

const zg = @import("zg");

pub const Input = @import("input.zig").Input;
pub const InputContent = @import("input.zig").InputContent;
pub const InputMap = @import("input.zig").InputMap;
pub const inputParser = @import("input.zig").inputParser;
pub const spells = @import("spells.zig");
pub const Style = @import("Style.zig");
pub const Term = @import("Term.zig");
pub const TerminalCellWriter = @import("TerminalCellWriter.zig");
pub const TermInfo = @import("TermInfo.zig");
const wcWidth = @import("util.zig").wcWidth;

pub const GraphemeClusteringMode = enum {
    codepoint,
    grapheme,
};

pub fn initUnicodeData(allocator: std.mem.Allocator) !void {
    try zg.initData(allocator, &.{ .graphemes, .display_width });
}

pub fn deinitUnicodeData(allocator: std.mem.Allocator) void {
    zg.deinitData(allocator, &.{ .graphemes, .display_width });
}

pub fn graphemeWidth(bytes: []const u8, mode: GraphemeClusteringMode) u32 {
    switch (mode) {
        .codepoint => {
            var iter: std.unicode.Utf8Iterator = .{ .bytes = bytes, .i = 0 };
            var width: u32 = 0;
            while (iter.nextCodepoint()) |cp|
                width += @import("util.zig").wcWidth(cp);
            return width;
        },
        .grapheme => return @intCast(zg.display_width.strWidth(bytes, .{}).width),
    }
}

pub const TextAlignment = enum { left, right, center };

pub fn getLeftRightPadding(pad: u32, alignment: TextAlignment) [2]u32 {
    return switch (alignment) {
        .left => .{ 0, pad },
        .right => .{ pad, 0 },
        .center => .{ pad / 2, pad - (pad / 2) },
    };
}

/// Write a single string, truncating above `max_width`. For formatted printing or incremental
/// writes see `terminal_cell_writer`.
pub fn writeTruncating(
    str: []const u8,
    max_width: u32,
    alignment: TextAlignment,
    writer: *std.io.Writer,
) !void {
    const res = zg.display_width.strWidth(str, .{ .max_width = max_width });
    const width: u32 = @intCast(res.width);
    if (res.len == str.len) {
        const pad = max_width - width;
        const left, const right = getLeftRightPadding(pad, alignment);

        try writer.splatByteAll(' ', left);
        try writer.writeAll(str);
        try writer.splatByteAll(' ', right);
        return;
    }

    const truncated = str[0..res.len];
    if (width == max_width) {
        var iter = zg.graphemes.reverseIterator(truncated);
        const g = iter.prev().?;
        try writer.print("{s}…", .{truncated[0..g.offset]});
        return;
    }

    const pad = max_width - width - 1;
    const left, const right = getLeftRightPadding(pad, alignment);
    try writer.splatByteAll(' ', left);
    try writer.print("{s}…", .{truncated});
    try writer.splatByteAll(' ', right);
}

test {
    @import("std").testing.refAllDeclsRecursive(@This());
}
