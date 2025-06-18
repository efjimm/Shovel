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

const wcWidth = @import("wcwidth").wcWidth;
const zg = @import("zg");

pub const Input = @import("input.zig").Input;
pub const InputContent = @import("input.zig").InputContent;
pub const InputMap = @import("input.zig").InputMap;
pub const inputParser = @import("input.zig").inputParser;
pub const spells = @import("spells.zig");
pub const Style = @import("Style.zig");
pub const Term = @import("Term.zig");
pub const terminal_cell_writer = @import("terminal_cell_writer.zig");
pub const terminalCellWriter = @import("terminal_cell_writer.zig").terminalCellWriter;
pub const TerminalCellWriter = @import("terminal_cell_writer.zig").TerminalCellWriter;
pub const TermInfo = @import("TermInfo.zig");

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
                width += wcWidth(cp);
            return width;
        },
        .grapheme => return terminal_cell_writer.graphemeWidth2027(bytes),
    }
}

test {
    @import("std").testing.refAllDeclsRecursive(@This());
}
