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

pub const version = "0.1.0";

pub const spells = @import("spells.zig");

pub const Style = @import("Style.zig");
pub const Term = @import("Term.zig");
pub const inputParser = @import("input.zig").inputParser;
pub const Input = @import("input.zig").Input;
pub const InputContent = @import("input.zig").InputContent;

pub const terminalCellWriter = @import("terminal_cell_writer.zig").terminalCellWriter;
pub const terminal_cell_writer = @import("terminal_cell_writer.zig");
pub const TerminalCellWriter = @import("terminal_cell_writer.zig").TerminalCellWriter;
pub const TermInfo = @import("TermInfo.zig");
pub const InputMap = @import("input.zig").InputMap;

pub const WidthStrategy = terminal_cell_writer.WidthStrategy;

const utf8 = @import("grapheme").utf8;
const wcWidth = @import("wcwidth").wcWidth;

pub fn graphemeWidth(bytes: []const u8, width_strategy: WidthStrategy) u32 {
    switch (width_strategy) {
        .legacy => {
            var iter: utf8.Iterator = .{ .bytes = bytes };
            var width: u32 = 0;
            while (iter.nextCodepoint()) |cp|
                width += wcWidth(cp);
            return width;
        },
        .mode_2027 => return terminal_cell_writer.graphemeWidth2027(bytes),
    }
}

test {
    @import("std").testing.refAllDeclsRecursive(@This());
}
