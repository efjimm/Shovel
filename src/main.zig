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

pub const cellWriter = @import("cell_writer.zig").cellWriter;
pub const CellWriter = @import("cell_writer.zig").CellWriter;
pub const TermInfo = @import("TermInfo.zig");
pub const InputMap = @import("input.zig").InputMap;

test {
    _ = @import("input.zig");
    _ = @import("input_description.zig");
    _ = @import("colour_description.zig");
    _ = @import("cell_writer.zig");
    _ = @import("TermInfo.zig");
}
