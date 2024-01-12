// This file is part of zig-spoon, a TUI library for the zig language.
//
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

pub const restrictedPaddingWriter = @import("restricted_padding_writer.zig").restrictedPaddingWriter;
pub const RestrictedPaddingWriter = @import("restricted_padding_writer.zig").RestrictedPaddingWriter;
pub const TermInfo = @import("TermInfo.zig");
pub const InputMap = @import("input.zig").InputMap;

test {
    _ = @import("input.zig");
    _ = @import("input_description.zig");
    _ = @import("colour_description.zig");
    _ = @import("restricted_padding_writer.zig");
    _ = @import("TermInfo.zig");
}
