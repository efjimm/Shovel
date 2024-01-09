// This file is part of zig-spoon, a TUI library for the zig language.
//
// Copyright © 2021 Leon Henrik Plickat
// Copyright © 2022 Hugo Machet
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
const Self = @This();
const TermInfo = @import("terminfo.zig");

pub const Colour = union(enum(u5)) {
    const colour_desc = @import("colour_description.zig");
    pub const FromDescriptionError = colour_desc.ParseError;
    pub const fromDescription = colour_desc.parseColourDescription;

    // TODO since the default colours are also part of the 256 colour spec,
    //	  maybe we should just use that. The dump function would then special
    //	  case them and use legacy escape sequences.
    none,
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    bright_black,
    bright_red,
    bright_green,
    bright_yellow,
    bright_blue,
    bright_magenta,
    bright_cyan,
    bright_white,

    @"256": u8,
    rgb: [3]u8,
};

pub const Attribute = packed struct {
    bold: bool = false,
    dimmed: bool = false,
    italic: bool = false,
    standout: bool = false,
    underline: bool = false,
    blinking: bool = false,
    reverse: bool = false,
    hidden: bool = false,
    strikethrough: bool = false,
};

fg: Colour = .none,
bg: Colour = .none,
attrs: Attribute = .{},

pub fn init(fg: Colour, bg: Colour, attrs: Attribute) Self {
    return .{
        .fg = fg,
        .bg = bg,
        .attrs = attrs,
    };
}

pub fn eql(self: Self, other: Self) bool {
    inline for (@typeInfo(Self).Struct.fields) |field| {
        if (@field(self, field.name) != @field(other, field.name)) return false;
    }
    return true;
}

/// Dumps the attributes to `writer`, using the capabilities reported by `ti`.
pub fn dump(self: Self, terminfo: ?*const TermInfo, writer: anytype) !void {
    const ti = terminfo orelse return self.dumpRaw(writer);

    if (ti.getStringCapability(.set_attributes)) |sgr| {
        try TermInfo.writeParamSequence(sgr, writer, .{
            self.attrs.standout,
            self.attrs.underline,
            self.attrs.reverse,
            self.attrs.blinking,
            self.attrs.dimmed,
            self.attrs.bold,
            self.attrs.hidden,
            false,
            false,
        });
    } else {
        // Terminfo does not define any standard capabilities to disable bold/reverse/blink/dim/
        // blank apart from sgr, so the only way to turn them off here is to reset all attributes.
        // Other attributes are disabled individually anyway in case sgr0 is not defined.
        if (ti.getStringCapability(.exit_attribute_mode)) |sgr0| try writer.writeAll(sgr0);

        if (self.attrs.standout) {
            if (ti.getStringCapability(.enter_standout_mode)) |so| try writer.writeAll(so);
        } else if (ti.getStringCapability(.exit_standout_mode)) |so| try writer.writeAll(so);

        if (self.attrs.underline) {
            if (ti.getStringCapability(.enter_underline_mode)) |ul| try writer.writeAll(ul);
        } else if (ti.getStringCapability(.exit_underline_mode)) |ul| try writer.writeAll(ul);

        if (self.attrs.reverse) {
            if (ti.getStringCapability(.enter_reverse_mode)) |rev| try writer.writeAll(rev);
        }

        if (self.attrs.blinking) {
            if (ti.getStringCapability(.enter_blink_mode)) |blink| try writer.writeAll(blink);
        }

        if (self.attrs.dimmed) {
            if (ti.getStringCapability(.enter_dim_mode)) |dim| try writer.writeAll(dim);
        }

        if (self.attrs.bold) {
            if (ti.getStringCapability(.enter_bold_mode)) |bold| try writer.writeAll(bold);
        }

        if (self.attrs.hidden) {
            if (ti.getStringCapability(.enter_secure_mode)) |invis| try writer.writeAll(invis);
        }
    }

    if (self.attrs.strikethrough) {
        if (ti.getExtendedString("smxx")) |xx| try writer.writeAll(xx);
    } else {
        if (ti.getExtendedString("rmxx")) |xx| try writer.writeAll(xx);
    }

    switch (self.fg) {
        .none => {},
        .black, .red, .green, .yellow, .blue, .magenta, .cyan, .white => {
            const n = @intFromEnum(self.fg) - 1;
            if (ti.getStringCapability(.set_a_foreground)) |setaf| {
                try TermInfo.writeParamSequence(setaf, writer, .{n});
            } else if (ti.getStringCapability(.set_foreground)) |setf| {
                // Rare case where setaf is not defined.
                // Red/blue are swapped for setf.
                const t = switch (self.fg) {
                    .cyan => .yellow,
                    .yellow => .cyan,
                    .red => .blue,
                    .blue => .red,
                    else => self.fg,
                };
                try TermInfo.writeParamSequence(setf, writer, .{@intFromEnum(t) - 1});
            } else {
                // No color support!
            }
        },
        .bright_black,
        .bright_red,
        .bright_green,
        .bright_yellow,
        .bright_blue,
        .bright_magenta,
        .bright_cyan,
        .bright_white,
        => {
            const num_colors = ti.getNumberCapability(.max_colors) orelse 8;
            if (num_colors >= 16) {
                if (ti.getStringCapability(.set_a_foreground)) |setaf| {
                    try TermInfo.writeParamSequence(setaf, writer, .{@intFromEnum(self.fg) - 1});
                }
            }
        },

        .@"256" => |value| {
            const num_colors = ti.getNumberCapability(.max_colors) orelse 8;
            if (num_colors >= 256) {
                if (ti.getStringCapability(.set_a_foreground)) |setaf| {
                    try TermInfo.writeParamSequence(setaf, writer, .{value});
                }
            }
        },
        .rgb => |rgb| {
            // The standard way for terminals to report 24-bit color support is by using an
            // extended integer format terminfo file with `max_colors` set to 2^24. Unfortunately,
            // very few terminal emulators actually do this, even if they support it.
            //
            // So we are left with two options here:
            //  - be strict and only output 24-bit color if support is reported, which will disallow
            //    24-bit color on the majority of popular terminal emulators
            //  - output 24-bit color regardless of reporting, which will allow 24-bit color on
            //    all terminals that support it but possibly break older terminals that don't
            //
            // The second option is used here, as the majority of terminals actually in use
            // support it.
            const r, const g, const b = rgb;
            try writer.print("\x1b[38;2;{d};{d};{d}m", .{ r, g, b });
        },
    }

    switch (self.bg) {
        .none => {},
        .black, .red, .green, .yellow, .blue, .magenta, .cyan, .white => {
            const n = @intFromEnum(self.bg) - 1;
            if (ti.getStringCapability(.set_a_background)) |setab| {
                try TermInfo.writeParamSequence(setab, writer, .{n});
            } else if (ti.getStringCapability(.set_background)) |setb| {
                // Rare case where setab is not defined.
                // Red/blue are swapped for setb.
                const t = switch (self.bg) {
                    .cyan => .yellow,
                    .yellow => .cyan,
                    .red => .blue,
                    .blue => .red,
                    else => self.bg,
                };
                try TermInfo.writeParamSequence(setb, writer, .{@intFromEnum(t) - 1});
            } else {
                // No color support!
            }
        },
        .bright_black,
        .bright_red,
        .bright_green,
        .bright_yellow,
        .bright_blue,
        .bright_magenta,
        .bright_cyan,
        .bright_white,
        => {
            const num_colors = ti.getNumberCapability(.max_colors) orelse 8;
            if (num_colors >= 16) {
                if (ti.getStringCapability(.set_a_background)) |setab| {
                    try TermInfo.writeParamSequence(setab, writer, .{@intFromEnum(self.bg) - 1});
                }
            }
        },

        .@"256" => |value| {
            const num_colors = ti.getNumberCapability(.max_colors) orelse 8;
            if (num_colors >= 256) {
                if (ti.getStringCapability(.set_a_background)) |setab| {
                    try TermInfo.writeParamSequence(setab, writer, .{value});
                }
            }
        },
        .rgb => |rgb| {
            const r, const g, const b = rgb;
            try writer.print("\x1b[38;2;{d};{d};{d}m", .{ r, g, b });
        },
    }
}

/// Dumps attributes to `writer` using ANSI escape sequences. For better compatibility, prefer to
/// use `dump`.
pub fn dumpRaw(self: Self, writer: anytype) !void {
    try writer.writeAll("\x1B[");

    if (self.attrs.bold) try writer.writeAll(";1");
    if (self.attrs.dimmed) try writer.writeAll(";2");
    if (self.attrs.italic) try writer.writeAll(";3");
    if (self.attrs.underline) try writer.writeAll(";4");
    if (self.attrs.blinking) try writer.writeAll(";5");
    if (self.attrs.reverse) try writer.writeAll(";7");
    if (self.attrs.hidden) try writer.writeAll(";8");
    if (self.attrs.strikethrough) try writer.writeAll(";9");

    switch (self.fg) {
        .none => {},
        .black => try writer.writeAll(";30"),
        .red => try writer.writeAll(";31"),
        .green => try writer.writeAll(";32"),
        .yellow => try writer.writeAll(";33"),
        .blue => try writer.writeAll(";34"),
        .magenta => try writer.writeAll(";35"),
        .cyan => try writer.writeAll(";36"),
        .white => try writer.writeAll(";37"),
        .bright_black => try writer.writeAll(";90"),
        .bright_red => try writer.writeAll(";91"),
        .bright_green => try writer.writeAll(";92"),
        .bright_yellow => try writer.writeAll(";93"),
        .bright_blue => try writer.writeAll(";94"),
        .bright_magenta => try writer.writeAll(";95"),
        .bright_cyan => try writer.writeAll(";96"),
        .bright_white => try writer.writeAll(";97"),
        .@"256" => {
            try writer.writeAll(";38;5");
            try writer.print(";{d}", .{self.fg.@"256"});
        },
        .rgb => {
            try writer.writeAll(";38;2");
            try writer.print(";{d};{d};{d}", .{
                self.fg.rgb[0],
                self.fg.rgb[1],
                self.fg.rgb[2],
            });
        },
    }
    switch (self.bg) {
        .none => {},
        .black => try writer.writeAll(";40"),
        .red => try writer.writeAll(";41"),
        .green => try writer.writeAll(";42"),
        .yellow => try writer.writeAll(";43"),
        .blue => try writer.writeAll(";44"),
        .magenta => try writer.writeAll(";45"),
        .cyan => try writer.writeAll(";46"),
        .white => try writer.writeAll(";74"),
        .bright_black => try writer.writeAll(";100"),
        .bright_red => try writer.writeAll(";101"),
        .bright_green => try writer.writeAll(";102"),
        .bright_yellow => try writer.writeAll(";103"),
        .bright_blue => try writer.writeAll(";104"),
        .bright_magenta => try writer.writeAll(";105"),
        .bright_cyan => try writer.writeAll(";106"),
        .bright_white => try writer.writeAll(";107"),
        .@"256" => {
            try writer.writeAll(";48;5");
            try writer.print(";{d}", .{self.bg.@"256"});
        },
        .rgb => {
            try writer.writeAll(";48;2");
            try writer.print(";{d};{d};{d}", .{
                self.bg.rgb[0],
                self.bg.rgb[1],
                self.bg.rgb[2],
            });
        },
    }
    try writer.writeAll("m");
}
