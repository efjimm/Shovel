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

// TODO: Standardize spelling of color/colour

const std = @import("std");
const Style = @This();
const TermInfo = @import("TermInfo.zig");
const log = @import("log.zig");

pub const Colour = union(enum(u5)) {
    const colour_desc = @import("colour_description.zig");
    pub const FromDescriptionError = colour_desc.ParseError;
    pub const fromDescription = colour_desc.parseColourDescription;

    // TODO since the default colours are also part of the 256 colour spec,
    //      maybe we should just use that. The dump function would then special
    //      case them and use legacy escape sequences.
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

    pub const none: Attribute = .{};
};

fg: Colour = .none,
bg: Colour = .none,
attrs: Attribute = .{},

pub fn init(fg: Colour, bg: Colour, attrs: Attribute) Style {
    return .{
        .fg = fg,
        .bg = bg,
        .attrs = attrs,
    };
}

pub fn eql(a: Style, b: Style) bool {
    return std.meta.eql(a, b);
}

pub const DumpOptions = struct {
    /// When set to false, truecolor colors will be approximated to the standard 256-color pallette
    /// instead.
    truecolor: bool = true,
    terminfo: ?*const TermInfo = null,
};

/// Dumps the attributes to `writer`, using the capabilities reported by `ti`.
pub fn dump(style: Style, writer: *std.io.Writer, opts: DumpOptions) !void {
    const ti = opts.terminfo orelse return style.dumpRaw(writer);

    if (ti.getStringCapability(.set_attributes)) |sgr| {
        try TermInfo.writeParamSequence(sgr, writer, .{
            style.attrs.standout,
            style.attrs.underline,
            style.attrs.reverse,
            style.attrs.blinking,
            style.attrs.dimmed,
            style.attrs.bold,
            style.attrs.hidden,
            false,
            false,
        });
    } else {
        // Terminfo does not define any standard capabilities to disable bold/reverse/blink/dim/
        // blank apart from sgr, so the only way to turn them off here is to reset all attributes.
        // Other attributes are disabled individually anyway in case sgr0 is not defined.
        if (ti.getStringCapability(.exit_attribute_mode)) |sgr0| try writer.writeAll(sgr0);

        if (style.attrs.standout) {
            if (ti.getStringCapability(.enter_standout_mode)) |so| try writer.writeAll(so);
        } else if (ti.getStringCapability(.exit_standout_mode)) |so| try writer.writeAll(so);

        if (style.attrs.underline) {
            if (ti.getStringCapability(.enter_underline_mode)) |ul| try writer.writeAll(ul);
        } else if (ti.getStringCapability(.exit_underline_mode)) |ul| try writer.writeAll(ul);

        if (style.attrs.reverse) {
            if (ti.getStringCapability(.enter_reverse_mode)) |rev| try writer.writeAll(rev);
        }

        if (style.attrs.blinking) {
            if (ti.getStringCapability(.enter_blink_mode)) |blink| try writer.writeAll(blink);
        }

        if (style.attrs.dimmed) {
            if (ti.getStringCapability(.enter_dim_mode)) |dim| try writer.writeAll(dim);
        }

        if (style.attrs.bold) {
            if (ti.getStringCapability(.enter_bold_mode)) |bold| try writer.writeAll(bold);
        }

        if (style.attrs.hidden) {
            if (ti.getStringCapability(.enter_secure_mode)) |invis| try writer.writeAll(invis);
        }
    }

    if (style.attrs.strikethrough) {
        if (ti.getExtendedString("smxx")) |xx| try writer.writeAll(xx);
    } else {
        if (ti.getExtendedString("rmxx")) |xx| try writer.writeAll(xx);
    }

    switch (style.fg) {
        .none => {},
        .black, .red, .green, .yellow, .blue, .magenta, .cyan, .white => {
            const n = @intFromEnum(style.fg) - 1;
            if (ti.getStringCapability(.set_a_foreground)) |setaf| {
                try TermInfo.writeParamSequence(setaf, writer, .{n});
            } else if (ti.getStringCapability(.set_foreground)) |setf| {
                // Rare case where setaf is not defined.
                // Red/blue are swapped for setf.
                const t = switch (style.fg) {
                    .cyan => .yellow,
                    .yellow => .cyan,
                    .red => .blue,
                    .blue => .red,
                    else => style.fg,
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
                    try TermInfo.writeParamSequence(setaf, writer, .{@intFromEnum(style.fg) - 1});
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
            if (opts.truecolor) {
                try dump24BitColour(rgb, .fg, writer);
            } else {
                const index = approximateTruecolor(rgb);
                const num_colors = ti.getNumberCapability(.max_colors) orelse 8;
                if (num_colors >= 256) {
                    if (ti.getStringCapability(.set_a_foreground)) |setaf| {
                        try TermInfo.writeParamSequence(setaf, writer, .{index});
                    }
                }
            }
        },
    }

    switch (style.bg) {
        .none => {},
        .black, .red, .green, .yellow, .blue, .magenta, .cyan, .white => {
            const n = @intFromEnum(style.bg) - 1;
            if (ti.getStringCapability(.set_a_background)) |setab| {
                try TermInfo.writeParamSequence(setab, writer, .{n});
            } else if (ti.getStringCapability(.set_background)) |setb| {
                // Rare case where setab is not defined.
                // Red/blue are swapped for setb.
                const t = switch (style.bg) {
                    .cyan => .yellow,
                    .yellow => .cyan,
                    .red => .blue,
                    .blue => .red,
                    else => style.bg,
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
                    try TermInfo.writeParamSequence(setab, writer, .{@intFromEnum(style.bg) - 1});
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
            if (opts.truecolor) {
                try dump24BitColour(rgb, .bg, writer);
            } else {
                const index = approximateTruecolor(rgb);
                const num_colors = ti.getNumberCapability(.max_colors) orelse 8;
                if (num_colors >= 256) {
                    if (ti.getStringCapability(.set_a_background)) |setab| {
                        try TermInfo.writeParamSequence(setab, writer, .{index});
                    }
                }
            }
        },
    }
}

/// Given a 24 bit color value, return the index of the closest match in the 256 color table.
pub fn approximateTruecolor(rgb: [3]u8) u8 {
    const r, const g, const b = rgb;
    return 16 + (r / 51) * 36 + (g / 51) * 6 + (b / 51);
}

pub fn dump24BitColour(rgb: [3]u8, comptime kind: enum { fg, bg }, writer: anytype) !void {
    const fmt = switch (kind) {
        .fg => "\x1b[38;2;{d};{d};{d}m",
        .bg => "\x1b[48;2;{d};{d};{d}m",
    };
    const r, const g, const b = rgb;
    try writer.print(fmt, .{ r, g, b });
}

/// Dumps attributes to `writer` using ANSI escape sequences. For better compatibility, prefer to
/// use `dump`.
fn dumpRaw(style: Style, writer: anytype) !void {
    var buf: std.BoundedArray(u8, 64) = .{};
    buf.appendSliceAssumeCapacity("\x1B[");

    if (style.attrs.bold) buf.appendSliceAssumeCapacity("1;");
    if (style.attrs.dimmed) buf.appendSliceAssumeCapacity("2;");
    if (style.attrs.italic) buf.appendSliceAssumeCapacity("3;");
    if (style.attrs.underline) buf.appendSliceAssumeCapacity("4;");
    if (style.attrs.blinking) buf.appendSliceAssumeCapacity("5;");
    if (style.attrs.reverse) buf.appendSliceAssumeCapacity("7;");
    if (style.attrs.hidden) buf.appendSliceAssumeCapacity("8;");
    if (style.attrs.strikethrough) buf.appendSliceAssumeCapacity("9;");

    switch (style.fg) {
        .none => {},
        .black => buf.appendSliceAssumeCapacity("30;"),
        .red => buf.appendSliceAssumeCapacity("31;"),
        .green => buf.appendSliceAssumeCapacity("32;"),
        .yellow => buf.appendSliceAssumeCapacity("33;"),
        .blue => buf.appendSliceAssumeCapacity("34;"),
        .magenta => buf.appendSliceAssumeCapacity("35;"),
        .cyan => buf.appendSliceAssumeCapacity("36;"),
        .white => buf.appendSliceAssumeCapacity("37;"),
        .bright_black => buf.appendSliceAssumeCapacity("90;"),
        .bright_red => buf.appendSliceAssumeCapacity("91;"),
        .bright_green => buf.appendSliceAssumeCapacity("92;"),
        .bright_yellow => buf.appendSliceAssumeCapacity("93;"),
        .bright_blue => buf.appendSliceAssumeCapacity("94;"),
        .bright_magenta => buf.appendSliceAssumeCapacity("95;"),
        .bright_cyan => buf.appendSliceAssumeCapacity("96;"),
        .bright_white => buf.appendSliceAssumeCapacity("97;"),
        .@"256" => |n| {
            buf.writer().print("38;5;{d};", .{n}) catch unreachable;
        },
        .rgb => {
            buf.writer().print("38;2;{d};{d};{d};", .{
                style.fg.rgb[0],
                style.fg.rgb[1],
                style.fg.rgb[2],
            }) catch unreachable;
        },
    }
    switch (style.bg) {
        .none => {},
        .black => buf.appendSliceAssumeCapacity("40;"),
        .red => buf.appendSliceAssumeCapacity("41;"),
        .green => buf.appendSliceAssumeCapacity("42;"),
        .yellow => buf.appendSliceAssumeCapacity("43;"),
        .blue => buf.appendSliceAssumeCapacity("44;"),
        .magenta => buf.appendSliceAssumeCapacity("45;"),
        .cyan => buf.appendSliceAssumeCapacity("46;"),
        .white => buf.appendSliceAssumeCapacity("74;"),
        .bright_black => buf.appendSliceAssumeCapacity("100;"),
        .bright_red => buf.appendSliceAssumeCapacity("101;"),
        .bright_green => buf.appendSliceAssumeCapacity("102;"),
        .bright_yellow => buf.appendSliceAssumeCapacity("103;"),
        .bright_blue => buf.appendSliceAssumeCapacity("104;"),
        .bright_magenta => buf.appendSliceAssumeCapacity("105;"),
        .bright_cyan => buf.appendSliceAssumeCapacity("106;"),
        .bright_white => buf.appendSliceAssumeCapacity("107;"),
        .@"256" => |n| {
            buf.writer().print("48;5;{d};", .{n}) catch unreachable;
        },
        .rgb => {
            buf.writer().print("48;2;{d};{d};{d};", .{
                style.bg.rgb[0],
                style.bg.rgb[1],
                style.bg.rgb[2],
            }) catch unreachable;
        },
    }
    // Style was empty, we dont write anything in this case
    if (buf.buffer[buf.len - 1] != ';') return;

    buf.buffer[buf.len - 1] = 'm';
    try writer.writeAll(buf.constSlice());
    log.perf.debug("dump style {d} bytes", .{buf.len});
}
