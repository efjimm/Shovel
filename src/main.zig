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
pub const Screen = @import("Screen.zig");
pub const spells = @import("spells.zig");
pub const Style = @import("Style.zig");
pub const Term = @import("Term.zig");
pub const TerminalCellWriter = @import("TerminalCellWriter.zig");
pub const TermInfo = @import("TermInfo.zig");
const wcWidth = @import("util.zig").wcWidth;

pub const GraphemeClusteringMode = enum { codepoint, grapheme };

pub fn initUnicodeData(allocator: std.mem.Allocator) !void {
    try zg.initData(allocator, &.{ .graphemes, .display_width });
}

pub fn deinitUnicodeData(allocator: std.mem.Allocator) void {
    zg.deinitData(allocator, &.{ .graphemes, .display_width });
}

pub fn stringWidth(
    bytes: []const u8,
    mode: GraphemeClusteringMode,
    opts: zg.DisplayWidth.StrWidthOptions,
) zg.DisplayWidth.StrWidthResult {
    switch (mode) {
        .codepoint => {
            var iter: zg.codepoint.Iterator = .init(bytes);
            var width: u32 = 0;
            while (iter.next()) |cp| {
                const w = @import("util.zig").wcWidth(cp.code);
                if (width + w > opts.max_width) {
                    return .{ .len = cp.offset, .width = width };
                }
                width += w;
            }
            return .{ .len = bytes.len, .width = width };
        },
        .grapheme => return zg.display_width.strWidth(bytes, opts),
    }
}

/// Iterator over characters that stops at a max width.
/// What a character is depends on the grapheme clustering mode.
///
/// * For .codepoint, a character is a codepoint with non-zero width.
/// * For .grapheme, a character is an extended grapheme cluster.
pub const WidthIterator = struct {
    pub const Result = struct {
        offset: usize,
        len: usize,
        width: usize,
    };

    bytes: []const u8,
    mode: GraphemeClusteringMode,
    graphemes: zg.Graphemes.Iterator,
    codepoints: zg.codepoint.Iterator,
    max_width: usize,

    pub fn init(bytes: []const u8, mode: GraphemeClusteringMode, max_width: usize) WidthIterator {
        return .{
            .bytes = bytes,
            .mode = mode,
            .graphemes = .init(bytes, &zg.graphemes),
            .codepoints = .init(bytes),
            .max_width = max_width,
        };
    }

    pub fn next(i: *WidthIterator) ?Result {
        if (i.max_width == 0) return null;

        return switch (i.mode) {
            .grapheme => i.nextGrapheme(),
            .codepoint => i.nextCodepoint(),
        };
    }

    pub fn nextGrapheme(i: *WidthIterator) ?Result {
        const gr = i.graphemes.next() orelse return null;
        const bytes = gr.bytes(i.bytes);
        const res = stringWidth(bytes, .grapheme, .{ .max_width = i.max_width });
        if (res.len < bytes.len) {
            i.max_width = 0;
            return null;
        }
        i.max_width -= res.width;
        return .{
            .offset = gr.offset,
            .len = gr.len,
            .width = res.width,
        };
    }

    pub fn nextCodepoint(i: *WidthIterator) ?Result {
        const cp = i.codepoints.next() orelse return null;
        const bytes = i.bytes[cp.offset..][0..cp.len];
        const res = stringWidth(bytes, .codepoint, .{ .max_width = i.max_width });
        i.max_width -= res.width;
        return .{
            .offset = cp.offset,
            .len = cp.len,
            .width = res.width,
        };
    }
};

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
    mode: GraphemeClusteringMode,
    writer: *std.io.Writer,
) !void {
    const res = stringWidth(str, mode, .{ .max_width = max_width });
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
    if (truncated.len == 0) {
        try writer.splatByteAll(' ', max_width);
        return;
    }

    if (width == max_width) {
        // Replace the last character with '…'
        const w, const off = blk: {
            if (mode == .codepoint) {
                var cp_iter: zg.codepoint.Iterator = .initEnd(truncated);
                while (cp_iter.prev()) |cp| {
                    const w = wcWidth(cp.code);
                    if (wcWidth(cp.code) != 0) break :blk .{ w, cp.offset };
                }

                break :blk .{ 0, res.len };
            }

            var iter = zg.graphemes.reverseIterator(truncated);
            const g = iter.prev().?;

            var cp_iter: zg.codepoint.Iterator = .init(truncated[g.offset..]);
            break :blk .{ wcWidth(cp_iter.next().?.code), g.offset };
        };

        try writer.print("{s}…", .{truncated[0..off]});

        // If the last character we replaced was a wide character, we need to pad with an extra
        // space.
        if (w == 2) try writer.writeByte(' ');
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
