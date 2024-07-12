//! A buffer representing a terminal screen.
//! Assumes that all bytes written to the canvas are valid UTF-8.
//! Writes text to its internal buffer without any modification. This means newlines, tabs, and
//! control characters will stay in the text literally.
const std = @import("std");
const Style = @import("Style.zig");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const wcWidth = @import("wcwidth").wcWidth;
const Utf8Iterator = @import("grapheme").utf8.Iterator;
const TermInfo = @import("TermInfo.zig");

text: std.ArrayListUnmanaged(u8) = .{},
cells: std.MultiArrayList(Cell) = .{},
line_lens: std.ArrayListUnmanaged(usize) = .{},
width: u16,
width_method: WidthMethod,
allocator: Allocator,

pub const WidthMethod = enum {
    wcwidth,
    mode_2027,
};

pub const Cell = struct {
    text_len: u16,
    width: Width,
    style: Style = .{},

    pub const Width = enum { single, double };

    pub const empty: Cell = .{ .text_len = 0, .width = .single };
};

pub fn lineTextOffset(canvas: *const Canvas, line: u16) usize {
    var offset: usize = 0;
    for (canvas.line_lens.items[0..line]) |len| offset += len;
    return offset;
}

pub fn textOffset(canvas: *const Canvas, line: u16, column: u16) usize {
    var offset: usize = 0;
    for (canvas.line_lens.items[0..line]) |len|
        offset += len;
    for (canvas.cells.items(.text_len)[@as(u32, line) * canvas.width ..][0..column]) |len|
        offset += len;
    return offset;
}

pub fn lineText(canvas: *const Canvas, line: u16) []u8 {
    const offset = canvas.lineTextOffset(line);
    const len = canvas.line_lens.items[line];
    return canvas.text.items[offset..][0..len];
}

pub fn dumpLine(
    canvas: *const Canvas,
    line: u16,
    options: DumpOptions,
    writer: anytype,
) !void {
    if (canvas.line_lens.items[line] == 0) return;

    const line_text = canvas.lineText(line);

    const cells_slice = canvas.cells.slice();
    const cells_start = @as(u32, line) * canvas.width;

    const first_style = cells_slice.items(.style)[0];
    if (options.colour)
        try first_style.dump(options.terminfo, writer);
    var last_style: Style = first_style;

    var text_index: usize = 0;
    for (cells_start..cells_start + canvas.width) |i| {
        const cell = cells_slice.get(i);

        if (options.colour and !Style.eql(cell.style, last_style)) {
            try cell.style.dump(options.terminfo, writer);
            last_style = cell.style;
        }
        if (cell.text_len > 0) {
            try writer.writeAll(line_text[text_index..][0..cell.text_len]);
        }
        text_index += cell.text_len;
    }
}

// TODO Investigate 'raw' naming consistency between other functions
pub fn dumpLineRaw(canvas: *Canvas, line: u16, writer: anytype) !void {
    const line_text = canvas.lineText(line);
    if (line_text.len == 0) return;
    const cells_slice = canvas.cells.slice();
    const cells_start = @as(u32, line) * canvas.width;

    var text_index: usize = 0;
    for (cells_start..cells_start + canvas.width) |i| {
        const cell = cells_slice.get(i);

        if (cell.text_len > 0) {
            try writer.writeAll(line_text[text_index..][0..cell.text_len]);
        }
        text_index += cell.text_len;
    }
}

const Canvas = @This();

pub fn init(
    allocator: Allocator,
    width_method: WidthMethod,
) Canvas {
    return .{
        .width_method = width_method,
        .allocator = allocator,
        .width = 0,
    };
}

pub fn deinit(canvas: *Canvas) void {
    canvas.text.deinit(canvas.allocator);
    canvas.line_lens.deinit(canvas.allocator);
    canvas.cells.deinit(canvas.allocator);
}

pub fn height(canvas: Canvas) u16 {
    return @intCast(canvas.line_lens.items.len);
}

pub inline fn width(canvas: Canvas) u16 {
    return canvas.width;
}

/// TODO: Make this y/x instead of x/y?
pub fn resize(canvas: *Canvas, new_width: u16, new_height: u16) !void {
    if (new_width == 0 or new_height == 0) {
        canvas.text.items.len = 0;
        canvas.line_lens.items.len = 0;
        canvas.cells.len = 0;
        canvas.width = 0;
        return;
    }
    const old_width = canvas.width;
    const old_height = canvas.height();

    // TODO MultiArrayList performs a new allocation on resize,
    //      so there is a redundant memcpy here
    try canvas.cells.ensureTotalCapacity(canvas.allocator, @as(u32, new_width) * new_height);
    try canvas.line_lens.ensureTotalCapacity(canvas.allocator, new_height);
    errdefer comptime unreachable;

    canvas.line_lens.items.len = new_height;

    if (new_height > old_height) {
        @memset(canvas.line_lens.items[old_height..], 0);
    }

    if (new_width > old_width) {
        canvas.cells.len = @as(u32, new_width) * new_height;

        // Loop through each slice in the MultiArrayList and copy old lines to new lines
        const cells_slice = canvas.cells.slice();
        inline for (@typeInfo(Cell).Struct.fields) |field| {
            const tag = @field(std.MultiArrayList(Cell).Field, field.name);
            const slice = cells_slice.items(tag);

            var i: usize = @as(usize, old_width) * @min(old_height, new_height);
            while (i > 0) {
                i -= old_width;
                const src = slice[i..][0..old_width];
                const dest = slice[i / old_width * new_width ..][0..new_width];
                std.mem.copyBackwards(field.type, dest[0..old_width], src);
                // Fill the rest of the slice with empty cells
                @memset(dest[old_width..], @field(Cell.empty, field.name));
            }

            if (new_height > old_height)
                @memset(slice[@as(u32, old_width) * old_height ..], @field(Cell.empty, field.name));
        }
    } else if (new_width < old_width) {
        const cells_slice = canvas.cells.slice();
        inline for (@typeInfo(Cell).Struct.fields) |field| {
            const tag = @field(std.MultiArrayList(Cell).Field, field.name);
            const slice = cells_slice.items(tag);

            for (1..@min(old_height, new_height)) |y| {
                const src = slice[old_width * y ..][0..new_width];
                const dest = slice[new_width * y ..][0..new_width];
                std.mem.copyForwards(field.type, dest, src);
            }
        }

        canvas.cells.len = @as(u32, new_width) * new_height;

        // Truncate text on each line
        const text_lens = cells_slice.items(.text_len);
        var src_offset: usize = 0;
        var dest_offset: usize = 0;
        for (canvas.line_lens.items, 0..) |*line_len, i| {
            var len: usize = 0;
            for (text_lens[i * new_width ..][0 .. new_width - 1]) |text_len|
                len += text_len;

            const last_char_len = text_lens[i * new_width ..][new_width - 1];
            const last_char = canvas.text.items[src_offset + len ..][0..last_char_len];
            if (textWidth(last_char, canvas.width_method) > 1) {
                // Last cell contains a wide character which will be cut in half by the resize,
                // so erase it!
                text_lens[i * new_width ..][new_width - 1] = 0;
            } else {
                len += last_char_len;
            }

            std.mem.copyForwards(
                u8,
                canvas.text.items[dest_offset..][0..len],
                canvas.text.items[src_offset..][0..len],
            );

            src_offset += line_len.*;
            dest_offset += len;
            line_len.* = len;
        }
        canvas.text.items.len = dest_offset;
    }

    canvas.width = new_width;
}

const Character = struct {
    bytes: []const u8,
    width: u16,
};

/// Iterator over what a terminal would consider a single character. For terminals that do not
/// support mode 2027, this is a single codepoint. Otherwise this is grapheme clusters.
/// Returns the bytes of the character along with its display width.
fn CharacterIterator(comptime width_method: WidthMethod) type {
    return struct {
        bytes: []const u8,
        child_iter: switch (width_method) {
            .wcwidth => std.unicode.Utf8Iterator,
            .mode_2027 => Utf8Iterator,
        },

        fn init(
            bytes: []const u8,
        ) @This() {
            return .{
                .bytes = bytes,
                .child_iter = switch (width_method) {
                    .wcwidth => .{ .bytes = bytes, .i = 0 },
                    // `data.data` is a pointer, this is safe
                    .mode_2027 => Utf8Iterator{ .bytes = bytes },
                },
            };
        }

        fn next(iter: *@This()) ?Character {
            return switch (width_method) {
                .wcwidth => iter.nextWcWidth(),
                .mode_2027 => iter.next2027(),
            };
        }

        fn next2027(iter: *@This()) ?Character {
            const gc = iter.child_iter.nextGrapheme() orelse return null;
            const w = graphemeWidth(gc);
            return .{ .bytes = gc, .width = @intCast(w) };
        }

        fn nextWcWidth(iter: *@This()) ?Character {
            const bytes = iter.child_iter.nextCodepointSlice() orelse return null;
            const cp = std.unicode.utf8Decode(bytes) catch unreachable;
            const w = wcWidth(cp);
            return .{
                .bytes = bytes,
                .width = w,
            };
        }
    };
}

// TODO: Word wrapping

/// Wrapper around `Canvas`. Provides a writer which does automatic line wrapping and handles escape
/// sequences.
pub const Pen = struct {
    canvas: *Canvas,
    line: u16,
    column: u16,
    style: Style = .{},
    /// Automatically wrap lines if true.
    wrap: bool = true,

    /// Cached byte offset into `canvas.text` to speed up sequential writes
    text_offset: usize,
    /// If it's the first write we need to make sure we properly erase any double width characters we
    /// partially overwrite. If it's not the first write we can skip all that work.
    first_write: bool = true,

    pub const WriteError = error{EndOfCanvas} || Allocator.Error;
    pub const Writer = std.io.Writer(*Pen, WriteError, Pen.write);

    pub fn write(p: *Pen, bytes: []const u8) WriteError!usize {
        if (p.line >= p.canvas.height()) return error.EndOfCanvas;

        var start_col = p.column;
        var bytes_written: usize = 0;

        while (bytes_written < bytes.len) {
            const end: ?usize = for (bytes[bytes_written..], bytes_written..) |c, i| {
                if (std.ascii.isControl(c)) break i;
            } else null;

            const len, const cells_written = try p.canvas.write(
                bytes[bytes_written .. end orelse bytes.len],
                p.line,
                p.column,
                .{ .text_offset = p.text_offset, .no_start_adjust = !p.first_write },
            );
            p.first_write = false;
            p.column += cells_written;
            p.text_offset += len;
            // Writing 0 bytes means that we have a double wide character that can't be written
            // without wrapping. So wrap!
            if (p.column >= p.canvas.width or len == 0) {
                p.canvas.setStyleRect(p.line, start_col, p.canvas.width - start_col, p.style);
                if (!p.wrap) return bytes.len;

                if (p.column < p.canvas.width) {
                    // If we're wrapping without touching the last column in a line we need to add
                    // its text offset.
                    var replace_len: usize = 0;
                    for (
                        p.canvas.cells.items(.text_len)[@as(u32, p.line) * p.canvas.width ..][p.column..p.canvas.width],
                    ) |*text_len| {
                        replace_len += text_len.*;
                        text_len.* = 0;
                    }
                    p.canvas.text.replaceRange(
                        p.canvas.allocator,
                        p.text_offset,
                        replace_len,
                        &.{},
                    ) catch unreachable;
                    p.canvas.line_lens.items[p.line] -= replace_len;
                }

                p.column = 0;
                p.line += 1;
                start_col = 0;
                if (p.line >= p.canvas.height() and bytes_written + len < bytes.len)
                    return error.EndOfCanvas;
            }
            bytes_written += len;

            const e = end orelse continue;
            var bw: std.io.BufferedWriter(512, Writer) = .{ .unbuffered_writer = p.writer() };

            for (bytes[e..]) |c| {
                if (!std.ascii.isControl(c)) break;

                switch (c) {
                    '\n', '\r' => {
                        p.canvas.setStyleRect(p.line, start_col, p.column - start_col, p.style);
                        p.line += 1;
                        p.column = 0;
                    },
                    else => try bw.writer().print("^{c}", .{c | 0x40}),
                }
                bytes_written += 1;
            }

            try bw.flush();
        }
        if (p.line < p.canvas.height()) {
            p.canvas.setStyleRect(p.line, start_col, p.column - start_col, p.style);
        }
        return bytes.len;
    }

    pub fn writer(p: *Pen) Writer {
        return .{ .context = p };
    }

    pub fn move(p: *Pen, line: u16, column: u16) void {
        p.line = line;
        p.column = column;
        p.text_offset = p.canvas.textOffset(line, column);
        p.first_write = true;
    }
};

pub fn pen(canvas: *Canvas, line: u16, column: u16) Pen {
    return .{
        .canvas = canvas,
        .line = line,
        .column = column,
        .text_offset = canvas.textOffset(line, column),
    };
}

/// Clears the canvas, maintaining memory allocations for cells and text.
pub fn clearRetainingCapacity(canvas: *Canvas) void {
    canvas.text.clearRetainingCapacity();

    const cells_slice = canvas.cells.slice();
    inline for (@typeInfo(Cell).Struct.fields) |field| {
        const tag = @field(std.MultiArrayList(Cell).Field, field.name);
        const slice = cells_slice.items(tag);
        @memset(slice, @field(Cell.empty, field.name));
    }
}

// TODO: `write` function that fast-paths writing sequentially

pub const WriteOptions = struct {
    /// If true, assume that we aren't writing over half of a double width character at the start of
    /// the write. Reduces the amount of work required to write.
    no_start_adjust: bool = false,
    /// Assume that the text at `line` and `col` in the canvas is at this offset in `canvas.text`.
    text_offset: ?usize = null,
};

// Returns true if `bytes` consists entirely of ASCII characters.
fn isAllAscii(bytes: []const u8) bool {
    for (bytes) |c| {
        if (c >= 128) return false;
    }
    return true;
}

/// Write `bytes` to the canvas, starting at the given line and column. Cells will be overwritten.
/// Does not wrap. Partially overwriting a double-width character will result in the entire
/// character being erased. Bytes written past the end of the line will be truncated.
/// Asserts that `line` and `column` are within the bounds of the canvas.
///
/// Returns the number of bytes written, and the number of columns written to.
pub fn write(
    canvas: *Canvas,
    bytes: []const u8,
    line: u16,
    column: u16,
    options: WriteOptions,
) Allocator.Error!struct { usize, u16 } {
    assert(line < canvas.height());
    assert(column < canvas.width);

    if (bytes.len == 0) return .{ 0, 0 };

    // Need to do this before modifying anything so we
    // don't end up with an inconsistent state on error.
    try canvas.text.ensureUnusedCapacity(canvas.allocator, bytes.len);
    errdefer comptime unreachable;

    const text = canvas.text.items[options.text_offset orelse canvas.lineTextOffset(line) ..];

    var cells_slice = canvas.cells.slice();
    const text_lens = cells_slice.items(.text_len)[line * canvas.width ..][0..canvas.width];

    // Set cells and get display width of `bytes`, truncating it if necessary.
    const remaining_width = canvas.width - column;

    // If the first column is the right side of a double width character we need to erase
    // the whole character. `start_adjust` handles this.
    const text_start, const start_adjust = if (!options.no_start_adjust and column > 0) blk: {
        var start: usize = 0;
        for (text_lens[0 .. column - 1]) |text_len| start += text_len;

        // Text for cell before `column`
        const str = text[start..][0..text_lens[column - 1]];
        start += text_lens[column - 1];

        const start_adjust = if (textWidth(str, canvas.width_method) == 2) blk2: {
            cells_slice.set(line * canvas.width + (column - 1), .{
                .text_len = 0,
                .width = .single,
            });
            break :blk2 str.len;
        } else 0;

        break :blk .{ start, start_adjust };
    } else .{ 0, 0 };

    const write_len, const replace_len, const w = switch (canvas.width_method) {
        inline else => |tag| blk: {
            var w: u16 = 0;
            var i: usize = column;
            var replace_len: usize = 0;

            if (isAllAscii(bytes)) {
                for (bytes, 0..) |c, j| {
                    const ch_width = wcWidth(c);
                    w += ch_width;

                    if (w > remaining_width)
                        break :blk .{
                            j,
                            replace_len,
                            w - ch_width,
                        };

                    replace_len += text_lens[i];
                    cells_slice.set(line * canvas.width + i, .{
                        .text_len = 1,
                        .width = .single,
                    });

                    i += ch_width;
                }
                break :blk .{ bytes.len, replace_len, w };
            }

            var iter = CharacterIterator(tag).init(bytes);

            while (iter.next()) |ch| {
                if (ch.width == 0) continue;
                w += ch.width;

                if (w > remaining_width)
                    break :blk .{
                        @intFromPtr(ch.bytes.ptr) - @intFromPtr(bytes.ptr),
                        replace_len,
                        w - ch.width,
                    };

                const ch_width: Cell.Width = switch (ch.width) {
                    1 => .single,
                    2 => .double,
                    else => unreachable,
                };

                replace_len += text_lens[i];
                cells_slice.set(line * canvas.width + i, .{
                    .text_len = @intCast(ch.bytes.len),
                    .width = ch_width,
                });

                if (ch_width == .double) {
                    replace_len += text_lens[i + 1];
                    cells_slice.set(line * canvas.width + i + 1, .{ .text_len = 0, .width = .double });
                }

                i += ch.width;
            }

            break :blk .{ bytes.len, replace_len, w };
        },
    };

    canvas.text.replaceRangeAssumeCapacity(
        (options.text_offset orelse canvas.lineTextOffset(line)) + text_start - start_adjust,
        replace_len + start_adjust,
        bytes[0..write_len],
    );

    canvas.line_lens.items[line] -= replace_len + start_adjust;
    canvas.line_lens.items[line] += write_len;

    return .{ write_len, w };
}

pub const DumpOptions = struct {
    colour: bool = true,
    terminfo: ?*const TermInfo = null,
};

pub fn dump(
    canvas: *Canvas,
    options: DumpOptions,
    writer: anytype,
) !void {
    const spells = @import("spells.zig");

    const move_to, const clear_to_eol = blk: {
        const ti = options.terminfo orelse break :blk .{
            spells.move_cursor_fmt,
            spells.clear_to_eol,
        };

        break :blk .{
            ti.getStringCapability(.cursor_address) orelse spells.move_cursor_fmt,
            ti.getStringCapability(.clr_eol) orelse spells.clear_to_eol,
        };
    };

    for (0..canvas.height()) |line| {
        if (canvas.line_lens.items[line] == 0) continue;

        try TermInfo.writeParamSequence(move_to, writer, .{ @as(u16, @intCast(line)), 0 });
        try writer.writeAll(clear_to_eol);

        try canvas.dumpLine(@intCast(line), options, writer);
    }
}

/// Dump without outputting any escape sequences.
pub fn dumpRaw(canvas: *Canvas, writer: anytype) !void {
    for (0..canvas.height()) |line| {
        if (canvas.line_lens.items[line] == 0) continue;

        try canvas.dumpLineRaw(@intCast(line), writer);
    }
}

pub fn textWidth(bytes: []const u8, method: WidthMethod) usize {
    return switch (method) {
        .wcwidth => textWidthWcWidth(bytes),
        .mode_2027 => textWidthMode2027(bytes),
    };
}

pub fn graphemeWidth(grapheme: []const u8) usize {
    var cp_iter: Utf8Iterator = .{ .bytes = grapheme };
    return while (cp_iter.nextCodepoint()) |cp| {
        const w = wcWidth(cp);
        if (w == 0) continue;

        if (cp_iter.nextCodepoint()) |ncp| {
            // emoji text sequence.
            if (ncp == 0xFE0E) break 1;
            if (ncp == 0xFE0F) break 2;
        }

        break w;
    } else 0;
}

// TODO: ASCII fast path
pub fn textWidthMode2027(bytes: []const u8) usize {
    var total: usize = 0;
    var g_iter: Utf8Iterator = .{ .bytes = bytes };
    while (g_iter.nextGrapheme()) |grapheme| {
        total += graphemeWidth(grapheme);
    }

    return total;
}

/// Returns the display width of `bytes` using wcwidth.
/// Asserts that `bytes` is a valid utf-8 sequence.
pub fn textWidthWcWidth(bytes: []const u8) usize {
    var total: usize = 0;
    var iter: std.unicode.Utf8Iterator = .{ .bytes = bytes, .i = 0 };
    while (iter.nextCodepoint()) |cp| total += wcWidth(cp);

    return total;
}

fn multiArrayListCopyAssumeCapacity(src: anytype, dest: *@TypeOf(src)) void {
    const T = @TypeOf(src);
    dest.len = src.len;

    const src_slice = src.slice();
    const dest_slice = dest.slice();

    inline for (@typeInfo(T.Field).Enum.fields) |field| {
        const tag = @field(T.Field, field.name);
        const src_items = src_slice.items(tag);
        const dest_items = dest_slice.items(tag);
        @memcpy(dest_items, src_items);
    }
}

/// Writes the minimal amount of bytes required to transform `old` into `new`. Copies the contents
/// of `new` into `old`.
pub fn writeDiff(
    new: *const Canvas,
    old: *Canvas,
    terminfo: ?*const TermInfo,
    writer: anytype,
) !void {
    const spells = @import("spells.zig");
    assert(new.width == old.width);
    assert(new.height() == old.height());

    try old.text.ensureTotalCapacity(old.allocator, new.text.items.len);
    try old.cells.ensureTotalCapacity(old.allocator, new.cells.len);
    try old.line_lens.ensureTotalCapacity(old.allocator, new.line_lens.items.len);

    const move_cursor = if (terminfo) |ti|
        ti.getStringCapability(.cursor_address) orelse spells.move_cursor_fmt
    else
        spells.move_cursor_fmt;

    var new_offset: usize = 0;
    var old_offset: usize = 0;
    var last_pos: usize = 0;

    const new_cells = new.cells.slice();
    const old_cells = old.cells.slice();
    var last_style: ?Style = null;

    for (
        new_cells.items(.text_len),
        old_cells.items(.text_len),
        new_cells.items(.style),
        old_cells.items(.style),
        0..,
    ) |new_text_len, old_text_len, new_style, old_style, pos| {
        const new_text = new.text.items[new_offset..][0..new_text_len];
        const old_text = old.text.items[old_offset..][0..old_text_len];

        new_offset += new_text_len;
        old_offset += old_text_len;

        const dirty = !std.mem.eql(u8, new_text, old_text) or
            !new_style.eql(old_style) or
            last_style == null or
            !new_style.eql(last_style.?);
        if (!dirty) continue;

        if (pos != last_pos + 1) {
            const line = pos / new.width;
            const column = pos % new.width;
            try TermInfo.writeParamSequence(move_cursor, writer, .{
                @as(i32, @intCast(line)),
                @as(i32, @intCast(column)),
            });
        }
        last_pos = pos;

        if (last_style == null or !last_style.?.eql(new_style)) {
            try new_style.dump(terminfo, writer);
            last_style = new_style;
        }
        try writer.writeAll(new_text);
    }

    new.copyTo(old) catch unreachable;
}

/// Deep-copies all text, cells, and lines from `src` to `dest`. Also copies the `width` and
/// `width_method` fields.
pub fn copyTo(src: *const Canvas, dest: *Canvas) !void {
    try dest.text.ensureTotalCapacity(dest.allocator, src.text.items.len);
    try dest.cells.ensureTotalCapacity(dest.allocator, src.cells.len);
    try dest.line_lens.ensureTotalCapacity(dest.allocator, src.line_lens.items.len);
    errdefer comptime unreachable;

    dest.text.items.len = src.text.items.len;
    @memcpy(dest.text.items, src.text.items);

    multiArrayListCopyAssumeCapacity(src.cells, &dest.cells);
    dest.line_lens.items.len = src.line_lens.items.len;
    @memcpy(dest.line_lens.items, src.line_lens.items);

    dest.width = src.width;
    dest.width_method = src.width_method;
}

pub fn getStyle(canvas: *const Canvas, line: u16, column: u16) Style {
    assert(line < canvas.height());
    assert(column < canvas.width);
    return canvas.cells.items(.style)[line * canvas.width + column];
}

pub fn setStyle(canvas: *Canvas, line: u16, column: u16, style: Style) void {
    assert(line < canvas.height());
    assert(column < canvas.width);
    canvas.cells.items(.style)[line * canvas.width + column] = style;
}

pub fn setStyleRect(
    canvas: *Canvas,
    line: u16,
    column_start: u16,
    len: u16,
    style: Style,
) void {
    assert(line < canvas.height());
    assert(column_start < canvas.width);
    assert(column_start + len <= canvas.width);
    @memset(canvas.cells.items(.style)[line * canvas.width + column_start ..][0..len], style);
}

const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;
const expectEqualSlices = std.testing.expectEqualSlices;
const expectError = std.testing.expectError;

test "canvas write wcwidth" {
    var canvas = init(std.testing.allocator, .wcwidth);
    defer deinit(&canvas);

    try canvas.resize(80, 24);
    const str = "🧑‍🌾 one two";
    _ = try canvas.write(str, 0, 0, .{});

    try expectEqualStrings(str, canvas.lineText(0));

    try expectEqual(4, canvas.cells.get(0).text_len);
    try expectEqual(0, canvas.cells.get(1).text_len);
    try expectEqual(4, canvas.cells.get(2).text_len);
    try expectEqual(0, canvas.cells.get(3).text_len);
    for (4..str.len - 11) |i| try expectEqual(1, canvas.cells.get(i).text_len);
    for (str.len..80) |i| try expectEqual(0, canvas.cells.get(i).text_len);
}

test "canvas write mode 2027" {
    var canvas = init(std.testing.allocator, .mode_2027);
    defer deinit(&canvas);

    try canvas.resize(80, 24);
    const str = "🧑‍🌾 one two";
    _ = try canvas.write(str, 0, 0, .{});

    try expectEqualStrings(str, canvas.lineText(0));

    try expectEqual(11, canvas.cells.get(0).text_len);
    for (2..str.len - 11) |i| try expectEqual(1, canvas.cells.get(i).text_len);
    for (str.len..80) |i| try expectEqual(0, canvas.cells.get(i).text_len);

    _ = try canvas.write(str, 0, 0, .{});

    try expectEqualStrings(str, canvas.lineText(0));

    try expectEqual(11, canvas.cells.get(0).text_len);
    for (2..str.len - 11) |i| try expectEqual(1, canvas.cells.get(i).text_len);
    for (str.len..80) |i| try expectEqual(0, canvas.cells.get(i).text_len);
}

test "canvas partial overwrite of double width char" {
    var canvas = init(std.testing.allocator, .mode_2027);
    defer deinit(&canvas);

    try canvas.resize(6, 1);
    const str = "🧑‍🌾huh";
    _ = try canvas.write(str, 0, 0, .{});

    try expectEqualStrings(str, canvas.lineText(0));

    try expectEqual(Cell{ .text_len = 11, .width = .double }, canvas.cells.get(0));
    try expectEqual(Cell{ .text_len = 0, .width = .double }, canvas.cells.get(1));
    try expectEqual(Cell{ .text_len = 1, .width = .single }, canvas.cells.get(2));

    const str2 = "j";
    _ = try canvas.write(str2, 0, 1, .{});

    try expectEqualStrings("jhuh", canvas.lineText(0));

    try expectEqual(Cell{ .text_len = 0, .width = .single }, canvas.cells.get(0));
    try expectEqual(Cell{ .text_len = 1, .width = .single }, canvas.cells.get(1));
    try expectEqual(Cell{ .text_len = 1, .width = .single }, canvas.cells.get(2));
}

test "canvas write too long" {
    var canvas = init(std.testing.allocator, .mode_2027);
    defer deinit(&canvas);

    try canvas.resize(10, 24);
    const str = "one two three";
    _ = try canvas.write(str, 0, 0, .{});

    try expectEqualStrings(str[0..10], canvas.lineText(0));
    for (0..10) |i| try expectEqual(1, canvas.cells.get(i).text_len);
}

test "canvas write too long multiwidth" {
    var canvas = init(std.testing.allocator, .mode_2027);
    defer deinit(&canvas);

    try canvas.resize(10, 24);
    const str = "one two||🧑‍🌾";
    _ = try canvas.write(str, 0, 0, .{});

    try expectEqualStrings(str[0..9], canvas.lineText(0));
    for (0..9) |i| try expectEqual(1, canvas.cells.get(i).text_len);
    try expectEqual(0, canvas.cells.get(9).text_len);
}

test "pen writer" {
    var canvas = init(std.testing.allocator, .mode_2027);
    defer deinit(&canvas);

    try canvas.resize(4, 4);

    var p = canvas.pen(0, 0);
    const writer = p.writer();
    try writer.writeAll("one two three");

    try expectEqualStrings("one ", canvas.lineText(0));
    try expectEqualStrings("two ", canvas.lineText(1));
    try expectEqualStrings("thre", canvas.lineText(2));
    try expectEqualStrings("e", canvas.lineText(3));

    var buf: [128]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);

    try canvas.dumpLineRaw(0, fbs.writer());
    try expectEqualSlices(u8, "one ", fbs.getWritten());
    fbs.reset();

    try canvas.dumpLineRaw(1, fbs.writer());
    try expectEqualSlices(u8, "two ", fbs.getWritten());
    fbs.reset();

    try canvas.dumpLineRaw(2, fbs.writer());
    try expectEqualSlices(u8, "thre", fbs.getWritten());
    fbs.reset();

    try canvas.dumpLineRaw(3, fbs.writer());
    try expectEqualSlices(u8, "e", fbs.getWritten());
    fbs.reset();

    try canvas.dumpRaw(fbs.writer());
    try expectEqualSlices(u8, "one two three", fbs.getWritten());
    fbs.reset();

    // Test wrapping
    p.move(0, 3);
    try writer.writeAll("🧑‍🌾");

    try canvas.dumpLineRaw(0, fbs.writer());
    try expectEqualSlices(u8, "one", fbs.getWritten());
    fbs.reset();

    try canvas.dumpLineRaw(1, fbs.writer());
    try expectEqualSlices(u8, "🧑‍🌾o ", fbs.getWritten());
    fbs.reset();
}

test "pen write out of canvas bounds" {
    var canvas = init(std.testing.allocator, .mode_2027);
    defer deinit(&canvas);

    var p = canvas.pen(0, 0);
    const writer = p.writer();

    try writer.writeAll("");
    try expectError(error.EndOfCanvas, writer.writeAll("hi"));

    try canvas.resize(4, 4);

    try expectError(error.EndOfCanvas, writer.writeAll("This is a test. A very nice test indeed."));
}

test "canvas downsize" {
    var canvas = init(std.testing.allocator, .mode_2027);
    defer deinit(&canvas);
    const text =
        \\Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris scelerisque aliquet dui eget feugiat. Nunc nec risus vehicula neque malesuada lobortis sit amet quis turpis. In finibus volutpat lacus sed viverra. Etiam sed nisl quis tellus luctus pulvinar non eu tortor. Maecenas convallis lacus ac sapien ornare venenatis. Pellentesque at cursus erat. Fusce condimentum ultricies cursus. Aliquam non.
    ;

    try canvas.resize(20, 20);

    var p = canvas.pen(0, 0);
    try p.writer().writeAll(text);

    for (canvas.cells.items(.text_len)) |text_len|
        try expectEqual(1, text_len);

    try canvas.resize(10, 10);

    for (canvas.cells.items(.text_len)) |text_len|
        try expectEqual(1, text_len);

    var window = std.mem.window(u8, text, 10, 20);
    var line: u16 = 0;
    while (window.next()) |expected| : (line += 1) {
        if (line >= canvas.height()) break;
        try expectEqualStrings(expected, canvas.lineText(line));
    }
}

test "pen write control codes" {
    var canvas = init(std.testing.allocator, .mode_2027);
    defer deinit(&canvas);

    try canvas.resize(10, 10);
    var p = canvas.pen(0, 0);
    const writer = p.writer();

    try writer.writeAll("test1\ntest2\ntest3\r\n\t:)");

    for (0..5, 10..15, 20..25) |i, j, k| {
        try expectEqual(Cell{ .text_len = 1, .width = .single }, canvas.cells.get(i));
        try expectEqual(Cell{ .text_len = 1, .width = .single }, canvas.cells.get(j));
        try expectEqual(Cell{ .text_len = 1, .width = .single }, canvas.cells.get(k));
    }
    for (5..10, 15..20, 25..30) |i, j, k| {
        try expectEqual(Cell{ .text_len = 0, .width = .single }, canvas.cells.get(i));
        try expectEqual(Cell{ .text_len = 0, .width = .single }, canvas.cells.get(j));
        try expectEqual(Cell{ .text_len = 0, .width = .single }, canvas.cells.get(k));
    }

    try expectEqualStrings("test1", canvas.lineText(0));
    try expectEqualStrings("test2", canvas.lineText(1));
    try expectEqualStrings("test3", canvas.lineText(2));
    try expectEqualStrings("", canvas.lineText(3));
    try expectEqualStrings("^I:)", canvas.lineText(4));
}

test "canvas copyTo" {
    var src_canvas = init(std.testing.allocator, .mode_2027);
    defer deinit(&src_canvas);

    try src_canvas.resize(10, 10);

    var dest_canvas = init(std.testing.allocator, .mode_2027);
    defer dest_canvas.deinit();

    var p = src_canvas.pen(0, 0);
    const writer = p.writer();
    try writer.writeAll("test0\ntest1\ntest2\ntest3");

    try src_canvas.copyTo(&dest_canvas);

    inline for (0..4) |i| {
        const str = std.fmt.comptimePrint("test{d}", .{i});
        try expectEqualStrings(str, src_canvas.lineText(i));
        try expectEqualStrings(str, dest_canvas.lineText(i));
    }

    try expectEqual(src_canvas.width, dest_canvas.width);
    try expectEqual(src_canvas.width_method, dest_canvas.width_method);
}

test "canvas styles" {
    var canvas = init(std.testing.allocator, .mode_2027);
    defer deinit(&canvas);

    try canvas.resize(10, 10);

    canvas.setStyle(0, 0, .{ .fg = .red, .bg = .yellow });
    try expectEqual(Style{ .fg = .red, .bg = .yellow }, canvas.getStyle(0, 0));

    canvas.setStyleRect(1, 0, 10, .{ .fg = .blue });
    try expectEqualSlices(
        Style,
        &[_]Style{.{ .fg = .blue }} ** 10,
        canvas.cells.items(.style)[canvas.width..][0..10],
    );

    var p = canvas.pen(0, 9);
    try p.writer().writeByte('a');
    p.move(0, 9);

    p.style = .{ .fg = .yellow };
    try p.writer().writeAll("🧑‍🌾");

    try expectEqualStrings("", canvas.lineText(0));
    try expectEqualStrings("🧑‍🌾", canvas.lineText(1));

    try expectEqual(Style{ .fg = .yellow }, canvas.getStyle(0, 9));
    try expectEqual(Style{ .fg = .yellow }, canvas.getStyle(1, 0));
    try expectEqual(Style{ .fg = .yellow }, canvas.getStyle(1, 1));
}

test "canvas double buffering" {
    var c1 = init(std.testing.allocator, .mode_2027);
    defer c1.deinit();

    var c2 = init(std.testing.allocator, .mode_2027);
    defer c2.deinit();

    try c1.resize(80, 24);
    try c2.resize(80, 24);

    var p = c1.pen(0, 0);
    const writer = p.writer();

    p.style = .{ .fg = .red };
    try writer.writeAll("line1\n");
    p.style = .{ .fg = .blue, .bg = .yellow };
    try writer.writeAll("line2\n");
    p.style = .{ .fg = .white, .bg = .black, .attrs = .{ .bold = true } };
    try writer.writeAll("line3\n");

    try expectEqualSlices(Style, &[_]Style{.{ .fg = .red }} ** 5, c1.cells.items(.style)[0..5]);

    var buf: [1024]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try c1.writeDiff(&c2, null, fbs.writer());

    try expectEqualSlices(
        u8,
        "\x1B[1;1H\x1B[31mline1\x1B[2;1H\x1B[34;43mline2\x1B[3;1H\x1B[1;37;40mline3",
        fbs.getWritten(),
    );
}

test "canvas dump" {
    var canvas = init(std.testing.allocator, .mode_2027);
    defer canvas.deinit();
    try canvas.resize(200, 24);

    var p = canvas.pen(0, 0);
    try p.writer().writeAll(
        \\Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris scelerisque aliquet dui
        \\eget feugiat. Nunc nec risus vehicula neque malesuada lobortis sit amet quis turpis. In
        \\finibus volutpat lacus sed viverra. Etiam sed nisl quis tellus luctus pulvinar non eu
        \\tortor. Maecenas convallis lacus ac sapien ornare venenatis. Pellentesque at cursus erat.
        \\Fusce condimentum ultricies cursus. Aliquam non.
    );

    var buf: std.BoundedArray(u8, 8192) = .{};
    canvas.dump(.{}, buf.writer()) catch |err| {
        std.debug.print("ERR: {}\n", .{err});
        return err;
    };

    try expectEqualSlices(
        u8,
        "\x1B[1;1H\x1B[0KLorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris scelerisque aliquet dui\x1B[2;1H\x1B[0Keget feugiat. Nunc nec risus vehicula neque malesuada lobortis sit amet quis turpis. In\x1B[3;1H\x1B[0Kfinibus volutpat lacus sed viverra. Etiam sed nisl quis tellus luctus pulvinar non eu\x1B[4;1H\x1B[0Ktortor. Maecenas convallis lacus ac sapien ornare venenatis. Pellentesque at cursus erat.\x1B[5;1H\x1B[0KFusce condimentum ultricies cursus. Aliquam non.",
        buf.constSlice(),
    );
}

// TODO: Tests for widths of certain weird codepoints, including zero widths
//       Test allocation failures
