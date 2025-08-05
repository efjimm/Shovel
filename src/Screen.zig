// TODO: Allow automatically resizing screens to fit content.
const std = @import("std");
const assert = std.debug.assert;
const expectEqualStrings = std.testing.expectEqualStrings;

const zg = @import("zg");

const GraphemeClusteringMode = @import("main.zig").GraphemeClusteringMode;
const spells = @import("spells.zig");
const stringWidth = @import("main.zig").stringWidth;
const Style = @import("Style.zig");
const TermInfo = @import("TermInfo.zig");
const util = @import("util.zig");

const Screen = @This();

text: std.ArrayListUnmanaged(u8),
lines: std.ArrayListUnmanaged(Line),
cells: std.MultiArrayList(Cell).Slice,

width: u16,
grapheme_clustering_mode: GraphemeClusteringMode,

gpa: std.mem.Allocator,

pub const Window = struct {
    screen: *const Screen,
    rect: Rect,
    x: u16,
    y: u16,
    visible: bool = true,
};

pub fn fullRect(s: *const Screen) Rect {
    return .{
        .width = s.width,
        .height = s.height(),
        .x = 0,
        .y = 0,
    };
}

/// Clamps the width/height of `rect` to the screen.
pub fn clampedRect(s: *const Screen, rect: Rect) Rect {
    return .{
        .height = @min(rect.height, s.height() -| rect.y),
        .width = @min(rect.width, s.width -| rect.x),
        .x = @min(rect.x, s.width),
        .y = @min(rect.y, s.height()),
    };
}

/// Draw all windows to `dest` in order. When windows overlap, the last drawn window takes
/// precedence.
pub fn flatten(dest: *Screen, windows: []const Window) !void {
    for (windows) |*window| {
        if (window.rect.x == 0 and window.rect.y == 0 and
            window.rect.width == dest.width and window.rect.height == dest.height())
        {
            try dest.text.ensureTotalCapacity(dest.gpa, window.screen.text.items.len);
            dest.text.clearRetainingCapacity();
            dest.text.appendSliceAssumeCapacity(window.screen.text.items);
            @memcpy(dest.lines.items, window.screen.lines.items);
            @memcpy(dest.cells.items(.lw), window.screen.cells.items(.lw));
            @memcpy(dest.cells.items(.style), window.screen.cells.items(.style));
            continue;
        }

        var dest_cursor: Cursor = .reset;
        var src_iter = window.screen.iterator(window.rect);
        var y = window.y;
        while (src_iter.next()) |src_line| : (y += 1) {
            dest.cursorTo(&dest_cursor, y, window.x);

            const dest_cells = dest.cells.subslice(
                dest_cursor.cell_offset,
                window.rect.width,
            );

            var replace_len: usize = 0;
            for (dest_cells.items(.lw)) |lw| replace_len += lw.len;

            try dest.text.replaceRange(
                dest.gpa,
                dest_cursor.text_offset,
                replace_len,
                src_line.text,
            );

            @memcpy(dest_cells.items(.lw), src_line.cells.items(.lw));
            @memcpy(dest_cells.items(.style), src_line.cells.items(.style));

            dest.lines.items[y].len -= replace_len;
            dest.lines.items[y].len += src_line.text.len;
        }
    }
}

/// Iterator over the lines in `rect`.
pub fn iterator(s: *const Screen, rect: Rect) Iterator {
    return .{
        .rect = rect,
        .s = s,
        .cursor = s.cursorAt(rect.y, rect.x, .{}),
    };
}

pub const Iterator = struct {
    s: *const Screen,
    rect: Rect,
    cursor: Cursor,

    pub const NextResult = struct {
        cells: std.MultiArrayList(Cell).Slice,
        text: []u8,
        text_off: usize,
    };

    pub fn next(iter: *Iterator) ?NextResult {
        const cy = iter.cursor.cell_offset / iter.s.width;
        if (cy - iter.rect.y >= iter.rect.height)
            return null;

        const cells = iter.s.cells.subslice(iter.cursor.cell_offset, iter.rect.width);
        var text_len: usize = 0;
        for (cells.items(.lw)) |lw| text_len += lw.len;
        const text_off = iter.cursor.text_offset;
        const text = iter.s.text.items[text_off..][0..text_len];

        iter.cursor = iter.s.cursorAt(@intCast(cy + 1), iter.rect.x, .{});
        return .{
            .cells = cells,
            .text = text,
            .text_off = text_off,
        };
    }
};

pub const Rect = struct {
    x: u16,
    y: u16,
    width: u16,
    height: u16,

    pub fn eql(a: Rect, b: Rect) bool {
        return a.x == b.x and a.y == b.y and a.width == b.width and a.height == b.height;
    }

    pub fn init(x: u16, y: u16, w: u16, h: u16) Rect {
        return .{
            .height = h,
            .width = w,
            .x = x,
            .y = y,
        };
    }
};

pub const OverflowMode = enum {
    /// Truncate any text written past the end of the rectangle.
    truncate,
    /// Wrap to the next line of the rectangle.
    wrap,
};

/// Writes to a rectangle on the screen.
pub const Writer = struct {
    s: *Screen,
    rect: Rect,
    interface: std.io.Writer,
    cursor: Cursor,

    overflow_mode: OverflowMode,

    /// The last position we successfully wrote at. This is used when a write call continues the last
    /// grapheme cluster of the previous write call.
    last_cursor: Cursor = .reset,
    /// The last codepoint written to the screen. This is used to check if a write is a continuation of
    /// a previously written grapheme cluster.
    last_cp: u21 = 0,
    /// This is used to check if a write is a continuation of a previously written grapheme cluster.
    state: zg.Graphemes.State = .reset,

    /// When this is set to `.ascii`, optimizes writes for ASCII characters.
    unicode_mode: UnicodeMode,

    pub const UnicodeMode = enum {
        ascii,
        unicode,
    };

    /// Writes the current style to all cells until the end of the line.
    pub fn styleToEol(w: *Writer) !void {
        try w.flush();
        for (w.cellsAtCursor().items(.style)) |*style| style.* = w.cursor.style;
    }

    /// Write the current style to the next `n` cells after the cursor.
    pub fn styleN(w: *Writer, n: u16) !void {
        try w.flush();
        for (w.cellsAtCursor().items(.style)[0..n]) |*style| style.* = w.cursor.style;
    }

    pub fn styleRect(w: *Writer, rect: Rect, style: Style) void {
        var iter = w.s.iterator(rect);
        while (iter.next()) |line| {
            for (line.cells.items(.style)) |*s|
                s.* = style;
        }
    }

    /// Clear the rectangle
    pub fn clear(w: *Writer) void {
        if (w.rect.x == 0 and w.rect.width == w.s.width) {
            if (w.rect.y == 0 and w.rect.height == w.s.height()) {
                w.s.text.clearRetainingCapacity();
                @memset(w.s.cells.items(.lw), .{ .len = 0, .width = 0 });
                @memset(w.s.cells.items(.style), .{});
                @memset(w.s.lines.items, .{ .len = 0 });
                w.cursor = .{
                    .cell_offset = 0,
                    .text_offset = 0,
                    .style = w.cursor.style,
                };
                return;
            }

            // Cells are contiguous
            const cells_start = w.rect.y * w.s.width;
            const cells_len = w.rect.height * w.s.width;
            const cells = w.s.cells.subslice(cells_start, cells_len);
            @memset(cells.items(.lw), .{ .len = 0, .width = 0 });
            @memset(cells.items(.style), .{});

            var text_off: usize = 0;
            for (w.s.lines.items[0..w.rect.y]) |line| text_off += line.len;
            var text_len: usize = 0;
            for (w.s.lines.items[w.rect.y..][0..w.rect.height]) |*line| {
                text_len += line.len;
                line.len = 0;
            }
            w.s.text.replaceRange(w.s.gpa, text_off, text_len, "") catch unreachable;
            w.cursor = .{
                .cell_offset = w.rect.y * w.s.width,
                .text_offset = text_off,
                .style = w.cursor.style,
            };
            return;
        }

        var dest_cursor: Cursor = .reset;
        var y: u16 = w.rect.y;
        while (y < w.rect.y + w.rect.height) : (y += 1) {
            w.s.cursorTo(&dest_cursor, y, w.rect.x);

            const dest_cells = w.s.cells.subslice(
                dest_cursor.cell_offset,
                w.rect.width,
            );

            var remove_len: usize = 0;
            for (dest_cells.items(.lw)) |lw| remove_len += lw.len;

            w.s.text.replaceRangeAssumeCapacity(dest_cursor.text_offset, remove_len, "");

            @memset(dest_cells.items(.lw), .{ .len = 0, .width = 0 });
            @memset(dest_cells.items(.style), .{});

            w.s.lines.items[y].len -= remove_len;
        }
    }

    pub fn clearToEol(w: *Writer) !void {
        try w.flush();
        var len: usize = 0;
        const cells = w.cellsAtCursor();
        for (cells.items(.lw), cells.items(.style)) |*lw, *style| {
            style.* = w.cursor.style;
            len += lw.len;
            lw.len = 0;
        }
        try w.s.text.replaceRange(w.s.gpa, w.cursor.text_offset, len, "");
        const line = w.cursor.cell_offset / w.s.width;
        w.s.lines.items[line].len -= len;
    }

    pub fn padAscii(w: *Writer, c: u8) !void {
        try w.flush();
        const remaining = w.remainingCellsInLine();
        try w.interface.splatByteAll(c, remaining);
    }

    pub fn setUnicodeMode(w: *Writer, mode: UnicodeMode) !void {
        if (w.unicode_mode != mode) {
            try w.flush();
            w.unicode_mode = mode;
        }
    }

    inline fn isAsciiOnly(text: []const u8) bool {
        var res = true;
        for (text) |c| res = res and c < 0x80;
        return res;
    }

    inline fn writeAscii(w: *Writer, text: []const u8) !usize {
        @branchHint(.likely);

        if (!w.s.cellInRect(w.cursor.cell_offset, w.rect)) {
            @branchHint(.unlikely);
            return text.len;
        }

        const res = w.writeCellsAscii(text);
        const truncated_text = text[0..res.data_len];
        if (res.data_len == 0) {
            if (res.discard_len == 0) {
                @branchHint(.unlikely);
                const line: u16 = @intCast(w.cursor.cell_offset / w.s.width);
                if (line >= w.rect.y +| w.rect.height) return text.len;
                w.s.cursorTo(&w.cursor, line + 1, w.rect.x);
            }
            return switch (w.overflow_mode) {
                .wrap => res.discard_len,
                .truncate => text.len,
            };
        }

        w.s.text.replaceRange(
            w.s.gpa,
            w.cursor.text_offset,
            res.replace_len,
            truncated_text,
        ) catch |err| {
            w.clear();
            return err;
        };

        // Screen line, not rect line
        const line: u16 = @intCast(w.cursor.cell_offset / w.s.width);
        if (res.data_len < res.replace_len) {
            w.s.lines.items[line].len -= res.replace_len - res.data_len;
        } else if (res.data_len > res.replace_len) {
            w.s.lines.items[line].len += res.data_len - res.replace_len;
        } else {
            @branchHint(.likely);
        }
        w.cursor.cell_offset += res.data_len;
        w.cursor.text_offset += res.data_len;

        // If we truncated it means the text doesn't fully fit on this line, so we need to move to
        // the next line. We may not have moved to the next line if the cursor is on the last cell
        // but the next character is a wide character, so that's what this check does.
        const truncated = res.data_len < text.len and w.cursor.cell_offset % w.rect.width != 0;
        if (w.overflow_mode == .wrap and (truncated or !w.s.cellInRect(w.cursor.cell_offset, w.rect))) {
            w.s.cursorTo(&w.cursor, line + 1, w.rect.x);
        }

        w.last_cp = 0;

        return switch (w.overflow_mode) {
            .wrap => res.data_len,
            .truncate => text.len,
        };
    }

    inline fn write(w: *Writer, text: []const u8) std.mem.Allocator.Error!usize {
        return switch (w.unicode_mode) {
            .ascii => w.writeAscii(text),
            .unicode => w.writeNonAscii(text),
        };
    }

    fn writeNonAscii(w: *Writer, text: []const u8) std.mem.Allocator.Error!usize {
        if (text.len == 0) return 0;

        if (text[0] > 0x7f and w.s.grapheme_clustering_mode == .grapheme and
            !zg.graphemes.isBreak(w.last_cp, zg.codepoint.decode(text).code, &w.state))
        {
            @branchHint(.unlikely);
            return w.continuePreviousWrite(text);
        } else if (!w.s.cellInRect(w.cursor.cell_offset, w.rect)) {
            @branchHint(.unlikely);
            return text.len;
        } else if (isAsciiOnly(text)) {
            return w.writeAscii(text);
        }

        w.state = .reset;
        const end = util.truncatePartialCodepoint(text) catch
            unreachable; // Invalid UTF-8
        const valid_text = text[0..end];

        const res = w.writeCells(valid_text);
        const truncated_text = valid_text[0..res.data_len];
        if (res.data_len == 0) return switch (w.overflow_mode) {
            .wrap => res.discard_len,
            .truncate => text.len,
        };

        try w.s.text.replaceRange(w.s.gpa, w.cursor.text_offset, res.replace_len, truncated_text);

        // Screen line, not rect line
        const line: u16 = @intCast(w.cursor.cell_offset / w.s.width);
        if (res.data_len < res.replace_len) {
            w.s.lines.items[line].len -= res.replace_len - res.data_len;
        } else {
            w.s.lines.items[line].len += res.data_len - res.replace_len;
        }

        w.cursor.cell_offset += res.width;
        w.cursor.text_offset += res.data_len;

        w.last_cursor = .{
            .cell_offset = w.cursor.cell_offset - res.last_width,
            .text_offset = w.cursor.text_offset,
            .style = .{},
        };

        // If we truncated it means the text doesn't fully fit on this line, so we need to move to
        // the next line. We may not have moved to the next line if the cursor is on the last cell
        // but the next character is a wide character, so that's what this check does.
        const truncated = res.data_len < valid_text.len and w.cursor.cell_offset % w.rect.width != 0;
        if (!w.s.cellInRect(w.cursor.cell_offset, w.rect) or truncated) {
            if (w.overflow_mode == .wrap)
                w.s.cursorTo(&w.cursor, line + 1, w.rect.x);
        }

        var cp_iter: zg.codepoint.Iterator = .initEnd(truncated_text);
        w.last_cp = cp_iter.prev().?.code;

        return switch (w.overflow_mode) {
            .wrap => res.data_len,
            .truncate => text.len,
        };
    }

    const WriteCellsResult = struct {
        /// Number of bytes to write from `data`.
        data_len: usize,
        /// Total width of the bytes to write.
        width: usize,

        /// Number of bytes to discard from `data` after writing.
        /// This is used to discard zero width characters.
        discard_len: usize,

        /// Number of bytes to replace from the text buffer.
        replace_len: usize,

        /// When the grapheme clustering mode is set to `.grapheme`. this is the final state of the
        /// grapheme iterator when returning from this function. This is used to check if a subsequent
        /// write continues the last grapheme cluster of the previous write.
        state: zg.Graphemes.State,

        /// Width of the last character written. This is used to get the last written cell position.
        last_width: usize,
    };

    pub fn remainingCellsInLine(w: *const Writer) usize {
        const cx = w.cursor.cell_offset % w.s.width;
        return w.rect.width -| (cx -| w.rect.x);
    }

    fn cellsAtCursor(w: *const Writer) std.MultiArrayList(Cell).Slice {
        const len = w.remainingCellsInLine();
        const off = w.cursor.cell_offset;
        return w.s.cells.subslice(off, len);
    }

    fn writeCells(w: *Writer, data: []const u8) WriteCellsResult {
        const WidthIterator = @import("main.zig").WidthIterator;

        const remaining_cells = w.remainingCellsInLine();
        var cells = w.cellsAtCursor();

        var iter: WidthIterator = .init(data, w.s.grapheme_clustering_mode, remaining_cells);

        var width: usize = 0;
        var len: usize = 0;
        var replace_len: usize = 0;
        var last_width: usize = 0;
        var discard_len: usize = 0;
        while (iter.next()) |res| : (width += res.width) {
            assert(res.width <= 2);

            // We don't care about zero-width characters.
            if (res.width == 0) {
                @branchHint(.unlikely);
                discard_len = res.len;
                break;
            }

            len = res.offset + res.len;
            const char_bytes = data[res.offset..][0..res.len];

            // Grapheme cluster longer than 2^15 bytes, just ignore it.
            if (char_bytes.len > std.math.maxInt(u15)) {
                @branchHint(.cold);
                continue;
            }

            replace_len += cells.items(.lw)[width].len;
            cells.set(width, .{
                .lw = .{
                    .len = @intCast(char_bytes.len),
                    .width = @intCast(res.width - 1),
                },
                .style = w.cursor.style,
            });

            // We got a wide character. Blank the next cell.
            if (res.width == 2) {
                @branchHint(.unlikely);
                cells.set(width + 1, .blank);
            }
            last_width = res.width;
        }

        return .{
            .data_len = len,
            .replace_len = replace_len,
            .width = width,
            .state = iter.graphemes.state,
            .last_width = last_width,
            .discard_len = discard_len,
        };
    }

    const WriteCellsAsciiResult = struct {
        /// Number of bytes to write from `data`.
        data_len: usize,
        /// Number of bytes to replace from the text buffer.
        replace_len: usize,
        discard_len: usize,
    };

    inline fn writeCellsAscii(w: *Writer, data: []const u8) WriteCellsAsciiResult {
        const remaining_cells = w.remainingCellsInLine();
        if (remaining_cells == 0) {
            return .{
                .data_len = 0,
                .replace_len = 0,
                .discard_len = 0,
            };
        }

        const tlen = @min(data.len, remaining_cells);
        var cells = w.cellsAtCursor().subslice(0, tlen);

        var replace_len: usize = 0;
        for (data[0..tlen], cells.items(.lw), cells.items(.style), 0..) |c, *lw, *style, i| {
            assert(c < 0x80); // Encountered non-ASCII byte in ASCII mode
            const width = ascii_widths[c];

            // We don't care about zero-width characters.
            if (width == 0) {
                @branchHint(.unlikely);

                return .{
                    .data_len = i,
                    .replace_len = replace_len,
                    .discard_len = 1,
                };
            }

            replace_len += lw.len;
            lw.* = .{ .len = 1, .width = 0 };
            style.* = w.cursor.style;
        }

        return .{
            .data_len = tlen,
            .replace_len = replace_len,
            .discard_len = 0,
        };
    }

    /// Write `data` as a continuation of the last written grapheme cluster.
    ///
    /// Partial codepoints are handled by ignoring them and keeping them in the internal buffer
    /// instead. This works because codepoints are a maximum of 4 bytes. The partial codepoints
    /// will then be completed by subsequent calls that fill up the writer's internal buffer.
    ///
    /// Grapheme clusters on the other hand, have an unbounded length. A maliciously crafted
    /// grapheme cluster can be terabytes in size, and it would be valid unicode. We only
    /// handle grapheme clusters up to 2^15 in size but we don't want to impose such high space
    /// requirements on the writer's internal buffer. So instead, we commit partial grapheme
    /// clusters to the underlying screen and check if a following write call continues the last
    /// written grapheme cluster. If it does, we append in-place to the last position we wrote
    /// to. That is what this branch does.
    fn continuePreviousWrite(w: *Writer, data: []const u8) !usize {
        @branchHint(.unlikely);
        assert(w.s.grapheme_clustering_mode == .grapheme);

        // Get the first grapheme
        var iter = zg.graphemes.iterator(data);
        iter.state = w.state;
        const first = iter.nextState().?;

        // Check how many bytes we can fit
        const cell_len = &w.s.cells.items(.lw)[w.last_cursor.cell_offset].len;
        const remaining_len = std.math.maxInt(u15) - cell_len.*;
        const len: u15 = @intCast(@min(remaining_len, first.len));
        if (len == 0) {
            @branchHint(.cold);
            return first.len;
        }

        const end: u15 = @intCast(util.truncatePartialCodepoint(first.bytes(data)[0..len]) catch
            unreachable); // Invalid UTF-8
        const bytes = data[0..end];

        try w.s.text.insertSlice(w.s.gpa, w.last_cursor.text_offset, bytes);
        errdefer comptime unreachable;

        // Screen line, not rect line
        const line = w.last_cursor.cell_offset / w.s.width;
        w.s.lines.items[line].len += end;
        cell_len.* += end;
        w.last_cursor.text_offset += end;

        var cp_iter: zg.codepoint.Iterator = .initEnd(bytes);
        w.last_cp = cp_iter.prev().?.code;
        w.state = iter.state;

        return first.len;
    }

    pub fn flush(w: *Writer) !void {
        try ioFlush(&w.interface);
    }

    fn ioFlush(w: *std.io.Writer) !void {
        const wr: *Writer = @fieldParentPtr("interface", w);
        switch (wr.unicode_mode) {
            .ascii => {
                while (w.end != 0) {
                    const bytes_written = wr.write(w.buffered()) catch return error.WriteFailed;
                    _ = w.consume(bytes_written);
                }
            },
            .unicode => {
                while (w.end != 0) {
                    const buffered = w.buffered();
                    const expected_len = std.unicode.utf8ByteSequenceLength(buffered[0]) catch 1;
                    if (buffered.len < expected_len) break;
                    const bytes_written = wr.write(buffered) catch return error.WriteFailed;
                    _ = w.consume(bytes_written);
                }
            },
        }
    }

    fn drain(io_writer: *std.io.Writer, data: []const []const u8, splat: usize) !usize {
        const w: *Writer = @fieldParentPtr("interface", io_writer);
        const buffered = w.interface.buffered();
        if (buffered.len > 0) {
            const bytes_written = w.write(buffered) catch return error.WriteFailed;
            _ = w.interface.consume(bytes_written);

            if (bytes_written != buffered.len) return 0;
        }

        var total_len: usize = 0;
        for (data[0 .. data.len - 1]) |slice| {
            const bytes_written = w.write(slice) catch
                return error.WriteFailed;

            total_len += bytes_written;

            if (bytes_written < slice.len) return total_len;
        }

        const pattern = data[data.len - 1];
        for (0..splat) |_| {
            const bytes_written = w.write(pattern) catch
                return error.WriteFailed;

            total_len += bytes_written;

            if (bytes_written < pattern.len) return total_len;
        }
        return total_len;
    }

    pub fn setStyle(w: *Writer, style: Style) !void {
        try w.flush();
        w.cursor.style = style;
    }

    /// Sets the current cursor position, flushing any buffered text beforehand. It is allowed to
    /// set the cursor position off the screen. Writing to a position off screen will do nothing.
    pub fn setCursor(w: *Writer, row: u16, col: u16) !void {
        try w.flush(); // Avoid vtable
        // w.s.cursorTo(&w.cursor, w.rect.y + row, w.rect.x + col);
        w.cursor = w.s.cursorAt(w.rect.y + row, w.rect.x + col, w.cursor.style);
    }

    /// Asserts that rect does not go off screen.
    ///
    /// Also see `setRectClamp`.
    pub fn setRect(w: *Writer, rect: Rect) !void {
        assert(rect.y + rect.height <= w.s.height());
        assert(rect.x + rect.width <= w.s.width);

        try w.flush();

        w.rect = rect;
        w.s.cursorTo(&w.cursor, rect.y, rect.x);
    }

    /// Sets the current rectangle of the writer. If the rectangle does not entirely fit within the
    /// screen, it's dimensions are adjusted to make it fit. Zero sized rectangles are allowed.
    pub fn setRectClamp(w: *Writer, rect: Rect) !void {
        try w.setRect(w.s.clampedRect(rect));
    }

    pub fn setRectAtCursor(w: *Writer, rect_width: u16, rect_height: u16) !void {
        try w.flush();
        const cx: u16 = @intCast(w.cursor.cell_offset % w.s.width);
        const cy: u16 = @intCast(w.cursor.cell_offset / w.s.width);

        w.rect = .init(cx, cy, rect_width, rect_height);
    }

    pub fn setRectClampAtCursor(w: *Writer, rect_width: u16, rect_height: u16) !void {
        try w.flush();
        const cx: u16 = @intCast(w.cursor.cell_offset % w.s.width);
        const cy: u16 = @intCast(w.cursor.cell_offset / w.s.width);

        w.rect = .{
            .height = @min(rect_height, w.s.height() -| cy),
            .width = @min(rect_width, w.s.width -| cx),
            .x = @min(cx, w.s.width),
            .y = @min(cy, w.s.height()),
        };
    }
};

pub const Line = struct {
    len: usize,
};

pub const Cell = struct {
    lw: packed struct {
        len: u15,
        width: u1,
    },
    style: Style,

    pub const blank: Cell = .{
        .lw = .{ .len = 0, .width = 0 },
        .style = .{},
    };
};

pub const Cursor = struct {
    cell_offset: usize,
    text_offset: usize,
    style: Style,

    pub const reset: Cursor = .{
        .cell_offset = 0,
        .text_offset = 0,
        .style = .{},
    };
};

/// Asserts `buffer.len >= 4`.
pub fn init(
    gpa: std.mem.Allocator,
    grapheme_clustering_mode: GraphemeClusteringMode,
) Screen {
    return .{
        .grapheme_clustering_mode = grapheme_clustering_mode,
        .text = .empty,
        .lines = .empty,
        .cells = .empty,
        .width = 0,
        .gpa = gpa,
    };
}

pub fn deinit(s: *Screen) void {
    s.text.deinit(s.gpa);
    s.lines.deinit(s.gpa);
    s.cells.deinit(s.gpa);
}

/// Allocate enough memory to fit the given width and height. Does not allocate any extra memory
/// for text, just cell and line data.
pub fn ensureResizeCapacity(s: *Screen, new_width: u16, new_height: u16) !void {
    const n = @as(u32, new_width) * new_height;
    try s.ensureTotalCellCapacity(n);
    try s.lines.ensureTotalCapacity(s.gpa, new_height);
}

/// Resizes the screen to fit the new dimensions, clearing all text.
pub fn resize(s: *Screen, new_width: u16, new_height: u16) !void {
    try s.ensureResizeCapacity(new_width, new_height);
    s.resizeAssumeCapacity(new_width, new_height);
}

/// Resizes the screen and clears all text.
pub fn resizeAssumeCapacity(s: *Screen, new_width: u16, new_height: u16) void {
    const n = @as(u32, new_width) * new_height;
    s.cells.len = n;
    s.lines.items.len = new_height;
    @memset(s.cells.items(.lw), .{ .len = 0, .width = 0 });
    @memset(s.cells.items(.style), .{});
    @memset(s.lines.items, .{ .len = 0 });
    s.width = new_width;
}

/// Move the cursor to `row, col`. Moves from the current position, so it'll be more efficient if
/// the new position is near the old one.
pub fn cursorTo(s: *const Screen, cursor: *Cursor, row: u16, col: u16) void {
    if (row >= s.height() or col >= s.width) {
        cursor.* = .{
            .cell_offset = s.cells.len,
            .text_offset = s.text.items.len,
            .style = cursor.style,
        };
        return;
    }

    var off: usize = 0;
    const old_row = cursor.cell_offset / s.width;
    if (row != old_row) {
        for (s.lines.items[0..row]) |line|
            off += line.len;
        for (s.cells.items(.lw)[row * s.width ..][0..col]) |lw|
            off += lw.len;
    } else {
        off = cursor.text_offset;
        const old_col = cursor.cell_offset % s.width;
        if (old_col < col) {
            for (s.cells.items(.lw)[row * s.width ..][old_col..col]) |lw|
                off += lw.len;
        } else {
            for (s.cells.items(.lw)[row * s.width ..][col..old_col]) |lw|
                off -= lw.len;
        }
    }

    cursor.* = .{
        .cell_offset = row * s.width + col,
        .text_offset = off,
        .style = cursor.style,
    };
}

/// Returns a cursor at the given position.
fn cursorAt(s: *const Screen, row: u16, col: u16, style: Style) Cursor {
    if (row >= s.height() or col >= s.width) return .{
        .cell_offset = s.cells.len,
        .text_offset = s.text.items.len,
        .style = style,
    };

    var off: usize = 0;
    for (s.lines.items[0..row]) |line|
        off += line.len;
    for (s.cells.items(.lw)[row * s.width ..][0..col]) |c|
        off += c.len;
    return .{
        .cell_offset = row * s.width + col,
        .text_offset = off,
        .style = style,
    };
}

pub fn height(s: *const Screen) u16 {
    return @intCast(s.lines.items.len);
}

/// Write out the entire contents of the screen. Attempts to home the cursor first, if the given
/// terminfo defines `clear_screen` or `cursor_home`. Otherwise uses zero escape sequences.
///
/// Generally you should use `DoubleBuffer.dump` instead.
pub fn dump(s: *const Screen, ti: *const TermInfo, w: *std.io.Writer) !void {
    var current_style: Style = .{};
    var off: usize = 0;
    var cell_off: usize = 0;
    var y: u16 = 0;
    while (cell_off < s.cells.len) : ({
        cell_off += s.width;
        y += 1;
    }) {
        // try TermInfo.writeParamSequence(move, w, .{ y, 0 });
        const cells = s.cells.subslice(cell_off, s.width);
        for (cells.items(.lw), cells.items(.style)) |lw, style| {
            const text = s.text.items[off..][0..lw.len];

            if (!Style.eql(current_style, style)) {
                try style.dump(w, .{ .terminfo = ti });
                current_style = style;
            }

            if (text.len == 0) {
                try w.writeByte(' ');
            } else {
                try w.writeAll(text);
            }

            off += lw.len;
        }

        if (cell_off < s.cells.len - s.width)
            try w.writeAll("\r\n");
    }
}

/// Writes the bytes necessary to transform `old` into `new` to the given writer.
/// Works on a line by line basis.
///
/// If the supplied terminfo does not have a sequence to address the cursor, the entire
/// contents of `new` will by output.
pub fn dumpDiff(new: *Screen, old: *Screen, ti: *const TermInfo, w: *std.io.Writer) !void {
    assert(new.width == old.width);
    assert(new.height() == old.height());

    const move = ti.getStringCapability(.cursor_address) orelse {
        // Very dumb terminal that doesn't have an addressable cursor, OR a messed up terminfo. The
        // terminfo API in shovel has a way to specify fallbacks for things like this, so if we get
        // here unintentionally it's a programmer error. Enjoy your lack of escape sequences!
        //
        // Redraw the whole UI because we can't address the cursor.
        try new.dump(ti, w);
        return;
    };
    var current_style: Style = .{};
    try current_style.dump(w, .{ .terminfo = ti });

    var l1_off: usize = 0;
    var l2_off: usize = 0;
    for (new.lines.items, old.lines.items, 0..) |l1, l2, y| {
        const l1_text = new.text.items[l1_off..][0..l1.len];
        const l2_text = old.text.items[l2_off..][0..l2.len];
        l2_off += l2.len;
        if (std.mem.eql(u8, l1_text, l2_text)) {
            const cell_off: u16 = @intCast(y * new.width);
            const l1_cells = new.cells.subslice(cell_off, new.width);
            const l2_cells = old.cells.subslice(cell_off, new.width);
            for (
                l1_cells.items(.lw),
                l1_cells.items(.style),
                l2_cells.items(.lw),
                l2_cells.items(.style),
            ) |lw1, style1, lw2, style2| {
                if (lw1 != lw2 or !style1.eql(style2)) break;
            } else {
                l1_off += l1.len;
                continue;
            }
        }

        try TermInfo.writeParamSequence(move, w, .{ @as(i32, @intCast(y)), 0 });
        const cells = new.cells.subslice(y * new.width, new.width);
        var total_len: usize = 0;
        var blank_start: usize = 0;
        for (cells.items(.lw), cells.items(.style)) |lw, style| {
            total_len += lw.len;

            const text = new.text.items[l1_off..][0..lw.len];
            l1_off += lw.len;

            if ((text.len == 0 or text[0] == ' ') and style.eql(current_style)) {
                blank_start += 1;
                continue;
            }

            if (blank_start > 0) {
                try w.splatByteAll(' ', blank_start);
            }

            if (!Style.eql(current_style, style)) {
                try style.dump(w, .{ .terminfo = ti, .diff = current_style });
                current_style = style;
            }

            if (text.len == 0) {
                blank_start = 1;
            } else {
                try w.writeAll(text);
                blank_start = 0;
            }
        }

        if (blank_start > 0) {
            try w.splatByteAll(' ', blank_start);
        }
    }
}

pub const DoubleBuffer = struct {
    read: Screen,
    write: Screen,
    dump_all: bool,
    ti: *const TermInfo,

    pub fn init(
        gpa: std.mem.Allocator,
        ti: *const TermInfo,
        grapheme_clustering_mode: GraphemeClusteringMode,
    ) DoubleBuffer {
        return .{
            .read = .init(gpa, grapheme_clustering_mode),
            .write = .init(gpa, grapheme_clustering_mode),
            .ti = ti,
            .dump_all = false,
        };
    }

    pub fn deinit(db: *DoubleBuffer) void {
        db.read.deinit();
        db.write.deinit();
    }

    pub fn resize(d: *DoubleBuffer, new_width: u16, new_height: u16) !void {
        try d.read.ensureResizeCapacity(new_width, new_height);
        try d.write.ensureResizeCapacity(new_width, new_height);

        d.read.resizeAssumeCapacity(new_width, new_height);
        d.write.resizeAssumeCapacity(new_width, new_height);

        // Screens are cleared on resize, but the terminal emulator's screen buffer isn't. So we use
        // this flag to force redraw everything after a resize.
        d.dump_all = true;
    }

    pub fn dump(d: *DoubleBuffer, w: *std.io.Writer) !void {
        if (d.dump_all) {
            if (d.ti.getStringCapability(.clear_screen) orelse d.ti.getStringCapability(.cursor_home)) |clear|
                try w.writeAll(clear);

            try d.write.dump(d.ti, w);
            d.dump_all = false;
        } else {
            try d.write.dumpDiff(&d.read, d.ti, w);
        }

        try d.read.text.ensureTotalCapacity(d.read.gpa, d.write.text.items.len);
        try d.read.ensureTotalCellCapacity(d.write.cells.len);
        try d.read.lines.ensureTotalCapacity(d.read.gpa, d.write.lines.items.len);

        d.read.text.clearRetainingCapacity();
        d.read.text.appendSliceAssumeCapacity(d.write.text.items);
        d.read.cells.len = d.write.cells.len;
        @memcpy(d.read.cells.items(.lw), d.write.cells.items(.lw));
        @memcpy(d.read.cells.items(.style), d.write.cells.items(.style));
        d.read.lines.clearRetainingCapacity();
        d.read.lines.appendSliceAssumeCapacity(d.write.lines.items);
    }
};

pub fn ensureTotalCellCapacity(s: *Screen, n: usize) !void {
    var m = s.cells.toMultiArrayList();
    try m.ensureTotalCapacity(s.gpa, n);
    s.cells = m.slice();
}

pub fn lineText(s: *const Screen, line: u16) []u8 {
    var off: usize = 0;
    for (s.lines.items[0..line]) |l| off += l.len;
    return s.text.items[off..][0..s.lines.items[line].len];
}

pub fn colsText(s: *const Screen, line: u16, col: u16, n: u16) []u8 {
    var off: usize = 0;
    for (s.lines.items[0..line]) |l| off += l.len;
    for (s.cells.items(.lw)[line * s.width ..][0..col]) |lw| off += lw.len;
    var len: usize = 0;
    for (s.cells.items(.lw)[line * s.width + col ..][0..n]) |lw| len += lw.len;
    return s.text.items[off..][0..len];
}

pub fn cellInRect(s: *const Screen, cell: usize, rect: Rect) bool {
    const x = cell % s.width;
    const y = cell / s.width;
    return y >= rect.y and y - rect.y < rect.height and
        x >= rect.x and x - rect.x < rect.width;
}

/// Returns a writer to the specified rectangle on screen. If the writer encounters OOM, the
/// rectangle area of the screen will be cleared.
pub fn writer(
    s: *Screen,
    /// Writer buffer.
    buffer: []u8,
    /// The rectangular region of the screen to write to.
    rect: Rect,
    /// Behaviour when writing past the end of the rectangle.
    overflow_mode: OverflowMode,
    /// Write in ASCII or unicode mode. Unicode mode works for all text, but is slower. ASCII mode
    /// is faster at writing but asserts that all text written is ASCII.
    ///
    /// Use unicode mode by default, and ASCII mode when you can guarantee all your text will be
    /// ASCII.
    ///
    /// You can change the unicode mode at any point by calling `Writer.setUnicodeMode`.
    unicode_mode: Writer.UnicodeMode,
) Writer {
    assert(buffer.len >= 4); // Required for buffering unicode codepoints
    return .{
        .overflow_mode = overflow_mode,
        .s = s,
        .rect = rect,
        .cursor = s.cursorAt(rect.y, rect.x, .{}),
        .interface = .{
            .buffer = buffer,
            .vtable = &.{
                .drain = Writer.drain,
                .flush = Writer.ioFlush,
            },
        },
        .unicode_mode = unicode_mode,
    };
}

/// Returns a writer covering the whole screen.
pub fn writerFull(
    s: *Screen,
    buffer: []u8,
    overflow_mode: OverflowMode,
    unicode_mode: Writer.UnicodeMode,
) Writer {
    return s.writer(buffer, .init(0, 0, s.width, s.height()), overflow_mode, unicode_mode);
}

test "screen raw writes ascii" {
    try initData();
    defer deinitData();

    var s: Screen = .init(std.testing.allocator, .grapheme);
    defer s.deinit();

    try s.resize(10, 3);

    var buf: [128]u8 = undefined;
    var w = s.writerFull(&buf, .wrap, .ascii);

    var bytes_written = try w.write("idk someth");
    try std.testing.expectEqual(10, bytes_written);
    bytes_written = try w.write("some more");
    try std.testing.expectEqual(9, bytes_written);
    bytes_written = try w.write("wrap around");
    try std.testing.expectEqual(1, bytes_written);
    bytes_written = try w.write("rap around");
    try std.testing.expectEqual(10, bytes_written);

    try std.testing.expectEqualStrings("idk someth", s.lineText(0));
    try std.testing.expectEqualStrings("some morew", s.lineText(1));
    try std.testing.expectEqualStrings("rap around", s.lineText(2));
}

test "screen writer" {
    try initData();
    defer deinitData();

    var s: Screen = .init(std.testing.allocator, .grapheme);
    defer s.deinit();

    try s.resize(10, 3);

    var buf: [128]u8 = undefined;
    var w = s.writerFull(&buf, .wrap, .ascii);

    try w.interface.writeAll("idk someth");
    try w.interface.writeAll("some more");
    try w.interface.writeAll("wrap around");
    try w.interface.flush();

    try std.testing.expectEqualStrings("idk someth", s.lineText(0));
    try std.testing.expectEqualStrings("some morew", s.lineText(1));
    try std.testing.expectEqualStrings("rap around", s.lineText(2));
}

test "screen wide characters" {
    try initData();
    defer deinitData();

    var s: Screen = .init(std.testing.allocator, .grapheme);
    defer s.deinit();

    try s.resize(10, 3);

    var buf: [128]u8 = undefined;
    var w = s.writerFull(&buf, .wrap, .unicode);

    try w.interface.writeAll("🧑‍🌾🧑‍🌾🧑‍🌾🧑‍🌾🧑‍🌾🧑‍🌾");
    try w.interface.writeAll("hi :)");
    try w.setCursor(2, 0);
    try w.interface.writeAll("h🧑‍🌾🧑‍🌾🧑‍🌾🧑‍🌾🧑‍🌾");
    try w.interface.writeAll("guh");
    try w.interface.flush();

    try std.testing.expectEqualStrings("🧑‍🌾🧑‍🌾🧑‍🌾🧑‍🌾🧑‍🌾", s.lineText(0));
    try std.testing.expectEqualStrings("🧑‍🌾hi :)", s.lineText(1));
    try std.testing.expectEqualStrings("h🧑‍🌾🧑‍🌾🧑‍🌾🧑‍🌾", s.lineText(2));
}

test "screen split graphemes across writes" {
    try initData();
    defer deinitData();

    var s: Screen = .init(std.testing.allocator, .grapheme);
    defer s.deinit();

    try s.resize(2, 1);
    var buf: [128]u8 = undefined;
    var w = s.writerFull(&buf, .wrap, .unicode);

    try w.interface.writeAll("🧑");
    try w.interface.flush();
    try w.interface.writeAll("\u{200D}");
    try w.interface.flush();
    try w.interface.writeAll("🌾");
    try w.interface.flush();

    try std.testing.expectEqualStrings("🧑‍🌾", s.lineText(0));
}

test "screen graphemes to codepoints" {
    try initData();
    defer deinitData();

    var s: Screen = .init(std.testing.allocator, .codepoint);
    defer s.deinit();

    try s.resize(6, 1);
    var buf: [128]u8 = undefined;
    var w = s.writerFull(&buf, .wrap, .unicode);

    try w.interface.writeAll("🧑");
    try w.interface.flush();
    try w.interface.writeAll("\u{200D}🌾");
    try w.interface.flush();
    try w.interface.writeAll("🌾");
    try w.interface.flush();

    try std.testing.expectEqualStrings("🧑🌾🌾", s.lineText(0));
}

test "screen OOM handling" {
    try initData();
    defer deinitData();

    for (0..500) |i| {
        var fa: std.testing.FailingAllocator = .init(
            std.testing.allocator,
            .{ .fail_index = i },
        );
        var s: Screen = .init(fa.allocator(), .codepoint);
        defer s.deinit();

        s.resize(6, 3) catch continue;
        var buf: [128]u8 = undefined;
        var w = s.writerFull(&buf, .wrap, .ascii);

        try w.interface.writeAll("l");
        w.interface.flush() catch {
            try expectEqualStrings("", s.lineText(0));
            continue;
        };
        try w.interface.writeAll("y");
        w.interface.flush() catch {
            try expectEqualStrings("l", s.lineText(0));
            continue;
        };
        try w.interface.writeAll("j");
        w.setCursor(1, 1) catch {
            try expectEqualStrings("ly", s.lineText(0));
            continue;
        };
        try w.interface.writeAll("hi");
        w.interface.flush() catch {
            try expectEqualStrings("lyj", s.lineText(0));
            try expectEqualStrings("", s.lineText(1));
            continue;
        };
        break;
    }
}

test "flush partial codepoint" {
    try initData();
    defer deinitData();

    var s: Screen = .init(std.testing.allocator, .codepoint);
    defer s.deinit();

    try s.resize(80, 20);

    var buf: [128]u8 = undefined;
    var w = s.writerFull(&buf, .wrap, .unicode);

    try w.interface.writeByte(0xc1);
    try w.interface.flush();
}

test "rectangular writes" {
    try initData();
    defer deinitData();

    var s: Screen = .init(std.testing.allocator, .codepoint);
    defer s.deinit();

    try s.resize(80, 20);

    var buf: [128]u8 = undefined;
    var wr = s.writer(&buf, .init(10, 10, 10, 10), .wrap, .ascii);
    const w = &wr.interface;

    try w.writeAll("howdy");
    try w.flush();

    try expectEqualStrings("", s.colsText(10, 0, 10));
    try expectEqualStrings("", s.colsText(10, 10, 0));
    try expectEqualStrings("how", s.colsText(10, 10, 3));
    try expectEqualStrings("howd", s.colsText(10, 10, 4));
    try expectEqualStrings("howdy", s.colsText(10, 10, 5));
    try expectEqualStrings("howdy", s.colsText(10, 10, 10));
    try expectEqualStrings("dy", s.colsText(10, 13, 5));
    try expectEqualStrings("", s.colsText(10, 15, 5));

    try w.writeAll("This is a long string");
    try w.flush();

    try expectEqualStrings("howdyThis ", s.colsText(10, 10, 10));
    try expectEqualStrings("is a long ", s.colsText(11, 10, 10));
    try expectEqualStrings("string", s.colsText(12, 10, 10));

    try wr.setRect(.init(0, 0, 5, 1));
    try w.writeAll("hey there");
    try w.flush();

    try expectEqualStrings("hey t", s.colsText(0, 0, 80));

    try w.writeAll("More stuff");
    try w.flush();

    try expectEqualStrings("hey t", s.colsText(0, 0, 80));

    try wr.setRect(.init(1, 1, 0, 0));
    try w.writeAll("Howdy");
    try w.flush();

    try expectEqualStrings("", s.colsText(1, 1, 80));
}

fn fuzz(_: void, bytes: []const u8) !void {
    var s: Screen = .init(std.testing.allocator, .grapheme);
    defer s.deinit();

    try s.resize(256, 256);

    var buf: [128]u8 = undefined;
    var w = s.writerFull(&buf, .wrap, .unicode);

    for (bytes, 0..) |c, i| {
        try w.interface.writeByte(c);
        try w.interface.flush();
        if (i > 0 and i % 40 == 0) {
            try w.setCursor(bytes[i - 1], bytes[i - 2]);
        }
    }
}

test "screen fuzz" {
    try initData();
    defer deinitData();

    try std.testing.fuzz({}, fuzz, .{});
}

fn initData() !void {
    try @import("main.zig").initUnicodeData(std.testing.allocator);
}

fn deinitData() void {
    @import("main.zig").deinitUnicodeData(std.testing.allocator);
}

const ascii_widths: [128]u8 = .{
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
};
