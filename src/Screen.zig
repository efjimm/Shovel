//! Virtual representation of a terminal screen. Allows writing out the minimal number of bytes !
//! necessary to tranform one screen into another, allowing for good rendering performance across
//! all terminal emulators without having to manually track what changed. Use `DoubleBuffer` for
//! this.
//
// This implementation is somewhat based off of Ghostty's page implementation. See license
// attribution at the end of this file.
//
// TODO:
//   The width of characters is not 100% consistent between terminal emulators. The best way to
//   handle this would be to create separate tables for width data for each terminal emulator and
//   set it based on TERM.
//
//   Right now width data is based on what `zg` reports, which is derived from the East_Asian_Width
//   unicode property. This should be 99.9% correct, but currently I know of a few edge cases in
//   certain terminal emulators which are not correct. The Linux TTY displays emojis as narrow
//   characters, and non mode 2027 terminals have inconsistent widths for the emoji variation
//   selector.

// TODO:
//   - Represent actual terminal cursor
//   - Benchmarks
const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const Style = @import("Style.zig");
const stringWidth = @import("main.zig").stringWidth;
const BitmapAllocator = @import("bma.zig").BitmapAllocator;
const GraphemeClusteringMode = @import("main.zig").GraphemeClusteringMode;

const zg = @import("zg");

const Screen = @This();

const GraphemeAlloc = BitmapAllocator(grapheme_chunk_len);
const GraphemeMap = std.AutoHashMapUnmanaged(Cell.Index, GraphemeSlice);
const grapheme_chunk_len = 16;

const GraphemeSlice = struct {
    offset: usize,
    len: usize,
};

rows: std.ArrayList(Row),
cells: std.ArrayList(Cell),

grapheme_alloc: GraphemeAlloc,
grapheme_map: GraphemeMap,

styles: std.MultiArrayList(struct {
    refcount: u16,
    style: Style.Packed,
}),

size: struct { rows: u16, cols: u16 },

pub const empty: Screen = .{
    .rows = .empty,
    .cells = .empty,
    .grapheme_alloc = .empty,
    .grapheme_map = .empty,
    .styles = .empty,
    .size = .{ .rows = 0, .cols = 0 },
};

pub const Cell = packed struct(u64) {
    unused: u11 = 0,
    style: Cell.CellStyle,
    content_tag: enum(u2) {
        ascii,
        codepoint,
        codepoint_grapheme,
    },
    wide: enum(u2) {
        narrow,
        wide,
        spacer_tail,
        spacer_head,
    },
    codepoint: u21,

    pub const CellStyle = packed struct(u28) {
        attrs: Style.Attribute,
        color_type: enum(u1) { palette, rgb },
        colors: packed union(u18) {
            palette: packed struct(u18) {
                fg: packed struct(u9) {
                    none: bool,
                    @"256": u8,
                },
                bg: packed struct(u9) {
                    none: bool,
                    @"256": u8,
                },
            },
            rgb: packed struct(u18) {
                unused: u2 = 0,
                id: u16,
            },
        },

        pub const default: CellStyle = .{
            .attrs = .none,
            .color_type = .palette,
            .colors = .{
                .palette = .{
                    .fg = .{ .none = true, .@"256" = 0 },
                    .bg = .{ .none = true, .@"256" = 0 },
                },
            },
        };
    };

    pub const Index = enum(u32) {
        _,
    };
};

pub const Row = struct {
    grapheme: bool = false,
};

/// Deinitialize the screen, freeing any backing memory. Do NOT call
/// this if you allocated the backing memory yourself (i.e. you used
/// initBuf).
pub inline fn deinit(s: *Screen, gpa: Allocator) void {
    s.cells.deinit(gpa);
    s.rows.deinit(gpa);
    s.styles.deinit(gpa);
    s.grapheme_map.deinit(gpa);
    s.grapheme_alloc.deinit(gpa);
    s.* = undefined;
}

pub fn resize(s: *Screen, gpa: Allocator, new_height: u16, new_width: u16) !void {
    std.log.debug("Resize to {d} rows, {d} cols", .{ new_height, new_width });
    const new_area = @as(u32, new_height) * @as(u32, new_width);
    if (new_height > s.rows.capacity or new_area > s.cells.capacity) {
        const old_area = @as(u32, s.size.rows) * @as(u32, s.size.cols);

        var adjusted_area = old_area;
        while (adjusted_area < new_area) adjusted_area = adjusted_area *| 2 + 32;

        try s.cells.ensureTotalCapacity(gpa, adjusted_area);
        try s.rows.ensureTotalCapacity(gpa, new_height);
    }
    s.cells.items.len = new_area;
    s.rows.items.len = new_height;

    s.clear();
    s.size = .{ .rows = new_height, .cols = new_width };
}

pub fn addStyle(s: *Screen, gpa: Allocator, style: Style) !u16 {
    const packed_style = style.toPacked();

    for (s.styles.items(.style), 0..) |ps, i| {
        if (ps == packed_style) return @intCast(i);
    }

    // Need to create a new style.
    // Attempt to re-use a style slot with zero references.
    for (s.styles.items(.refcount), s.styles.items(.style), 0..) |refcount, *ps, i| {
        if (refcount == 0) {
            ps.* = style.toPacked();
            return @intCast(i);
        }
    }

    const ret = s.styles.len;
    if (ret > std.math.maxInt(u16)) {
        // TODO
        std.log.err("Reached maximum style count, using first available", .{});
        return 0;
    }

    try s.styles.append(gpa, .{
        .refcount = 0, // Incremented when cell is set
        .style = packed_style,
    });

    return @intCast(ret);
}

pub fn getRowCells(s: *const Screen, row: u16) []Cell {
    const start = @as(usize, row) * @as(usize, s.size.cols);
    return s.cells.items[start..][0..s.size.cols];
}

pub fn getCell(s: *const Screen, cell_index: Cell.Index) *Cell {
    return &s.cells.items[@intFromEnum(cell_index)];
}

pub fn getCellRowCol(s: *const Screen, row: u16, col: u16) *Cell {
    const index = s.cellIndex(row, col);
    return s.getCell(index);
}

pub fn getRowFromCell(s: *const Screen, cell_index: Cell.Index) *Row {
    const n = @intFromEnum(cell_index) / s.size.cols;
    return s.getRows()[n];
}

fn cellIndex(s: *const Screen, row: u16, col: u16) Cell.Index {
    return @enumFromInt(@as(u32, row) * s.size.cols + col);
}

fn cellIndexPtr(s: *const Screen, cell: *const Cell) Cell.Index {
    return @enumFromInt(
        @divExact(@intFromPtr(cell) - @intFromPtr(s.cells.items.ptr), @sizeOf(Cell)),
    );
}

/// Erases the cell at the given address.
pub fn eraseCell(s: *Screen, row: u16, col: u16, new_style: ?Cell.CellStyle) void {
    const cell = s.getCellRowCol(row, col);
    defer assert(cell.* == Cell{
        .codepoint = 0,
        .content_tag = .ascii,
        .style = new_style orelse cell.style,
        .wide = .narrow,
    });

    const r = &s.rows.items[row];
    if (cell.content_tag == .codepoint_grapheme) {
        assert(r.grapheme);
        const grapheme_slice = s.grapheme_map.fetchRemove(s.cellIndex(row, col)).?.value;
        const cps = s.grapheme_alloc.chunkPtr(u21, grapheme_slice.offset)[0..grapheme_slice.len];
        s.grapheme_alloc.free(cps);
    }

    switch (cell.wide) {
        .narrow => {},
        .spacer_head => {},
        .spacer_tail => {
            return s.eraseCell(row, col - 1, new_style);
        },
        .wide => {
            assert(col < s.size.cols - 1);
            const spacer = s.getCellRowCol(row, col + 1);
            assert(spacer.wide == .spacer_tail);
            spacer.* = .{
                .codepoint = 0,
                .content_tag = .ascii,
                .style = spacer.style,
                .wide = .narrow,
            };
            if (new_style) |style| s.setCellStyle(spacer, style);
        },
    }

    cell.* = .{
        .codepoint = 0,
        .content_tag = .ascii,
        .style = cell.style,
        .wide = .narrow,
    };

    if (new_style) |style| s.setCellStyle(cell, style);
}

fn setCellStyle(s: *Screen, cell: *Cell, style: Cell.CellStyle) void {
    if (style.color_type == .rgb) {
        const id = style.colors.rgb.id;
        s.styles.items(.refcount)[id] += 1;
    }

    if (cell.style.color_type == .rgb) {
        const id = cell.style.colors.rgb.id;
        s.styles.items(.refcount)[id] -= 1;
    }

    cell.style = style;
}

/// Append a codepoint to the given cell as a grapheme.
fn appendGrapheme(
    s: *Screen,
    gpa: Allocator,
    row: *Row,
    cell: *Cell,
    cp: u21,
) Allocator.Error!void {
    // defer self.assertIntegrity();
    // if (build_options.slow_runtime_safety) assert(cell.codepoint() != 0);

    const cell_index = s.cellIndexPtr(cell);

    // If this cell has no graphemes, we can go faster by knowing we
    // need to allocate a new grapheme slice and update the map.
    if (cell.content_tag != .codepoint_grapheme) {
        const cps = try s.grapheme_alloc.alloc(gpa, u21, 1);
        errdefer s.grapheme_alloc.free(cps);
        cps[0] = cp;

        try s.grapheme_map.putNoClobber(gpa, cell_index, .{
            .offset = s.grapheme_alloc.chunkOffset(cps),
            .len = 1,
        });
        errdefer s.grapheme_map.remove(cell_index);

        cell.content_tag = .codepoint_grapheme;
        row.grapheme = true;

        return;
    }

    // The cell already has graphemes. We need to append to the existing
    // grapheme slice and update the map.
    assert(row.grapheme);

    const slice = s.grapheme_map.getPtr(cell_index).?;

    // If our slice len doesn't divide evenly by the grapheme chunk
    // length then we can utilize the additional chunk space.
    if (slice.len % grapheme_chunk_len != 0) {
        const cps = s.grapheme_alloc.chunkPtr(u21, slice.offset);
        cps[slice.len] = cp;
        slice.len += 1;
        return;
    }

    // We are out of chunk space. There is no fast path here. We need
    // to allocate a larger chunk. This is a very slow path. We expect
    // most graphemes to fit within our chunk size.
    const cps = try s.grapheme_alloc.alloc(gpa, u21, slice.len + 1);
    errdefer s.grapheme_alloc.free(cps);
    const old_cps = s.grapheme_alloc.chunkPtr(u21, slice.offset)[0..slice.len];
    @memcpy(cps[0..old_cps.len], old_cps);
    cps[slice.len] = cp;
    slice.* = .{
        .offset = s.grapheme_alloc.chunkOffset(cps),
        .len = slice.len + 1,
    };

    // Free our old chunk
    s.grapheme_alloc.free(old_cps);
}

const WriteResult = struct {
    bytes_consumed: usize,
    grapheme_break: bool,
};

/// Get the row and cell for the given X/Y within this page.
pub inline fn getRowAndCell(self: *const Screen, x: usize, y: usize) struct { *Row, *Cell } {
    assert(y < self.size.rows);
    assert(x < self.size.cols);

    const rows = self.rows.ptr(self.memory);
    const row = &rows[y];
    const cell = &row.cells.ptr(self.memory)[x];

    return .{ row, cell };
}

pub fn writer(s: *Screen, gpa: Allocator, rect: Rect) Writer {
    return .{
        .s = s,
        .gpa = gpa,
        .rect = rect,
        .cursor = .origin,
        .grapheme_clustering_mode = .grapheme,
        .edge_mode = .wrap,
    };
}

pub fn writerFull(s: *Screen, gpa: Allocator) Writer {
    return s.writer(gpa, .{
        .height = s.size.rows,
        .width = s.size.cols,
        .x = 0,
        .y = 0,
    });
}

pub fn styleToCellStyle(s: *Screen, gpa: Allocator, style: Style) !Cell.CellStyle {
    if (style.fg == .rgb or style.bg == .rgb) {
        const id = try s.addStyle(gpa, style);
        return .{
            .attrs = style.attrs,
            .color_type = .rgb,
            .colors = .{ .rgb = .{ .id = id } },
        };
    }
    return .{
        .attrs = style.attrs,
        .color_type = .palette,
        .colors = .{ .palette = .{
            .fg = .{ .none = style.fg == .none, .@"256" = style.fg.palette() orelse 0 },
            .bg = .{ .none = style.bg == .none, .@"256" = style.bg.palette() orelse 0 },
        } },
    };
}

pub fn styleRect(s: *Screen, gpa: Allocator, r: Rect, style: Style) !void {
    s.styleRectCell(r, try s.styleToCellStyle(gpa, style));
}

pub fn styleRectCell(s: *Screen, r: Rect, style: Cell.CellStyle) void {
    var row: u16 = 0;
    while (row < r.height) : (row += 1) {
        const cells = s.getRowCells(r.y + row);
        for (cells[r.x..][0..r.width]) |*c| {
            s.setCellStyle(c, style);
        }
    }
}

/// Represents a sub-rectangle of a screen.
pub const Rect = struct {
    x: u16,
    y: u16,
    width: u16,
    height: u16,

    /// Returns a rectangle whose width and height is clamped to avoid extending outside of the
    /// parent rectangle.
    pub fn clamped(source: Rect, subrect: Rect) Rect {
        const new_x = source.x + subrect.x;
        const new_y = source.y + subrect.y;
        const width = @min(subrect.width, source.width -| subrect.x);
        const height = @min(subrect.height, source.height -| subrect.y);

        return .{
            .x = new_x,
            .y = new_y,
            .width = width,
            .height = height,
        };
    }

    /// Returns a rectangle position at `row`,`col` whose width and height extends to the edges of
    /// the parent rectangle.
    pub fn extended(source: Rect, row: u16, col: u16) Rect {
        assert(row < source.height);
        assert(col < source.width);
        return .{
            .x = source.x + col,
            .y = source.y + row,
            .width = source.width - col,
            .height = source.height - row,
        };
    }

    pub fn advance(rect: Rect, row: u16, col: u16, n: u32) [2]u16 {
        const x = col + n;
        const row_advance = x / rect.width;
        if (row + row_advance >= rect.height) {
            @branchHint(.unlikely);
            return .{ rect.height, 0 };
        }

        return .{ @intCast(row + x / rect.width), @intCast(x % rect.width) };
    }

    pub fn advanceRect(rect: Rect, parent: Rect, n: u32, new_width: u16) Rect {
        return parent.clamped(.{
            .x = @intCast(rect.x +| n),
            .y = rect.y,
            .width = new_width,
            .height = rect.height,
        });
    }
};

pub const Cursor = struct {
    row: u16,
    col: u16,
    style: Cell.CellStyle,

    pub const origin: Cursor = .{
        .row = 0,
        .col = 0,
        .style = .default,
    };
};

// TODO: Consider a different name to avoid conflation with `std.Io.Writer`.
/// State required for writing to the screen. Contains a cursor that moves as text is written,
/// performing wrapping and properly handling wide characters and grapheme cluster segmentation.
///
/// Only writes text in the bounds of the given rectangle, wrapping or truncating at the edges.
pub const Writer = struct {
    s: *Screen,
    gpa: Allocator,
    rect: Rect,

    /// Coordinates relative to the parent rectangle.
    cursor: Cursor,

    grapheme_state: zg.Graphemes.State = .reset,
    last_cp: u21 = 0,
    last_cursor: Cursor = .origin,

    grapheme_clustering_mode: GraphemeClusteringMode,
    edge_mode: EdgeMode,

    pub const EdgeMode = enum { wrap, truncate };

    pub fn setRect(w: *Writer, r: Rect) void {
        w.rect = r;
        w.setCursor(0, 0);
    }

    pub fn setRectClamp(w: *Writer, r: Rect) void {
        w.rect = w.s.screenRect().clamped(r);
        w.setCursor(0, 0);
    }

    pub fn advanceRect(w: *Writer, n: u16, new_width: u16) void {
        w.setCursor(0, 0);
        w.rect.x = @min(w.rect.x +| n, w.s.size.cols);
        w.rect.width = @min(w.s.size.cols - w.rect.x, new_width);
    }

    pub fn advanceRectToEol(w: *Writer, n: u16) void {
        w.advanceRect(n, w.s.size.cols);
    }

    pub fn extendRectToEol(w: *Writer) void {
        w.rect.width = w.s.size.cols - w.rect.x;
    }

    pub fn setRectWidth(w: *Writer, new_width: u16) void {
        w.rect.width = @max(w.s.size.cols - w.rect.x, new_width);
        w.setCursor(0, 0);
    }

    pub fn splatByteAll(w: *Writer, byte: u8, n: usize) !void {
        for (0..n) |_| {
            _ = try w.write(&.{byte});
        }
    }

    pub fn write(w: *Writer, bytes: []const u8) !usize {
        if (bytes.len == 0) {
            @branchHint(.unlikely);
            return 0;
        }

        if (w.cursor.row >= w.rect.height or w.cursor.col >= w.rect.width) {
            @branchHint(.unlikely);
            return bytes.len;
        }

        // ASCII
        if (bytes[0] & 0x80 == 0) {
            @branchHint(.likely);

            if (bytes[0] < 0x20) {
                // Control characters are currently ignored.
                // TODO: Handle newlines, carriage returns, and tabs.
                return 1;
            }

            w.eraseCurrentCell();
            const cell = w.getCurrentCell();
            cell.* = .{
                .style = cell.style, // Erase already set the style
                .content_tag = .ascii,
                .wide = .narrow,
                .codepoint = bytes[0],
            };

            w.last_cp = bytes[0];
            w.grapheme_state = .reset;

            w.last_cursor = w.cursor;
            w.advanceCursor(1);

            return 1;
        }

        const cp = zg.codepoint.decode(bytes);

        if (w.last_cp != 0 and w.grapheme_clustering_mode == .grapheme) {
            // Check if we're continuing a previous grapheme cluster
            var grapheme_state = w.grapheme_state;
            if (!zg.graphemes.isBreak(w.last_cp, cp.code, &grapheme_state)) {
                @branchHint(.unlikely);
                // Continue last write
                const last_cell = w.getLastCell();
                const last_row = w.getLastRow();
                try w.s.appendGrapheme(w.gpa, last_row, last_cell, cp.code);

                if (cp.code == 0xFE0F and last_cell.wide != .wide) {
                    // Emoji variation selector.
                    // Mode 2027 spec says this *must* have a width of 2.
                    last_cell.wide = .wide;
                    assert(w.cursor.col == w.last_cursor.col + 1);
                    w.eraseCurrentCell();
                    w.getCurrentCell().* = .{
                        .codepoint = 0,
                        .content_tag = .ascii,
                        .wide = .spacer_tail,
                        .style = last_cell.style,
                    };
                    w.advanceCursor(1);
                }

                w.last_cp = cp.code;
                w.grapheme_state = grapheme_state;
                return cp.len;
            }
        }

        w.last_cp = cp.code;
        w.grapheme_state = .reset;

        // New grapheme
        const cell = w.getCurrentCell();
        const row = w.getRow();
        const cp_width = zg.display_width.codePointWidth(cp.code);
        switch (cp_width) {
            0 => {
                // Zero width starting character - ignore.
                return cp.len;
            },
            1 => {
                w.eraseCurrentCell();
                cell.* = .{
                    .style = w.cursor.style,
                    .content_tag = .codepoint,
                    .wide = .narrow,
                    .codepoint = cp.code,
                };
                row.grapheme = true;
                w.last_cursor = w.cursor;
                w.advanceCursor(1);
            },
            2, 3 => {
                w.eraseCurrentCell();
                if (w.rect.width == 1) {
                    @branchHint(.unlikely);
                    w.last_cp = 0;
                    return cp.len;
                }

                const remaining_width = w.rect.width - w.cursor.col;
                if (remaining_width == 1) {
                    // We need to wrap
                    cell.* = .{
                        .style = w.cursor.style,
                        .content_tag = .ascii,
                        .wide = .spacer_head,
                        .codepoint = 0,
                    };
                    w.advanceCursor(1);
                    w.eraseCurrentCell();
                }

                w.last_cursor = w.cursor;

                // TODO: Re-use grapheme slice if possible.
                const new_cell = w.getCurrentCell();
                const new_row = w.getRow();
                new_cell.* = .{
                    .style = w.cursor.style,
                    .content_tag = .codepoint,
                    .wide = .wide,
                    .codepoint = cp.code,
                };

                w.advanceCursor(1);
                assert(w.getRow() == new_row);
                w.getCurrentCell().* = .{
                    .style = w.cursor.style,
                    .content_tag = .codepoint,
                    .wide = .spacer_tail,
                    .codepoint = 0,
                };
                w.advanceCursor(1);
            },
            else => unreachable,
        }

        return cp.len;
    }

    pub fn setStyle(w: *Writer, style: Style) !void {
        w.cursor.style = try w.s.styleToCellStyle(w.gpa, style);
    }

    pub fn getStyle(w: *const Writer) Style {
        return w.s.getCellStyle(w.cursor.style);
    }

    pub fn styleToEol(w: *Writer) void {
        const r: Rect = .{
            .height = 1,
            .width = w.rect.width - w.cursor.col,
            .x = w.rect.x + w.cursor.col,
            .y = w.rect.y + w.cursor.row,
        };
        w.s.styleRectCell(r, w.cursor.style);
        w.advanceCursor(r.width);
    }

    pub fn clearToEol(w: *Writer) !void {
        try w.splatByteAll(' ', w.rect.width -| w.cursor.col);
    }

    pub fn writeByte(w: *Writer, c: u8) !void {
        try w.writeAll(&.{c});
    }

    /// Advance the cursor by `n` cells.
    fn advanceCursor(w: *Writer, n: u32) void {
        switch (w.edge_mode) {
            .wrap => {
                w.cursor.row, w.cursor.col = w.rect.advance(w.cursor.row, w.cursor.col, n);
            },
            .truncate => {
                w.cursor.col +|= @intCast(n);
            },
        }
    }

    /// Returns the cell this writer is currently at.
    pub fn getCurrentCell(w: *Writer) *Cell {
        return w.s.getCellRowCol(w.rect.y + w.cursor.row, w.rect.x + w.cursor.col);
    }

    pub fn getLastCell(w: *Writer) *Cell {
        return w.s.getCellRowCol(w.rect.y + w.last_cursor.row, w.rect.x + w.last_cursor.col);
    }

    pub fn getRow(w: *Writer) *Row {
        return &w.s.rows.items[w.rect.y + w.cursor.row];
    }

    pub fn getLastRow(w: *Writer) *Row {
        return &w.s.rows.items[w.rect.y + w.last_cursor.row];
    }

    pub fn eraseCurrentCell(w: *Writer) void {
        w.s.eraseCell(w.rect.y + w.cursor.row, w.rect.x + w.cursor.col, w.cursor.style);
    }

    pub fn setCursor(w: *Writer, row: u16, col: u16) void {
        w.cursor.row = row;
        w.cursor.col = col;
        w.last_cp = 0;
    }

    pub fn writeAll(w: *Writer, bytes: []const u8) !void {
        var i: usize = 0;
        while (i < bytes.len) i += try w.write(bytes[i..]);
    }

    pub fn print(w: *Writer, comptime fmt: []const u8, args: anytype) !void {
        var buf: [1024]u8 = undefined;
        var wrapper: StdlibWriterWrapper = .{
            .w = w,
            .interface = .{
                .buffer = &buf,
                .vtable = &.{
                    .drain = StdlibWriterWrapper.drain,
                },
            },
        };
        wrapper.interface.print(fmt, args) catch return error.OutOfMemory;
        wrapper.interface.flush() catch return error.OutOfMemory;
    }

    pub fn writeSplatAll(w: *Writer, data: [][]const u8, splat: usize) !void {
        var buf: [1024]u8 = undefined;
        var wrapper: StdlibWriterWrapper = .{
            .w = w,
            .interface = .{
                .buffer = &buf,
                .vtable = &.{
                    .drain = StdlibWriterWrapper.drain,
                },
            },
        };
        wrapper.interface.writeSplatAll(data, splat) catch return error.OutOfMemory;
        wrapper.interface.flush() catch return error.OutOfMemory;
    }

    pub fn clear(w: *Writer) void {
        w.s.clearRect(w.rect, w.cursor.style);
    }

    pub const StdlibWriterWrapper = struct {
        w: *Writer,
        interface: std.Io.Writer,

        pub fn drain(
            io_writer: *std.Io.Writer,
            data: []const []const u8,
            splat: usize,
        ) std.Io.Writer.Error!usize {
            const wrapper: *StdlibWriterWrapper = @fieldParentPtr("interface", io_writer);
            const w = wrapper.w;

            const buffered = wrapper.interface.buffered();
            if (buffered.len > 0) {
                const bytes_written = w.write(buffered) catch return error.WriteFailed;
                _ = wrapper.interface.consume(bytes_written);

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
    };
};

/// Iterates over all of the codepoints in an area.
pub const CodepointIterator = struct {
    s: *const Screen,
    rect: Rect,
    /// Relative to `rect`.
    row: u16 = 0,
    /// Relative to `rect`.
    col: u16 = 0,
    n: ?u32 = null,

    pub fn next(iter: *CodepointIterator) ?u21 {
        if (iter.row >= iter.rect.height or iter.col >= iter.rect.width)
            return null;

        const cell = iter.s.getCellRowCol(iter.rect.y + iter.row, iter.rect.x + iter.col);
        switch (cell.wide) {
            .spacer_tail => {
                iter.advance(1);
                return iter.next();
            },
            .narrow, .wide, .spacer_head => {},
        }

        switch (cell.content_tag) {
            .ascii, .codepoint => {
                iter.advance(1);
                return cell.codepoint;
            },
            .codepoint_grapheme => {
                const n = iter.n orelse {
                    iter.n = 0;
                    return cell.codepoint;
                };

                const graphemes = iter.s.getGraphemes(cell);
                const ret = graphemes[n];
                if (n == graphemes.len - 1) {
                    iter.n = null;
                    iter.advance(1);
                } else {
                    iter.n = n + 1;
                }
                return ret;
            },
        }
    }

    fn advance(iter: *CodepointIterator, n: u32) void {
        iter.row, iter.col = iter.rect.advance(iter.row, iter.col, n);
    }
};

/// Returns a `Rect` the same size as the screen.
pub fn screenRect(s: *const Screen) Rect {
    return .{
        .x = 0,
        .y = 0,
        .width = s.size.cols,
        .height = s.size.rows,
    };
}

pub fn rowRect(s: *const Screen, row: u16, height: u16) Rect {
    return .{
        .height = height,
        .width = s.size.cols,
        .y = row,
        .x = 0,
    };
}

const TermInfo = @import("TermInfo.zig");

pub fn getCellStyle(s: *const Screen, cell_style: Cell.CellStyle) Style {
    if (cell_style.color_type == .rgb) {
        const style = s.styles.items(.style)[cell_style.colors.rgb.id];
        return .{
            .attrs = cell_style.attrs,
            .fg = switch (style.fg_tag) {
                .none => .none,
                .palette => .fromPalette(style.fg.palette.index),
                .rgb => .{ .rgb = .{ style.fg.rgb.r, style.fg.rgb.g, style.fg.rgb.b } },
            },
            .bg = switch (style.bg_tag) {
                .none => .none,
                .palette => .fromPalette(style.bg.palette.index),
                .rgb => .{ .rgb = .{ style.bg.rgb.r, style.bg.rgb.g, style.bg.rgb.b } },
            },
        };
    }

    return .{
        .attrs = cell_style.attrs,
        .fg = if (cell_style.colors.palette.fg.none)
            .none
        else
            .fromPalette(cell_style.colors.palette.fg.@"256"),
        .bg = if (cell_style.colors.palette.bg.none)
            .none
        else
            .fromPalette(cell_style.colors.palette.bg.@"256"),
    };
}

pub const DoubleBuffer = struct {
    write: Screen,
    read: Screen,
    ti: *const TermInfo,

    pub fn init(ti: *const TermInfo) DoubleBuffer {
        return .{
            .read = .empty,
            .write = .empty,
            .ti = ti,
        };
    }

    pub fn deinit(db: *DoubleBuffer, gpa: Allocator) void {
        db.write.deinit(gpa);
        db.read.deinit(gpa);
    }

    pub fn resize(db: *DoubleBuffer, gpa: Allocator, new_height: u16, new_width: u16) !void {
        try db.read.resize(gpa, new_height, new_width);
        try db.write.resize(gpa, new_height, new_width);
    }

    pub fn dump(db: *DoubleBuffer, w: *std.Io.Writer) !void {
        try dumpDiff(&db.read, &db.write, db.ti, w);
        const temp = db.read;
        db.read = db.write;
        db.write = temp;
        db.write.clear();
    }
};

/// Dumps the contents of the screen to the given writer, with no escape sequences. Lines are
/// separated with a carriage return and a newline.
pub fn dump(s: *const Screen, r: Rect, ti: *const TermInfo, w: *std.Io.Writer) !void {
    var current_style: Style = .{};
    var current_cell_style: Cell.CellStyle = .default;
    var row: u16 = 0;
    var col: u16 = 0;
    while (row < r.height) {
        const cell = s.getCellRowCol(r.y + row, r.x + col);

        if (cell.style != current_cell_style) {
            const new_style: Style = s.getCellStyle(cell.style);
            try new_style.dump(w, .{ .terminfo = ti });

            current_style = new_style;
            current_cell_style = cell.style;
        }

        if (cell.codepoint != 0) {
            switch (cell.content_tag) {
                .ascii => {
                    try w.writeByte(@intCast(cell.codepoint));
                },
                .codepoint => {
                    try w.printUnicodeCodepoint(cell.codepoint);
                },
                .codepoint_grapheme => {
                    try w.printUnicodeCodepoint(cell.codepoint);
                    const graphemes = s.getGraphemes(cell);
                    for (graphemes) |cp| {
                        try w.printUnicodeCodepoint(cp);
                    }
                },
            }
        }

        switch (cell.wide) {
            .narrow => {
                row, col = r.advance(row, col, 1);
            },
            .wide => {
                row, col = r.advance(row, col, 2);
            },
            .spacer_tail => unreachable,
            .spacer_head => {
                assert(cell.codepoint == 0);
                try w.writeByte(' ');
                row, col = r.advance(row, col, 1);
            },
        }

        if (col == 0 and row < r.height) {
            if (cell.style != Cell.CellStyle.default) {
                const new_style: Style = .{};
                try new_style.dump(w, .{ .terminfo = ti });

                current_style = .{};
                current_cell_style = .default;
            }
            try w.writeAll("\r\n");
        }
    }
}

fn isBlank(cell: *const Cell) bool {
    return cell.codepoint == ' ' or (cell.wide == .narrow and cell.codepoint == 0);
}

/// Returns the number of trailing blanks that share the same style.
fn countTrailingBlanks(cells: []const Cell) u16 {
    if (cells.len == 0) {
        @branchHint(.unlikely);
        return 0;
    }

    const style = cells[cells.len - 1].style;

    var count: u16 = 0;
    var i = cells.len;
    while (i > 0) {
        i -= 1;
        const blank = isBlank(&cells[i]) and cells[i].style == style;
        if (!blank) break;
        count += 1;
    }

    return count;
}

// TODO: Clean this up
/// Writes out the secape sequences necessary to transform `old` into `new`.
///
/// Any rows in `new` that do NOT have the dirty flag set are assumed to be unchanged.
pub fn dumpDiff(
    old: *const Screen,
    new: *Screen,
    ti: *const TermInfo,
    wr: *std.Io.Writer,
) !void {
    var cw: @import("util.zig").CountingWriter = .init(wr);
    const w: *std.Io.Writer = switch (@import("builtin").mode) {
        .Debug => &cw.interface,
        else => wr,
    };

    defer if (@import("builtin").mode == .Debug) {
        std.log.debug("Dumped {d} bytes", .{cw.bytes_written});
    };

    std.log.debug("  Start dump", .{});

    const move = ti.getStringCapability(.cursor_address) orelse {
        // Without an adressable cursor there's no way to rerender only parts of the screen.
        try new.dump(new.screenRect(), ti, wr);
        return;
    };

    const clear_to_eol = ti.getStringCapability(.clr_eol);

    var current_style: ?Style = null;
    var current_cell_style: ?Cell.CellStyle = null;
    var y: u16 = 0;
    while (y < new.size.rows) : (y += 1) {
        const old_cells = old.getRowCells(y);
        const new_cells = new.getRowCells(y);

        const start: u16 = for (old_cells, new_cells, 0..) |oc, nc, i| {
            if (oc != nc) break @intCast(i);
        } else continue;

        const trailing_blanks = countTrailingBlanks(new_cells);

        var i = new_cells.len;
        const end = while (i > 0) {
            i -= 1;
            if (old_cells[i] != new_cells[i]) break i + 1;
        } else unreachable;

        const changed_trailing_blanks = end -| (new.size.cols - trailing_blanks);
        const true_end = @max(start, @min(end, new.size.cols - trailing_blanks));

        const buffer_start = w.end;

        try TermInfo.writeParamSequence(move, w, .{ y, start });
        i = start;
        while (i < true_end) : (i += 1) {
            const nc = &new_cells[i];
            if (nc.style != current_cell_style) {
                const new_style: Style = new.getCellStyle(nc.style);
                try new_style.dump(w, .{
                    .diff = current_style,
                    .terminfo = ti,
                });

                current_style = new_style;
                current_cell_style = nc.style;
            }

            // TODO: Consider using spaces for blank cells instead of doing this.
            if (nc.codepoint == 0) {
                try w.writeByte(' ');
                continue;
            }

            switch (nc.content_tag) {
                .ascii => {
                    try w.writeByte(@intCast(nc.codepoint));
                },
                .codepoint => {
                    try w.printUnicodeCodepoint(nc.codepoint);
                },
                .codepoint_grapheme => {
                    try w.printUnicodeCodepoint(nc.codepoint);
                    const graphemes = new.getGraphemes(nc);
                    for (graphemes) |cp| {
                        try w.printUnicodeCodepoint(cp);
                    }
                },
            }

            switch (nc.wide) {
                .narrow => {},
                .wide => i += 1,
                .spacer_head, .spacer_tail => unreachable,
            }
        }

        std.log.debug("Dumping line {d}, col {d}-{d}", .{
            y, start, true_end,
        });
        std.log.debug("Line {d} \"{f}\"", .{
            y,
            std.ascii.hexEscape(w.buffered()[buffer_start..], .lower),
        });

        if (clear_to_eol) |clr_eol| {
            @branchHint(.likely);
            if (changed_trailing_blanks == 0) {
                @branchHint(.likely);
                continue;
            }
            var eol_style = new.getCellStyle(new_cells[new_cells.len - 1].style);
            // Attributes are not propagated by clear_to_eol, so if we have the reverse
            // attribute we need to manually implement it.
            if (eol_style.attrs.reverse) {
                eol_style.attrs.reverse = false;
                const temp = eol_style.fg;
                eol_style.fg = eol_style.bg;
                eol_style.bg = temp;
            }
            try eol_style.dump(w, .{ .diff = current_style, .terminfo = ti });
            current_style = eol_style;
            current_cell_style = new_cells[new_cells.len - 1].style;

            if (changed_trailing_blanks > clr_eol.len) {
                try w.writeAll(clr_eol);
            } else {
                try w.splatByteAll(' ', changed_trailing_blanks);
            }
        }
    }

    if (@import("builtin").mode == .Debug) {
        // std.log.debug("{x}", .{w.buffered()});
        try w.flush();
    }
}

/// Clears the rectangle without setting any dirty flags.
pub fn clearRect(s: *Screen, r: Rect, new_style: ?Cell.CellStyle) void {
    // TODO: This could probably be done more efficiently
    var row: u16 = 0;
    while (row < r.height) : (row += 1) {
        var col: u16 = 0;
        while (col < r.width) : (col += 1) {
            s.eraseCell(r.y + row, r.x + col, new_style);
        }
    }
}

pub fn clear(s: *Screen) void {
    @memset(s.cells.items, .{
        .codepoint = 0,
        .content_tag = .ascii,
        .style = .default,
        .wide = .narrow,
    });

    @memset(s.rows.items, .{ .grapheme = false });
    s.styles.clearRetainingCapacity();

    s.grapheme_alloc.clearRetainingCapacity();
    s.grapheme_map.clearRetainingCapacity();
}

pub fn getGraphemes(s: *const Screen, cell: *Cell) []u21 {
    assert(cell.content_tag == .codepoint_grapheme);
    const graphemes = s.grapheme_map.get(s.cellIndexPtr(cell)).?;
    return s.grapheme_alloc.chunkPtr(u21, graphemes.offset)[0..graphemes.len];
}

fn expectRowText(w: *Writer, row: u16, expected_text: []const u8) !void {
    var screen_cp_iter: CodepointIterator = .{
        .s = w.s,
        .rect = .{
            .x = w.rect.x,
            .y = w.rect.y + row,
            .width = w.rect.width,
            .height = 1,
        },
    };
    var expected_cp_iter: std.unicode.Utf8Iterator = .{
        .bytes = expected_text,
        .i = 0,
    };
    while (expected_cp_iter.nextCodepoint()) |expected| {
        const actual = screen_cp_iter.next().?;
        try expectEqualCodepoints(expected, actual);
    }

    try std.testing.expectEqual(null, screen_cp_iter.next());
}

fn expectEqualCodepoints(expected: u21, actual: u21) !void {
    if (actual != expected) {
        std.debug.print("expected '{u}' ({d}), found '{u}' ({d})\n", .{
            expected, expected,
            actual,   actual,
        });
        return error.TestExpectedEqual;
    }
}

fn expectEmpty(s: *Screen, rect: Rect) !void {
    var iter: CodepointIterator = .{
        .s = s,
        .rect = rect,
    };
    while (iter.next()) |cp| {
        if (cp != 0) return error.ExpectedEmpty;
    }
}

test "unicode" {
    try initData();
    defer deinitData();
    const gpa = std.testing.allocator;

    var s: Screen = .empty;
    defer s.deinit(gpa);
    try s.resize(gpa, 20, 80);

    var wr = s.writer(gpa, .{
        .height = 3,
        .width = 3,
        .x = 0,
        .y = 0,
    });

    wr.setCursor(0, 0);
    try wr.writeAll("🧑‍🌾");

    try std.testing.expectEqual(0, wr.cursor.row);
    try std.testing.expectEqual(2, wr.cursor.col);
}

test "basic ASCII" {
    try initData();
    defer deinitData();
    const gpa = std.testing.allocator;

    var s: Screen = .empty;
    defer s.deinit(gpa);
    try s.resize(gpa, 20, 80);

    var wr = s.writer(gpa, .{
        .height = 3,
        .width = 3,
        .x = 0,
        .y = 0,
    });

    try wr.writeAll("This is really epic!");

    try expectRowText(&wr, 0, "Thi");
    try expectRowText(&wr, 1, "s i");
    try expectRowText(&wr, 2, "s r");
    try expectEmpty(&s, s.screenRect().extended(0, 3));
    try expectEmpty(&s, s.screenRect().extended(3, 0));

    wr.setCursor(0, 0);
    try wr.writeAll("g");

    try expectRowText(&wr, 0, "ghi");
    try expectRowText(&wr, 1, "s i");
    try expectRowText(&wr, 2, "s r");
    try expectEmpty(&s, s.screenRect().extended(0, 3));
    try expectEmpty(&s, s.screenRect().extended(3, 0));
}

test "wide characters" {
    try initData();
    defer deinitData();
    const gpa = std.testing.allocator;

    var s: Screen = .empty;
    defer s.deinit(gpa);
    try s.resize(gpa, 20, 80);

    var wr = s.writer(gpa, .{
        .height = 3,
        .width = 3,
        .x = 0,
        .y = 0,
    });
    try wr.writeAll("This is really epic!");

    try expectRowText(&wr, 0, "Thi");
    try expectRowText(&wr, 1, "s i");
    try expectRowText(&wr, 2, "s r");
    try expectEmpty(&s, s.screenRect().extended(0, 3));
    try expectEmpty(&s, s.screenRect().extended(3, 0));

    wr.setCursor(0, 0);
    try wr.writeAll("🧑‍🌾");

    try std.testing.expectEqual(0, wr.cursor.row);
    try std.testing.expectEqual(2, wr.cursor.col);

    try expectRowText(&wr, 0, "🧑‍🌾i");
    try expectRowText(&wr, 1, "s i");
    try expectRowText(&wr, 2, "s r");
    try expectEmpty(&s, s.screenRect().extended(0, 3));
    try expectEmpty(&s, s.screenRect().extended(3, 0));
}

test "wrap wide characters" {
    try initData();
    defer deinitData();
    const gpa = std.testing.allocator;

    var s: Screen = .empty;
    defer s.deinit(gpa);
    try s.resize(gpa, 20, 80);

    var wr = s.writer(gpa, .{
        .height = 3,
        .width = 3,
        .x = 0,
        .y = 0,
    });
    try wr.writeAll("This is really epic!");

    try expectRowText(&wr, 0, "Thi");
    try expectRowText(&wr, 1, "s i");
    try expectRowText(&wr, 2, "s r");
    try expectEmpty(&s, s.screenRect().extended(0, 3));
    try expectEmpty(&s, s.screenRect().extended(3, 0));

    wr.setCursor(0, 2);
    try wr.writeAll("🧑‍🌾");

    try expectRowText(&wr, 0, "Th\x00");
    try expectRowText(&wr, 1, "🧑‍🌾i");
    try expectRowText(&wr, 2, "s r");
    try expectEmpty(&s, s.screenRect().extended(0, 3));
    try expectEmpty(&s, s.screenRect().extended(3, 0));
}

test "overwrite wide char start" {
    try initData();
    defer deinitData();
    const gpa = std.testing.allocator;

    var s: Screen = .empty;
    defer s.deinit(gpa);
    try s.resize(gpa, 20, 80);

    var wr = s.writer(gpa, .{
        .height = 3,
        .width = 3,
        .x = 0,
        .y = 0,
    });
    try wr.writeAll("This is really epic!");

    try expectRowText(&wr, 0, "Thi");
    try expectRowText(&wr, 1, "s i");
    try expectRowText(&wr, 2, "s r");
    try expectEmpty(&s, s.screenRect().extended(0, 3));
    try expectEmpty(&s, s.screenRect().extended(3, 0));

    wr.setCursor(0, 0);
    try wr.writeAll("🧑‍🌾");

    try expectRowText(&wr, 0, "🧑‍🌾i");
    try expectRowText(&wr, 1, "s i");
    try expectRowText(&wr, 2, "s r");
    try expectEmpty(&s, s.screenRect().extended(0, 3));
    try expectEmpty(&s, s.screenRect().extended(3, 0));

    wr.setCursor(0, 0);
    try wr.writeAll("T");

    try expectRowText(&wr, 0, "T\x00i");
    try expectRowText(&wr, 1, "s i");
    try expectRowText(&wr, 2, "s r");
    try expectEmpty(&s, s.screenRect().extended(0, 3));
    try expectEmpty(&s, s.screenRect().extended(3, 0));

    try wr.writeAll("h");

    try expectRowText(&wr, 0, "Thi");
    try expectRowText(&wr, 1, "s i");
    try expectRowText(&wr, 2, "s r");
    try expectEmpty(&s, s.screenRect().extended(0, 3));
    try expectEmpty(&s, s.screenRect().extended(3, 0));
}

test "overwrite wide char spacer tail" {
    try initData();
    defer deinitData();
    const gpa = std.testing.allocator;

    var s: Screen = .empty;
    defer s.deinit(gpa);
    try s.resize(gpa, 20, 80);

    var wr = s.writer(gpa, .{
        .height = 3,
        .width = 3,
        .x = 0,
        .y = 0,
    });
    try wr.writeAll("This is really epic!");

    try expectRowText(&wr, 0, "Thi");
    try expectRowText(&wr, 1, "s i");
    try expectRowText(&wr, 2, "s r");
    try expectEmpty(&s, s.screenRect().extended(0, 3));
    try expectEmpty(&s, s.screenRect().extended(3, 0));

    wr.setCursor(0, 0);
    try wr.writeAll("🧑‍🌾");

    try expectRowText(&wr, 0, "🧑‍🌾i");
    try expectRowText(&wr, 1, "s i");
    try expectRowText(&wr, 2, "s r");
    try expectEmpty(&s, s.screenRect().extended(0, 3));
    try expectEmpty(&s, s.screenRect().extended(3, 0));

    wr.setCursor(0, 1);
    try wr.writeAll("h");

    try expectRowText(&wr, 0, "\x00hi");
    try expectRowText(&wr, 1, "s i");
    try expectRowText(&wr, 2, "s r");
    try expectEmpty(&s, s.screenRect().extended(0, 3));
    try expectEmpty(&s, s.screenRect().extended(3, 0));

    wr.setCursor(0, 0);
    try wr.writeAll("T");

    try expectRowText(&wr, 0, "Thi");
    try expectRowText(&wr, 1, "s i");
    try expectRowText(&wr, 2, "s r");
    try expectEmpty(&s, s.screenRect().extended(0, 3));
    try expectEmpty(&s, s.screenRect().extended(3, 0));
}

test "single width rect" {
    try initData();
    defer deinitData();
    const gpa = std.testing.allocator;

    var s: Screen = .empty;
    defer s.deinit(gpa);
    try s.resize(gpa, 20, 80);

    var wr = s.writer(gpa, .{
        .height = 3,
        .width = 1,
        .x = 0,
        .y = 0,
    });
    try wr.writeAll("T");

    try expectRowText(&wr, 0, "T");

    wr.setCursor(0, 0);
    try wr.writeAll("🧑‍🌾");

    try expectRowText(&wr, 0, "\x00");
}

test "zero width rect" {
    try initData();
    defer deinitData();
    const gpa = std.testing.allocator;

    var s: Screen = .empty;
    defer s.deinit(gpa);
    try s.resize(gpa, 20, 80);

    var wr = s.writer(gpa, .{
        .height = 3,
        .width = 0,
        .x = 0,
        .y = 0,
    });
    try wr.writeAll("This");

    try expectRowText(&wr, 0, "");

    wr.setCursor(0, 0);
    try wr.writeAll("🧑‍🌾");

    try expectRowText(&wr, 0, "");
}

test "variation selectors" {
    try initData();
    defer deinitData();
    const gpa = std.testing.allocator;

    var s: Screen = .empty;
    defer s.deinit(gpa);
    try s.resize(gpa, 20, 80);

    var wr = s.writer(gpa, .{
        .height = 3,
        .width = 3,
        .x = 0,
        .y = 0,
    });
    try expectRowText(&wr, 0, "\x00\x00\x00");
    // Text variation - narrow width. (TODO: Is it *always* narrow?)
    try wr.writeAll("❤\u{FE0E}");
    try expectRowText(&wr, 0, "❤\u{FE0E}\x00\x00");
    try std.testing.expectEqual(1, wr.cursor.col);

    wr.setCursor(0, 0);
    try wr.writeAll("❤\u{FE0F}");

    try std.testing.expectEqual(2, wr.cursor.col);
    try expectRowText(&wr, 0, "❤\u{FE0F}\x00");
}

test "no grapheme clustering" {
    try initData();
    defer deinitData();
    const gpa = std.testing.allocator;

    var s: Screen = .empty;
    defer s.deinit(gpa);
    try s.resize(gpa, 20, 80);

    var wr = s.writer(gpa, .{
        .height = 3,
        .width = 4,
        .x = 0,
        .y = 0,
    });
    wr.grapheme_clustering_mode = .codepoint;
    try wr.writeAll("123412341234");

    try expectRowText(&wr, 0, "1234");
    try expectRowText(&wr, 1, "1234");
    try expectRowText(&wr, 2, "1234");
    try expectEmpty(&s, s.screenRect().extended(0, 4));
    try expectEmpty(&s, s.screenRect().extended(3, 0));

    wr.setCursor(0, 0);
    try wr.writeAll("🧑‍🌾");

    try expectRowText(&wr, 0, "🧑🌾");
    try expectRowText(&wr, 1, "1234");
    try expectRowText(&wr, 2, "1234");
    try expectEmpty(&s, s.screenRect().extended(0, 4));
    try expectEmpty(&s, s.screenRect().extended(3, 0));

    wr.setCursor(1, 1);
    try wr.writeAll("🧑‍🌾");

    try expectRowText(&wr, 0, "🧑🌾");
    try expectRowText(&wr, 1, "1🧑\x00");
    try expectRowText(&wr, 2, "🌾34");
    try expectEmpty(&s, s.screenRect().extended(0, 4));
    try expectEmpty(&s, s.screenRect().extended(3, 0));
}

test "truncating" {
    try initData();
    defer deinitData();
    const gpa = std.testing.allocator;

    var s: Screen = .empty;
    defer s.deinit(gpa);

    try s.resize(gpa, 20, 80);

    var wr = s.writer(gpa, .{
        .height = 3,
        .width = 4,
        .x = 0,
        .y = 0,
    });
    wr.edge_mode = .truncate;
    wr.grapheme_clustering_mode = .codepoint;
    try wr.writeAll("123412341234");

    try expectRowText(&wr, 0, "1234");
    try expectEmpty(&s, s.screenRect().extended(1, 0));

    wr.setCursor(0, 3);
    try wr.writeAll("🧑");

    try expectRowText(&wr, 0, "123\x00");
    try expectEmpty(&s, s.screenRect().extended(1, 0));
}

// test "styling" {
//     try initData();
//     defer deinitData();

//     var s: Screen = try .init(.{});
//     defer s.deinit();

//     var wr = s.writer(.{
//         .height = 3,
//         .width = 5,
//         .x = 0,
//         .y = 0,
//     });
//     try wr.writeAll("hi :)");
//     try std.testing.expectEqual(Style.Attribute.none, s.getCellRowCol(0, 0).style.attrs);
//     try std.testing.expectEqual(.palette, s.getCellRowCol(0, 0).style.color_type);
//     try std.testing.expectEqual(true, s.getCellRowCol(0, 0).style.colors.palette.fg.none);
//     try std.testing.expectEqual(true, s.getCellRowCol(0, 0).style.colors.palette.bg.none);

//     wr.setCursor(0, 0);
//     try wr.setStyle(.{ .attrs = .{ .bold = true }, .fg = .black, .bg = .blue });
//     try wr.writeAll("h");

//     try std.testing.expectEqual(Style.Attribute{ .bold = true }, s.getCellRowCol(0, 0).style.attrs);
//     try std.testing.expectEqual(.palette, s.getCellRowCol(0, 0).style.color_type);
//     try std.testing.expectEqual(false, s.getCellRowCol(0, 0).style.colors.palette.fg.none);
//     try std.testing.expectEqual(false, s.getCellRowCol(0, 0).style.colors.palette.bg.none);
//     try std.testing.expectEqual(0, s.getCellRowCol(0, 0).style.colors.palette.fg.@"256");
//     try std.testing.expectEqual(4, s.getCellRowCol(0, 0).style.colors.palette.bg.@"256");

//     try wr.setStyle(.{
//         .attrs = .{ .underline = true },
//         .fg = .{ .rgb = .{ 0xff, 0x00, 0xff } },
//         .bg = .white,
//     });
//     try wr.writeAll("i");

//     const cell = s.getCellRowCol(0, 1);
//     try std.testing.expectEqual(Style.Attribute{ .underline = true }, cell.style.attrs);
//     try std.testing.expectEqual(.rgb, cell.style.color_type);
//     const style = s.styles.get(s.memory, cell.style.colors.rgb.id);
//     try std.testing.expectEqual(.rgb, style.fg_tag);
//     try std.testing.expectEqual(0xff, style.fg.rgb.r);
//     try std.testing.expectEqual(0x00, style.fg.rgb.g);
//     try std.testing.expectEqual(0xff, style.fg.rgb.b);
//     try std.testing.expectEqual(.palette, style.bg_tag);
//     try std.testing.expectEqual(7, style.bg.palette.index);
// }

// test "dumpDiff" {
//     if (true) return error.SkipZigTest;
//     try initData();
//     defer deinitData();

//     var source: Screen = try .init(.{
//         .rows = 10,
//         .cols = 5,
//     });
//     defer source.deinit();

//     var dest: Screen = try .init(.{
//         .rows = 10,
//         .cols = 5,
//     });
//     defer dest.deinit();

//     try dest.resize(gpa, 10, 5);

//     var swr = source.writerFull();
//     var dwr = dest.writerFull();

//     try swr.writeAll("This is nice!");
//     try dwr.writeAll("This is bad!");

//     var ti = try TermInfo.Fallback.getTermInfo(.@"xterm-256color", std.testing.allocator, .failing, .empty);
//     defer ti.deinit(std.testing.allocator);

//     var buffer: [16384]u8 = undefined;
//     var w: std.Io.Writer = .fixed(&buffer);
//     try dumpDiff(&source, &dest, &ti, &w);
//     try std.testing.expectEqualSlices(u8, "", w.buffered());
// }

test "erase" {
    try initData();
    defer deinitData();
    const gpa = std.testing.allocator;

    var s: Screen = .empty;
    defer s.deinit(gpa);

    try s.resize(gpa, 10, 5);

    var wr = s.writerFull(gpa);
    wr.clear();
    try wr.writeAll("☀️");
    wr.setCursor(0, 0);
    wr.clear();
    try wr.writeAll("☀️");
    wr.clear();
}

test "guhbuh" {
    try initData();
    defer deinitData();

    const res = zg.display_width.codePointWidth('☀');
    try std.testing.expectEqual(1, res);
}

fn initData() !void {
    try @import("main.zig").initUnicodeData(std.testing.allocator);
}

fn deinitData() void {
    @import("main.zig").deinitUnicodeData(std.testing.allocator);
}

// This file uses code adapted from [Ghostty](https://github.com/ghostty-org/ghostty)
//
//
// MIT License
//
// Copyright (c) 2024 Mitchell Hashimoto, Ghostty contributors
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
