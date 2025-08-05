// TODO: Escape non printing characters.
//       Possibly implement this as a separate writer that wraps this one.
const std = @import("std");
const assert = std.debug.assert;
const Writer = std.io.Writer;

const zg = @import("zg");

const GraphemeClusteringMode = @import("main.zig").GraphemeClusteringMode;
const graphemeWidth = @import("main.zig").stringWidth;
const wcWidth = @import("util.zig").wcWidth;

pub const trunc_str = "…";

child: *Writer,

grapheme_clustering_mode: GraphemeClusteringMode,
remaining_width: u32,
finished: bool,

/// Buffer for partial codepoints
partial_cp_buf: [4]u8,
partial_cp_buf_len: u8,

/// Buffer for grapheme clusters which may overflow the width
character_buf: std.BoundedArray(u8, 64),
state: zg.Graphemes.State = .reset,
last_cp: u21,

interface: Writer,

const TerminalCellWriter = @This();

pub fn init(
    child: *Writer,
    grapheme_clustering_mode: GraphemeClusteringMode,
    width: u32,
) TerminalCellWriter {
    return .{
        .child = child,
        .grapheme_clustering_mode = grapheme_clustering_mode,
        .remaining_width = width,
        .finished = width == 0,
        .partial_cp_buf = undefined,
        .partial_cp_buf_len = 0,
        .character_buf = .{},
        .last_cp = 0,
        .interface = .{
            .buffer = &.{},
            .vtable = comptime &.{
                .drain = drain,
            },
        },
    };
}

pub fn finish(tcw: *TerminalCellWriter) !void {
    assert(tcw.partial_cp_buf_len == 0);
    try tcw.interface.flush();
    if (tcw.character_buf.len > 0) {
        const slice = tcw.character_buf.constSlice();
        const width: u32 = @intCast(graphemeWidth(slice, .grapheme, .{}).width);
        try tcw.child.writeAll(slice);
        tcw.remaining_width -= width;
    }
}

pub fn pad(tcw: *TerminalCellWriter) !void {
    try tcw.finish();
    try tcw.child.splatByteAll(' ', tcw.remaining_width);
}

fn drain(
    w: *Writer,
    data: []const []const u8,
    splat: usize,
) Writer.Error!usize {
    const tcw: *TerminalCellWriter = @fieldParentPtr("interface", w);
    const buffered = w.buffered();
    if (buffered.len > 0) {
        const bytes_written = tcw.write(w.buffered()) catch return error.WriteFailed;
        const remaining = w.consume(bytes_written);

        if (remaining > 0)
            return 0;
    }

    var total_len: usize = 0;
    for (data[0 .. data.len - 1]) |slice| {
        const bytes_written = tcw.write(slice) catch
            return error.WriteFailed;

        total_len += bytes_written;

        if (bytes_written < slice.len) return total_len;
    }

    const pattern = data[data.len - 1];
    for (0..splat) |_| {
        const bytes_written = tcw.write(pattern) catch
            return error.WriteFailed;

        total_len += bytes_written;

        if (bytes_written < pattern.len) return total_len;
    }
    return total_len;
}

pub fn write(tcw: *TerminalCellWriter, bytes: []const u8) Writer.Error!usize {
    if (bytes.len == 0 or tcw.finished)
        return bytes.len;

    const first_non_ascii = firstNonAscii(bytes) orelse bytes.len;
    const ascii_slice = bytes[0..first_non_ascii];

    if (ascii_slice.len > 0) {
        // ASCII fast path. Not dependent on width strategy.
        @branchHint(.likely);
        const end = try tcw.writeAscii(ascii_slice);
        if (end) return bytes.len;
    }

    const slice = bytes[first_non_ascii..];
    if (slice.len == 0) return bytes.len;

    // If we have a partial codepoint from the last write, it should be completed by this
    // write.
    if (tcw.partial_cp_buf_len > 0) {
        const target_len = std.unicode.utf8ByteSequenceLength(tcw.partial_cp_buf[0]) catch unreachable;
        const take_len = target_len - tcw.partial_cp_buf_len;
        if (take_len > slice.len) {
            // The codepoint is buffered across more writes :/
            @memcpy(tcw.partial_cp_buf[tcw.partial_cp_buf_len..][0..slice.len], slice);
            tcw.partial_cp_buf_len += @intCast(slice.len);
            return bytes.len;
        }
        var buf: [4]u8 = undefined;
        @memcpy(buf[0..tcw.partial_cp_buf_len], tcw.partial_cp_buf[0..tcw.partial_cp_buf_len]);
        @memcpy(buf[tcw.partial_cp_buf_len..][0..take_len], slice[0..take_len]);
        tcw.partial_cp_buf_len = 0;
        _ = try tcw.write(buf[0..target_len]);
        return take_len;
    }

    assert(tcw.partial_cp_buf_len == 0);

    const last_cp_start = lastCodepointIndex(slice);
    const expected_len = std.unicode.utf8ByteSequenceLength(slice[last_cp_start]) catch unreachable;
    const actual_len = slice.len - last_cp_start;
    assert(expected_len >= actual_len); // Mangled utf-8

    const valid_len = blk: {
        if (expected_len > actual_len) {
            // If `slice` ends in a partial codepoint, buffer it to be completed on the next write.
            @memcpy(tcw.partial_cp_buf[0..actual_len], slice[last_cp_start..]);
            tcw.partial_cp_buf_len = @intCast(actual_len);

            break :blk slice.len - actual_len;
        }

        break :blk slice.len;
    };

    if (valid_len == 0) return bytes.len;
    const valid_bytes = slice[0..valid_len];
    switch (tcw.grapheme_clustering_mode) {
        .codepoint => try tcw.writeLegacy(valid_bytes),
        .grapheme => try tcw.writeMode2027(valid_bytes),
    }
    return bytes.len;
}

fn writeAscii(tcw: *TerminalCellWriter, bytes: []const u8) !bool {
    assert(tcw.partial_cp_buf_len == 0);

    var start: usize = 0;
    for (bytes, 0..) |c, i| switch (c) {
        // Ignore zero-width characters
        0...0x1F, 0x7F => {
            const slice = bytes[start..i];
            start = i + 1;
            const end = try tcw.writeAsciiSlice(slice);
            if (end) return true;
        },
        else => {},
    };
    const end = bytes[start..];
    return try tcw.writeAsciiSlice(end);
}

fn writeAsciiSlice(tcw: *TerminalCellWriter, bytes: []const u8) !bool {
    assert(firstNonAscii(bytes) == null);
    if (bytes.len == 0) return false;

    if (tcw.character_buf.len != 0) {
        try tcw.truncate();
        return true;
    }

    if (tcw.remaining_width > bytes.len) {
        assert(tcw.character_buf.len == 0);
        try tcw.child.writeAll(bytes);
        tcw.remaining_width -= @intCast(bytes.len);
        return false;
    }

    if (tcw.remaining_width == bytes.len) {
        const slice = bytes[0 .. bytes.len - 1];
        try tcw.child.writeAll(slice);
        tcw.remaining_width -= @intCast(slice.len);
        tcw.character_buf.appendAssumeCapacity(bytes[bytes.len - 1]);
        return false;
    }

    const writeable = bytes[0..tcw.remaining_width -| 1];
    try tcw.child.writeAll(writeable);
    tcw.remaining_width = 1;

    try tcw.truncate();
    return true;
}

fn writeLegacy(tcw: *TerminalCellWriter, bytes: []const u8) !void {
    assert(std.unicode.utf8ValidateSlice(bytes));
    var iter: std.unicode.Utf8Iterator = .{ .bytes = bytes, .i = 0 };

    while (iter.nextCodepointSlice()) |cp_slice| {
        const cp = std.unicode.utf8Decode(cp_slice) catch unreachable;
        const width = wcWidth(cp);

        if (width == 0) continue; // TODO: Allow variation selectors

        const end = try tcw.writeCharacter(cp_slice, width);
        if (end) break;
    }
}

fn writeMode2027(tcw: *TerminalCellWriter, bytes: []const u8) !void {
    assert(std.unicode.utf8ValidateSlice(bytes));
    var iter: std.unicode.Utf8Iterator = .{ .bytes = bytes, .i = 0 };

    const first_cp = iter.nextCodepoint().?;
    const continues_previous_write = !zg.graphemes.isBreak(tcw.last_cp, first_cp, &tcw.state);
    if (continues_previous_write) {
        var last_cp = first_cp;
        while (iter.nextCodepointSlice()) |cp_slice| {
            const cp = std.unicode.utf8Decode(cp_slice) catch unreachable;
            const have_break = zg.graphemes.isBreak(last_cp, cp, &tcw.state);
            if (have_break) {
                iter.i -= cp_slice.len;
                break;
            }
            last_cp = cp;
        }

        const grapheme_slice = bytes[0..iter.i];
        try tcw.writeContinuingGrapheme(grapheme_slice);
    } else {
        tcw.state = .reset;
        iter.i = 0;
    }

    var grapheme_iter = zg.graphemes.iterator(bytes[iter.i..]);
    while (grapheme_iter.next()) |grapheme| {
        const grapheme_slice = grapheme.bytes(bytes[iter.i..]);
        // Take the width of the first non-zero width codepoint as the width of the whole grapheme
        // cluster. TODO: Special case variation selectors!
        const width: u2 = @intCast(graphemeWidth(grapheme_slice, .grapheme, .{}).width);
        if (width == 0) continue;

        const end = try tcw.writeCharacter(grapheme_slice, width);
        if (end) break;
    }
}

fn bufferBytes(tcw: *TerminalCellWriter, bytes: []const u8) void {
    tcw.character_buf.appendSlice(bytes) catch {
        // The buffer doesn't have enough capacity to fit the entire grapheme cluster.
        // Truncate it.
        const dest = tcw.character_buf.unusedCapacitySlice();
        const src = blk: {
            const src = bytes[0..dest.len];
            if (isStartByte(bytes[dest.len]))
                break :blk src;

            // We truncated in the middle of a utf-8 sequence, so we need to chop off the
            // incomplete codepoint at the end.
            const end = lastCodepointIndex(src);
            break :blk src[0..end];
        };

        @memcpy(dest[0..src.len], src);
    };

    tcw.last_cp = lastCodepoint(tcw.character_buf.constSlice());
}

inline fn writeContinuingGrapheme(tcw: *TerminalCellWriter, bytes: []const u8) !void {
    // Writing these bytes should not affect the output width.
    if (tcw.character_buf.len > 0) {
        tcw.bufferBytes(bytes);
    } else {
        try tcw.child.writeAll(bytes);
        tcw.last_cp = lastCodepoint(bytes);
    }
}

inline fn writeCharacter(
    tcw: *TerminalCellWriter,
    bytes: []const u8,
    width: u2,
) !bool {
    if (tcw.remaining_width < width) {
        try tcw.truncate();
        return true;
    }

    assert(width > 0);

    tcw.state = .reset;

    // If we already have a buffered character then we need to truncate.
    if (tcw.character_buf.len > 0) {
        try tcw.truncate();
        return true;
    }

    if (tcw.remaining_width > width) {
        try tcw.child.writeAll(bytes);
        tcw.remaining_width -= width;
        tcw.last_cp = lastCodepoint(bytes);
    } else {
        assert(tcw.remaining_width == width);
        tcw.bufferBytes(bytes);
    }
    return false;
}

fn truncate(tcw: *TerminalCellWriter) !void {
    if (tcw.remaining_width > 0) {
        try tcw.child.writeAll(trunc_str);
        tcw.remaining_width -= 1;
    }
    tcw.state = .reset;
    tcw.last_cp = 0;
    tcw.character_buf.clear();
    tcw.finished = true;
}

pub fn terminalCellWriter(
    wr: *Writer,
    grapheme_clustering_mode: GraphemeClusteringMode,
    width: u32,
) TerminalCellWriter {
    return .init(wr, grapheme_clustering_mode, width);
}

fn firstNonAscii(bytes: []const u8) ?usize {
    for (bytes, 0..) |c, i| {
        if (c >= 0x80) return i;
    }

    return null;
}

/// Returns the last codepoint in `bytes`. Asserts that `bytes` is valid utf-8.
fn lastCodepoint(bytes: []const u8) u21 {
    const i = lastCodepointIndex(bytes);
    return std.unicode.utf8Decode(bytes[i..]) catch unreachable;
}

fn lastCodepointIndex(bytes: []const u8) usize {
    var i = bytes.len;
    while (i > 0) {
        i -= 1;
        if (isStartByte(bytes[i])) break;
    }
    return i;
}

fn isStartByte(c: u8) bool {
    return (c & 0xC0) != 0x80;
}

fn utf8Iterator(bytes: []const u8) std.unicode.Utf8Iterator {
    return .{ .bytes = bytes, .i = 0 };
}

// fn fuzz(_: void, input: []const u8) anyerror!void {
//     if (input.len == 0) return;

//     var tcw = terminalCellWriter(std.io.null_writer, .grapheme, input[0] +| 1);
//     const codepoints = std.mem.bytesAsSlice(u21, input[0 .. input.len - input.len % 4]);
//     for (codepoints) |cp| {
//         const valid_codepoint = std.unicode.utf8ValidCodepoint(cp);
//         if (valid_codepoint) {
//             var buf: [4]u8 = undefined;
//             const len = try std.unicode.utf8Encode(cp, &buf);
//             for (buf[0..len]) |c| {
//                 const bytes_written = try tcw.writer().write(&.{c});
//                 try std.testing.expectEqual(1, bytes_written);
//             }
//         }
//     }

//     try tcw.finish();
// }

// test "tcw fuzz" {
//     try std.testing.fuzz({}, fuzz, .{});
// }

fn testWriter(
    comptime mode: GraphemeClusteringMode,
    width: u32,
    input: []const u8,
    expected: []const u8,
) !void {
    var buf: [4096]u8 = undefined;
    var child: Writer = .fixed(&buf);
    var tcw: TerminalCellWriter = .init(&child, mode, width);
    for (input) |c| {
        const bytes_written = try tcw.interface.write(&.{c});
        try std.testing.expectEqual(1, bytes_written);
    }
    try tcw.finish();
    try std.testing.expectEqualStrings(expected, child.buffered());

    child.end = 0;
    tcw = .init(&child, mode, width);
    try tcw.interface.writeAll(input);
    try tcw.finish();
    try std.testing.expectEqualStrings(expected, child.buffered());
}

fn testLegacy(width: u32, input: []const u8, expected: []const u8) !void {
    try testWriter(.codepoint, width, input, expected);
}

fn test2027(width: u32, input: []const u8, expected: []const u8) !void {
    try testWriter(.grapheme, width, input, expected);
}

fn testAll(width: u32, input: []const u8, expected: []const u8) !void {
    try testWriter(.codepoint, width, input, expected);
    try testWriter(.grapheme, width, input, expected);
}

fn initData() !void {
    try @import("main.zig").initUnicodeData(std.testing.allocator);
}

fn deinitData() void {
    @import("main.zig").deinitUnicodeData(std.testing.allocator);
}

test "tcw ascii" {
    try initData();
    defer deinitData();

    try testAll(0, "", "");
    try testAll(0, "a", "");
    try testAll(0, "hello world", "");
    try testAll(1, "hello world", trunc_str);
    try testAll(1, "", "");
    try testAll(5, "hi", "hi");
    try testAll(1, "hi", trunc_str);
    try testAll(3, "howdy", "ho" ++ trunc_str);
    try testAll(2, "漢a", trunc_str);
}

test "tcw ASCII zero width" {
    try initData();
    defer deinitData();

    try testAll(0, "\n", "");
    try testAll(0, "\t", "");
    try testAll(0, "\x00", "");
    try testAll(1, "\nw", "w");
    try testAll(1, "\tw", "w");
    try testAll(1, "\x00w", "w");
    try testAll(0, "\x00w", "");
    try testAll(2, "\x00howdy", "h" ++ trunc_str);

    var buf: [32]u8 = undefined;
    for (&buf, 0..) |*dest, i| dest.* = @intCast(i);
    try testAll(1, &buf, "");
}

test "tcw double width" {
    try initData();
    defer deinitData();

    try testAll(0, "漢", "");
    try testAll(1, "漢", trunc_str);
    try testAll(1, "w漢", trunc_str);
    try testAll(1, "漢w", trunc_str);
    try testAll(1, "漢字漢字漢字漢字", trunc_str);

    try testAll(2, "漢", "漢");
    try testAll(2, "漢字", trunc_str);
    try testAll(2, "漢字漢字漢字漢字", trunc_str);
    try testAll(2, "漢w", trunc_str);
    try testAll(2, "w漢", "w" ++ trunc_str);
    try testAll(2, "ww漢", "w" ++ trunc_str);
    try testAll(2, "漢ww漢", trunc_str);

    try testAll(3, "漢", "漢");
    try testAll(3, "漢字", "漢" ++ trunc_str);
    try testAll(3, "漢字漢字漢字漢字", "漢" ++ trunc_str);
    try testAll(3, "漢w", "漢w");
    try testAll(3, "漢ww", "漢" ++ trunc_str);
}

test "tcw multi codepoint graphemes" {
    try initData();
    defer deinitData();

    const str = "🧑‍🌾";
    try testLegacy(0, str, "");
    try testLegacy(1, str, trunc_str);
    try testLegacy(2, str, trunc_str);
    try testLegacy(3, str, "🧑" ++ trunc_str);
    try testLegacy(4, str, "🧑🌾"); // zwj shouldn't be written in legacy
    try testLegacy(5, str, "🧑🌾");

    try initData();
    defer deinitData();

    try test2027(0, str, "");
    try test2027(1, str, trunc_str);
    try test2027(2, str, str);
    try test2027(3, str, str);
    try test2027(4, str, str);
}

test TerminalCellWriter {
    try initData();
    defer deinitData();

    const data = .{
        .{ .finish, 8, "12345678", "12345678" },
        .{ .finish, 7, "12345678", "123456…" },
        .{ .pad, 20, "12345678", "12345678            " },
        .{ .pad, 10, "漢字漢字漢字漢字", "漢字漢字… " },
        .{ .pad, 16, "漢字漢字漢字漢字", "漢字漢字漢字漢字" },
        .{ .pad, 20, "漢字漢字漢字漢字", "漢字漢字漢字漢字    " },
        .{ .pad, 6, "漢字漢字", "漢字… " },
    };

    inline for (data) |d| {
        const end, const width, const input, const expected = d;
        var buf: [128]u8 = undefined;
        var child: Writer = .fixed(&buf);
        var rpw: TerminalCellWriter = .init(&child, .codepoint, width);
        try rpw.interface.writeAll(input);
        switch (end) {
            .finish => try rpw.finish(),
            .pad => try rpw.pad(),
            else => comptime unreachable,
        }
        try std.testing.expectEqualStrings(expected, child.buffered());
    }
}

test "tcw single write" {
    try initData();
    defer deinitData();

    var buf: [4096]u8 = undefined;
    var child: Writer = .fixed(&buf);
    var tcw: TerminalCellWriter = .init(&child, .codepoint, 100);
    const str = "single write!🧑‍🌾";
    const bytes_written = tcw.interface.write(str);
    try std.testing.expectEqual(str.len, bytes_written);
}
