const std = @import("std");
const shovel = @import("shovel");

pub const std_options: std.Options = .{
    .log_level = .err,
};

pub fn main() !void {
    var dbg: std.heap.DebugAllocator(.{}) = .init;
    defer _ = dbg.deinit();
    const gpa = dbg.allocator();

    var term: shovel.Term = try .init(gpa, .{
        .terminfo = .{
            .fallback = .@"xterm-256color",
            .fallback_mode = .last_resort,
        },
        .truecolour = .check,
    });
    defer term.deinit(gpa);

    var screen: shovel.Screen = .init(gpa, term.grapheme_clustering_mode);
    defer screen.deinit();

    try term.fetchSize();
    try screen.resize(term.width, 3);

    var buf: [8192]u8 = undefined;
    var w = screen.writerFull(&buf, .truncate, .ascii);

    try w.setRectClamp(.{ .x = 0, .y = 0, .width = 256, .height = 1 });
    for (0..256) |i| {
        try w.setStyle(.{
            .fg = w.cursor.style.fg,
            .attrs = w.cursor.style.attrs,
            .bg = .{ .rgb = .{ 0, 0, @intCast(i) } },
        });
        try w.interface.writeByte(' ');
    }

    try w.setRectClamp(.{ .x = 0, .y = 1, .width = 256, .height = 1 });
    for (0..256) |i| {
        try w.setStyle(.{
            .fg = w.cursor.style.fg,
            .attrs = w.cursor.style.attrs,
            .bg = .{ .rgb = .{ 0, @intCast(i), 0 } },
        });
        try w.interface.writeByte(' ');
    }

    try w.setRectClamp(.{ .x = 0, .y = 2, .width = 256, .height = 1 });
    for (0..256) |i| {
        try w.setStyle(.{
            .fg = w.cursor.style.fg,
            .attrs = w.cursor.style.attrs,
            .bg = .{ .rgb = .{ @intCast(i), 0, 0 } },
        });
        try w.interface.writeByte(' ');
    }

    try w.flush();

    var wr = std.fs.File.stdout().writerStreaming(&buf);
    try screen.dump(term.terminfo, &wr.interface);
    try shovel.Style.dump(.{}, &wr.interface, .{});
    try wr.interface.flush();
}
