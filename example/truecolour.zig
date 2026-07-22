const std = @import("std");
const shovel = @import("shovel");

pub const std_options: std.Options = .{
    .logFn = log,
    .log_level = .debug,
};
fn log(
    comptime message_level: std.log.Level,
    comptime scope: @EnumLiteral(),
    comptime format: []const u8,
    args: anytype,
) void {
    var buf: [1024]u8 = undefined;
    var w = log_file.writerStreaming(global_io, &buf);
    w.interface.print("[{t}] {t}: ", .{ scope, message_level }) catch @panic("guh");
    w.interface.print(format, args) catch @panic("guh");
    w.interface.writeByte('\n') catch @panic("guh");
    w.flush() catch @panic("guh");
}

var global_io: std.Io = undefined;
var log_file: std.Io.File = undefined;

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    const io = init.io;
    global_io = io;

    log_file = try std.Io.Dir.cwd().createFile(global_io, "log.txt", .{});

    var term: shovel.Term = try .init(gpa, io, init.minimal.environ, .{
        .terminfo = .{
            .fallback = .@"xterm-256color",
            .fallback_mode = .last_resort,
        },
        .truecolour = .check,
    });
    defer term.deinit(gpa);

    var screen: shovel.Screen = .empty;
    defer screen.deinit(gpa);

    try term.fetchSize();

    try screen.resize(gpa, 16, 80);

    var w = screen.writerFull(gpa);

    w.rect = screen.screenRect().clamped(.{ .x = 0, .y = 0, .width = 16, .height = 16 });
    for (0..256) |i| {
        const row, const col = waveform(@intCast(i), 16);
        w.setCursor(row, col);
        try w.setStyle(.{
            .bg = .{ .rgb = .{ 0, 0, @intCast(i) } },
        });
        try w.writeAll(" ");
    }

    w.rect = screen.screenRect().clamped(.{ .x = 16, .y = 0, .width = 16, .height = 16 });
    for (0..256) |i| {
        const row, const col = waveform(@intCast(i), 16);
        w.setCursor(row, col);
        try w.setStyle(.{
            .bg = .{ .rgb = .{ 0, @intCast(i), 0 } },
        });
        try w.writeAll(" ");
    }

    w.rect = screen.screenRect().clamped(.{ .x = 32, .y = 0, .width = 16, .height = 16 });
    for (0..256) |i| {
        const row, const col = waveform(@intCast(i), 16);
        w.setCursor(row, col);
        try w.setStyle(.{
            .bg = .{ .rgb = .{ @intCast(i), 0, 0 } },
        });
        try w.writeAll(" ");
    }

    w.rect = screen.screenRect().clamped(.{ .x = 48, .y = 0, .width = 16, .height = 16 });
    for (0..256) |i| {
        const j: u8 = @intCast(i);
        const row, const col = waveform(j, 16);
        w.setCursor(row, col);
        try w.setStyle(.{
            .bg = .{ .rgb = .{ j, j, j } },
        });
        try w.writeAll(" ");
    }

    const colors = [_][3]i9{
        // .{ 0x1b, 0x1b, 0x6f },
        .{ 0x20, 0x60, 0xc0 },
        // .{ 0x28, 0xb8, 0xd6 },
        .{ 0x40, 0xd0, 0x60 },
        .{ 0xf0, 0xd0, 0x40 },
        // .{ 0xe0, 0x60, 0x40 },
        .{ 0xd4, 0x3d, 0x3d },
        // .{ 0xe0, 0x40, 0xa0 },
        .{ 0xa0, 0x30, 0x80 },
    };

    w.rect = screen.screenRect().clamped(.{ .x = 64, .y = 0, .width = 16, .height = 16 });
    w.setCursor(0, 0);
    for (0..256) |i| {
        std.log.debug("Colors {d}", .{i / 64});
        const left = colors[i / 64];
        const right = colors[i / 64 + 1];
        const diff: [3]i9 = .{
            right[0] - left[0],
            right[1] - left[1],
            right[2] - left[2],
        };

        const f = @as(f64, @floatFromInt(i % 64)) / 64;

        const new: [3]u8 = .{
            @round(left[0] + @as(f64, @floatFromInt(diff[0])) * f),
            @round(left[1] + @as(f64, @floatFromInt(diff[1])) * f),
            @round(left[2] + @as(f64, @floatFromInt(diff[2])) * f),
        };

        const row, const col = waveform(@intCast(i), 16);
        w.setCursor(row, col);

        try w.setStyle(.{ .bg = .{ .rgb = new } });
        try w.writeAll(" ");
    }

    var buf: [8192]u8 = undefined;
    var wr = std.Io.File.stdout().writerStreaming(io, &buf);
    try screen.dump(screen.screenRect(), term.terminfo, &wr.interface);
    try shovel.Style.dump(.{}, &wr.interface, .{});
    try wr.interface.flush();
}

fn waveform(i: u16, wh: u16) [2]u16 {
    var k = i;
    var wave: u16 = 0;
    while (true) : (wave += 1) {
        const length =
            if (wave < wh) wave + 1 else 2 * wh - 1 - wave;

        if (k < length) break;
        k -= length;
    }

    if (wave < wh)
        return .{ k, wave - k };
    return .{ wave - (wh - 1) + k, (wh - 1) - k };
}
