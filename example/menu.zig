const std = @import("std");
const heap = std.heap;
const math = std.math;
const mem = std.mem;
const os = std.os;
const posix = std.posix;
const builtin = @import("builtin");

const shovel = @import("shovel");

pub const std_options: std.Options = .{
    .log_level = .debug,
    .logFn = log,
};

var logfile: std.fs.File = undefined;

pub fn log(
    comptime _: std.log.Level,
    comptime _: @TypeOf(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    var wr = logfile.writerStreaming(&.{});
    wr.interface.print(format ++ "\n", args) catch @panic("");
    wr.interface.flush() catch @panic("");
}

var term: shovel.Term = undefined;
var running: bool = true;

var cursor: usize = 0;

const entries = [_][]const u8{ "foo", "bar", "baz", "longer", "→µ←" };

pub fn main() !void {
    logfile = try std.fs.cwd().createFile("log.txt", .{});
    defer logfile.close();

    var dbg_allocator: std.heap.DebugAllocator(.{}) = .init;
    const gpa, const is_debug = switch (builtin.mode) {
        .Debug, .ReleaseSafe => .{ dbg_allocator.allocator(), true },
        .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
    };
    defer _ = if (is_debug) dbg_allocator.deinit();

    try shovel.initUnicodeData(gpa);
    defer shovel.deinitUnicodeData(gpa);

    term = try shovel.Term.init(gpa, .{
        .terminfo = .{
            .fallback = .@"xterm-256color",
            .fallback_mode = .merge,
        },
    });
    defer term.deinit(gpa);

    var db = term.doubleBuffer(gpa);
    defer db.deinit();

    posix.sigaction(
        posix.SIG.WINCH,
        &.{
            .handler = .{ .handler = handleSigWinch },
            .mask = posix.sigemptyset(),
            .flags = 0,
        },
        null,
    );

    // Shovel will return the terminal back to cooked state automatically
    // when we call `term.deinit()`.
    try term.uncook(gpa, .{});

    try term.fetchSize();
    try term.setWindowTitle("Shovel example: menu", .{});

    var term_buf: [4096]u8 = undefined;
    var term_writer = term.writer(&term_buf);

    var input_buf: [16]u8 = undefined;
    while (running) {
        if (needs_update) {
            if (needs_resize) {
                try term.fetchSize();
                try db.resize(term.width, term.height);
                needs_resize = false;
            }
            try blit(&db.write, term.width);
            try db.dump(&term_writer.interface);
            try term_writer.interface.flush();
            needs_update = false;
        }

        const slice = try term.readInput(&input_buf);
        var it = term.inputParser(slice);
        while (it.next()) |in| {
            // The input descriptor parser is not only useful for user-configuration.
            // Since it can work at comptime, you can use it to simplify the
            // matching of hardcoded keybinds as well. Down below we specify the
            // typical keybinds a terminal user would expect for moving up and
            // down, without getting our hands dirty in the interals of Shovels
            // Input object.
            if (in.eqlDescription("escape") or in.eqlDescription("q")) {
                running = false;
                break;
            }

            if (in.eqlDescription("arrow-down") or in.eqlDescription("C-n") or in.eqlDescription("j")) {
                if (cursor < entries.len - 1) {
                    cursor += 1;
                    needs_update = true;
                }
            } else if (in.eqlDescription("arrow-up") or in.eqlDescription("C-p") or in.eqlDescription("k")) {
                cursor -|= 1;
                needs_update = true;
            } else if (in.eqlDescription("g")) {
                needs_update = true;
            }
        }
    }
}

var needs_update: bool = true;
var needs_resize: bool = true;

/// Render to the given screen.
fn blit(s: *shovel.Screen, width: u16) !void {
    var buf: [4096]u8 = undefined;
    var wr = s.writerFull(&buf, .truncate, .unicode);
    const w = &wr.interface;

    if (width < 20) {
        wr.clear();
        try wr.setStyle(.{ .fg = .red, .attrs = .{ .bold = true } });
        try w.writeAll("Terminal too small!");
        try wr.flush();
        return;
    }

    try wr.setCursor(0, 0);
    try wr.setStyle(.{ .fg = .green, .attrs = .{ .reverse = true } });

    try w.writeAll(" shovel example program: menu");
    try wr.styleToEol();

    try wr.setCursor(1, 0);
    try wr.setStyle(.{ .fg = .red, .attrs = .{ .bold = true } });
    try w.writeAll(" Up and Down arrows to select, q to exit.");

    try wr.setRectClamp(.init(1, 3, 10, @intCast(entries.len)));

    for (entries, 0..) |str, y| {
        try wr.setCursor(@intCast(y), 1);
        try wr.setStyle(.{ .fg = .blue, .attrs = .{ .reverse = cursor == y } });
        try w.print(" {s}", .{str});
        try wr.styleToEol();
    }

    try w.flush();
}

/// This signal is sent to terminal applications when they are resized.
///
/// Signal handles can be called at ANY point in your programs execution, so you should never call
/// functions that depend on application state. Instead, you should set a global flack that you
/// check in your main loop.
fn handleSigWinch(_: c_int) callconv(.c) void {
    needs_resize = true;
    needs_update = true;
}

/// Custom panic handler, so that we can try to cook the terminal on a crash,
/// as otherwise all messages will be mangled.
fn panicFn(msg: []const u8, ret_addr: ?usize) noreturn {
    @branchHint(.cold);
    term.cook() catch {};
    std.debug.defaultPanic(msg, ret_addr);
}

pub const panic = std.debug.FullPanic(panicFn);
