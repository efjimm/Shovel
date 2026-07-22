const std = @import("std");
const heap = std.heap;
const math = std.math;
const mem = std.mem;
const os = std.os;
const posix = std.posix;
const builtin = @import("builtin");

const shovel = @import("shovel");

var term: shovel.Term = undefined;
var running: bool = true;

var cursor: usize = 0;

const entries = [_][]const u8{ "foo", "☀️", "baz", "longer", "→µ←" };

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

    try shovel.initUnicodeData(gpa);
    defer shovel.deinitUnicodeData(gpa);

    term = try shovel.Term.init(gpa, io, init.minimal.environ, .{
        .terminfo = .{
            .fallback = .@"xterm-256color",
            .fallback_mode = .merge,
        },
    });
    defer term.deinit(gpa);

    var db: shovel.Screen.DoubleBuffer = .init(term.terminfo);
    defer db.deinit(gpa);

    posix.sigaction(
        .WINCH,
        &.{
            .handler = .{ .handler = handleSigWinch },
            .mask = posix.sigemptyset(),
            .flags = 0,
        },
        null,
    );

    // Shovel will return the terminal back to cooked state automatically when we call
    // `term.deinit()`.
    try term.uncook(.{});

    try term.fetchSize();
    try term.setWindowTitle("Shovel example: menu", .{});

    var term_buf: [4096]u8 = undefined;
    var term_writer = term.writer(&term_buf);

    var input_buf: [16]u8 = undefined;
    while (running) {
        if (needs_update) {
            if (needs_resize) {
                try term.fetchSize();
                try db.resize(gpa, term.height, term.width);
                needs_resize = false;
            }
            try blit(gpa, &db.write, term.width);

            const file = try std.Io.Dir.cwd().createFile(io, "guhbuh.txt", .{});
            defer file.close(io);

            // var file_writer = file.writerStreaming(io, &.{});
            // try db.dump(&file_writer.interface);
            try db.dump(&term_writer.interface);

            try term_writer.interface.flush();
            needs_update = false;
        }

        const slice = term.readInputSingleThreadedBlocking(&input_buf) catch |err| switch (err) {
            error.Interrupted => &.{},
            else => |e| return e,
        };
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
fn blit(gpa: std.mem.Allocator, s: *shovel.Screen, width: u16) !void {
    // var buf: [4096]u8 = undefined;
    var wr = s.writerFull(gpa);
    wr.edge_mode = .truncate;

    if (width < 20) {
        wr.clear();
        try wr.setStyle(.{ .fg = .red, .attrs = .{ .bold = true } });
        try wr.writeAll("Terminal too small!");
        return;
    }

    wr.setCursor(0, 0);
    try wr.setStyle(.{});
    wr.clear();

    wr.setCursor(0, 0);
    std.debug.assert(wr.getCurrentCell().style.colors.palette.fg.none == true);
    try wr.setStyle(.{ .fg = .black, .bg = .green });

    try wr.writeAll(" shovel example program: menu");
    wr.styleToEol();

    wr.setCursor(1, 0);
    try wr.setStyle(.{ .fg = .red, .attrs = .{ .bold = true } });
    try wr.writeAll(" Up and Down arrows to select, q to exit.");

    // try wr.setRectClamp(.init(1, 3, 10, @intCast(entries.len)));
    wr.rect = s.screenRect().clamped(.{
        .y = 3,
        .x = 1,
        .height = @intCast(entries.len),
        .width = 10,
    });

    for (entries, 0..) |str, y| {
        wr.setCursor(@intCast(y), 1);
        try wr.setStyle(if (cursor == y) .{ .fg = .black, .bg = .blue } else .{ .fg = .blue });
        try wr.print(" {s}", .{str});
        wr.styleToEol();
    }
}

/// This signal is sent to terminal applications when they are resized.
///
/// Signal handles can be called at ANY point in your programs execution, so you should never call
/// functions that depend on application state. Instead, you should set a global flack that you
/// check in your main loop.
fn handleSigWinch(_: std.posix.SIG) callconv(.c) void {
    needs_resize = true;
    needs_update = true;
}

/// Custom panic handler, so that we can try to cook the terminal on a crash,
/// as otherwise all messages will be mangled.
fn panicFn(msg: []const u8, ret_addr: ?usize) noreturn {
    @branchHint(.cold);
    // term.cook() catch {};
    std.debug.defaultPanic(msg, ret_addr);
}

pub const panic = std.debug.FullPanic(panicFn);
