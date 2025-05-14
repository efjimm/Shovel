const std = @import("std");
const heap = std.heap;
const math = std.math;
const mem = std.mem;
const os = std.os;
const posix = std.posix;

const shovel = @import("shovel");

pub const std_options: std.Options = .{
    .log_level = .err,
};

var term: shovel.Term = undefined;
var loop: bool = true;

var cursor: usize = 0;

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    term = try shovel.Term.init(allocator, .{});
    defer term.deinit(allocator);

    posix.sigaction(posix.SIG.WINCH, &posix.Sigaction{
        .handler = .{ .handler = handleSigWinch },
        .mask = posix.sigemptyset(),
        .flags = 0,
    }, null);

    // Shovel will return the terminal back to cooked state automatically
    // when we call term.deinit().
    try term.uncook(.{});

    try term.fetchSize();
    try term.setWindowTitle("Shovel example: menu", .{});
    try render();

    var buf: [16]u8 = undefined;
    while (loop) {
        const slice = try term.readInput(&buf);
        var it = term.inputParser(slice);
        while (it.next()) |in| {
            // The input descriptor parser is not only useful for user-configuration.
            // Since it can work at comptime, you can use it to simplify the
            // matching of hardcoded keybinds as well. Down below we specify the
            // typical keybinds a terminal user would expect for moving up and
            // down, without getting our hands dirty in the interals of Shovels
            // Input object.
            if (in.eqlDescription("escape") or in.eqlDescription("q")) {
                loop = false;
                break;
            } else if (in.eqlDescription("arrow-down") or in.eqlDescription("C-n") or in.eqlDescription("j")) {
                if (cursor < 3) {
                    cursor += 1;
                    try render();
                }
            } else if (in.eqlDescription("arrow-up") or in.eqlDescription("C-p") or in.eqlDescription("k")) {
                cursor -|= 1;
                try render();
            }
        }
    }
}

fn render() !void {
    var rc = try term.getRenderContext(4096);
    defer rc.done() catch {};

    try rc.clear();

    if (term.width < 6) {
        try rc.setStyle(.{ .fg = .red, .attrs = .{ .bold = true } });
        try rc.writeAllWrapping("Terminal too small!");
        return;
    }

    try rc.moveCursorTo(0, 0);
    try rc.setStyle(.{ .fg = .green, .attrs = .{ .reverse = true } });

    // The CellWriter helps us avoid writing more than the terminal
    // is wide. It exposes a normal writer interface you can use with any
    // function that integrates with that, such as print(), write() and writeAll().
    // The CellWriter.pad() function will fill the remaining space
    // with whitespace padding.
    var cw = rc.cellWriter(term.width);
    try cw.writer().writeAll(" shovel example program: menu");
    try cw.pad();

    try rc.moveCursorTo(1, 0);
    try rc.setStyle(.{ .fg = .red, .attrs = .{ .bold = true } });
    cw = rc.cellWriter(term.width);
    try cw.writer().writeAll(" Up and Down arrows to select, q to exit.");
    try cw.finish(); // No need to pad here, since there is no background.

    const entry_width = @min(term.width - 2, 8);
    try menuEntry(&rc, " foo", 3, entry_width);
    try menuEntry(&rc, " bar", 4, entry_width);
    try menuEntry(&rc, " baz", 5, entry_width);
    try menuEntry(&rc, " →µ←", 6, entry_width);
}

fn menuEntry(rc: anytype, name: []const u8, row: u16, width: u16) !void {
    try rc.moveCursorTo(row, 2);
    try rc.setStyle(.{ .fg = .blue, .attrs = .{ .reverse = (cursor == row - 3) } });
    var cw = rc.cellWriter(width - 1);
    defer cw.pad() catch {};
    try cw.writer().writeAll(name);
}

fn handleSigWinch(_: c_int) callconv(.C) void {
    term.fetchSize() catch {};
    render() catch {};
}

/// Custom panic handler, so that we can try to cook the terminal on a crash,
/// as otherwise all messages will be mangled.
fn panic(msg: []const u8, ret_addr: ?usize) noreturn {
    @branchHint(.cold);
    term.cook() catch {};
    std.debug.defaultPanic(msg, ret_addr);
}

pub const Panic = std.debug.FullPanic(panic);
