const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const os = std.os;
const posix = std.posix;
const unicode = std.unicode;

const shovel = @import("shovel");

var term: shovel.Term = undefined;
var loop: bool = true;
var buf: [32]u8 = undefined;
var read: usize = undefined;
var empty = true;

pub fn main() !void {
    const force_legacy = blk: {
        var i: usize = 1;
        while (i < os.argv.len) : (i += 1) {
            if (mem.eql(u8, mem.span(os.argv[i]), "--force-legacy")) break :blk true;
        }
        break :blk false;
    };
    const mouse = blk: {
        var i: usize = 1;
        while (i < os.argv.len) : (i += 1) {
            if (mem.eql(u8, mem.span(os.argv[i]), "--mouse")) break :blk true;
        }
        break :blk false;
    };
    log_file = try std.fs.cwd().createFile("log.txt", .{});
    defer log_file.close();

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    term = try shovel.Term.init(allocator, .{});
    defer term.deinit(allocator);

    posix.sigaction(posix.SIG.WINCH, &.{
        .handler = .{ .handler = handleSigWinch },
        .mask = posix.sigemptyset(),
        .flags = 0,
    }, null);

    try term.uncook(.{
        .request_kitty_keyboard_protocol = !force_legacy,
        .request_mouse_tracking = mouse,
    });

    try term.fetchSize();
    try term.setWindowTitle("Shovel example: input-demo", .{});
    try render();

    while (loop) {
        read = (try term.readInput(&buf)).len;
        empty = false;
        try render();
    }
}

var log_file: std.fs.File = undefined;

pub const std_options: std.Options = .{
    .log_level = .debug,
    .logFn = log,
};

pub fn log(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    const writer = log_file.writer();
    writer.print("[{s}] {s}: ", .{ @tagName(scope), @tagName(level) }) catch {};
    writer.print(format, args) catch {};
    writer.writeByte('\n') catch {};
}

fn render() !void {
    var rc = try term.getRenderContext(4096);
    defer rc.done() catch {};

    try rc.clear();

    try rc.moveCursorTo(0, 0);
    try rc.setStyle(.{ .fg = .green, .attrs = .{ .reverse = true } });
    var cw = rc.cellWriter(term.width);
    try cw.writer().writeAll(" shovel example program: input-demo");
    try cw.writer().print("kitty keyboard {s}", .{
        if (term.kitty_enabled) "active" else "inactive",
    });
    try cw.pad();

    try rc.moveCursorTo(1, 0);
    try rc.setStyle(.{ .fg = .red, .attrs = .{ .bold = true } });
    cw = rc.cellWriter(term.width);
    try cw.writer().writeAll(" Input demo / tester, q to exit.");
    try cw.finish();

    try rc.moveCursorTo(3, 0);
    try rc.setStyle(.{ .attrs = .{ .bold = true } });
    if (empty) {
        cw = rc.cellWriter(term.width);
        try cw.writer().writeAll(" Press a key! Or try to paste something!");
        try cw.finish();
    } else {
        cw = rc.cellWriter(term.width);
        var writer = cw.writer();
        try writer.writeAll(" Bytes read:    ");
        try rc.setStyle(.{});
        try writer.print("{}", .{read});
        try cw.finish();

        var valid_unicode = true;
        _ = unicode.Utf8View.init(buf[0..read]) catch {
            valid_unicode = false;
        };
        try rc.moveCursorTo(4, 0);
        try rc.setStyle(.{ .attrs = .{ .bold = true } });
        cw = rc.cellWriter(term.width);
        writer = cw.writer();
        try writer.writeAll(" Valid unicode: ");
        try rc.setStyle(.{});
        if (valid_unicode) {
            try writer.writeAll("yes: \"");
            for (buf[0..read]) |c| {
                switch (c) {
                    127 => try writer.writeAll("^H"),
                    '\x1B' => try writer.writeAll("\\x1B"),
                    '\t' => try writer.writeAll("\\t"),
                    '\n' => try writer.writeAll("\\n"),
                    '\r' => try writer.writeAll("\\r"),
                    'a' & '\x1F' => try writer.writeAll("^a"),
                    'b' & '\x1F' => try writer.writeAll("^b"),
                    'c' & '\x1F' => try writer.writeAll("^c"),
                    'd' & '\x1F' => try writer.writeAll("^d"),
                    'e' & '\x1F' => try writer.writeAll("^e"),
                    'f' & '\x1F' => try writer.writeAll("^f"),
                    'g' & '\x1F' => try writer.writeAll("^g"),
                    'h' & '\x1F' => try writer.writeAll("^h"),
                    'k' & '\x1F' => try writer.writeAll("^k"),
                    'l' & '\x1F' => try writer.writeAll("^l"),
                    'n' & '\x1F' => try writer.writeAll("^n"),
                    'o' & '\x1F' => try writer.writeAll("^o"),
                    'p' & '\x1F' => try writer.writeAll("^p"),
                    'q' & '\x1F' => try writer.writeAll("^q"),
                    'r' & '\x1F' => try writer.writeAll("^r"),
                    's' & '\x1F' => try writer.writeAll("^s"),
                    't' & '\x1F' => try writer.writeAll("^t"),
                    'u' & '\x1F' => try writer.writeAll("^u"),
                    'v' & '\x1F' => try writer.writeAll("^v"),
                    'w' & '\x1F' => try writer.writeAll("^w"),
                    'x' & '\x1F' => try writer.writeAll("^x"),
                    'y' & '\x1F' => try writer.writeAll("^y"),
                    'z' & '\x1F' => try writer.writeAll("^z"),
                    else => try writer.writeByte(c),
                }
            }
            try writer.writeByte('"');
        } else {
            try writer.writeAll("no");
        }
        try cw.finish();

        var it = term.inputParser(buf[0..read]);
        var i: u16 = 1;
        while (it.next()) |in| : (i += 1) {
            cw = rc.cellWriter(term.width);
            writer = cw.writer();

            try rc.moveCursorTo(5 + (i - 1), 0);

            const msg = " Input events:  ";
            if (i == 1) {
                try rc.setStyle(.{ .attrs = .{ .bold = true } });
                try writer.writeAll(msg);
                try rc.setStyle(.{ .attrs = .{ .bold = false } });
            } else {
                try writer.writeByteNTimes(' ', msg.len);
            }

            var mouse: ?struct { x: u16, y: u16 } = null;

            try writer.print("{}: ", .{i});
            switch (in.content) {
                .codepoint => |cp| {
                    if (cp == 'q') {
                        loop = false;
                        return;
                    }
                    try writer.print("codepoint: {} x{X}", .{ cp, cp });
                },
                .function => |f| try writer.print("F{}", .{f}),
                .mouse => |m| {
                    mouse = .{ .x = m.x, .y = m.y };
                    try writer.print("mouse {s} {} {}", .{ @tagName(m.button), m.x, m.y });
                },
                else => try writer.writeAll(@tagName(in.content)),
            }
            if (in.mod_shift) try writer.writeAll(" +Shift");
            if (in.mod_alt) try writer.writeAll(" +Alt");
            if (in.mod_ctrl) try writer.writeAll(" +Ctrl");
            if (in.mod_super) try writer.writeAll(" +Super");

            try cw.finish();

            if (mouse) |m| {
                try rc.moveCursorTo(m.y, m.x);
                try rc.setStyle(.{ .bg = .red, .attrs = .{ .bold = true } });
                try rc.buffer.writer().writeByte('X');
            }
        }
    }
}

fn handleSigWinch(_: c_int) callconv(.C) void {
    term.fetchSize() catch {};
}

/// Custom panic handler, so that we can try to cook the terminal on a crash,
/// as otherwise all messages will be mangled.
fn panic(msg: []const u8, ret_addr: ?usize) noreturn {
    @branchHint(.cold);
    term.cook() catch {};
    std.debug.defaultPanic(msg, ret_addr);
}

pub const Panic = std.debug.FullPanic(panic);
