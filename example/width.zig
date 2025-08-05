// TODO: Rework this shitty example
const std = @import("std");
const shovel = @import("shovel");

pub const std_options: std.Options = .{
    .log_level = .err,
};

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    try shovel.initUnicodeData(allocator);
    defer shovel.deinitUnicodeData(allocator);

    var term = try shovel.Term.init(allocator, .{
        .terminfo = .{
            .fallback = .@"xterm-256color",
            .fallback_mode = .last_resort,
        },
    });
    defer term.deinit(allocator);

    try term.uncook(allocator, .{});
    try term.fetchSize();

    var buf: [16]u8 = undefined;
    var running = true;
    while (running) {
        try render(&term);

        const slice = try term.readInput(&buf);
        var iter = term.inputParser(slice);
        while (iter.next()) |in| {
            switch (in.content) {
                .codepoint => |cp| switch (cp) {
                    'q', 27 => running = false,
                    else => {},
                },
                else => {},
            }
        }
    }
}

fn render(term: *shovel.Term) !void {
    var buf: [4096]u8 = undefined;
    var rc = try term.getRenderContext(&buf);

    try rc.clear();

    try rc.moveCursorTo(0, 0);
    try testPaddingWriter(&rc, "😀…", "😀😀This is epic", 4);

    try rc.moveCursorTo(1, 0);
    try testPaddingWriter(&rc, "…", "😀😀This is epic", 1);

    try rc.moveCursorTo(2, 0);
    try testPaddingWriter(&rc, "😀😀…", "😀😀This is epic", 5);

    try rc.moveCursorTo(3, 0);
    try testPaddingWriter(&rc, "漢字漢…", "漢字漢字", 7);

    try rc.done();
}

fn testPaddingWriter(
    rc: anytype,
    desired: []const u8,
    string: []const u8,
    width: u16,
) !void {
    const writer = &rc.writer.interface;

    try writer.print("Should display '{s}': '", .{desired});

    var cw = rc.cellWriter(width);

    try cw.interface.writeAll(string);
    try cw.finish();

    try writer.writeAll("'");
}
