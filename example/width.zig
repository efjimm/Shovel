const std = @import("std");
const shovel = @import("shovel");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var term = try shovel.Term.init(allocator, .{});
    defer term.deinit(allocator);

    try term.uncook(.{});
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

const RenderError = shovel.Term.WriteError;

fn render(term: *shovel.Term) RenderError!void {
    var rc = try term.getRenderContext(4096);

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
    const writer = rc.buffer.writer();

    try writer.print("Should display '{s}': '", .{desired});

    var cw = rc.cellWriter(width);

    try cw.writer().writeAll(string);
    try cw.finish();

    try writer.writeAll("'");
}
