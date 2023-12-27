const std = @import("std");
const spoon = @import("spoon");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();

    var term = try spoon.Term.init(gpa.allocator(), .{});
    defer term.deinit(gpa.allocator());

    try term.uncook(.{});
    try term.fetchSize();

    var buf: [16]u8 = undefined;
    var running = true;
    while (running) {
        try render(&term);

        const slice = try term.readInput(&buf);
        var iter = spoon.inputParser(slice);
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

const RenderError = spoon.Term.WriteError;

fn render(term: *spoon.Term) RenderError!void {
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

    var rpw = rc.restrictedPaddingWriter(width);

    try rpw.writer().writeAll(string);
    try rpw.finish();

    try writer.writeAll("'");
}
