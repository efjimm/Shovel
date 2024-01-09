const std = @import("std");
const io = std.io;

const spoon = @import("spoon");

const title_colour = spoon.Style.Colour.fromDescription("7") catch
    @compileError("bad colour description");
const title = spoon.Style{ .fg = title_colour, .attrs = .{ .bold = true } };
const reset = spoon.Style{};

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();

    var term = try spoon.Term.init(gpa.allocator(), .{});
    defer term.deinit(gpa.allocator());

    const writer = io.getStdOut().writer();

    var colour: u8 = 0;
    var column: usize = 0;

    try writeTitle(term.terminfo, writer, "Standard colours (0 to 15)");
    while (colour < 16) : (colour += 1) {
        const attr = spoon.Style{ .bg = .{ .@"256" = colour } };

        try attr.dump(term.terminfo, writer);
        try writer.writeAll("    ");
        try reset.dump(term.terminfo, writer);

        column += 1;
    }

    try writeTitle(term.terminfo, writer, "\n6x6x6 cubic palette (16 to 231)");
    column = 0;
    while (colour < 232) : (colour += 1) {
        const attr = spoon.Style{ .bg = .{ .@"256" = colour } };

        if (column == 16) {
            column = 0;
            try writer.writeByte('\n');
        }

        try attr.dump(term.terminfo, writer);
        try writer.writeAll("    ");
        try reset.dump(term.terminfo, writer);

        column += 1;
    }

    try writeTitle(term.terminfo, writer, "\nGrayscale (232 to 255)");
    column = 0;
    while (colour < 256) : (colour += 1) {
        const attr = spoon.Style{ .bg = .{ .@"256" = colour } };

        if (column == 16) {
            column = 0;
            try writer.writeByte('\n');
        }

        try attr.dump(term.terminfo, writer);
        try writer.writeAll("    ");
        try reset.dump(term.terminfo, writer);

        column += 1;

        if (colour == 255) break;
    }

    try writer.writeByte('\n');
}

fn writeTitle(ti: ?*spoon.TermInfo, writer: anytype, bytes: []const u8) !void {
    try title.dump(ti, writer);
    try writer.writeByte('\n');
    try writer.writeAll(bytes);
    try writer.writeByte('\n');
    try reset.dump(ti, writer);
}
