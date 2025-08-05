const std = @import("std");
const io = std.io;

const shovel = @import("shovel");

const title_colour = shovel.Style.Colour.fromDescription("7") catch
    @compileError("bad colour description");
const title = shovel.Style{ .fg = title_colour, .attrs = .{ .bold = true } };
const reset = shovel.Style{};

pub const std_options: std.Options = .{
    .log_level = .err,
};

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();

    var term = try shovel.Term.init(gpa.allocator(), .{
        .terminfo = .{
            .fallback = .@"xterm-256color",
            .fallback_mode = .last_resort,
        },
    });
    defer term.deinit(gpa.allocator());

    var file_writer = std.fs.File.stdout().writer(&.{});
    const writer = &file_writer.interface;

    var colour: u8 = 0;
    var column: usize = 0;

    const dump_opts: shovel.Style.DumpOptions = .{
        .terminfo = term.terminfo,
    };

    try writeTitle(term.terminfo, writer, "Standard colours (0 to 15)");
    while (colour < 16) : (colour += 1) {
        const attr = shovel.Style{ .bg = .{ .@"256" = colour } };

        try attr.dump(writer, dump_opts);
        try writer.writeAll("    ");
        try reset.dump(writer, dump_opts);

        column += 1;
    }

    try writeTitle(term.terminfo, writer, "\n6x6x6 cubic palette (16 to 231)");
    column = 0;
    while (colour < 232) : (colour += 1) {
        const attr = shovel.Style{ .bg = .{ .@"256" = colour } };

        if (column == 16) {
            column = 0;
            try writer.writeByte('\n');
        }

        try attr.dump(writer, dump_opts);
        try writer.writeAll("    ");
        try reset.dump(writer, dump_opts);

        column += 1;
    }

    try writeTitle(term.terminfo, writer, "\nGrayscale (232 to 255)");
    column = 0;
    while (colour < 256) : (colour += 1) {
        const attr = shovel.Style{ .bg = .{ .@"256" = colour } };

        if (column == 16) {
            column = 0;
            try writer.writeByte('\n');
        }

        try attr.dump(writer, dump_opts);
        try writer.writeAll("    ");
        try reset.dump(writer, dump_opts);

        column += 1;

        if (colour == 255) break;
    }

    try writer.writeByte('\n');
}

fn writeTitle(ti: ?*shovel.TermInfo, writer: *std.io.Writer, bytes: []const u8) !void {
    try title.dump(writer, .{ .terminfo = ti });
    try writer.writeByte('\n');
    try writer.writeAll(bytes);
    try writer.writeByte('\n');
    try reset.dump(writer, .{ .terminfo = ti });
}
