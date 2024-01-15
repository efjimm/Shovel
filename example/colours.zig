// This example programs demonstrates that shovel.Style can also be used
// stand-alone without using shovel.Term.

const std = @import("std");
const io = std.io;

const shovel = @import("shovel");

const red = shovel.Style{ .fg = .red, .attrs = .{ .italic = true } };
const green = shovel.Style{ .fg = .green, .attrs = .{ .blinking = true } };
const blue = shovel.Style{ .fg = .blue, .attrs = .{ .bold = true } };
const cyan = shovel.Style{ .fg = .cyan, .attrs = .{ .reverse = true } };
const parsed = shovel.Style.Colour.fromDescription("magenta") catch
    @compileError("bad colour description");
const magenta = shovel.Style{ .fg = parsed, .attrs = .{ .dimmed = true } };
const reset = shovel.Style{};

pub fn main() !void {
    const writer = io.getStdOut().writer();

    try red.dumpRaw(writer);
    try writer.writeAll("foo ");
    try green.dumpRaw(writer);
    try writer.writeAll("bar ");
    try blue.dumpRaw(writer);
    try writer.writeAll("baz ");
    try magenta.dumpRaw(writer);
    try writer.writeAll("zig ");
    try cyan.dumpRaw(writer);
    try writer.writeAll("shovel\n");

    try reset.dumpRaw(writer);
}
