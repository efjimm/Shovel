// This example programs demonstrates that spoon.Style can also be used
// stand-alone without using spoon.Term.

const std = @import("std");
const io = std.io;

const spoon = @import("spoon");

const red = spoon.Style{ .fg = .red, .attrs = .{ .italic = true } };
const green = spoon.Style{ .fg = .green, .attrs = .{ .blinking = true } };
const blue = spoon.Style{ .fg = .blue, .attrs = .{ .bold = true } };
const cyan = spoon.Style{ .fg = .cyan, .attrs = .{ .reverse = true } };
const parsed = spoon.Style.Colour.fromDescription("magenta") catch
    @compileError("bad colour description");
const magenta = spoon.Style{ .fg = parsed, .attrs = .{ .dimmed = true } };
const reset = spoon.Style{};

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
    try writer.writeAll("spoon\n");

    try reset.dumpRaw(writer);
}
