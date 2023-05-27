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

    try red.dump(writer);
    try writer.writeAll("foo ");
    try green.dump(writer);
    try writer.writeAll("bar ");
    try blue.dump(writer);
    try writer.writeAll("baz ");
    try magenta.dump(writer);
    try writer.writeAll("zig ");
    try cyan.dump(writer);
    try writer.writeAll("spoon\n");

    try reset.dump(writer);
}
