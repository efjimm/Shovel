const std = @import("std");
const Case = std.fmt.Case;
const assert = std.debug.assert;
const math = std.math;

pub fn isZigString(comptime T: type) bool {
    return comptime blk: {
        // Only pointer types can be strings, no optionals
        const info = @typeInfo(T);
        if (info != .pointer) break :blk false;

        const ptr = &info.pointer;
        // Check for CV qualifiers that would prevent coerction to []const u8
        if (ptr.is_volatile or ptr.is_allowzero) break :blk false;

        // If it's already a slice, simple check.
        if (ptr.size == .slice) {
            break :blk ptr.child == u8;
        }

        // Otherwise check if it's an array type that coerces to slice.
        if (ptr.size == .one) {
            const child = @typeInfo(ptr.child);
            if (child == .array) {
                const arr = &child.array;
                break :blk arr.child == u8;
            }
        }

        break :blk false;
    };
}

pub const FormatOptions = struct {
    precision: ?usize = null,
    width: ?usize = null,
    plus: bool = false,
    space: bool = false,
    alignment: std.fmt.Alignment = .right,
};

pub fn formatInt(
    value: i32,
    base: u8,
    case: Case,
    options: FormatOptions,
    writer: anytype,
) !void {
    assert(base >= 2);

    const width = options.width orelse 0;
    const precision = options.precision orelse 0;
    const digit_count = if (value == 0) 1 else (std.math.log10_int(@abs(value)) + 1);
    const write_sign = @intFromBool(value < 0 or options.plus or options.space);

    // Write fill
    // std.debug.print("width: {d}, p: {d}, d: {d}\n", .{ width, precision, digit_count });
    if (options.alignment == .right) {
        try writer.writeByteNTimes(' ', width -| precision -| digit_count -| write_sign);
    }

    // Write sign if necessary
    if (value < 0) {
        try writer.writeByte('-');
    } else if (options.plus) {
        try writer.writeByte('+');
    } else if (options.space) {
        try writer.writeByte(' ');
    }

    // Write leading zeroes if necessary
    try writer.writeByteNTimes('0', precision -| digit_count);

    // Write integer value
    try std.fmt.formatInt(@abs(value), base, case, .{}, writer);

    if (options.alignment == .left) {
        try writer.writeByteNTimes(' ', width -| precision -| digit_count -| write_sign);
    }
}

// Converts values in the range [0, 100) to a string.
fn digits2(value: usize) [2]u8 {
    return ("0001020304050607080910111213141516171819" ++
        "2021222324252627282930313233343536373839" ++
        "4041424344454647484950515253545556575859" ++
        "6061626364656667686970717273747576777879" ++
        "8081828384858687888990919293949596979899")[value * 2 ..][0..2].*;
}
