const std = @import("std");
const root = @import("root");
const build_options = @import("build_options");
const log = std.log.scoped(.spoon);

const logging_enabled = build_options.logging_enabled;

pub fn debug(comptime format: []const u8, args: anytype) void {
    if (!logging_enabled) return;

    log.debug(format, args);
}

pub fn info(comptime format: []const u8, args: anytype) void {
    if (!logging_enabled) return;

    log.info(format, args);
}

pub fn warn(comptime format: []const u8, args: anytype) void {
    if (!logging_enabled) return;

    log.warn(format, args);
}

pub fn err(comptime format: []const u8, args: anytype) void {
    if (!logging_enabled) return;

    log.err(format, args);
}
