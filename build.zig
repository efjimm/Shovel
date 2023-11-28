const std = @import("std");
const Build = std.Build;

fn example(
    b: *Build,
    spoon_module: *Build.Module,
    opts: Build.ExecutableOptions,
) void {
    const exe = b.addExecutable(opts);
    exe.addModule("spoon", spoon_module);
    b.installArtifact(exe);
}

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const wcwidth = b.dependency("wcwidth", .{}).module("wcwidth");

    const spoon_module = b.addModule("spoon", .{
        .source_file = .{ .path = "src/main.zig" },
        .dependencies = &.{.{ .name = "wcwidth", .module = wcwidth }},
    });

    const tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    tests.addModule("wcwidth", wcwidth);

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_tests.step);

    _ = example(b, spoon_module, .{
        .name = "menu",
        .root_source_file = .{ .path = "example/menu.zig" },
        .target = target,
        .optimize = optimize,
    });

    example(b, spoon_module, .{
        .name = "menu-libc",
        .root_source_file = .{ .path = "example/menu.zig" },
        .link_libc = true,
        .target = target,
        .optimize = optimize,
    });

    _ = example(b, spoon_module, .{
        .name = "input-demo",
        .root_source_file = .{ .path = "example/input-demo.zig" },
        .target = target,
        .optimize = optimize,
    });
    _ = example(b, spoon_module, .{
        .name = "colours",
        .root_source_file = .{ .path = "example/colours.zig" },
        .target = target,
        .optimize = optimize,
    });
    _ = example(b, spoon_module, .{
        .name = "table-256-colours",
        .root_source_file = .{ .path = "example/table-256-colours.zig" },
        .target = target,
        .optimize = optimize,
    });
    _ = example(b, spoon_module, .{
        .name = "width",
        .root_source_file = .{ .path = "example/width.zig" },
        .target = target,
        .optimize = optimize,
    });
}
