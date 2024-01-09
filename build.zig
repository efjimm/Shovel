const std = @import("std");
const Build = std.Build;

fn example(
    b: *Build,
    spoon_module: *Build.Module,
    opts: Build.ExecutableOptions,
) void {
    const exe = b.addExecutable(opts);
    exe.root_module.addImport("spoon", spoon_module);
    b.installArtifact(exe);
}

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const wcwidth = b.dependency("wcwidth", .{}).module("wcwidth");

    const enable_logging = b.option(bool, "logging", "Enable logging") orelse false;
    const opts = b.addOptions();
    opts.addOption(bool, "logging_enabled", enable_logging);

    const spoon_module = b.addModule("spoon", .{
        .root_source_file = .{ .path = "src/main.zig" },
    });
    const opts_module = opts.createModule();
    spoon_module.addImport("wcwidth", wcwidth);
    spoon_module.addImport("build_options", opts_module);

    const filter = b.option([]const u8, "test-filter", "Filter string for tests");

    const tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
        .filter = filter,
    });
    tests.root_module.addImport("build_options", opts_module);
    tests.root_module.addImport("wcwidth", wcwidth);

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
