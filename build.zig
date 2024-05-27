const std = @import("std");
const Build = std.Build;

fn example(
    b: *Build,
    shovel_module: *Build.Module,
    opts: Build.ExecutableOptions,
) void {
    const exe = b.addExecutable(opts);
    exe.root_module.addImport("shovel", shovel_module);
    b.installArtifact(exe);
}

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const wcwidth = b.dependency("wcwidth", .{}).module("wcwidth");
    const critbit = b.dependency("critbit-zig", .{}).module("critbit");

    const enable_logging = b.option(bool, "logging", "Enable logging") orelse false;
    const opts = b.addOptions();
    opts.addOption(bool, "logging_enabled", enable_logging);

    const shovel_module = b.addModule("shovel", .{
        .root_source_file = b.path("src/main.zig"),
    });
    // TODO: I have no idea if this works on BSD, because Zig does not ship libc for any BSDs.
    // A proper BSD system is required to test. Since ziglang/zig#18910, the necessary termios
    // constants should exist now for OpenBSD and FreeBSD.
    if (target.result.isBSD())
        shovel_module.link_libc = true;
    const opts_module = opts.createModule();
    shovel_module.addImport("wcwidth", wcwidth);
    shovel_module.addImport("critbit", critbit);
    shovel_module.addImport("build_options", opts_module);

    const filter = b.option([]const u8, "test-filter", "Filter string for tests");

    const tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .filter = filter,
    });
    tests.root_module.addImport("wcwidth", wcwidth);
    tests.root_module.addImport("critbit", critbit);
    tests.root_module.addImport("build_options", opts_module);
    if (target.result.isBSD())
        tests.linkLibC();

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_tests.step);

    _ = example(b, shovel_module, .{
        .name = "menu",
        .root_source_file = b.path("example/menu.zig"),
        .target = target,
        .optimize = optimize,
    });

    example(b, shovel_module, .{
        .name = "menu-libc",
        .root_source_file = b.path("example/menu.zig"),
        .link_libc = true,
        .target = target,
        .optimize = optimize,
    });

    _ = example(b, shovel_module, .{
        .name = "input-demo",
        .root_source_file = b.path("example/input-demo.zig"),
        .target = target,
        .optimize = optimize,
    });
    _ = example(b, shovel_module, .{
        .name = "colours",
        .root_source_file = b.path("example/colours.zig"),
        .target = target,
        .optimize = optimize,
    });
    _ = example(b, shovel_module, .{
        .name = "table-256-colours",
        .root_source_file = b.path("example/table-256-colours.zig"),
        .target = target,
        .optimize = optimize,
    });
    _ = example(b, shovel_module, .{
        .name = "width",
        .root_source_file = b.path("example/width.zig"),
        .target = target,
        .optimize = optimize,
    });
}
