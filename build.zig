const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const test_filter = b.option([]const u8, "test-filter", "Filter string for tests");
    const enable_logging = b.option(bool, "logging", "Enable logging") orelse false;
    const llvm = b.option(bool, "llvm", "Use LLVM");

    const xterm = b.createModule(.{
        .root_source_file = b.path("terminfo/xterm-256color"),
    });

    const dumb = b.createModule(.{
        .root_source_file = b.path("terminfo/dumb"),
    });

    const critbit = b.dependency("critbit", .{
        .target = target,
        .optimize = optimize,
    }).module("critbit");

    const zg = b.dependency("zg", .{
        .target = target,
        .optimize = optimize,
    }).module("zg");

    const opts = b.addOptions();
    opts.addOption(bool, "logging_enabled", enable_logging);
    const opts_module = opts.createModule();

    const imports: []const std.Build.Module.Import = &.{
        .{ .name = "critbit", .module = critbit },
        .{ .name = "build_options", .module = opts_module },
        .{ .name = "zg", .module = zg },
        .{ .name = "xterm-256color", .module = xterm },
        .{ .name = "dumb", .module = dumb },
    };

    const shovel_module = b.addModule("shovel", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = imports,
    });

    const tests = b.addTest(.{
        .root_module = shovel_module,
        .filters = &.{test_filter orelse ""},
        .use_llvm = llvm,
    });

    b.step("test-exe", "").dependOn(&b.addInstallArtifact(tests, .{}).step);

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_tests.step);

    const check_step = b.step("check", "check for compile errors");
    check_step.dependOn(&tests.step);

    const examples = .{
        .{ "truecolour", "example/truecolour.zig" },
        .{ "menu", "example/menu.zig" },
        .{ "input-demo", "example/input-demo.zig" },
        .{ "colours", "example/colours.zig" },
        .{ "table-256-colours", "example/table-256-colours.zig" },
        .{ "width", "example/width.zig" },
    };

    inline for (examples) |data| {
        const name, const path = data;
        const example = b.addExecutable(.{
            .name = name,
            .root_module = b.createModule(.{
                .root_source_file = b.path(path),
                .target = target,
                .optimize = optimize,
                .imports = &.{
                    .{ .name = "shovel", .module = shovel_module },
                },
            }),
        });
        b.installArtifact(example);
        check_step.dependOn(&example.step);
    }

    const run_coverage = b.addSystemCommand(&.{ "kcov", "kcov-out", "--include-path", "src" });
    run_coverage.addArtifactArg(tests);

    const coverage_step = b.step("coverage", "Test coverage");
    coverage_step.dependOn(&run_coverage.step);
}
