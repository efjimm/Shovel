const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const use_llvm = !(b.option(bool, "native", "use native codegen backends") orelse false);

    const wcwidth = b.dependency("wcwidth", .{
        .target = target,
        .optimize = optimize,
    }).module("wcwidth");
    const critbit = b.dependency("critbit-zig", .{
        .target = target,
        .optimize = optimize,
    }).module("critbit");
    const grapheme = b.dependency("libgrapheme-zig", .{
        .target = target,
        .optimize = optimize,
    });

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
    shovel_module.addImport("grapheme", grapheme.module("grapheme"));
    shovel_module.addImport("build_options", opts_module);

    const filter = b.option([]const u8, "test-filter", "Filter string for tests");

    const tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .filter = filter,
        .use_llvm = use_llvm,
        .use_lld = use_llvm,
    });
    tests.root_module.addImport("wcwidth", wcwidth);
    tests.root_module.addImport("critbit", critbit);
    tests.root_module.addImport("grapheme", grapheme.module("grapheme"));
    tests.root_module.addImport("build_options", opts_module);
    if (target.result.isBSD())
        tests.linkLibC();

    const check_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .filter = filter,
        .use_llvm = use_llvm,
        .use_lld = use_llvm,
    });
    check_tests.root_module.addImport("wcwidth", wcwidth);
    check_tests.root_module.addImport("critbit", critbit);
    check_tests.root_module.addImport("grapheme", grapheme.module("grapheme"));
    check_tests.root_module.addImport("build_options", opts_module);
    if (target.result.isBSD())
        check_tests.linkLibC();

    const check_step = b.step("check", "check for compile errors");
    check_step.dependOn(&check_tests.step);

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_tests.step);
    const examples = .{
        .{ "menu", "example/menu.zig" },
        .{ "menu-libc", "example/menu.zig" },
        .{ "input-demo", "example/input-demo.zig" },
        .{ "colours", "example/colours.zig" },
        .{ "table-256-colours", "example/table-256-colours.zig" },
        .{ "width", "example/width.zig" },
    };

    inline for (examples) |data| {
        const name, const path = data;
        const example_opts: Build.ExecutableOptions = .{
            .name = name,
            .root_source_file = b.path(path),
            .target = target,
            .optimize = optimize,
            .use_llvm = use_llvm,
            .use_lld = use_llvm,
        };

        const example = b.addExecutable(example_opts);
        example.root_module.addImport("shovel", shovel_module);
        b.installArtifact(example);

        const check_example = b.addExecutable(example_opts);
        check_example.root_module.addImport("shovel", shovel_module);

        check_step.dependOn(&check_example.step);
    }

    const bench_step = b.step("bench", "Run benchmarks");
    const bench_install = b.step("bench-install", "Install benchmark executables");

    const bench_vaxis = b.option(bool, "bench-vaxis", "Benchmark against libvaxis") orelse false;

    const vaxis: ?*Build.Dependency = if (bench_vaxis)
        b.lazyDependency("vaxis", .{
            .target = target,
            .optimize = optimize,
        }) orelse return
    else
        null;

    const bench_options = b.addOptions();
    bench_options.addOption(bool, "vaxis", bench_vaxis);
    const bench_options_module = bench_options.createModule();

    inline for (.{
        .{ "canvas", "src/bench.zig" },
    }) |data| {
        const name, const path = data;
        const exe = b.addExecutable(.{
            .name = name,
            .root_source_file = b.path(path),
            .target = target,
            .optimize = optimize,
            .use_llvm = use_llvm,
            .use_lld = use_llvm,
            .strip = false,
        });
        if (vaxis) |dep| exe.root_module.addImport("vaxis", dep.module("vaxis"));
        exe.root_module.addImport("shovel", shovel_module);
        exe.root_module.addImport("benchmark_options", bench_options_module);
        const run = b.addRunArtifact(exe);
        bench_step.dependOn(&run.step);
        const install = b.addInstallArtifact(exe, .{
            .dest_dir = .{ .override = .{ .custom = "bench" } },
        });
        bench_install.dependOn(&install.step);
    }

    const run_coverage = b.addSystemCommand(&.{ "kcov", "kcov-out", "--include-path", "src" });
    run_coverage.addArtifactArg(tests);

    const coverage_step = b.step("coverage", "Test coverage");
    coverage_step.dependOn(&run_coverage.step);
}
