const std = @import("std");
const Build = std.Build;

fn example(
	b: *Build,
	name: []const u8,
	file: []const u8,
	target: anytype,
	optimize: std.builtin.Mode,
) *Build.CompileStep {
	const exe = b.addExecutable(.{
		.name = name,
		.root_source_file = .{ .path = file },
		.target = target,
		.optimize = optimize,
	});
    exe.addAnonymousModule("spoon", .{
    	.source_file = .{ .path = "import.zig" },
    });
    b.installArtifact(exe);
    return exe;
}

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const wcwidth = b.dependency("wcwidth", .{}).module("wcwidth");
    b.modules.put(b.dupe("wcwidth"), wcwidth) catch @panic("OOM");

    _ = b.addModule("spoon", .{
    	.source_file = .{ .path = "import.zig" },
    	.dependencies = &.{
    		.{
    			.name = "wcwidth",
    			.module = wcwidth,
    		},
    	},
    });

    const tests = b.addTest(.{
    	.root_source_file = .{ .path = "test_main.zig" },
    	.target = target,
    	.optimize = optimize,
    });
    tests.addModule("wcwidth", wcwidth);

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_tests.step);

    _ = example(b, "menu", "example/menu.zig", target, optimize);

    const menu_libc = example(b, "menu-libc", "example/menu.zig", target, optimize);
	menu_libc.linkLibC();

	_ = example(b, "input-demo", "example/input-demo.zig", target, optimize);
	_ = example(b, "colours", "example/colours.zig", target, optimize);
	_ = example(b, "table-256-colours", "example/table-256-colours.zig", target, optimize);
	_ = example(b, "width", "example/width.zig", target, optimize);
}
