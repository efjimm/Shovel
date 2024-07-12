const std = @import("std");
const shovel = @import("shovel");
const benchmark_options = @import("benchmark_options");

pub fn Benchmark(Context: type, comptime func: anytype) type {
    return struct {
        name: []const u8,
        context: Context,
        timings: std.ArrayListUnmanaged(u64),
        count: usize,

        pub fn run(bench: *Benchmark(Context, func), allocator: std.mem.Allocator) !void {
            try bench.timings.ensureTotalCapacity(allocator, bench.count);

            var timer = try std.time.Timer.start();
            for (0..bench.count) |_| {
                timer.reset();
                try func(bench.context);
                const time = timer.read();
                bench.timings.appendAssumeCapacity(time);
            }
        }

        pub fn results(bench: Benchmark(Context, func), writer: anytype) !void {
            const SortContext = struct {
                fn lessThan(_: @This(), a: u64, b: u64) bool {
                    return a < b;
                }
            };
            std.mem.sortUnstable(u64, bench.timings.items, SortContext{}, SortContext.lessThan);

            const start = bench.timings.items.len / 20;
            const end = bench.timings.items.len - start;
            var total: usize = 0;
            for (bench.timings.items[start..end]) |time|
                total += time;

            const avg = total / (end - start);
            const median = bench.timings.items[bench.timings.items.len / 2];
            try writer.print("{s} ({d} runs)\n  Average: {}\n  Median:  {}\n\n", .{
                bench.name,
                bench.timings.items.len,
                std.fmt.fmtDuration(avg),
                std.fmt.fmtDuration(median),
            });
        }

        pub fn deinit(b: *Benchmark(Context, func), allocator: std.mem.Allocator) void {
            b.timings.deinit(allocator);
        }
    };
}

fn benchmark(
    name: []const u8,
    count: usize,
    ctx: anytype,
    func: anytype,
) Benchmark(@TypeOf(ctx), func) {
    return .{
        .name = name,
        .count = count,
        .timings = .{},
        .context = ctx,
    };
}

const BenchCanvas = struct {
    canvas: shovel.Canvas,

    fn init(allocator: std.mem.Allocator, lines: u16, cols: u16) !BenchCanvas {
        var canvas = shovel.Canvas.init(allocator, .mode_2027);
        try canvas.resize(lines, cols);
        return .{ .canvas = canvas };
    }

    fn writeAscii(b: *BenchCanvas) !void {
        var pen = b.canvas.pen(0, 0);
        const writer = pen.writer();
        while (writer.writeAll("a")) |_| {} else |err| switch (err) {
            error.EndOfCanvas => {},
            else => |e| return e,
        }
    }

    fn writeUnicode(b: *BenchCanvas) !void {
        var pen = b.canvas.pen(0, 0);
        const writer = pen.writer();
        while (writer.writeAll("🧑‍🌾")) |_| {} else |err| switch (err) {
            error.EndOfCanvas => {},
            else => |e| return e,
        }
    }

    fn render(b: *BenchCanvas) !void {
        try b.canvas.dump(.{}, std.io.null_writer);
    }

    fn deinit(b: *BenchCanvas) void {
        b.canvas.deinit();
    }
};

const BenchVaxis = struct {
    const vaxis = @import("vaxis");
    // A pointer because vaxis can't be copied by value :/
    vx: *vaxis.Vaxis,

    fn init(allocator: std.mem.Allocator, lines: u16, cols: u16) !BenchVaxis {
        const vx = try allocator.create(vaxis.Vaxis);
        vx.* = try vaxis.init(allocator, .{});
        try vx.resize(allocator, std.io.null_writer.any(), .{
            .cols = cols,
            .rows = lines,
            .x_pixel = 0,
            .y_pixel = 0,
        });
        return .{ .vx = vx };
    }

    fn deinit(b: *BenchVaxis, allocator: std.mem.Allocator) void {
        b.vx.deinit(allocator, std.io.null_writer.any());
        allocator.destroy(b.vx);
    }

    fn writeAscii(b: *BenchVaxis) !void {
        const win = b.vx.window();
        win.clear();

        for (0..win.height) |y| {
            for (0..win.width) |x| {
                _ = try win.printSegment(.{ .text = "a" }, .{
                    .row_offset = y,
                    .col_offset = x,
                });
            }
        }
    }

    fn writeUnicode(b: *BenchVaxis) !void {
        const win = b.vx.window();
        win.clear();

        for (0..win.height) |y| {
            for (0..win.width / 2) |x| {
                _ = try win.printSegment(.{ .text = "🧑‍🌾" }, .{
                    .row_offset = y,
                    .col_offset = x * 2,
                });
            }
        }
    }
};

const BenchDummy = struct {
    fn deinit(_: BenchDummy, _: std.mem.Allocator) void {}
};

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var canvas_bench = try BenchCanvas.init(alloc, 100, 400);
    defer canvas_bench.deinit();

    var vaxis_bench = if (benchmark_options.vaxis)
        try BenchVaxis.init(alloc, 100, 400);
    defer if (benchmark_options.vaxis) vaxis_bench.deinit(alloc);

    var benches = .{
        benchmark("Canvas write 40,000 ASCII chars", 1000, &canvas_bench, BenchCanvas.writeAscii),
        benchmark("Canvas write 20,000 unicode chars", 1000, &canvas_bench, BenchCanvas.writeUnicode),
        benchmark("Canvas dump", 1000, &canvas_bench, BenchCanvas.render),
    } ++ if (benchmark_options.vaxis) .{
        benchmark("Vaxis write 40,000 ASCII chars", 1000, &vaxis_bench, BenchVaxis.writeAscii),
        benchmark("Vaxis write 20,000 Unicode chars", 1000, &vaxis_bench, BenchVaxis.writeUnicode),
    } else .{};
    defer inline for (&benches) |*bench| bench.deinit(alloc);

    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    const writer = bw.writer();

    inline for (&benches) |*bench| {
        if (@TypeOf(bench) == *BenchDummy) continue;
        try bench.run(alloc);
        try bench.results(writer);
        try bw.flush();
    }
}
