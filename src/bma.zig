//! Bitmap allocator directly based on Ghostty's implementation.
const std = @import("std");
const assert = std.debug.assert;

pub fn BitmapAllocator(comptime chunksize: usize) type {
    return struct {
        bytes: []align(@alignOf(Bitmap)) u8,
        bitmap_count: usize,

        const Self = @This();

        pub const empty: Self = .{ .bytes = &.{}, .bitmap_count = 0 };

        pub const Bitmap = u64;
        pub const Chunk = [chunksize]u8;

        pub fn bitmapsEnd(bitmap_count: usize) usize {
            return bitmap_count * @sizeOf(Bitmap);
        }

        pub fn chunksStart(bitmap_count: usize) usize {
            return std.mem.alignForward(usize, bitmapsEnd(bitmap_count), chunksize);
        }

        pub fn chunksSize(bitmap_count: usize) usize {
            return chunksize * bitmap_count * 64;
        }

        pub fn getBitmaps(self: *const Self) []u64 {
            const slice: []Bitmap = @ptrCast(self.bytes);
            return slice[0..self.bitmap_count];
        }

        pub fn getChunks(self: *const Self) []Chunk {
            return @ptrCast(@alignCast(self.bytes[chunksStart(self.bitmap_count)..]));
        }

        pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
            gpa.free(self.bytes);
            self.* = undefined;
        }

        pub fn clearRetainingCapacity(self: *Self) void {
            @memset(self.getBitmaps(), 0xFF);
        }

        pub fn grow(
            self: *Self,
            gpa: std.mem.Allocator,
            minimum: usize,
        ) std.mem.Allocator.Error!void {
            const new_bitmap_count = self.bitmap_count + (minimum +| (minimum / 2 + 8));
            const new_chunk_count = new_bitmap_count * @bitSizeOf(Bitmap);

            const new_size = std.mem.alignForward(usize, new_bitmap_count * @sizeOf(Bitmap), chunksize) +
                new_chunk_count * chunksize;

            self.bytes = try gpa.realloc(self.bytes, new_size);
            const old_chunks_start = chunksStart(self.bitmap_count);
            const old_chunks_size = chunksSize(self.bitmap_count);
            const new_chunks_start = chunksStart(new_bitmap_count);

            @memmove(self.bytes[new_chunks_start..].ptr, self.bytes[old_chunks_start..][0..old_chunks_size]);

            const old_bitmaps_end = bitmapsEnd(self.bitmap_count);
            @memset(self.bytes[old_bitmaps_end..new_chunks_start], 0xFF);

            self.bitmap_count = new_bitmap_count;
        }

        pub fn chunkOffset(self: *const Self, slice: anytype) usize {
            const byte_offset = @intFromPtr(slice.ptr) - @intFromPtr(self.bytes.ptr) -
                chunksStart(self.bitmap_count);
            const chunk_offset = @divExact(byte_offset, chunksize);

            return chunk_offset;
        }

        pub fn chunkPtr(self: *const Self, T: type, chunk_offset: usize) [*]T {
            return @ptrCast(@alignCast(self.getChunks()[chunk_offset..].ptr));
        }

        pub fn alloc(self: *Self, gpa: std.mem.Allocator, T: type, n: usize) error{OutOfMemory}![]T {
            const byte_count = std.math.mul(usize, n, @sizeOf(T)) catch
                return error.OutOfMemory;
            const chunk_count = std.math.divCeil(usize, byte_count, chunksize) catch
                return error.OutOfMemory;

            const chunk_index = findFreeChunks(self.getBitmaps(), chunk_count) orelse blk: {
                try self.grow(gpa, chunk_count);
                break :blk findFreeChunks(self.getBitmaps(), chunk_count) orelse unreachable;
            };

            const ptr: [*]u8 = @ptrCast(&self.getChunks()[chunk_index]);
            return @ptrCast(@alignCast(ptr[0..byte_count]));
        }

        pub fn free(self: *Self, slice: anytype) void {
            // Convert the slice of whatever type to a slice of bytes. We
            // can then use the byte len and chunk size to determine the
            // number of chunks that were allocated.
            const bytes = std.mem.sliceAsBytes(slice);
            const aligned_len = std.mem.alignForward(usize, bytes.len, chunksize);
            const chunk_count = @divExact(aligned_len, chunksize);

            // From the pointer, we can calculate the exact index.
            const chunks = self.getChunks();
            const chunk_idx = @divExact(@intFromPtr(slice.ptr) - @intFromPtr(chunks.ptr), chunksize);

            const bitmaps = self.getBitmaps();

            // Current bitmap index.
            var i: usize = @divFloor(chunk_idx, 64);
            // Number of chunks we still have to mark as free.
            var rem: usize = chunk_count;

            // Mark any bits in the starting bitmap that need to be marked.
            {
                // Bit index.
                const bit = chunk_idx % 64;
                // Number of bits we need to mark in this bitmap.
                const bits = @min(rem, 64 - bit);

                bitmaps[i] |= ~@as(u64, 0) >> @intCast(64 - bits) << @intCast(bit);
                rem -= bits;
            }

            // Mark any full bitmaps worth of bits that need to be marked.
            i += 1;
            while (rem > 64) : (i += 1) {
                bitmaps[i] = std.math.maxInt(u64);
                rem -= 64;
            }

            // Mark any bits at the start of this last bitmap if it needs it.
            if (rem > 0) {
                bitmaps[i] |= ~@as(u64, 0) >> @intCast(64 - rem);
            }
        }
    };
}

/// Find `n` sequential free chunks in the given bitmaps and return the index
/// of the first chunk. If no chunks are found, return `null`. This also updates
/// the bitmap to mark the chunks as used.
fn findFreeChunks(bitmaps: []u64, n: usize) ?usize {
    // NOTE: This is a naive implementation that just iterates through the
    // bitmaps. There is very likely a more efficient way to do this but
    // I'm not a bit twiddling expert. Perhaps even SIMD could be used here
    // but unsure. Contributor friendly: let's benchmark and improve this!

    // Large chunks require special handling.
    if (n > @bitSizeOf(u64)) {
        var i: usize = 0;
        search: while (i < bitmaps.len) {
            // Number of chunks available at the end of this bitmap.
            const prefix = @clz(~bitmaps[i]);

            // If there are no chunks available at the end of this bitmap
            // then we can't start in it, so we'll try the next one.
            if (prefix == 0) {
                i += 1;
                continue;
            }

            // Starting position if we manage to find the span we need here.
            const start_bitmap = i;
            const start_bit = 64 - prefix;

            // The remaining number of sequential free chunks we need to find.
            var rem: usize = n - prefix;

            i += 1;
            while (rem > 64) : (i += 1) {
                // We ran out of bitmaps, there's no sufficiently large gap.
                if (i >= bitmaps.len) return null;

                // There's more than 64 remaining chunks and this bitmap has
                // content in it, so we try starting again with this bitmap.
                if (bitmaps[i] != std.math.maxInt(u64)) continue :search;

                // This bitmap is completely free, we can subtract 64 from
                // our remaining number.
                rem -= 64;
            }

            // If the number of available chunks at the start of this bitmap
            // is less than the remaining required, we have to try again.
            if (@ctz(~bitmaps[i]) < rem) continue;

            const suffix = (n - prefix) % 64;

            // Found! Mark everything between our start and end as full.
            bitmaps[start_bitmap] ^= ~@as(u64, 0) >> @intCast(start_bit) << @intCast(start_bit);
            const full_bitmaps = @divFloor(n - prefix - suffix, 64);
            for (bitmaps[start_bitmap + 1 ..][0..full_bitmaps]) |*bitmap| {
                bitmap.* = 0;
            }
            if (suffix > 0) bitmaps[i] ^= ~@as(u64, 0) >> @intCast(64 - suffix);

            return start_bitmap * 64 + start_bit;
        }

        return null;
    }

    assert(n <= @bitSizeOf(u64));
    for (bitmaps, 0..) |*bitmap, idx| {
        // Shift the bitmap to find `n` sequential free chunks.
        // EXAMPLE:
        // n = 4
        // shifted = 001111001011110010
        //         & 000111100101111001
        //         & 000011110010111100
        //         & 000001111001011110
        //         = 000001000000010000
        //                ^       ^
        // In this example there are 2 places with at least 4 sequential 1s.
        var shifted: u64 = bitmap.*;
        for (1..n) |i| shifted &= bitmap.* >> @intCast(i);

        // If we have zero then we have no matches
        if (shifted == 0) continue;

        // Trailing zeroes gets us the index of the first bit index with at
        // least `n` sequential 1s. In the example above, that would be `4`.
        const bit = @ctz(shifted);

        // Calculate the mask so we can mark it as used
        const mask = (@as(u64, std.math.maxInt(u64)) >> @intCast(64 - n)) << @intCast(bit);
        bitmap.* ^= mask;

        return (idx * 64) + bit;
    }

    return null;
}
