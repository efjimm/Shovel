// This file is part of zig-spoon, a TUI library for the zig language.
//
// Copyright © 2021 - 2022 Leon Henrik Plickat
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 3 as published
// by the Free Software Foundation.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

const std = @import("std");
const ascii = std.ascii;
const fmt = std.fmt;
const unicode = std.unicode;
const meta = std.meta;
const TermInfo = @import("TermInfo.zig");

// Kitty supports a few more modifiers, but these are the ones that actually
// make sense. Ok, super probably does not make a lot of sense, but complex
// terminal applications commonly support it, so let's just follow their lead.
const kitty_shift = 0b1;
const kitty_alt = 0b10;
const kitty_ctrl = 0b100;
const kitty_super = 0b1000;

pub const Input = struct {
    const input_description = @import("input_description.zig");
    pub const fromDescription = input_description.parseInputDescription;
    pub const FromDescriptionError = input_description.ParseError;

    /// Checks whether the Input equals an input description. Description must
    /// be comptime known.
    pub fn eqlDescription(self: Input, comptime descr: []const u8) bool {
        const description_input = comptime Input.fromDescription(descr) catch @compileError("zig-spoon: Bad input descriptor.");
        return meta.eql(self, description_input);
    }

    mod_alt: bool = false,
    mod_ctrl: bool = false,
    mod_super: bool = false,
    // The shift modifier is only reported for non-text keys
    mod_shift: bool = false,
    content: InputContent,
};

pub const InputContent = union(enum) {
    unknown,
    escape,
    arrow_up,
    arrow_down,
    arrow_left,
    arrow_right,
    begin,
    end,
    home,
    page_up,
    page_down,
    delete,
    insert,
    print,
    scroll_lock,
    pause,
    tab,
    backspace,
    command,
    enter,

    function: u8,
    codepoint: u21,

    mouse: struct { x: u16, y: u16, button: MouseButton },
};

const critbit = @import("critbit");
pub const InputMap = critbit.CritBitMap([]const u8, Input, critbit.StringContext);

pub const MouseButton = enum { btn1, btn2, btn3, release, scroll_up, scroll_down };

pub const InputParser = struct {
    // Types of escape sequences this parser can detect:
    // 1) Legacy alt escape sequences, example: "\x1Ba"
    // 2) Single letter escape sequences, example: "\x1B[H", "\x1BOF"
    // 3) Single integer escape sequences, optionally with kitty modifier, example: "\x1B[2~", "\x1B[2;3~"
    // 4) Kitty unicode sequences, optionally with modifier, example: "\x1B[127u", "\x1B[127;5u"
    // 5) Kitty modified version of 2, example: "\x1B[1;5H"
    // 6) Mouse input related escape sequences (legacy, not SGR yet)

    const Self = @This();

    bytes: []const u8,
    term: ?*Term = null,

    pub fn next(self: *Self) ?Input {
        if (self.bytes.len == 0) return null;

        // Normally we want to prioritize terminfo sequences first, however enabling the kitty
        // keyboard protocol can render the terminfo definitions for the terminal incorrect. So
        // if we were able to successfully enable the kitty keyboard protocol, then we should
        // prioritize them first.
        const kitty_first = self.term != null and self.term.?.kitty_enabled;

        if (kitty_first and self.bytes[0] == '\x1B') {
            if (self.maybeKittyEscapeSequence()) |in| return in;
        }

        if (self.term) |term| {
            if (term.terminfo) |ti| if (ti.input_map) |*map| {
                if (map.getPrefix(self.bytes)) |kv| {
                    self.advanceBufferBy(kv.key.len);
                    return kv.value;
                }
            };
        }

        if (self.bytes[0] == '\x1B') {
            return self.maybeEscapeSequence();
        } else {
            return self.utf8();
        }
    }

    fn maybeKittyEscapeSequence(self: *Self) ?Input {
        if (self.bytes.len == 1) {
            self.bytes = &.{};
            return .{ .content = .escape };
        }

        if (self.bytes[1] == '[' and self.bytes.len > 2) {
            if (ascii.isDigit(self.bytes[2])) {
                return self.numericEscapeSequence();
            } else if (singleLetterSpecialInput(self.bytes[2])) |ev| {
                self.advanceBufferBy("\x1B[A".len);
                return ev;
            }
        }

        return null;
    }

    fn utf8(self: *Self) Input {
        var advance: usize = 1;
        defer self.advanceBufferBy(advance);

        // Check for legacy control characters.
        return switch (self.bytes[0]) {
            0 => .{ .content = .{ .codepoint = ' ' }, .mod_ctrl = true },
            'a' & 0x1F => .{ .content = .{ .codepoint = 'a' }, .mod_ctrl = true },
            'b' & 0x1F => .{ .content = .{ .codepoint = 'b' }, .mod_ctrl = true },
            'c' & 0x1F => .{ .content = .{ .codepoint = 'c' }, .mod_ctrl = true },
            'd' & 0x1F => .{ .content = .{ .codepoint = 'd' }, .mod_ctrl = true },
            'e' & 0x1F => .{ .content = .{ .codepoint = 'e' }, .mod_ctrl = true },
            'f' & 0x1F => .{ .content = .{ .codepoint = 'f' }, .mod_ctrl = true },
            'g' & 0x1F => .{ .content = .{ .codepoint = 'g' }, .mod_ctrl = true },
            'h' & 0x1F => .{ .content = .{ .codepoint = 'h' }, .mod_ctrl = true },
            'i' & 0x1F => .{ .content = .tab },
            'j' & 0x1F => .{ .content = .enter }, // Carriage return, which we convert to newline.
            'k' & 0x1F => .{ .content = .{ .codepoint = 'k' }, .mod_ctrl = true },
            'l' & 0x1F => .{ .content = .{ .codepoint = 'l' }, .mod_ctrl = true },
            'm' & 0x1F => .{ .content = .enter },
            'n' & 0x1F => .{ .content = .{ .codepoint = 'n' }, .mod_ctrl = true },
            'o' & 0x1F => .{ .content = .{ .codepoint = 'o' }, .mod_ctrl = true },
            'p' & 0x1F => .{ .content = .{ .codepoint = 'p' }, .mod_ctrl = true },
            'q' & 0x1F => .{ .content = .{ .codepoint = 'q' }, .mod_ctrl = true },
            'r' & 0x1F => .{ .content = .{ .codepoint = 'r' }, .mod_ctrl = true },
            's' & 0x1F => .{ .content = .{ .codepoint = 's' }, .mod_ctrl = true },
            't' & 0x1F => .{ .content = .{ .codepoint = 't' }, .mod_ctrl = true },
            'u' & 0x1F => .{ .content = .{ .codepoint = 'u' }, .mod_ctrl = true },
            'v' & 0x1F => .{ .content = .{ .codepoint = 'v' }, .mod_ctrl = true },
            'w' & 0x1F => .{ .content = .{ .codepoint = 'w' }, .mod_ctrl = true },
            'x' & 0x1F => .{ .content = .{ .codepoint = 'x' }, .mod_ctrl = true },
            'y' & 0x1F => .{ .content = .{ .codepoint = 'y' }, .mod_ctrl = true },
            'z' & 0x1F => .{ .content = .{ .codepoint = 'z' }, .mod_ctrl = true },
            0x1D => .{ .content = .{ .codepoint = ']' }, .mod_ctrl = true },
            0x1F => .{ .content = .{ .codepoint = '/' }, .mod_ctrl = true },
            else => {
                // The terminal sends us input encoded as utf8.
                advance = unicode.utf8ByteSequenceLength(self.bytes[0]) catch
                    return .{ .content = .unknown };

                // TODO check if buffer is long enough
                if (self.bytes.len < advance) return .{ .content = .unknown };
                return if (unicode.utf8Decode(self.bytes[0..advance])) |codepoint|
                    .{ .content = .{ .codepoint = codepoint } }
                else |_|
                    .{ .content = .unknown };
            },
        };
    }

    fn maybeEscapeSequence(self: *Self) Input {
        // If \x1B is the last/only byte, it can be safely interpreted as the
        // escape key.
        if (self.bytes.len == 1) {
            self.bytes = &.{};
            return .{ .content = .escape };
        }

        // Pretty much all common escape sequences begin with '['. All of them
        // are at least three bytes long, so if we have less, this likely is
        // just a press of the scape key followed by a press of the '[' key.
        if (self.bytes[1] == '[' and self.bytes.len > 2) {
            // There are two types of '[' escape sequences.
            if (ascii.isDigit(self.bytes[2])) {
                return self.numericEscapeSequence() orelse {
                    // It most definitely is an escape sequence, just one we don't know.
                    // Since there is a good chance we can guess it's length based on the
                    // buffer length, let's  just swallow it.
                    self.bytes = &.{};
                    return .{ .content = .unknown };
                };
            } else if (self.bytes[2] == 'M') {
                return self.legacyMouseEscapeSequence();
            } else {
                return self.singleLetterEscapeSequence();
            }
        }

        // There are weird and redundant escape sequences beginning with 'O'
        // that are different for the sake of being different. Or the escape
        // character followed by the letter 'O'. Who knows! Let the heuristics
        // begin.
        if (self.bytes[1] == 'O' and self.bytes.len > 2) {
            if (singleLetterSpecialInput(self.bytes[2])) |res| {
                self.advanceBufferBy("\x1BOA".len);
                return res;
            }
        }

        // This may be either a M-[a-z] code, or we accidentally received an
        // escape key press and a letter key press together. There is literally
        // no way to differentiate. However the second case is less likely.
        switch (self.bytes[1]) {
            0x1B => {},
            inline '\t', '\r', '\n' => |tag| {
                defer self.advanceBufferBy("\x1Ba".len);
                return .{
                    .content = if (tag == '\t') .tab else .enter,
                    .mod_alt = true,
                };
            },
            else => if (ascii.isASCII(self.bytes[1])) {
                defer self.advanceBufferBy("\x1Ba".len);

                if (ascii.isControl(self.bytes[1]))
                    return .{
                        .content = .{ .codepoint = self.bytes[1] + 0x40 },
                        .mod_alt = true,
                        .mod_ctrl = true,
                    };
                return .{ .content = .{ .codepoint = self.bytes[1] }, .mod_alt = true };
            },
        }

        // If this point is reached, this is not an escape sequence, at least
        // not one that follows any common standard I am aware of. So let's just
        // pretend this is an escape key press and then treat all following
        // bytes separately.
        defer self.advanceBufferBy(1);
        return .{ .content = .escape };
    }

    fn singleLetterEscapeSequence(self: *Self) Input {
        const ev = singleLetterSpecialInput(self.bytes[2]) orelse {
            // Oh, turns out this is not an escape sequence. Well
            // this is awkward... Let's hope / pretend that the next
            // few bytes can be interpreted on their own. Well, it
            // might actually be an escape sequence after all, just
            // one that we don't know yet. Would be pretty nice to
            // just skip it. But we have literally no idea how long
            // this sequence is supposed to be, so it's safer to
            // just treat it as separate content pressed.
            self.advanceBufferBy(1);
            return .{ .content = .escape };
        };
        self.advanceBufferBy("\x1B[A".len);
        return ev;
    }

    fn singleLetterSpecialInput(byte: u8) ?Input {
        return .{
            .content = switch (byte) {
                'A' => .arrow_up,
                'B' => .arrow_down,
                'C' => .arrow_right,
                'D' => .arrow_left,
                'E' => .begin,
                'F' => .end,
                'H' => .home,
                'P' => .{ .function = 1 },
                'Q' => .{ .function = 2 },
                'R' => .{ .function = 3 },
                'S' => .{ .function = 4 },
                else => return null,
            },
        };
    }

    fn numericEscapeSequence(self: *Self) ?Input {
        // When this function is called, we already know that:
        // 1) the sequence starts with '\x1B[' (well... duh)
        // 2) self.bytes[3] is an ascii numeric caracter
        if (self.bytes.len > 3) {
            for (self.bytes[3..], 0..) |byte, i| {
                if (!ascii.isDigit(byte)) {
                    const first_num_bytes = self.bytes[2 .. i + 3];
                    switch (byte) {
                        '~' => return self.numericTildeEscapeSequence(first_num_bytes, null),
                        'u' => return self.kittyEscapeSequence(first_num_bytes, null),
                        ';' => return self.doubleNumericEscapeSequence(first_num_bytes),
                        else => break, // Unexpected, but not impossible.
                    }
                }
            }
        }

        return null;
    }

    fn legacyMouseEscapeSequence(self: *Self) Input {
        // This parses legacy mouse sequences like "\x1B[M" followed by three bytes.
        // https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h2-Mouse-Tracking
        // TODO also support SGR(1006) sequences.
        if (self.bytes.len < "\x1B[Mabc".len) {
            self.advanceBufferBy(1);
            return .{ .content = .unknown };
        }
        const a = self.bytes[3];
        const b = self.bytes[4];
        const c = self.bytes[5];

        // The first two bits of a encode the button.
        var ret = Input{ .content = .{ .mouse = undefined } };
        if (a & 0b01000000 > 0) {
            ret.content.mouse.button = if (a & 0b00000001 > 0) .scroll_down else .scroll_up;
        } else {
            ret.content.mouse.button = switch (a & 0b00000011) {
                0 => .btn1,
                1 => .btn2,
                2 => .btn3,
                3 => .release,
                else => unreachable,
            };
        }

        // The next three bits of a encode the modifiers.
        const SHIFT: u8 = 0b0000100;
        const META: u8 = 0b00001000;
        const CTRL: u8 = 0b00010000;
        if (a & SHIFT > 0) ret.mod_shift = true;
        if (a & META > 0) ret.mod_alt = true;
        if (a & CTRL > 0) ret.mod_ctrl = true;

        // b and c are the x and y coordinates.
        // <rant>
        //   32 is always added to the coordinates to ensure that they are
        //   printable chars. This hack dates back to X10. Yes. Also they are
        //   indexed starting at 1. In zig-spoon we (try to) enforce sane 0
        //   based indexing so that also needs to go. And yet again we uncover
        //   abominations that allow us to laugh in the face of anyone who
        //   claims backwards compatability is a good idea. This is what people
        //   have to deal with if you are too afraid to just break your shitty
        //   API and do it right. And no, bolting a new /optional/ API on top
        //   does not count.
        // </rant>
        ret.content.mouse.x = b -| (32 + 1);
        ret.content.mouse.y = c -| (32 + 1);

        self.advanceBufferBy("\x1b[Mabc".len);
        return ret;
    }

    fn doubleNumericEscapeSequence(self: *Self, first_num_bytes: []const u8) Input {
        const semicolon_index = "\x1B[".len + first_num_bytes.len;
        if (self.bytes.len > semicolon_index + 1) {
            for (self.bytes[semicolon_index + 1 ..], 0..) |byte, i| {
                if (!ascii.isDigit(byte)) {
                    const second_num_bytes = self.bytes[semicolon_index + 1 .. i + semicolon_index + 1];
                    switch (byte) {
                        '~' => return self.numericTildeEscapeSequence(first_num_bytes, second_num_bytes),
                        'u' => return self.kittyEscapeSequence(first_num_bytes, second_num_bytes),
                        'A', 'B', 'C', 'D', 'E', 'F', 'H', 'P', 'Q', 'R', 'S' => {
                            defer self.advanceBufferBy("\x1B[".len + first_num_bytes.len + ";".len + second_num_bytes.len + "A".len);
                            var ev = singleLetterSpecialInput(byte) orelse unreachable;
                            const modifiers = (fmt.parseInt(u16, second_num_bytes, 10) catch return .{ .content = .unknown }) - @as(u16, 1);
                            ev.mod_alt = (modifiers & kitty_alt) > 0;
                            ev.mod_ctrl = (modifiers & kitty_ctrl) > 0;
                            ev.mod_super = (modifiers & kitty_super) > 0;
                            ev.mod_shift = (modifiers & kitty_shift) > 0;
                            return ev;
                        },
                        else => break,
                    }
                }
            }
        }

        self.advanceBufferBy(1);
        return .{ .content = .escape };
    }

    fn numericTildeEscapeSequence(self: *Self, num: []const u8, modifiers_str: ?[]const u8) Input {
        defer {
            var len = "\x1B[~".len + num.len;
            if (modifiers_str) |_| len += modifiers_str.?.len + ";".len;
            self.advanceBufferBy(len);
        }
        const sequences = std.ComptimeStringMap(Input, .{
            .{ "1", .{ .content = .home } },
            .{ "2", .{ .content = .insert } },
            .{ "3", .{ .content = .delete } },
            .{ "4", .{ .content = .end } },
            .{ "5", .{ .content = .page_up } },
            .{ "6", .{ .content = .page_down } },
            .{ "7", .{ .content = .home } },
            .{ "8", .{ .content = .home } },
            .{ "11", .{ .content = .{ .function = 1 } } },
            .{ "12", .{ .content = .{ .function = 2 } } },
            .{ "13", .{ .content = .{ .function = 3 } } },
            .{ "14", .{ .content = .{ .function = 4 } } },
            .{ "15", .{ .content = .{ .function = 5 } } },
            .{ "17", .{ .content = .{ .function = 6 } } },
            .{ "18", .{ .content = .{ .function = 7 } } },
            .{ "19", .{ .content = .{ .function = 8 } } },
            .{ "20", .{ .content = .{ .function = 9 } } },
            .{ "21", .{ .content = .{ .function = 10 } } },
            .{ "23", .{ .content = .{ .function = 11 } } },
            .{ "24", .{ .content = .{ .function = 12 } } },
        });
        var ev = sequences.get(num) orelse return .{ .content = .unknown };
        const modifiers = if (modifiers_str) |md| ((fmt.parseInt(u16, md, 10) catch return .{ .content = .unknown }) - @as(u16, 1)) else undefined;
        ev.mod_alt = if (modifiers_str) |_| ((modifiers & kitty_alt) > 0) else false;
        ev.mod_ctrl = if (modifiers_str) |_| ((modifiers & kitty_ctrl) > 0) else false;
        ev.mod_super = if (modifiers_str) |_| ((modifiers & kitty_super) > 0) else false;
        ev.mod_shift = if (modifiers_str) |_| ((modifiers & kitty_shift) > 0) else false;
        return ev;
    }

    fn kittyEscapeSequence(self: *Self, codepoint_str: []const u8, modifiers_str: ?[]const u8) Input {
        defer {
            var len = "\x1b[".len + codepoint_str.len + "u".len;
            if (modifiers_str) |mods| len += mods.len + ";".len;
            self.advanceBufferBy(len);
        }
        const codepoint = fmt.parseInt(u21, codepoint_str, 10) catch
            return .{ .content = .unknown };

        const modifiers = if (modifiers_str) |md| ((fmt.parseInt(u16, md, 10) catch
            return .{ .content = .unknown }) - @as(u16, 1)) else undefined;

        return .{
            .content = switch (codepoint) {
                9 => .tab,
                57414, 10, 13 => .enter, // Both newline and carriage return will return a newline.
                27 => .escape,
                57359 => .scroll_lock,
                57361 => .print,
                57362 => .pause,
                57409 => .{ .codepoint = ',' },
                57410 => .{ .codepoint = '/' },
                57411 => .{ .codepoint = '*' },
                57412 => .{ .codepoint = '-' },
                57413 => .{ .codepoint = '+' },
                57415 => .{ .codepoint = '=' },
                57417 => .arrow_left,
                57418 => .arrow_right,
                57419 => .arrow_up,
                57420 => .arrow_down,
                57421 => .page_up,
                57422 => .page_down,
                57423 => .home,
                57424 => .end,
                57425 => .insert,
                57426 => .delete,
                else => .{ .codepoint = codepoint },
            },
            .mod_alt = if (modifiers_str) |_| ((modifiers & kitty_alt) > 0) else false,
            .mod_ctrl = if (modifiers_str) |_| ((modifiers & kitty_ctrl) > 0) else false,
            .mod_super = if (modifiers_str) |_| ((modifiers & kitty_super) > 0) else false,
            .mod_shift = if (modifiers_str) |_| ((modifiers & kitty_shift) > 0) else false,
        };
    }

    fn advanceBufferBy(self: *Self, amount: usize) void {
        self.bytes = if (self.bytes.len > amount) self.bytes[amount..] else &.{};
    }
};

// pub const InputParserOptions = struct {
//     terminfo: ?*TermInfo = null,
//     /// Normally terminfo input sequences are prioritized over any others. Enabling the kitty
//     /// keyboard protocol can make the sequences generated by inputs inconsistent with terminfo
//     /// definitions. This happens in Kitty itself with the F1 key. Setting this flag to true will
//     /// prioritize Kitty keyboard protocol sequences over terminfo ones.
//     has_kitty_keyboard: bool = false,
// };

const Term = @import("Term.zig");

pub fn inputParser(bytes: []const u8, term: ?*Term) InputParser {
    return .{ .bytes = bytes, .term = term };
}

test "input parser: Multiple bytes, legacy escape sequence embedded within" {
    const testing = std.testing;
    var parser = inputParser("abc\x1B[Ad", null);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'a' } }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'b' } }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'c' } }, parser.next().?);
    try testing.expectEqual(Input{ .content = .arrow_up }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'd' } }, parser.next().?);
    try testing.expect(parser.next() == null);
}

test "input parser: Newline, carriage return, enter" {
    const testing = std.testing;
    var parser = inputParser("\r\n\x1B[10;1u\x1B[13;1u", null);
    try testing.expectEqual(Input{ .content = .enter }, parser.next().?);
    try testing.expectEqual(Input{ .content = .enter }, parser.next().?);
    try testing.expectEqual(Input{ .content = .enter }, parser.next().?);
    try testing.expectEqual(Input{ .content = .enter }, parser.next().?);
    try testing.expect(parser.next() == null);
}

test "input parser: Multiple legacy escape sequences and legacy control characters" {
    const testing = std.testing;
    var parser = inputParser("\x1Ba\x1B[2~\x1B[H" ++ [_]u8{'b' & '\x1F'} ++ [_]u8{'m' & '\x1F'} ++ [_]u8{'i' & '\x1F'}, null);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'a' }, .mod_alt = true }, parser.next().?);
    try testing.expectEqual(Input{ .content = .insert }, parser.next().?);
    try testing.expectEqual(Input{ .content = .home }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'b' }, .mod_ctrl = true }, parser.next().?);
    try testing.expectEqual(Input{ .content = .enter }, parser.next().?);
    try testing.expectEqual(Input{ .content = .tab }, parser.next().?);
    try testing.expect(parser.next() == null);
}

test "input parser: kitty" {
    const testing = std.testing;
    var parser = inputParser("\x1B[1;7A\x1B[27u\x1B[2;3~", null);
    try testing.expectEqual(Input{ .content = .arrow_up, .mod_alt = true, .mod_ctrl = true }, parser.next().?);
    try testing.expectEqual(Input{ .content = .escape }, parser.next().?);
    try testing.expectEqual(Input{ .content = .insert, .mod_alt = true }, parser.next().?);
    try testing.expect(parser.next() == null);
}

test "input parser: Mixed legacy terminal utf8 and kitty u21 codepoint" {
    const testing = std.testing;
    var parser = inputParser("µ\x1B[181;1u", null);
    try testing.expectEqual(Input{ .content = .{ .codepoint = '\xB5' } }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = '\xB5' } }, parser.next().?);
    try testing.expect(parser.next() == null);
}

test "input parser: O escape sequence" {
    const testing = std.testing;
    var parser = inputParser("\x1BOP", null);
    try testing.expectEqual(Input{ .content = .{ .function = 1 } }, parser.next().?);
    try testing.expectEqual(null, parser.next());
}

test "input parser: Some random weird edge cases" {
    const testing = std.testing;
    var parser = inputParser("\x1B\x1BO\x1Ba", null);
    try testing.expectEqual(Input{ .content = .escape }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'O' }, .mod_alt = true }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'a' }, .mod_alt = true }, parser.next().?);
    try testing.expect(parser.next() == null);
}

test "input parser: Unfinished numerical escape sequence" {
    const testing = std.testing;
    var parser = inputParser("\x1B[2;", null);
    try testing.expectEqual(Input{ .content = .escape }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = '[' } }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = '2' } }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = ';' } }, parser.next().?);
    try testing.expect(parser.next() == null);
}

test "input parser: Unfinished [ single letter escape sequence" {
    const testing = std.testing;
    var parser = inputParser("\x1B[", null);
    try testing.expectEqual(Input{ .content = .{ .codepoint = '[' }, .mod_alt = true }, parser.next().?);
    try testing.expect(parser.next() == null);
}

test "input parser: Unfinished O single letter escape sequence" {
    const testing = std.testing;
    var parser = inputParser("\x1BO", null);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'O' }, .mod_alt = true }, parser.next().?);
    try testing.expect(parser.next() == null);
}

test "input parser: Just escape" {
    const testing = std.testing;
    var parser = inputParser("\x1B", null);
    try testing.expectEqual(Input{ .content = .escape }, parser.next().?);
    try testing.expect(parser.next() == null);
}

test "input parser: Unrecognized single letter escape sequences" {
    const testing = std.testing;
    var parser = inputParser("\x1BOZ\x1B[Y", null);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'O' }, .mod_alt = true }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'Z' } }, parser.next().?);
    try testing.expectEqual(Input{ .content = .escape }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = '[' } }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'Y' } }, parser.next().?);
    try testing.expect(parser.next() == null);
}

test "input parser: bad unicode" {
    const testing = std.testing;
    var parser = inputParser("\x1B[999999999999u", null);
    try testing.expectEqual(Input{ .content = .unknown }, parser.next().?);
    try testing.expect(parser.next() == null);
}

test "input parser: mixing plain ascii with multi-byte codepoints" {
    const testing = std.testing;
    var parser = inputParser("a↑b↓c", null);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'a' } }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 8593 } }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'b' } }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 8595 } }, parser.next().?);
    try testing.expectEqual(Input{ .content = .{ .codepoint = 'c' } }, parser.next().?);
    try testing.expect(parser.next() == null);
}

test "input parser: terminfo escape sequences" {
    const t = std.testing;

    const Context = struct {
        term: *Term,

        fn testInput(ctx: *@This(), input: []const u8, expected: Input) !void {
            var parser = inputParser(input, ctx.term);
            try t.expectEqual(expected, parser.next().?);
        }
    };

    const ti = try TermInfo.parse(t.allocator, @embedFile("descriptions/x/xterm"));
    defer ti.destroy(t.allocator);

    var term = Term{
        .tty = -1,
        .terminfo = ti,
    };
    try term.useTermInfoInputs(t.allocator);

    var ctx: Context = .{ .term = &term };
    try ctx.testInput("\x08", .{ .content = .backspace });
    try ctx.testInput("\x1b[3~", .{ .content = .delete });
    try ctx.testInput("\x1bOB", .{ .content = .arrow_down });
    try ctx.testInput("\x1bOP", .{ .content = .{ .function = 1 } });
    try ctx.testInput("\x1b[21~", .{ .content = .{ .function = 10 } });
    try ctx.testInput("\x1bOQ", .{ .content = .{ .function = 2 } });
    try ctx.testInput("\x1bOR", .{ .content = .{ .function = 3 } });
    try ctx.testInput("\x1bOS", .{ .content = .{ .function = 4 } });
    try ctx.testInput("\x1b[15~", .{ .content = .{ .function = 5 } });
    try ctx.testInput("\x1b[17~", .{ .content = .{ .function = 6 } });
    try ctx.testInput("\x1b[18~", .{ .content = .{ .function = 7 } });
    try ctx.testInput("\x1b[19~", .{ .content = .{ .function = 8 } });
    try ctx.testInput("\x1b[20~", .{ .content = .{ .function = 9 } });
    try ctx.testInput("\x1bOH", .{ .content = .home });
    try ctx.testInput("\x1b[2~", .{ .content = .insert });
    try ctx.testInput("\x1bOD", .{ .content = .arrow_left });
    try ctx.testInput("\x1b[6~", .{ .content = .page_down });
    try ctx.testInput("\x1b[5~", .{ .content = .page_up });
    try ctx.testInput("\x1bOC", .{ .content = .arrow_right });
    try ctx.testInput("\x1bOA", .{ .content = .arrow_up });
    try ctx.testInput("\x1b[Z", .{ .content = .tab, .mod_shift = true });
    try ctx.testInput("\x1bOF", .{ .content = .end });
    try ctx.testInput("\x1bOM", .{ .content = .enter });
    try ctx.testInput("\x1b[3;2~", .{ .content = .delete, .mod_shift = true });
    try ctx.testInput("\x1b[1;2H", .{ .content = .home, .mod_shift = true });
    try ctx.testInput("\x1b[2;2~", .{ .content = .insert, .mod_shift = true });
    try ctx.testInput("\x1b[1;2D", .{ .content = .arrow_left, .mod_shift = true });
    try ctx.testInput("\x1b[6;2~", .{ .content = .page_down, .mod_shift = true });
    try ctx.testInput("\x1b[5;2~", .{ .content = .page_up, .mod_shift = true });
    try ctx.testInput("\x1b[1;2C", .{ .content = .arrow_right, .mod_shift = true });
    try ctx.testInput("\x1b[23~", .{ .content = .{ .function = 11 } });
    try ctx.testInput("\x1b[24~", .{ .content = .{ .function = 12 } });
    try ctx.testInput("\x1b[1;2P", .{ .content = .{ .function = 13 } });
    try ctx.testInput("\x1b[1;2Q", .{ .content = .{ .function = 14 } });
    try ctx.testInput("\x1b[1;2R", .{ .content = .{ .function = 15 } });
    try ctx.testInput("\x1b[1;2S", .{ .content = .{ .function = 16 } });
    try ctx.testInput("\x1b[15;2~", .{ .content = .{ .function = 17 } });
    try ctx.testInput("\x1b[17;2~", .{ .content = .{ .function = 18 } });
    try ctx.testInput("\x1b[18;2~", .{ .content = .{ .function = 19 } });
    try ctx.testInput("\x1b[19;2~", .{ .content = .{ .function = 20 } });
    try ctx.testInput("\x1b[20;2~", .{ .content = .{ .function = 21 } });
    try ctx.testInput("\x1b[21;2~", .{ .content = .{ .function = 22 } });
    try ctx.testInput("\x1b[23;2~", .{ .content = .{ .function = 23 } });
    try ctx.testInput("\x1b[24;2~", .{ .content = .{ .function = 24 } });
    try ctx.testInput("\x1b[1;5P", .{ .content = .{ .function = 25 } });
    try ctx.testInput("\x1b[1;5Q", .{ .content = .{ .function = 26 } });
    try ctx.testInput("\x1b[1;5R", .{ .content = .{ .function = 27 } });
    try ctx.testInput("\x1b[1;5S", .{ .content = .{ .function = 28 } });
    try ctx.testInput("\x1b[15;5~", .{ .content = .{ .function = 29 } });
    try ctx.testInput("\x1b[17;5~", .{ .content = .{ .function = 30 } });
    try ctx.testInput("\x1b[18;5~", .{ .content = .{ .function = 31 } });
    try ctx.testInput("\x1b[19;5~", .{ .content = .{ .function = 32 } });
    try ctx.testInput("\x1b[20;5~", .{ .content = .{ .function = 33 } });
    try ctx.testInput("\x1b[21;5~", .{ .content = .{ .function = 34 } });
    try ctx.testInput("\x1b[23;5~", .{ .content = .{ .function = 35 } });
    try ctx.testInput("\x1b[24;5~", .{ .content = .{ .function = 36 } });
    try ctx.testInput("\x1b[1;6P", .{ .content = .{ .function = 37 } });
    try ctx.testInput("\x1b[1;6Q", .{ .content = .{ .function = 38 } });
    try ctx.testInput("\x1b[1;6R", .{ .content = .{ .function = 39 } });
    try ctx.testInput("\x1b[1;6S", .{ .content = .{ .function = 40 } });
    try ctx.testInput("\x1b[15;6~", .{ .content = .{ .function = 41 } });
    try ctx.testInput("\x1b[17;6~", .{ .content = .{ .function = 42 } });
    try ctx.testInput("\x1b[18;6~", .{ .content = .{ .function = 43 } });
    try ctx.testInput("\x1b[19;6~", .{ .content = .{ .function = 44 } });
    try ctx.testInput("\x1b[20;6~", .{ .content = .{ .function = 45 } });
    try ctx.testInput("\x1b[21;6~", .{ .content = .{ .function = 46 } });
    try ctx.testInput("\x1b[23;6~", .{ .content = .{ .function = 47 } });
    try ctx.testInput("\x1b[24;6~", .{ .content = .{ .function = 48 } });
    try ctx.testInput("\x1b[1;3P", .{ .content = .{ .function = 49 } });
    try ctx.testInput("\x1b[1;3Q", .{ .content = .{ .function = 50 } });
    try ctx.testInput("\x1b[1;3R", .{ .content = .{ .function = 51 } });
    try ctx.testInput("\x1b[1;3S", .{ .content = .{ .function = 52 } });
    try ctx.testInput("\x1b[15;3~", .{ .content = .{ .function = 53 } });
    try ctx.testInput("\x1b[17;3~", .{ .content = .{ .function = 54 } });
    try ctx.testInput("\x1b[18;3~", .{ .content = .{ .function = 55 } });
    try ctx.testInput("\x1b[19;3~", .{ .content = .{ .function = 56 } });
    try ctx.testInput("\x1b[20;3~", .{ .content = .{ .function = 57 } });
    try ctx.testInput("\x1b[21;3~", .{ .content = .{ .function = 58 } });
    try ctx.testInput("\x1b[23;3~", .{ .content = .{ .function = 59 } });
    try ctx.testInput("\x1b[24;3~", .{ .content = .{ .function = 60 } });
    try ctx.testInput("\x1b[1;4P", .{ .content = .{ .function = 61 } });
    try ctx.testInput("\x1b[1;4Q", .{ .content = .{ .function = 62 } });
    try ctx.testInput("\x1b[1;4R", .{ .content = .{ .function = 63 } });
}
