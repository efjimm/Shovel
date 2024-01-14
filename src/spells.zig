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

// These escape sequences are used when terminfo capabilities are not available.

const std = @import("std");

pub const hide_cursor = "\x1B[?25l";
pub const show_cursor = "\x1B[?25h";
pub const move_cursor_fmt = "\x1B[%i%p1%d;%p2%dH";

pub const CursorShape = enum(u3) {
    unknown,
    blinking_block = 1,
    block = 2,
    blinking_underline = 3,
    underline = 4,
    blinking_bar = 5,
    bar = 6,
};

pub const change_cursor = "\x1B[%d q";

// https://sw.kovidgoyal.net/kitty/keyboard-protocol/
// This enables an alternative input mode, that makes it possible, among
// others, to unambigously identify the escape key without waiting for
// a timer to run out. And implementing it can be implemented in a backwards
// compatible manner, so that the same code can handle kitty-enabled
// terminals as well as sad terminals.
//
// Must be enabled after entering the alt screen and disabled before leaving
// it.
pub const enable_kitty_keyboard = "\x1B[>1u";
pub const disable_kitty_keyboard = "\x1B[<u";

pub const save_cursor_position = "\x1B[s";
pub const save_screen = "\x1B[?47h";
pub const enter_alt_buffer = "\x1B[?1049h";

pub const leave_alt_buffer = "\x1B[?1049l";
pub const restore_screen = "\x1B[?47l";
pub const restore_cursor_position = "\x1B[u";

pub const clear_to_bot = "\x1B[0J";
pub const clear_to_top = "\x1B[1J";
pub const clear = "\x1B[2J";

pub const clear_to_eol = "\x1B[0K";
pub const clear_to_bol = "\x1B[1K";
pub const clear_line = "\x1B[2K";

pub const reset_attributes = "\x1B[0m";

pub const overwrite_mode = "\x1B[4l";

// Per https://espterm.github.io/docs/VT100%20escape%20codes.html
pub const enable_auto_wrap = "\x1B[?7h";
pub const reset_auto_wrap = "\x1B[?7l";
pub const reset_auto_repeat = "\x1B[?8l";
pub const reset_auto_interlace = "\x1B[?9l";

// 1000: just button tracking.
// 1003: all events, including movement.
//
// This enables standard button tracking. This is limited to maximum XY values
// of 255 - 32 = 223. If anyone complains, we can switch to SGR (1006) encoding,
// however since that is more annoying to parse I decided to take the easy route
// for now. Also this does not provide movement events. Can be enabled with 1003,
// but I don't see that being useful, so it's also not included right not. Let's
// see if anyone complains.
pub const enable_mouse_tracking = "\x1B[?1000h";
pub const disable_mouse_tracking = "\x1B[?1000l";
