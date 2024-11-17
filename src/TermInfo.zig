// TODO: Investigate using terminfo for mouse support
const std = @import("std");
const log = @import("log.zig");
const getenv = std.posix.getenv;
const sep = std.fs.path.sep;
const readInt = std.mem.readInt;
const native_endian = @import("builtin").cpu.arch.endian();
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const util = @import("util.zig");
const input = @import("input.zig");

// Maximum length of terminfo definition files
pub const max_file_length = 32768;
const Self = @This();

input_map: ?input.InputMap = null,

names: [:0]const u8 = "",
string_table: []const u8 = "",
ext_string_table: []u8 = "",

flags: std.EnumArray(Flag, bool) = std.EnumArray(Flag, bool).initFill(false),
numbers: std.EnumArray(Number, i32) = std.EnumArray(Number, i32).initFill(-1),
strings: std.EnumArray(String, i16) = std.EnumArray(String, i16).initFill(-1),

ext_flags: std.StringHashMapUnmanaged(void) = .{},
ext_nums: std.StringHashMapUnmanaged(u31) = .{},
ext_strs: std.StringHashMapUnmanaged(u15) = .{},

fn searchTermInfoDirectory(term: []const u8) ?std.fs.File {
    std.debug.assert(term.len > 0);

    const path = getenv("TERMINFO") orelse return null;
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    var list = std.ArrayListUnmanaged(u8).initBuffer(&buf);
    list.appendSliceAssumeCapacity(path);
    list.appendSliceAssumeCapacity(&.{ sep, term[0], sep });
    list.appendSliceAssumeCapacity(term);
    if (std.fs.openFileAbsolute(list.items, .{})) |file| {
        log.info("Found terminfo description at '{s}'", .{list.items});
        return file;
    } else |err| {
        log.info("{} when opening file '{s}'", .{ err, list.items });
        return null;
    }
}

fn searchHomeDirectory(term: []const u8) ?std.fs.File {
    std.debug.assert(term.len > 0);

    const home = getenv("HOME") orelse return null;
    const temp: []const u8 = .{sep} ++ ".terminfo" ++ .{ sep, term[0], sep };
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    var list = std.ArrayListUnmanaged(u8).initBuffer(&buf);
    list.appendSliceAssumeCapacity(home);
    list.appendSliceAssumeCapacity(temp);
    list.appendSliceAssumeCapacity(term);
    if (std.fs.openFileAbsolute(list.items, .{})) |file| {
        log.info("Found terminfo description at '{s}'", .{list.items});
        return file;
    } else |err| {
        log.info("Skipping file '{s}' ({})", .{ list.items, err });
        return null;
    }
}

fn searchTermInfoDirs(term: []const u8) ?std.fs.File {
    const dirs = getenv("TERMINFO_DIRS") orelse return null;
    var iter = std.mem.splitScalar(u8, dirs, ':');

    var buf: [std.fs.max_path_bytes]u8 = undefined;
    var list = std.ArrayListUnmanaged(u8).initBuffer(&buf);

    while (iter.next()) |path| {
        if (path.len == 0)
            return searchDefaultDirs(term) orelse continue;

        list.clearRetainingCapacity();
        list.appendSliceAssumeCapacity(path);
        list.appendSliceAssumeCapacity(&.{ sep, term[0], sep });
        list.appendSliceAssumeCapacity(term);
        if (std.fs.openFileAbsolute(list.items, .{})) |file| {
            log.info("Found terminfo decription at '{s}'", .{list.items});
            return file;
        } else |err| {
            log.info("Skipping file '{s}' ({})", .{ list.items, err });
            continue;
        }
    }

    return null;
}

fn searchDefaultDirs(term: []const u8) ?std.fs.File {
    std.debug.assert(term.len > 0);

    const dirs = [_][]const u8{
        "/usr/share/terminfo",
        "/usr/local/share/terminfo",
        "/lib/terminfo",
        "/usr/share/lib/terminfo",
        "/usr/local/share/lib/terminfo",
    };

    var buf: [std.fs.max_path_bytes]u8 = undefined;
    var list = std.ArrayListUnmanaged(u8).initBuffer(&buf);
    for (dirs) |dir| {
        list.clearRetainingCapacity();
        list.appendSliceAssumeCapacity(dir);
        list.appendSliceAssumeCapacity(&.{ sep, term[0], sep });
        list.appendSliceAssumeCapacity(term);
        if (std.fs.openFileAbsolute(list.items, .{})) |file| {
            log.info("Found terminfo decription at '{s}'", .{list.items});
            return file;
        } else |err| {
            log.info("Skipping file '{s}' ({})", .{ list.items, err });
            continue;
        }
    }

    return null;
}

pub const FileIter = struct {
    term: []const u8,
    state: State = .terminfo_var,

    pub const State = enum(u3) {
        terminfo_var,
        home,
        terminfo_dirs,
        system,
        done,
    };

    pub fn next(iter: *FileIter) ?std.fs.File {
        if (iter.term.len == 0) return null;

        const old_state = iter.state;
        iter.state = @enumFromInt(@min(@intFromEnum(State.done), @intFromEnum(iter.state) + 1));
        return switch (old_state) {
            .terminfo_var => searchTermInfoDirectory(iter.term) orelse iter.next(),
            .home => searchHomeDirectory(iter.term) orelse iter.next(),
            .terminfo_dirs => searchTermInfoDirs(iter.term) orelse iter.next(),
            .system => searchDefaultDirs(iter.term) orelse iter.next(),
            .done => null,
        };
    }
};

pub fn openTermInfoFile(term: []const u8) ?std.fs.File {
    if (term.len == 0) return null;

    // Searches for terminfo files in the same places as ncurses.
    // From man (5) terminfo:

    // The ncurses library searches for terminal descriptions in several
    // places.  It uses only the first description found.  The library has a
    // compiled-in list of places to search which can be overridden by
    // environment variables.  Before starting to search, ncurses eliminates
    // duplicates in its search list.

    // •   If the environment variable TERMINFO is set, it is interpreted as
    //     the pathname of a directory containing the compiled description you
    //     are working on.  Only that directory is searched.

    // •   If TERMINFO is not set, ncurses will instead look in the directory
    //     $HOME/.terminfo for a compiled description.

    // •   Next, if the environment variable TERMINFO_DIRS is set, ncurses
    //     will interpret the contents of that variable as a list of colon-
    //     separated directories (or database files) to be searched.

    //     An empty directory name (i.e., if the variable begins or ends with
    //     a colon, or contains adjacent colons) is interpreted as the system
    //     location /usr/share/terminfo.

    // •   Finally, ncurses searches these compiled-in locations:

    //     •   a list of directories (no default value), and

    //     •   the system terminfo directory, /usr/share/terminfo (the
    //         compiled-in default).

    return searchTermInfoDirectory(term) orelse
        searchHomeDirectory(term) orelse
        searchTermInfoDirs(term) orelse
        searchDefaultDirs(term);
}

fn readNonNegative(src: *const [2]u8) ParseError!u15 {
    return std.math.cast(u15, readInt(i16, src, .little)) orelse error.InvalidFormat;
}

pub fn destroy(self: *Self, allocator: Allocator) void {
    if (self.input_map) |*map| map.deinit(allocator);
    allocator.free(self.names);
    allocator.free(self.string_table);
    allocator.free(self.ext_string_table);
    self.ext_flags.deinit(allocator);
    self.ext_nums.deinit(allocator);
    self.ext_strs.deinit(allocator);
    allocator.destroy(self);
}

const keys = [_]struct { []const u8, input.Input }{
    .{ "backspace", .{ .content = .backspace } },
    .{ "beg", .{ .content = .begin } },
    .{ "command", .{ .content = .command } },
    .{ "dc", .{ .content = .delete } },
    .{ "down", .{ .content = .arrow_down } },
    .{ "end", .{ .content = .end } },
    .{ "enter", .{ .content = .enter } },
    .{ "home", .{ .content = .home } },
    .{ "ic", .{ .content = .insert } },
    .{ "left", .{ .content = .arrow_left } },
    .{ "next", .{ .content = .page_down } },
    .{ "npage", .{ .content = .page_down } },
    .{ "ppage", .{ .content = .page_up } },
    .{ "previous", .{ .content = .page_up } },
    .{ "print", .{ .content = .print } },
    .{ "right", .{ .content = .arrow_right } },
    .{ "up", .{ .content = .arrow_up } },

    .{ "sbeg", .{ .content = .begin, .mod_shift = true } },
    .{ "btab", .{ .content = .tab, .mod_shift = true } },
    .{ "scommand", .{ .content = .command, .mod_shift = true } },
    .{ "sdc", .{ .content = .delete, .mod_shift = true } },
    .{ "send", .{ .content = .end, .mod_shift = true } },
    .{ "shome", .{ .content = .home, .mod_shift = true } },
    .{ "sic", .{ .content = .insert, .mod_shift = true } },
    .{ "sleft", .{ .content = .arrow_left, .mod_shift = true } },
    .{ "sright", .{ .content = .arrow_right, .mod_shift = true } },
    .{ "sprint", .{ .content = .print, .mod_shift = true } },

    // Scroll forward/scroll backward are mapped to shift down/up
    .{ "sf", .{ .content = .arrow_down, .mod_shift = true } },
    .{ "sr", .{ .content = .arrow_up, .mod_shift = true } },

    // termkey defines these to be the same as pageup/pagedown
    .{ "next", .{ .content = .page_down } },
    .{ "previous", .{ .content = .page_up } },
    .{ "snext", .{ .content = .page_down, .mod_shift = true } },
    .{ "sprevious", .{ .content = .page_up, .mod_shift = true } },

    // These keys don't exist on modern keyboards
    // .{ "clear", .clear },
    // .{ "cancel", .cancel },
    // .{ "close", .close },
    // .{ "copy", .copy },
    // .{ "exit", .exit },
    // .{ "find", .find },
    // .{ "help", .help },
    // .{ "mark", .mark },
    // .{ "message", .message },
    // .{ "move", .move },
    // .{ "open", .open },
    // .{ "options", .options },
    // .{ "redo", .redo },
    // .{ "reference", .reference },
    // .{ "refresh", .refresh },
    // .{ "replace", .replace },
    // .{ "restart", .restart },
    // .{ "resume", .@"resume" },
    // .{ "save", .save },
    // .{ "select", .select },
    // .{ "suspend", .@"suspend" },
    // .{ "undo", .undo },

    // .{ "scancel", .shift_cancel },
    // .{ "scopy", .shift_copy },
    // .{ "screate", .shift_create },
    // .{ "sdl", .shift_delete_line },
    // .{ "seol", .shift_clear_to_eol },
    // .{ "sexit", .shift_exit },
    // .{ "sfind", .shift_find },
    // .{ "shelp", .shift_help },
    // .{ "smessage", .shift_message },
    // .{ "smove", .shift_move },
    // .{ "soptions", .shift_options },
    // .{ "sredo", .shift_redo },
    // .{ "sreplace", .shift_replace },
    // .{ "srsume", .shift_resume },
    // .{ "ssave", .shift_save },
    // .{ "ssuspend", .shift_suspend },
    // .{ "sundo", .shift_undo },
};

// TODO: Test this
pub fn createInputMap(
    self: *Self,
    allocator: Allocator,
) !input.InputMap {
    const print = std.fmt.comptimePrint;

    var map = input.InputMap.init();
    errdefer map.deinit(allocator);

    if (self.getStringCapability(.delete_character)) |str| {
        map.put(allocator, str, .{ .content = .delete }) catch |err| switch (err) {
            error.OutOfMemory => |e| return e,
            error.IsPrefix => {},
        };
    }

    if (self.getStringCapability(.cursor_left)) |str| {
        map.put(allocator, str, .{ .content = .backspace }) catch |err| switch (err) {
            error.OutOfMemory => |e| return e,
            error.IsPrefix => {},
        };
    }

    inline for (keys) |k| {
        const key_name, const in = k;
        const name = comptime print("key_{s}", .{key_name});
        const tag = comptime std.meta.stringToEnum(String, name).?;
        if (self.getStringCapability(tag)) |str| {
            map.put(allocator, str, in) catch |err| switch (err) {
                error.OutOfMemory => |e| return e,
                error.IsPrefix => {},
            };
        }
    }

    inline for (0..64) |i| {
        const name = comptime print("key_f{d}", .{i});
        const tag = comptime std.meta.stringToEnum(String, name).?;
        if (self.getStringCapability(tag)) |str| {
            map.put(allocator, str, .{
                .content = .{ .function = i },
            }) catch |err| switch (err) {
                error.OutOfMemory => |e| return e,
                error.IsPrefix => {},
            };
        }
    }

    // TODO: Handle `key_mouse`?

    self.input_map = map;
    return map;
}

pub const ParseError = error{
    InvalidFormat,
} || Allocator.Error;

fn readU15(reader: anytype) !u15 {
    const int = reader.readInt(i16, .little) catch return error.InvalidFormat;
    return std.math.cast(u15, int) orelse error.InvalidFormat;
}

pub fn parse(allocator: Allocator, bytes: []const u8) ParseError!*Self {
    return parseInternal(allocator, bytes) catch |err| switch (err) {
        error.EndOfStream => error.InvalidFormat,
        else => |e| e,
    };
}

const Format = enum(i16) {
    legacy = 0o0432,
    extended = 0o1036,
};

fn parseInternal(allocator: Allocator, bytes: []const u8) !*Self {
    var fbs = std.io.fixedBufferStream(bytes);
    const reader = fbs.reader();

    const ret = try allocator.create(Self);
    errdefer allocator.destroy(ret);
    ret.* = .{};

    const format = std.meta.intToEnum(Format, try readU15(reader)) catch
        return error.InvalidFormat;
    const names_len = try readU15(reader);
    const flags_len = try readU15(reader);
    const nums_len = try readU15(reader);
    const strings_len = try readU15(reader);
    const string_table_len = try readU15(reader);

    const names = bytes[try fbs.getPos()..][0 .. names_len - 1];
    ret.names = try allocator.dupeZ(u8, names);
    errdefer allocator.free(ret.names);

    try reader.skipBytes(names_len, .{});

    const flags_dest = &ret.flags.values;
    for (flags_dest[0..flags_len]) |*flag| {
        // Convert byte to bool
        flag.* = 0 != try reader.readByte();
    }

    try reader.skipBytes((names_len + flags_len) % 2, .{});
    assert(try fbs.getPos() % 2 == 0);

    switch (format) {
        inline else => |tag| {
            const T = switch (tag) {
                .legacy => i16,
                .extended => i32,
            };
            const nums_dest = &ret.numbers.values;
            for (nums_dest[0..nums_len]) |*num| {
                num.* = try reader.readInt(T, .little);
            }
        },
    }

    const strings_dest = &ret.strings.values;
    for (strings_dest[0..strings_len]) |*string_index| {
        string_index.* = try reader.readInt(i16, .little);
    }

    ret.string_table = try allocator.dupe(u8, bytes[try fbs.getPos()..][0..string_table_len]);
    errdefer allocator.free(ret.string_table);

    try reader.skipBytes(string_table_len, .{});

    if (try fbs.getEndPos() - try fbs.getPos() < 10)
        return ret;

    // Have more bytes, continue parsing extended format

    try reader.skipBytes(string_table_len % 2, .{});

    const ext_flags_len = try readU15(reader);
    const ext_nums_len = try readU15(reader);
    const ext_strings_len = try readU15(reader);
    const ext_string_table_count = try readU15(reader);
    const ext_string_table_len = try readU15(reader);

    if (ext_string_table_count != ext_flags_len + ext_nums_len + ext_strings_len * 2)
        return error.InvalidFormat;

    try ret.ext_flags.ensureTotalCapacity(allocator, ext_flags_len);
    errdefer ret.ext_flags.deinit(allocator);

    try ret.ext_nums.ensureTotalCapacity(allocator, ext_nums_len);
    errdefer ret.ext_nums.deinit(allocator);

    try ret.ext_strs.ensureTotalCapacity(allocator, ext_strings_len);
    errdefer ret.ext_strs.deinit(allocator);

    ret.ext_string_table = try allocator.alloc(u8, ext_string_table_len);
    errdefer allocator.free(ret.ext_string_table);

    const num_size: u8 = switch (format) {
        .legacy => 2,
        .extended => 4,
    };

    const ext_flags: []const bool = @ptrCast(bytes[try fbs.getPos()..][0..ext_flags_len]);
    try reader.skipBytes(ext_flags_len + ext_flags_len % 2, .{});

    const ext_nums_bytes = bytes[try fbs.getPos()..][0 .. ext_nums_len * num_size];
    try reader.skipBytes(ext_nums_len * num_size, .{});

    const ext_strings_bytes = bytes[try fbs.getPos()..][0 .. ext_strings_len * 2];
    try reader.skipBytes(ext_strings_len * 2, .{});

    // Skip name indices
    try reader.skipBytes(2 * (ext_flags_len + ext_nums_len + ext_strings_len), .{});

    @memcpy(ret.ext_string_table, bytes[try fbs.getPos()..][0..ext_string_table_len]);

    var strings_iter = std.mem.splitScalar(u8, ret.ext_string_table, 0);
    for (std.mem.bytesAsSlice(i16, ext_strings_bytes)) |string_index| {
        if (string_index >= 0) {
            _ = strings_iter.next();
        }
    }

    for (ext_flags) |_| {
        const name = strings_iter.next() orelse break;
        ret.ext_flags.putAssumeCapacity(name, {});
    }

    switch (format) {
        inline else => |tag| {
            const S, const T = switch (tag) {
                .legacy => .{ i16, u15 },
                .extended => .{ i32, u31 },
            };

            for (std.mem.bytesAsSlice(S, ext_nums_bytes)) |num| {
                const name = strings_iter.next() orelse break;
                if (std.math.cast(T, num)) |n| {
                    ret.ext_nums.putAssumeCapacity(name, n);
                }
            }
        },
    }

    for (std.mem.bytesAsSlice(i16, ext_strings_bytes)) |string_index| {
        const name = strings_iter.next() orelse break;
        if (std.math.cast(u15, string_index)) |index| {
            ret.ext_strs.putAssumeCapacity(name, index);
        }
    }

    return ret;
}

pub fn getFlagCapability(self: *const Self, tag: Flag) bool {
    return self.flags.get(tag);
}

pub fn getNumberCapability(self: *const Self, tag: Number) ?u31 {
    const v = self.numbers.get(tag);
    return if (v < 0) null else @intCast(v);
}

pub fn getStringCapability(self: *const Self, tag: String) ?[:0]const u8 {
    const v = std.math.cast(u16, self.strings.get(tag)) orelse return null;
    return std.mem.span(@as([*:0]const u8, @ptrCast(self.string_table[v..].ptr)));
}

pub fn getExtendedFlag(self: *const Self, name: []const u8) bool {
    return self.ext_flags.contains(name);
}

pub fn getExtendedNumber(self: *const Self, name: []const u8) ?u31 {
    return self.ext_nums.get(name);
}

pub fn getExtendedString(self: *const Self, name: []const u8) ?[:0]const u8 {
    if (self.ext_strs.get(name)) |index| {
        return std.mem.span(@as([*:0]const u8, @ptrCast(self.ext_string_table[index..].ptr)));
    }
    return null;
}

pub const Flag = enum {
    auto_left_margin,
    auto_right_margin,
    no_esc_ctlc,
    ceol_standout_glitch,
    eat_newline_glitch,
    erase_overstrike,
    generic_type,
    hard_copy,
    has_meta_key,
    has_status_line,
    insert_null_glitch,
    memory_above,
    memory_below,
    move_insert_mode,
    move_standout_mode,
    over_strike,
    status_line_esc_ok,
    dest_tabs_magic_smso,
    tilde_glitch,
    transparent_underline,
    xon_xoff,
    needs_xon_xoff,
    prtr_silent,
    hard_cursor,
    non_rev_rmcup,
    no_pad_char,
    non_dest_scroll_region,
    can_change,
    back_color_erase,
    hue_lightness_saturation,
    col_addr_glitch,
    cr_cancels_micro_mode,
    has_print_wheel,
    row_addr_glitch,
    semi_auto_right_margin,
    cpi_changes_res,
    lpi_changes_res,

    backspaces_with_bs,
    crt_no_scrolling,
    no_correctly_working_cr,
    gnu_has_meta_key,
    linefeed_is_newline,
    has_hardware_tabs,
    return_does_clr_eol,
};

pub const Number = enum {
    columns,
    init_tabs,
    lines,
    lines_of_memory,
    magic_cookie_glitch,
    padding_baud_rate,
    virtual_terminal,
    width_status_line,
    num_labels,
    label_height,
    label_width,
    max_attributes,
    maximum_windows,
    max_colors,
    max_pairs,
    no_color_video,
    buffer_capacity,
    dot_vert_spacing,
    dot_horz_spacing,
    max_micro_address,
    max_micro_jump,
    micro_col_size,
    micro_line_size,
    number_of_pins,
    output_res_char,
    output_res_line,
    output_res_horz_inch,
    output_res_vert_inch,
    print_rate,
    wide_char_size,
    buttons,
    bit_image_entwining,
    bit_image_type,

    magic_cookie_glitch_ul,
    carriage_return_delay,
    new_line_delay,
    backspace_delay,
    horizontal_tab_delay,
    number_of_function_keys,
};

pub const String = enum {
    back_tab,
    bell,
    carriage_return,
    change_scroll_region,
    clear_all_tabs,
    clear_screen,
    clr_eol,
    clr_eos,
    column_address,
    command_character,
    cursor_address,
    cursor_down,
    cursor_home,
    cursor_invisible,
    cursor_left,
    cursor_mem_address,
    cursor_normal,
    cursor_right,
    cursor_to_ll,
    cursor_up,
    cursor_visible,
    delete_character,
    delete_line,
    dis_status_line,
    down_half_line,
    enter_alt_charset_mode,
    enter_blink_mode,
    enter_bold_mode,
    enter_ca_mode,
    enter_delete_mode,
    enter_dim_mode,
    enter_insert_mode,
    enter_secure_mode,
    enter_protected_mode,
    enter_reverse_mode,
    enter_standout_mode,
    enter_underline_mode,
    erase_chars,
    exit_alt_charset_mode,
    exit_attribute_mode,
    exit_ca_mode,
    exit_delete_mode,
    exit_insert_mode,
    exit_standout_mode,
    exit_underline_mode,
    flash_screen,
    form_feed,
    from_status_line,
    init_1string,
    init_2string,
    init_3string,
    init_file,
    insert_character,
    insert_line,
    insert_padding,
    key_backspace,
    key_catab,
    key_clear,
    key_ctab,
    key_dc,
    key_dl,
    key_down,
    key_eic,
    key_eol,
    key_eos,
    key_f0,
    key_f1,
    key_f10,
    key_f2,
    key_f3,
    key_f4,
    key_f5,
    key_f6,
    key_f7,
    key_f8,
    key_f9,
    key_home,
    key_ic,
    key_il,
    key_left,
    key_ll,
    key_npage,
    key_ppage,
    key_right,
    key_sf,
    key_sr,
    key_stab,
    key_up,
    keypad_local,
    keypad_xmit,
    lab_f0,
    lab_f1,
    lab_f10,
    lab_f2,
    lab_f3,
    lab_f4,
    lab_f5,
    lab_f6,
    lab_f7,
    lab_f8,
    lab_f9,
    meta_off,
    meta_on,
    newline,
    pad_char,
    parm_dch,
    parm_delete_line,
    parm_down_cursor,
    parm_ich,
    parm_index,
    parm_insert_line,
    parm_left_cursor,
    parm_right_cursor,
    parm_rindex,
    parm_up_cursor,
    pkey_key,
    pkey_local,
    pkey_xmit,
    print_screen,
    prtr_off,
    prtr_on,
    repeat_char,
    reset_1string,
    reset_2string,
    reset_3string,
    reset_file,
    restore_cursor,
    row_address,
    save_cursor,
    scroll_forward,
    scroll_reverse,
    set_attributes,
    set_tab,
    set_window,
    tab,
    to_status_line,
    underline_char,
    up_half_line,
    init_prog,
    key_a1,
    key_a3,
    key_b2,
    key_c1,
    key_c3,
    prtr_non,
    char_padding,
    acs_chars,
    plab_norm,
    key_btab,
    enter_xon_mode,
    exit_xon_mode,
    enter_am_mode,
    exit_am_mode,
    xon_character,
    xoff_character,
    ena_acs,
    label_on,
    label_off,
    key_beg,
    key_cancel,
    key_close,
    key_command,
    key_copy,
    key_create,
    key_end,
    key_enter,
    key_exit,
    key_find,
    key_help,
    key_mark,
    key_message,
    key_move,
    key_next,
    key_open,
    key_options,
    key_previous,
    key_print,
    key_redo,
    key_reference,
    key_refresh,
    key_replace,
    key_restart,
    key_resume,
    key_save,
    key_suspend,
    key_undo,
    key_sbeg,
    key_scancel,
    key_scommand,
    key_scopy,
    key_screate,
    key_sdc,
    key_sdl,
    key_select,
    key_send,
    key_seol,
    key_sexit,
    key_sfind,
    key_shelp,
    key_shome,
    key_sic,
    key_sleft,
    key_smessage,
    key_smove,
    key_snext,
    key_soptions,
    key_sprevious,
    key_sprint,
    key_sredo,
    key_sreplace,
    key_sright,
    key_srsume,
    key_ssave,
    key_ssuspend,
    key_sundo,
    req_for_input,
    key_f11,
    key_f12,
    key_f13,
    key_f14,
    key_f15,
    key_f16,
    key_f17,
    key_f18,
    key_f19,
    key_f20,
    key_f21,
    key_f22,
    key_f23,
    key_f24,
    key_f25,
    key_f26,
    key_f27,
    key_f28,
    key_f29,
    key_f30,
    key_f31,
    key_f32,
    key_f33,
    key_f34,
    key_f35,
    key_f36,
    key_f37,
    key_f38,
    key_f39,
    key_f40,
    key_f41,
    key_f42,
    key_f43,
    key_f44,
    key_f45,
    key_f46,
    key_f47,
    key_f48,
    key_f49,
    key_f50,
    key_f51,
    key_f52,
    key_f53,
    key_f54,
    key_f55,
    key_f56,
    key_f57,
    key_f58,
    key_f59,
    key_f60,
    key_f61,
    key_f62,
    key_f63,
    clr_bol,
    clear_margins,
    set_left_margin,
    set_right_margin,
    label_format,
    set_clock,
    display_clock,
    remove_clock,
    create_window,
    goto_window,
    hangup,
    dial_phone,
    quick_dial,
    tone,
    pulse,
    flash_hook,
    fixed_pause,
    wait_tone,
    user0,
    user1,
    user2,
    user3,
    user4,
    user5,
    user6,
    user7,
    user8,
    user9,
    orig_pair,
    orig_colors,
    initialize_color,
    initialize_pair,
    set_color_pair,
    set_foreground,
    set_background,
    change_char_pitch,
    change_line_pitch,
    change_res_horz,
    change_res_vert,
    define_char,
    enter_doublewide_mode,
    enter_draft_quality,
    enter_italics_mode,
    enter_leftward_mode,
    enter_micro_mode,
    enter_near_letter_quality,
    enter_normal_quality,
    enter_shadow_mode,
    enter_subscript_mode,
    enter_superscript_mode,
    enter_upward_mode,
    exit_doublewide_mode,
    exit_italics_mode,
    exit_leftward_mode,
    exit_micro_mode,
    exit_shadow_mode,
    exit_subscript_mode,
    exit_superscript_mode,
    exit_upward_mode,
    micro_column_address,
    micro_down,
    micro_left,
    micro_right,
    micro_row_address,
    micro_up,
    order_of_pins,
    parm_down_micro,
    parm_left_micro,
    parm_right_micro,
    parm_up_micro,
    select_char_set,
    set_bottom_margin,
    set_bottom_margin_parm,
    set_left_margin_parm,
    set_right_margin_parm,
    set_top_margin,
    set_top_margin_parm,
    start_bit_image,
    start_char_set_def,
    stop_bit_image,
    stop_char_set_def,
    subscript_characters,
    superscript_characters,
    these_cause_cr,
    zero_motion,
    char_set_names,
    key_mouse,
    mouse_info,
    req_mouse_pos,
    get_mouse,
    set_a_foreground,
    set_a_background,
    pkey_plab,
    device_type,
    code_set_init,
    set0_des_seq,
    set1_des_seq,
    set2_des_seq,
    set3_des_seq,
    set_lr_margin,
    set_tb_margin,
    bit_image_repeat,
    bit_image_newline,
    bit_image_carriage_return,
    color_names,
    define_bit_image_region,
    end_bit_image_region,
    set_color_band,
    set_page_length,
    display_pc_char,
    enter_pc_charset_mode,
    exit_pc_charset_mode,
    enter_scancode_mode,
    exit_scancode_mode,
    pc_term_options,
    scancode_escape,
    alt_scancode_esc,
    enter_horizontal_hl_mode,
    enter_left_hl_mode,
    enter_low_hl_mode,
    enter_right_hl_mode,
    enter_top_hl_mode,
    enter_vertical_hl_mode,
    set_a_attributes,
    set_pglen_inch,

    termcap_init2,
    termcap_reset,
    linefeed_if_not_lf,
    backspace_if_not_bs,
    other_non_function_keys,
    arrow_key_map,
    acs_ulcorner,
    acs_llcorner,
    acs_urcorner,
    acs_lrcorner,
    acs_ltee,
    acs_rtee,
    acs_btee,
    acs_ttee,
    acs_hline,
    acs_vline,
    acs_plus,
    memory_lock,
    memory_unlock,
    box_chars_1,
};

const Parameter = union(enum) {
    number: i32,
    string: []const u8,
};

fn createParam(arg: anytype) Parameter {
    const T = @TypeOf(arg);
    return if (comptime util.isZigString(T))
        .{ .string = arg }
    else if (@TypeOf(arg) == bool)
        .{ .number = @intFromBool(arg) }
    else
        .{ .number = arg };
}

pub const ParamSequenceError = error{
    UnexpectedEndOfInput,
    InvalidSpecifier,
    UnpoppedStack,
    PoppedEmptyStack,
    NotEnoughParameters,
    UnexpectedConditionTerminator,
    MissingConditionTerminator,
    Overflow,
};

/// Validates that the given parameterized escape sequence is correct.
pub fn validateParamSequence(sequence: []const u8, param_count: usize) ParamSequenceError!void {
    const sub = std.math.sub;

    const s = sequence;
    var i: usize = 0;
    var stack_len: usize = 0;
    var nesting: usize = 0;
    while (i < s.len) : (i += 1) {
        if (s[i] != '%') continue;
        i += 1;
        if (i >= s.len) return error.UnexpectedEndOfInput;
        switch (s[i]) {
            ':', '#', ' ', '.', '0'...'9', 'd', 'o', 'x', 'X', 's' => |c| {
                if (c == ':') i += 1;
                if (i >= s.len) return error.UnexpectedEndOfInput;

                while (i < s.len) : (i += 1) {
                    switch (s[i]) {
                        '-', '+', '#', ' ' => {},
                        else => break,
                    }
                } else return error.UnexpectedEndOfInput;

                // Collect width
                const width_start = i;
                while (i < s.len) : (i += 1) {
                    switch (s[i]) {
                        '0'...'9' => {},
                        else => break,
                    }
                } else return error.UnexpectedEndOfInput;

                if (i > width_start) {
                    _ = std.fmt.parseUnsigned(u31, s[width_start..i], 10) catch |err| switch (err) {
                        error.InvalidCharacter => unreachable,
                        else => |e| return e,
                    };
                }

                // Collect precision
                if (s[i] == '.') {
                    i += 1;
                    if (i >= s.len) return error.UnexpectedEndOfInput;

                    // Negative precision is allowed, but acts as if 0.
                    // see printf(3)
                    if (s[i] == '-') i += 1;

                    const precision_start = i;
                    while (i < s.len) : (i += 1) {
                        switch (s[i]) {
                            '0'...'9' => {},
                            else => break,
                        }
                    } else return error.UnexpectedEndOfInput;

                    _ = std.fmt.parseUnsigned(u31, s[precision_start..i], 10) catch |err| switch (err) {
                        error.InvalidCharacter => unreachable,
                        else => |e| return e,
                    };
                }

                if (stack_len < 1) return error.PoppedEmptyStack;
                switch (s[i]) {
                    'd', 'o', 'x', 'X', 's' => {},
                    else => return error.InvalidSpecifier,
                }
                stack_len -= 1;
            },
            'c' => {
                stack_len -= 1;
            },
            '%' => {},
            'p' => {
                i += 1;
                if (i >= s.len) return error.UnexpectedEndOfInput;
                switch (s[i]) {
                    '1'...'9' => {},
                    else => return error.InvalidSpecifier,
                }
                stack_len += 1;
            },
            inline 'P', 'g' => |c| {
                i += 1;
                if (i >= s.len) return error.UnexpectedEndOfInput;
                switch (s[i]) {
                    'a'...'z', 'A'...'Z' => {},
                    else => return error.InvalidSpecifier,
                }

                switch (c) {
                    'P' => stack_len = sub(usize, stack_len, 1) catch return error.PoppedEmptyStack,
                    'g' => stack_len += 1,
                    else => unreachable,
                }
            },
            '\'' => {
                i += 1;
                if (i >= s.len) return error.UnexpectedEndOfInput;
                i += 1;
                if (i >= s.len) return error.UnexpectedEndOfInput;
                if (s[i] != '\'') return error.InvalidSpecifier;
                stack_len += 1;
            },
            '{' => {
                i += 1;
                if (i >= s.len) return error.UnexpectedEndOfInput;

                const start = i;
                if (s[i] == '-') i += 1;

                while (i < s.len) : (i += 1) {
                    switch (s[i]) {
                        '0'...'9' => {},
                        else => break,
                    }
                } else return error.UnexpectedEndOfInput;
                if (start + @intFromBool(s[start] == '-') == i) return error.InvalidSpecifier;
                _ = std.fmt.parseInt(i32, s[start..i], 10) catch |err| switch (err) {
                    error.InvalidCharacter => unreachable,
                    else => |e| return e,
                };
                if (i >= s.len) return error.UnexpectedEndOfInput;
                if (s[i] != '}') return error.InvalidSpecifier;
                stack_len += 1;
            },
            'l' => {
                if (stack_len == 0) return error.PoppedEmptyStack;
            },
            '+', '-', '*', '/', 'm', '&', '|', '^', '=', '>', '<' => {
                if (stack_len < 2) return error.PoppedEmptyStack;
                stack_len -= 1;
            },
            '!', '~' => {
                if (stack_len < 1) return error.PoppedEmptyStack;
            },
            'i' => {
                if (param_count < 2) return error.NotEnoughParameters;
            },
            '?' => {},
            't' => {
                if (stack_len < 1) return error.PoppedEmptyStack;
                nesting += 1;
                stack_len -= 1;
            },
            'e' => {},
            ';' => nesting = sub(usize, nesting, 1) catch return error.UnexpectedConditionTerminator,
            else => return error.InvalidSpecifier,
        }
    }
}

/// Writes a paramterized escape sequence to the given writer, with the specified arguments. This
/// function does no error checking. It is recommended to first validate parameterized sequences by
/// calling `validateParamSequence`.
pub fn writeParamSequence(str: []const u8, writer: anytype, args: anytype) !void {
    const PrintFlags = packed struct(u4) {
        minus: bool = false,
        plus: bool = false,
        hash: bool = false,
        space: bool = false,
    };

    var params = blk: {
        var params: [args.len]Parameter = undefined;
        inline for (args, &params) |arg, *p| {
            p.* = createParam(arg);
        }
        break :blk params;
    };

    var stack: std.BoundedArray(Parameter, 123) = .{};
    var dynamic_variables: [26]Parameter = [_]Parameter{.{ .number = 0 }} ** 26;
    var static_variables: [26]Parameter = [_]Parameter{.{ .number = 0 }} ** 26;

    var i: usize = 0;
    while (i < str.len) : (i += 1) {
        if (str[i] != '%') {
            try writer.writeByte(str[i]);
            continue;
        }
        i += 1;
        if (i >= str.len) break;

        switch (str[i]) {
            '%' => try writer.writeByte('%'),
            'p' => {
                if (args.len > 0) {
                    i += 1;
                    const index = str[i] - '1';
                    stack.appendAssumeCapacity(params[index]);
                }
            },
            // Set dynamic/static variable to pop()
            'P' => {
                i += 1;
                const value = stack.pop();
                switch (str[i]) {
                    'a'...'z' => dynamic_variables[str[i] - 'a'] = value,
                    'A'...'Z' => static_variables[str[i] - 'A'] = value,
                    else => unreachable,
                }
            },
            'g' => {
                i += 1;
                const value = switch (str[i]) {
                    'a'...'z' => dynamic_variables[str[i] - 'a'],
                    'A'...'Z' => static_variables[str[i] - 'A'],
                    else => unreachable,
                };
                stack.appendAssumeCapacity(value);
            },
            'c' => {
                const arg = stack.pop();
                const char: u8 = switch (arg) {
                    .number => |int| @intCast(int),
                    .string => |s| s[0],
                };
                try writer.print("{c}", .{char});
            },
            ':', '#', ' ', '.', '0'...'9', 'd', 'o', 'x', 'X', 's' => |c| {
                if (c == ':') i += 1;
                var flags: PrintFlags = .{};

                // Collect flags
                while (i < str.len) : (i += 1) {
                    switch (str[i]) {
                        '-' => flags.minus = true,
                        '+' => flags.plus = true,
                        '#' => flags.hash = true,
                        ' ' => flags.space = true,
                        else => break,
                    }
                } else unreachable;

                // Collect width
                const width_start = i;
                while (i < str.len) : (i += 1) {
                    switch (str[i]) {
                        '0'...'9' => {},
                        else => break,
                    }
                } else unreachable;

                const width: ?i32 = if (i > width_start)
                    std.fmt.parseInt(u31, str[width_start..i], 10) catch unreachable
                else
                    null;

                // Collect precision
                const precision: ?i32 = blk: {
                    if (str[i] != '.') break :blk null;
                    i += 1;
                    const prec_start = i;
                    while (i < str.len) : (i += 1) {
                        switch (str[i]) {
                            '0'...'9' => {},
                            else => break,
                        }
                    } else unreachable;
                    break :blk std.fmt.parseInt(i32, str[prec_start..i], 10) catch unreachable;
                };

                const fmt_opts: util.FormatOptions = .{
                    .precision = std.math.cast(usize, precision orelse -1),

                    .width = if (width) |w|
                        if (w < 0) blk: {
                            flags.minus = true;
                            break :blk @abs(w);
                        } else @intCast(w)
                    else
                        null,

                    .alignment = if (flags.minus) .left else .right,
                    .plus = flags.plus,
                    .space = flags.space,
                };

                switch (str[i]) {
                    inline 'd', 'o', 'x', 'X' => |f| {
                        const value: Parameter = stack.pop();
                        assert(value == .number);
                        const base, const case = switch (f) {
                            'd' => .{ 10, .lower },
                            'o' => .{ 8, .lower },
                            'x' => .{ 16, .lower },
                            'X' => .{ 16, .upper },
                            else => unreachable,
                        };
                        try util.formatInt(value.number, base, case, fmt_opts, writer);
                    },
                    's' => switch (stack.pop()) {
                        .number => |int| try util.formatInt(int, 10, .lower, fmt_opts, writer),
                        .string => |s| try std.fmt.formatBuf(s, .{
                            .precision = fmt_opts.precision,
                            .width = fmt_opts.width,
                            .alignment = fmt_opts.alignment,
                            .fill = ' ',
                        }, writer),
                    },
                    else => unreachable,
                }
            },
            '\'' => {
                i += 1;
                const char = str[i];
                i += 1;
                assert(str[i] == '\'');

                stack.appendAssumeCapacity(.{ .number = char });
            },
            '{' => {
                i += 1;
                const start = i;
                if (str[i] == '-') i += 1;
                while (i < str.len) : (i += 1) {
                    switch (str[i]) {
                        '0'...'9' => {},
                        else => break,
                    }
                } else unreachable;
                assert(i > start);
                assert(str[i] == '}');
                const int_value = std.fmt.parseInt(i32, str[start..i], 10) catch unreachable;
                stack.appendAssumeCapacity(.{ .number = int_value });
            },
            'l' => {
                const value = stack.pop();
                const len: i32 = switch (value) {
                    .number => |int| if (int == 0)
                        1
                    else
                        std.math.log10_int(@abs(int)) + 1 + @intFromBool(int < 0),
                    .string => |s| @intCast(s.len),
                };
                stack.appendAssumeCapacity(.{ .number = len });
            },
            inline '+', '-', '*', '/', 'm', '&', '|', '^', '=', '>', '<' => |op| {
                const rhs = stack.pop().number;
                const lhs = stack.pop().number;
                const value: i32 = switch (op) {
                    '+' => lhs +% rhs,
                    '-' => lhs -% rhs,
                    '*' => lhs *% rhs,
                    '/' => @divTrunc(lhs, rhs),
                    'm' => @rem(lhs, rhs),
                    '&' => lhs & rhs,
                    '|' => lhs | rhs,
                    '^' => lhs ^ rhs,
                    '=' => @intFromBool(lhs == rhs),
                    '>' => @intFromBool(lhs > rhs),
                    '<' => @intFromBool(lhs < rhs),
                    'A' => @intFromBool(lhs != 0 and rhs != 0),
                    'O' => @intFromBool(lhs != 0 or rhs != 0),
                    else => unreachable,
                };
                stack.appendAssumeCapacity(.{ .number = value });
            },
            inline '!', '~' => |op| {
                const operand = stack.pop().number;
                const value: i32 = switch (op) {
                    '!' => @intFromBool(operand == 0),
                    '~' => ~operand,
                    else => unreachable,
                };
                stack.appendAssumeCapacity(.{ .number = value });
            },
            'i' => {
                if (comptime params.len >= 2) {
                    const n1 = params[0].number +% 1;
                    const n2 = params[1].number +% 1;
                    params[0] = .{ .number = n1 };
                    params[1] = .{ .number = n2 };
                }
            },
            '?' => {},
            // 't' and 'e' implementation based on unibilium
            't' => if (stack.pop().number == 0) {
                // Condition was false, skip until the end of contion or 'else.'
                var nesting: usize = 0;
                while (i < str.len) : (i += 1) {
                    if (str[i] != '%') continue;
                    i += 1;
                    switch (str[i]) {
                        '?' => nesting += 1,
                        ';' => {
                            if (nesting == 0) break;
                            nesting -= 1;
                        },
                        'e' => if (nesting == 0) break,
                        else => {},
                    }
                }
            },
            'e' => {
                // This code is only reachable if the condition was true, and as such we skip this
                // block. The condition being false is handled in 't'
                var nesting: usize = 0;
                while (i < str.len) : (i += 1) {
                    if (str[i] != '%') continue;
                    i += 1;
                    switch (str[i]) {
                        '?' => nesting += 1,
                        ';' => {
                            if (nesting == 0) break;
                            nesting -= 1;
                        },
                        else => {},
                    }
                }
            },
            ';' => {},
            else => unreachable,
        }
    }
}

const t = std.testing;
const ta = t.allocator;
const terminfo = @embedFile("descriptions/s/st-256color").*;

test "Invalid" {
    try t.expectError(error.InvalidFormat, parse(ta, ""));
    try t.expectError(error.InvalidFormat, parse(ta, "some invalid text"));
    var buf = terminfo;
    const p = try parse(ta, &buf);
    p.destroy(ta);
    buf[0] = 10;
    try t.expectError(error.InvalidFormat, parse(ta, &buf));
}

test "Boolean capabilities" {
    const res = try parse(ta, &terminfo);
    defer res.destroy(ta);
    try t.expectEqual(false, res.flags.get(.auto_left_margin));
    try t.expectEqual(true, res.flags.get(.auto_right_margin));
    try t.expectEqual(false, res.flags.get(.no_esc_ctlc));
    try t.expectEqual(false, res.flags.get(.ceol_standout_glitch));
    try t.expectEqual(true, res.flags.get(.eat_newline_glitch));
    try t.expectEqual(false, res.flags.get(.erase_overstrike));
    try t.expectEqual(false, res.flags.get(.generic_type));
    try t.expectEqual(false, res.flags.get(.hard_copy));
    try t.expectEqual(false, res.flags.get(.has_meta_key));
    try t.expectEqual(true, res.flags.get(.has_status_line));
    try t.expectEqual(false, res.flags.get(.insert_null_glitch));
    try t.expectEqual(false, res.flags.get(.memory_above));
    try t.expectEqual(false, res.flags.get(.memory_below));
    try t.expectEqual(true, res.flags.get(.move_insert_mode));
    try t.expectEqual(true, res.flags.get(.move_standout_mode));
    try t.expectEqual(false, res.flags.get(.over_strike));
    try t.expectEqual(false, res.flags.get(.status_line_esc_ok));
    try t.expectEqual(false, res.flags.get(.dest_tabs_magic_smso));
    try t.expectEqual(false, res.flags.get(.tilde_glitch));
    try t.expectEqual(false, res.flags.get(.transparent_underline));
    try t.expectEqual(false, res.flags.get(.xon_xoff));
    try t.expectEqual(false, res.flags.get(.needs_xon_xoff));
    try t.expectEqual(false, res.flags.get(.prtr_silent));
    try t.expectEqual(false, res.flags.get(.hard_cursor));
    try t.expectEqual(false, res.flags.get(.non_rev_rmcup));
    try t.expectEqual(true, res.flags.get(.no_pad_char));
    try t.expectEqual(false, res.flags.get(.non_dest_scroll_region));
    try t.expectEqual(true, res.flags.get(.can_change));
    try t.expectEqual(true, res.flags.get(.back_color_erase));

    // Should all be false, as the number of flags is only 29
    try t.expectEqual(false, res.flags.get(.hue_lightness_saturation));
    try t.expectEqual(false, res.flags.get(.col_addr_glitch));
    try t.expectEqual(false, res.flags.get(.cr_cancels_micro_mode));
    try t.expectEqual(false, res.flags.get(.has_print_wheel));
    try t.expectEqual(false, res.flags.get(.row_addr_glitch));
    try t.expectEqual(false, res.flags.get(.semi_auto_right_margin));
    try t.expectEqual(false, res.flags.get(.cpi_changes_res));
    try t.expectEqual(false, res.flags.get(.lpi_changes_res));
}

fn expectNumber(expected: u16, actual: i16) !void {
    return t.expectEqual(@as(i16, @bitCast(expected)), actual);
}

fn expectNumber32(expected: u32, actual: i32) !void {
    return t.expectEqual(@as(i32, @bitCast(expected)), actual);
}

test "Number capabilities" {
    const res = try parse(ta, &terminfo);
    defer res.destroy(ta);

    // Hex values taken from running `hexdump -C` on the terminfo file
    try expectNumber32(0x0050, res.numbers.get(.columns));
    try expectNumber32(0x0008, res.numbers.get(.init_tabs));
    try expectNumber32(0x0018, res.numbers.get(.lines));
    try expectNumber32(0xffffffff, res.numbers.get(.lines_of_memory));
    try expectNumber32(0xffffffff, res.numbers.get(.magic_cookie_glitch));
    try expectNumber32(0xffffffff, res.numbers.get(.padding_baud_rate));
    try expectNumber32(0xffffffff, res.numbers.get(.virtual_terminal));
    try expectNumber32(0xffffffff, res.numbers.get(.width_status_line));
    try expectNumber32(0xffffffff, res.numbers.get(.num_labels));
    try expectNumber32(0xffffffff, res.numbers.get(.label_height));
    try expectNumber32(0xffffffff, res.numbers.get(.label_width));
    try expectNumber32(0xffffffff, res.numbers.get(.max_attributes));
    try expectNumber32(0xffffffff, res.numbers.get(.maximum_windows));
    try expectNumber32(0x0100, res.numbers.get(.max_colors));
    try expectNumber32(0x7fff, res.numbers.get(.max_pairs));

    // Not in terminfo file, so all -1
    try expectNumber32(0xffffffff, res.numbers.get(.no_color_video));
    try expectNumber32(0xffffffff, res.numbers.get(.buffer_capacity));
    try expectNumber32(0xffffffff, res.numbers.get(.dot_vert_spacing));
    try expectNumber32(0xffffffff, res.numbers.get(.dot_horz_spacing));
    try expectNumber32(0xffffffff, res.numbers.get(.max_micro_address));
    try expectNumber32(0xffffffff, res.numbers.get(.max_micro_jump));
    try expectNumber32(0xffffffff, res.numbers.get(.micro_col_size));
    try expectNumber32(0xffffffff, res.numbers.get(.micro_line_size));
    try expectNumber32(0xffffffff, res.numbers.get(.number_of_pins));
    try expectNumber32(0xffffffff, res.numbers.get(.output_res_char));
    try expectNumber32(0xffffffff, res.numbers.get(.output_res_line));
    try expectNumber32(0xffffffff, res.numbers.get(.output_res_horz_inch));
    try expectNumber32(0xffffffff, res.numbers.get(.output_res_vert_inch));
    try expectNumber32(0xffffffff, res.numbers.get(.print_rate));
    try expectNumber32(0xffffffff, res.numbers.get(.wide_char_size));
    try expectNumber32(0xffffffff, res.numbers.get(.buttons));
    try expectNumber32(0xffffffff, res.numbers.get(.bit_image_entwining));
    try expectNumber32(0xffffffff, res.numbers.get(.bit_image_type));
}

test "String capabilities" {
    const res = try parse(ta, &terminfo);
    defer res.destroy(ta);

    // Hex values taken from running `hexdump -C` on the terminfo file
    try expectNumber(0x0000, res.strings.get(.back_tab));
    try expectNumber(0x0004, res.strings.get(.bell));
    try expectNumber(0x0006, res.strings.get(.carriage_return));
    try expectNumber(0x0008, res.strings.get(.change_scroll_region));
    try expectNumber(0x0019, res.strings.get(.clear_all_tabs));
    try expectNumber(0x001e, res.strings.get(.clear_screen));
    try expectNumber(0x0026, res.strings.get(.clr_eol));
    try expectNumber(0x002a, res.strings.get(.clr_eos));
    try expectNumber(0x002e, res.strings.get(.column_address));
    try expectNumber(0xffff, res.strings.get(.command_character));
    try expectNumber(0x0039, res.strings.get(.cursor_address));
    try expectNumber(0x004a, res.strings.get(.cursor_down));
    try expectNumber(0x004c, res.strings.get(.cursor_home));
    try expectNumber(0x0050, res.strings.get(.cursor_invisible));
    try expectNumber(0x0057, res.strings.get(.cursor_left));
    try expectNumber(0xffff, res.strings.get(.cursor_mem_address));
    try expectNumber(0x0059, res.strings.get(.cursor_normal));
    try expectNumber(0x0066, res.strings.get(.cursor_right));
    try expectNumber(0xffff, res.strings.get(.cursor_to_ll));
    try expectNumber(0x006a, res.strings.get(.cursor_up));
    try expectNumber(0x006e, res.strings.get(.cursor_visible));
    try expectNumber(0x0075, res.strings.get(.delete_character));
    try expectNumber(0x0079, res.strings.get(.delete_line));
    try expectNumber(0xffff, res.strings.get(.dis_status_line));
    try expectNumber(0xffff, res.strings.get(.down_half_line));
    try expectNumber(0x007d, res.strings.get(.enter_alt_charset_mode));
}

fn expectStringCapability(self: *Self, expected: []const u8, comptime e: String) !void {
    const str = self.getStringCapability(e) orelse return error.InvalidCapabilityName;
    try t.expectEqualSlices(u8, expected, str);
}

fn expectExtendedString(self: *Self, expected: []const u8, name: []const u8) !void {
    const str = self.getExtendedString(name) orelse return error.InvalidCapabilityName;
    try t.expectEqualSlices(u8, expected, str);
}

// test "getCapability" {
//     const file = openTermInfoFile("st-256color") orelse return error.Fug;
//     defer file.close();

//     const buf = try file.readToEndAlloc(t.allocator, 32768);
//     defer t.allocator.free(buf);

//     const res = try parse(ta, buf);
//     defer res.destroy(ta);

//     try expectStringCapability(res, "\x1b[%p1%dC", .parm_right_cursor);
//     try expectStringCapability(res, "\x1b[%p1%dD", .parm_left_cursor);
//     try expectStringCapability(res, "\x1b[%p1%dB", .parm_down_cursor);
//     try expectStringCapability(res, "\x1b[%p1%dA", .parm_up_cursor);
//     try expectStringCapability(res, "\x1b[1K", .clr_bol);

//     const init_tabs = res.getNumberCapability(.init_tabs) orelse return error.InvalidCapabilityName;
//     try t.expectEqual(@as(u31, 8), init_tabs);

//     const auto_left_margin = res.getFlagCapability(.auto_left_margin);
//     const auto_right_margin = res.getFlagCapability(.auto_right_margin);
//     try t.expectEqual(false, auto_left_margin);
//     try t.expectEqual(true, auto_right_margin);

//     try res.expectExtendedString("\x1b[9m", "smxx");
// }

test "st" {
    const x = @embedFile("descriptions/s/st-256color");
    const res = try parse(ta, x);
    defer res.destroy(ta);

    try t.expectEqualSlices(u8, "\x1b[%p1%d q", res.getExtendedString("Ss").?);
}

test "tmux" {
    const x = @embedFile("descriptions/t/tmux");
    const res = try parse(ta, x);
    defer res.destroy(ta);

    try t.expectEqualStrings("tmux|tmux terminal multiplexer", res.names);

    try t.expectEqual(false, res.getFlagCapability(.auto_left_margin));
    try t.expectEqual(true, res.getFlagCapability(.auto_right_margin));
    try t.expectEqual(false, res.getFlagCapability(.no_esc_ctlc));
    try t.expectEqual(false, res.getFlagCapability(.ceol_standout_glitch));
    try t.expectEqual(true, res.getFlagCapability(.eat_newline_glitch));
    try t.expectEqual(false, res.getFlagCapability(.erase_overstrike));
    try t.expectEqual(false, res.getFlagCapability(.generic_type));
    try t.expectEqual(false, res.getFlagCapability(.hard_copy));
    try t.expectEqual(true, res.getFlagCapability(.has_meta_key));
    try t.expectEqual(true, res.getFlagCapability(.has_status_line));
    try t.expectEqual(false, res.getFlagCapability(.insert_null_glitch));
    try t.expectEqual(false, res.getFlagCapability(.memory_above));
    try t.expectEqual(false, res.getFlagCapability(.memory_below));
    try t.expectEqual(true, res.getFlagCapability(.move_insert_mode));
    try t.expectEqual(true, res.getFlagCapability(.move_standout_mode));
    try t.expectEqual(false, res.getFlagCapability(.over_strike));
    try t.expectEqual(false, res.getFlagCapability(.status_line_esc_ok));
    try t.expectEqual(false, res.getFlagCapability(.dest_tabs_magic_smso));
    try t.expectEqual(false, res.getFlagCapability(.tilde_glitch));
    try t.expectEqual(false, res.getFlagCapability(.transparent_underline));
    try t.expectEqual(false, res.getFlagCapability(.xon_xoff));
    try t.expectEqual(false, res.getFlagCapability(.needs_xon_xoff));
    try t.expectEqual(false, res.getFlagCapability(.prtr_silent));
    try t.expectEqual(false, res.getFlagCapability(.hard_cursor));
    try t.expectEqual(false, res.getFlagCapability(.non_rev_rmcup));
    try t.expectEqual(false, res.getFlagCapability(.no_pad_char));
    try t.expectEqual(false, res.getFlagCapability(.non_dest_scroll_region));
    try t.expectEqual(false, res.getFlagCapability(.can_change));
    try t.expectEqual(false, res.getFlagCapability(.back_color_erase));
    try t.expectEqual(false, res.getFlagCapability(.hue_lightness_saturation));
    try t.expectEqual(false, res.getFlagCapability(.col_addr_glitch));
    try t.expectEqual(false, res.getFlagCapability(.cr_cancels_micro_mode));
    try t.expectEqual(false, res.getFlagCapability(.has_print_wheel));
    try t.expectEqual(false, res.getFlagCapability(.row_addr_glitch));
    try t.expectEqual(false, res.getFlagCapability(.semi_auto_right_margin));
    try t.expectEqual(false, res.getFlagCapability(.cpi_changes_res));
    try t.expectEqual(false, res.getFlagCapability(.lpi_changes_res));
    try t.expectEqual(true, res.getFlagCapability(.backspaces_with_bs));
    try t.expectEqual(false, res.getFlagCapability(.crt_no_scrolling));
    try t.expectEqual(false, res.getFlagCapability(.no_correctly_working_cr));
    try t.expectEqual(false, res.getFlagCapability(.gnu_has_meta_key));
    try t.expectEqual(false, res.getFlagCapability(.linefeed_is_newline));
    try t.expectEqual(true, res.getFlagCapability(.has_hardware_tabs));
    try t.expectEqual(false, res.getFlagCapability(.return_does_clr_eol));

    try t.expectEqual(@as(?u31, 80), res.getNumberCapability(.columns));
    try t.expectEqual(@as(?u31, 8), res.getNumberCapability(.init_tabs));
    try t.expectEqual(@as(?u31, 24), res.getNumberCapability(.lines));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.lines_of_memory));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.magic_cookie_glitch));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.padding_baud_rate));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.virtual_terminal));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.width_status_line));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.num_labels));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.label_height));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.label_width));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.max_attributes));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.maximum_windows));
    try t.expectEqual(@as(?u31, 8), res.getNumberCapability(.max_colors));
    try t.expectEqual(@as(?u31, 64), res.getNumberCapability(.max_pairs));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.no_color_video));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.buffer_capacity));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.dot_vert_spacing));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.dot_horz_spacing));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.max_micro_address));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.max_micro_jump));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.micro_col_size));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.micro_line_size));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.number_of_pins));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.output_res_char));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.output_res_line));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.output_res_horz_inch));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.output_res_vert_inch));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.print_rate));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.wide_char_size));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.buttons));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.bit_image_entwining));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.bit_image_type));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.magic_cookie_glitch_ul));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.carriage_return_delay));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.new_line_delay));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.backspace_delay));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.horizontal_tab_delay));
    try t.expectEqual(@as(?u31, null), res.getNumberCapability(.number_of_function_keys));

    try t.expectEqualSlices(u8, "++,,--..00``aaffgghhiijjkkllmmnnooppqqrrssttuuvvwwxxyyzz{{||}}~~", res.getStringCapability(.acs_chars).?);
    try t.expectEqualSlices(u8, "\x1b[Z", res.getStringCapability(.back_tab).?);
    try t.expectEqualSlices(u8, "\x07", res.getStringCapability(.bell).?);
    try t.expectEqualSlices(u8, "\x0d", res.getStringCapability(.carriage_return).?);
    try t.expectEqualSlices(u8, "\x1b[%i%p1%d;%p2%dr", res.getStringCapability(.change_scroll_region).?);
    try t.expectEqualSlices(u8, "\x1b[3g", res.getStringCapability(.clear_all_tabs).?);
    try t.expectEqualSlices(u8, "\x1b[H\x1b[J", res.getStringCapability(.clear_screen).?);
    try t.expectEqualSlices(u8, "\x1b[1K", res.getStringCapability(.clr_bol).?);
    try t.expectEqualSlices(u8, "\x1b[K", res.getStringCapability(.clr_eol).?);
    try t.expectEqualSlices(u8, "\x1b[J", res.getStringCapability(.clr_eos).?);
    try t.expectEqualSlices(u8, "\x1b[%i%p1%dG", res.getStringCapability(.column_address).?);
    try t.expectEqualSlices(u8, "\x1b[%i%p1%d;%p2%dH", res.getStringCapability(.cursor_address).?);
    try t.expectEqualSlices(u8, "\x0a", res.getStringCapability(.cursor_down).?);
    try t.expectEqualSlices(u8, "\x1b[H", res.getStringCapability(.cursor_home).?);
    try t.expectEqualSlices(u8, "\x1b[?25l", res.getStringCapability(.cursor_invisible).?);
    try t.expectEqualSlices(u8, "\x08", res.getStringCapability(.cursor_left).?);
    try t.expectEqualSlices(u8, "\x1b[34h\x1b[?25h", res.getStringCapability(.cursor_normal).?);
    try t.expectEqualSlices(u8, "\x1b[C", res.getStringCapability(.cursor_right).?);
    try t.expectEqualSlices(u8, "\x1bM", res.getStringCapability(.cursor_up).?);
    try t.expectEqualSlices(u8, "\x1b[34l", res.getStringCapability(.cursor_visible).?);
    try t.expectEqualSlices(u8, "\x1b[P", res.getStringCapability(.delete_character).?);
    try t.expectEqualSlices(u8, "\x1b[M", res.getStringCapability(.delete_line).?);
    try t.expectEqualSlices(u8, "\x1b]0;\x07", res.getStringCapability(.dis_status_line).?);
    try t.expectEqualSlices(u8, "\x1b(B\x1b)0", res.getStringCapability(.ena_acs).?);
    try t.expectEqualSlices(u8, "\x0e", res.getStringCapability(.enter_alt_charset_mode).?);
    try t.expectEqualSlices(u8, "\x1b[5m", res.getStringCapability(.enter_blink_mode).?);
    try t.expectEqualSlices(u8, "\x1b[1m", res.getStringCapability(.enter_bold_mode).?);
    try t.expectEqualSlices(u8, "\x1b[?1049h", res.getStringCapability(.enter_ca_mode).?);
    try t.expectEqualSlices(u8, "\x1b[2m", res.getStringCapability(.enter_dim_mode).?);
    try t.expectEqualSlices(u8, "\x1b[4h", res.getStringCapability(.enter_insert_mode).?);
    try t.expectEqualSlices(u8, "\x1b[3m", res.getStringCapability(.enter_italics_mode).?);
    try t.expectEqualSlices(u8, "\x1b[7m", res.getStringCapability(.enter_reverse_mode).?);
    try t.expectEqualSlices(u8, "\x1b[8m", res.getStringCapability(.enter_secure_mode).?);
    try t.expectEqualSlices(u8, "\x1b[7m", res.getStringCapability(.enter_standout_mode).?);
    try t.expectEqualSlices(u8, "\x1b[4m", res.getStringCapability(.enter_underline_mode).?);
    try t.expectEqualSlices(u8, "\x0f", res.getStringCapability(.exit_alt_charset_mode).?);
    try t.expectEqualSlices(u8, "\x1b[m\x0f", res.getStringCapability(.exit_attribute_mode).?);
    try t.expectEqualSlices(u8, "\x1b[?1049l", res.getStringCapability(.exit_ca_mode).?);
    try t.expectEqualSlices(u8, "\x1b[4l", res.getStringCapability(.exit_insert_mode).?);
    try t.expectEqualSlices(u8, "\x1b[23m", res.getStringCapability(.exit_italics_mode).?);
    try t.expectEqualSlices(u8, "\x1b[27m", res.getStringCapability(.exit_standout_mode).?);
    try t.expectEqualSlices(u8, "\x1b[24m", res.getStringCapability(.exit_underline_mode).?);
    try t.expectEqualSlices(u8, "\x1bg", res.getStringCapability(.flash_screen).?);
    try t.expectEqualSlices(u8, "\x07", res.getStringCapability(.from_status_line).?);
    try t.expectEqualSlices(u8, "\x1b)0", res.getStringCapability(.init_2string).?);
    try t.expectEqualSlices(u8, "\x1b[L", res.getStringCapability(.insert_line).?);
    try t.expectEqualSlices(u8, "\x08", res.getStringCapability(.key_backspace).?);
    try t.expectEqualSlices(u8, "\x1b[Z", res.getStringCapability(.key_btab).?);
    try t.expectEqualSlices(u8, "\x1b[3~", res.getStringCapability(.key_dc).?);
    try t.expectEqualSlices(u8, "\x1bOB", res.getStringCapability(.key_down).?);
    try t.expectEqualSlices(u8, "\x1b[4~", res.getStringCapability(.key_end).?);
    try t.expectEqualSlices(u8, "\x1bOP", res.getStringCapability(.key_f1).?);
    try t.expectEqualSlices(u8, "\x1b[21~", res.getStringCapability(.key_f10).?);
    try t.expectEqualSlices(u8, "\x1b[23~", res.getStringCapability(.key_f11).?);
    try t.expectEqualSlices(u8, "\x1b[24~", res.getStringCapability(.key_f12).?);
    try t.expectEqualSlices(u8, "\x1b[1;2P", res.getStringCapability(.key_f13).?);
    try t.expectEqualSlices(u8, "\x1b[1;2Q", res.getStringCapability(.key_f14).?);
    try t.expectEqualSlices(u8, "\x1b[1;2R", res.getStringCapability(.key_f15).?);
    try t.expectEqualSlices(u8, "\x1b[1;2S", res.getStringCapability(.key_f16).?);
    try t.expectEqualSlices(u8, "\x1b[15;2~", res.getStringCapability(.key_f17).?);
    try t.expectEqualSlices(u8, "\x1b[17;2~", res.getStringCapability(.key_f18).?);
    try t.expectEqualSlices(u8, "\x1b[18;2~", res.getStringCapability(.key_f19).?);
    try t.expectEqualSlices(u8, "\x1bOQ", res.getStringCapability(.key_f2).?);
    try t.expectEqualSlices(u8, "\x1b[19;2~", res.getStringCapability(.key_f20).?);
    try t.expectEqualSlices(u8, "\x1b[20;2~", res.getStringCapability(.key_f21).?);
    try t.expectEqualSlices(u8, "\x1b[21;2~", res.getStringCapability(.key_f22).?);
    try t.expectEqualSlices(u8, "\x1b[23;2~", res.getStringCapability(.key_f23).?);
    try t.expectEqualSlices(u8, "\x1b[24;2~", res.getStringCapability(.key_f24).?);
    try t.expectEqualSlices(u8, "\x1b[1;5P", res.getStringCapability(.key_f25).?);
    try t.expectEqualSlices(u8, "\x1b[1;5Q", res.getStringCapability(.key_f26).?);
    try t.expectEqualSlices(u8, "\x1b[1;5R", res.getStringCapability(.key_f27).?);
    try t.expectEqualSlices(u8, "\x1b[1;5S", res.getStringCapability(.key_f28).?);
    try t.expectEqualSlices(u8, "\x1b[15;5~", res.getStringCapability(.key_f29).?);
    try t.expectEqualSlices(u8, "\x1bOR", res.getStringCapability(.key_f3).?);
    try t.expectEqualSlices(u8, "\x1b[17;5~", res.getStringCapability(.key_f30).?);
    try t.expectEqualSlices(u8, "\x1b[18;5~", res.getStringCapability(.key_f31).?);
    try t.expectEqualSlices(u8, "\x1b[19;5~", res.getStringCapability(.key_f32).?);
    try t.expectEqualSlices(u8, "\x1b[20;5~", res.getStringCapability(.key_f33).?);
    try t.expectEqualSlices(u8, "\x1b[21;5~", res.getStringCapability(.key_f34).?);
    try t.expectEqualSlices(u8, "\x1b[23;5~", res.getStringCapability(.key_f35).?);
    try t.expectEqualSlices(u8, "\x1b[24;5~", res.getStringCapability(.key_f36).?);
    try t.expectEqualSlices(u8, "\x1b[1;6P", res.getStringCapability(.key_f37).?);
    try t.expectEqualSlices(u8, "\x1b[1;6Q", res.getStringCapability(.key_f38).?);
    try t.expectEqualSlices(u8, "\x1b[1;6R", res.getStringCapability(.key_f39).?);
    try t.expectEqualSlices(u8, "\x1bOS", res.getStringCapability(.key_f4).?);
    try t.expectEqualSlices(u8, "\x1b[1;6S", res.getStringCapability(.key_f40).?);
    try t.expectEqualSlices(u8, "\x1b[15;6~", res.getStringCapability(.key_f41).?);
    try t.expectEqualSlices(u8, "\x1b[17;6~", res.getStringCapability(.key_f42).?);
    try t.expectEqualSlices(u8, "\x1b[18;6~", res.getStringCapability(.key_f43).?);
    try t.expectEqualSlices(u8, "\x1b[19;6~", res.getStringCapability(.key_f44).?);
    try t.expectEqualSlices(u8, "\x1b[20;6~", res.getStringCapability(.key_f45).?);
    try t.expectEqualSlices(u8, "\x1b[21;6~", res.getStringCapability(.key_f46).?);
    try t.expectEqualSlices(u8, "\x1b[23;6~", res.getStringCapability(.key_f47).?);
    try t.expectEqualSlices(u8, "\x1b[24;6~", res.getStringCapability(.key_f48).?);
    try t.expectEqualSlices(u8, "\x1b[1;3P", res.getStringCapability(.key_f49).?);
    try t.expectEqualSlices(u8, "\x1b[15~", res.getStringCapability(.key_f5).?);
    try t.expectEqualSlices(u8, "\x1b[1;3Q", res.getStringCapability(.key_f50).?);
    try t.expectEqualSlices(u8, "\x1b[1;3R", res.getStringCapability(.key_f51).?);
    try t.expectEqualSlices(u8, "\x1b[1;3S", res.getStringCapability(.key_f52).?);
    try t.expectEqualSlices(u8, "\x1b[15;3~", res.getStringCapability(.key_f53).?);
    try t.expectEqualSlices(u8, "\x1b[17;3~", res.getStringCapability(.key_f54).?);
    try t.expectEqualSlices(u8, "\x1b[18;3~", res.getStringCapability(.key_f55).?);
    try t.expectEqualSlices(u8, "\x1b[19;3~", res.getStringCapability(.key_f56).?);
    try t.expectEqualSlices(u8, "\x1b[20;3~", res.getStringCapability(.key_f57).?);
    try t.expectEqualSlices(u8, "\x1b[21;3~", res.getStringCapability(.key_f58).?);
    try t.expectEqualSlices(u8, "\x1b[23;3~", res.getStringCapability(.key_f59).?);
    try t.expectEqualSlices(u8, "\x1b[17~", res.getStringCapability(.key_f6).?);
    try t.expectEqualSlices(u8, "\x1b[24;3~", res.getStringCapability(.key_f60).?);
    try t.expectEqualSlices(u8, "\x1b[1;4P", res.getStringCapability(.key_f61).?);
    try t.expectEqualSlices(u8, "\x1b[1;4Q", res.getStringCapability(.key_f62).?);
    try t.expectEqualSlices(u8, "\x1b[1;4R", res.getStringCapability(.key_f63).?);
    try t.expectEqualSlices(u8, "\x1b[18~", res.getStringCapability(.key_f7).?);
    try t.expectEqualSlices(u8, "\x1b[19~", res.getStringCapability(.key_f8).?);
    try t.expectEqualSlices(u8, "\x1b[20~", res.getStringCapability(.key_f9).?);
    try t.expectEqualSlices(u8, "\x1b[1~", res.getStringCapability(.key_home).?);
    try t.expectEqualSlices(u8, "\x1b[2~", res.getStringCapability(.key_ic).?);
    try t.expectEqualSlices(u8, "\x1bOD", res.getStringCapability(.key_left).?);
    try t.expectEqualSlices(u8, "\x1b[M", res.getStringCapability(.key_mouse).?);
    try t.expectEqualSlices(u8, "\x1b[6~", res.getStringCapability(.key_npage).?);
    try t.expectEqualSlices(u8, "\x1b[5~", res.getStringCapability(.key_ppage).?);
    try t.expectEqualSlices(u8, "\x1bOC", res.getStringCapability(.key_right).?);
    try t.expectEqualSlices(u8, "\x1b[3;2~", res.getStringCapability(.key_sdc).?);
    try t.expectEqualSlices(u8, "\x1b[1;2F", res.getStringCapability(.key_send).?);
    try t.expectEqualSlices(u8, "\x1b[1;2B", res.getStringCapability(.key_sf).?);
    try t.expectEqualSlices(u8, "\x1b[1;2H", res.getStringCapability(.key_shome).?);
    try t.expectEqualSlices(u8, "\x1b[2;2~", res.getStringCapability(.key_sic).?);
    try t.expectEqualSlices(u8, "\x1b[1;2D", res.getStringCapability(.key_sleft).?);
    try t.expectEqualSlices(u8, "\x1b[6;2~", res.getStringCapability(.key_snext).?);
    try t.expectEqualSlices(u8, "\x1b[5;2~", res.getStringCapability(.key_sprevious).?);
    try t.expectEqualSlices(u8, "\x1b[1;2A", res.getStringCapability(.key_sr).?);
    try t.expectEqualSlices(u8, "\x1b[1;2C", res.getStringCapability(.key_sright).?);
    try t.expectEqualSlices(u8, "\x1bOA", res.getStringCapability(.key_up).?);
    try t.expectEqualSlices(u8, "\x1b[?1l\x1b>", res.getStringCapability(.keypad_local).?);
    try t.expectEqualSlices(u8, "\x1b[?1h\x1b=", res.getStringCapability(.keypad_xmit).?);
    try t.expectEqualSlices(u8, "\x1bE", res.getStringCapability(.newline).?);
    try t.expectEqualSlices(u8, "\x1b[39;49m", res.getStringCapability(.orig_pair).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dP", res.getStringCapability(.parm_dch).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dM", res.getStringCapability(.parm_delete_line).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dB", res.getStringCapability(.parm_down_cursor).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%d@", res.getStringCapability(.parm_ich).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dS", res.getStringCapability(.parm_index).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dL", res.getStringCapability(.parm_insert_line).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dD", res.getStringCapability(.parm_left_cursor).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dC", res.getStringCapability(.parm_right_cursor).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dA", res.getStringCapability(.parm_up_cursor).?);
    try t.expectEqualSlices(u8, "\x1bc\x1b[?1000l\x1b[?25h", res.getStringCapability(.reset_2string).?);
    try t.expectEqualSlices(u8, "\x1b8", res.getStringCapability(.restore_cursor).?);
    try t.expectEqualSlices(u8, "\x1b[%i%p1%dd", res.getStringCapability(.row_address).?);
    try t.expectEqualSlices(u8, "\x1b7", res.getStringCapability(.save_cursor).?);
    try t.expectEqualSlices(u8, "\x0a", res.getStringCapability(.scroll_forward).?);
    try t.expectEqualSlices(u8, "\x1bM", res.getStringCapability(.scroll_reverse).?);
    try t.expectEqualSlices(u8, "\x1b[4%p1%dm", res.getStringCapability(.set_a_background).?);
    try t.expectEqualSlices(u8, "\x1b[3%p1%dm", res.getStringCapability(.set_a_foreground).?);
    try t.expectEqualSlices(u8, "\x1b[0%?%p6%t;1%;%?%p1%t;7%;%?%p2%t;4%;%?%p3%t;7%;%?%p4%t;            5%;%?%p5%t;2%;m%?%p9%t\x0e%e\x0f%;", res.getStringCapability(.set_attributes).?);
    try t.expectEqualSlices(u8, "\x1bH", res.getStringCapability(.set_tab).?);
    try t.expectEqualSlices(u8, "\x09", res.getStringCapability(.tab).?);
    try t.expectEqualSlices(u8, "\x1b]0;", res.getStringCapability(.to_status_line).?);

    try t.expectEqual(null, res.getStringCapability(.acs_btee));
    try t.expectEqual(null, res.getStringCapability(.acs_hline));
    try t.expectEqual(null, res.getStringCapability(.acs_llcorner));
    try t.expectEqual(null, res.getStringCapability(.acs_lrcorner));
    try t.expectEqual(null, res.getStringCapability(.acs_ltee));
    try t.expectEqual(null, res.getStringCapability(.acs_plus));
    try t.expectEqual(null, res.getStringCapability(.acs_rtee));
    try t.expectEqual(null, res.getStringCapability(.acs_ttee));
    try t.expectEqual(null, res.getStringCapability(.acs_ulcorner));
    try t.expectEqual(null, res.getStringCapability(.acs_urcorner));
    try t.expectEqual(null, res.getStringCapability(.acs_vline));
    try t.expectEqual(null, res.getStringCapability(.alt_scancode_esc));
    try t.expectEqual(null, res.getStringCapability(.arrow_key_map));
    try t.expectEqual(null, res.getStringCapability(.backspace_if_not_bs));
    try t.expectEqual(null, res.getStringCapability(.bit_image_carriage_return));
    try t.expectEqual(null, res.getStringCapability(.bit_image_newline));
    try t.expectEqual(null, res.getStringCapability(.bit_image_repeat));
    try t.expectEqual(null, res.getStringCapability(.box_chars_1));
    try t.expectEqual(null, res.getStringCapability(.change_char_pitch));
    try t.expectEqual(null, res.getStringCapability(.change_line_pitch));
    try t.expectEqual(null, res.getStringCapability(.change_res_horz));
    try t.expectEqual(null, res.getStringCapability(.change_res_vert));
    try t.expectEqual(null, res.getStringCapability(.char_padding));
    try t.expectEqual(null, res.getStringCapability(.char_set_names));
    try t.expectEqual(null, res.getStringCapability(.clear_margins));
    try t.expectEqual(null, res.getStringCapability(.code_set_init));
    try t.expectEqual(null, res.getStringCapability(.color_names));
    try t.expectEqual(null, res.getStringCapability(.command_character));
    try t.expectEqual(null, res.getStringCapability(.create_window));
    try t.expectEqual(null, res.getStringCapability(.cursor_mem_address));
    try t.expectEqual(null, res.getStringCapability(.cursor_to_ll));
    try t.expectEqual(null, res.getStringCapability(.define_bit_image_region));
    try t.expectEqual(null, res.getStringCapability(.define_char));
    try t.expectEqual(null, res.getStringCapability(.device_type));
    try t.expectEqual(null, res.getStringCapability(.dial_phone));
    try t.expectEqual(null, res.getStringCapability(.display_clock));
    try t.expectEqual(null, res.getStringCapability(.display_pc_char));
    try t.expectEqual(null, res.getStringCapability(.down_half_line));
    try t.expectEqual(null, res.getStringCapability(.end_bit_image_region));
    try t.expectEqual(null, res.getStringCapability(.enter_am_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_delete_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_doublewide_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_draft_quality));
    try t.expectEqual(null, res.getStringCapability(.enter_horizontal_hl_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_left_hl_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_leftward_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_low_hl_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_micro_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_near_letter_quality));
    try t.expectEqual(null, res.getStringCapability(.enter_normal_quality));
    try t.expectEqual(null, res.getStringCapability(.enter_pc_charset_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_protected_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_right_hl_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_scancode_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_shadow_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_subscript_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_superscript_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_top_hl_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_upward_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_vertical_hl_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_xon_mode));
    try t.expectEqual(null, res.getStringCapability(.erase_chars));
    try t.expectEqual(null, res.getStringCapability(.exit_am_mode));
    try t.expectEqual(null, res.getStringCapability(.exit_delete_mode));
    try t.expectEqual(null, res.getStringCapability(.exit_doublewide_mode));
    try t.expectEqual(null, res.getStringCapability(.exit_leftward_mode));
    try t.expectEqual(null, res.getStringCapability(.exit_micro_mode));
    try t.expectEqual(null, res.getStringCapability(.exit_pc_charset_mode));
    try t.expectEqual(null, res.getStringCapability(.exit_scancode_mode));
    try t.expectEqual(null, res.getStringCapability(.exit_shadow_mode));
    try t.expectEqual(null, res.getStringCapability(.exit_subscript_mode));
    try t.expectEqual(null, res.getStringCapability(.exit_superscript_mode));
    try t.expectEqual(null, res.getStringCapability(.exit_upward_mode));
    try t.expectEqual(null, res.getStringCapability(.exit_xon_mode));
    try t.expectEqual(null, res.getStringCapability(.fixed_pause));
    try t.expectEqual(null, res.getStringCapability(.flash_hook));
    try t.expectEqual(null, res.getStringCapability(.form_feed));
    try t.expectEqual(null, res.getStringCapability(.get_mouse));
    try t.expectEqual(null, res.getStringCapability(.goto_window));
    try t.expectEqual(null, res.getStringCapability(.hangup));
    try t.expectEqual(null, res.getStringCapability(.init_1string));
    try t.expectEqual(null, res.getStringCapability(.init_3string));
    try t.expectEqual(null, res.getStringCapability(.init_file));
    try t.expectEqual(null, res.getStringCapability(.init_prog));
    try t.expectEqual(null, res.getStringCapability(.initialize_color));
    try t.expectEqual(null, res.getStringCapability(.initialize_pair));
    try t.expectEqual(null, res.getStringCapability(.insert_character));
    try t.expectEqual(null, res.getStringCapability(.insert_padding));
    try t.expectEqual(null, res.getStringCapability(.key_a1));
    try t.expectEqual(null, res.getStringCapability(.key_a3));
    try t.expectEqual(null, res.getStringCapability(.key_b2));
    try t.expectEqual(null, res.getStringCapability(.key_beg));
    try t.expectEqual(null, res.getStringCapability(.key_c1));
    try t.expectEqual(null, res.getStringCapability(.key_c3));
    try t.expectEqual(null, res.getStringCapability(.key_cancel));
    try t.expectEqual(null, res.getStringCapability(.key_catab));
    try t.expectEqual(null, res.getStringCapability(.key_clear));
    try t.expectEqual(null, res.getStringCapability(.key_close));
    try t.expectEqual(null, res.getStringCapability(.key_command));
    try t.expectEqual(null, res.getStringCapability(.key_copy));
    try t.expectEqual(null, res.getStringCapability(.key_create));
    try t.expectEqual(null, res.getStringCapability(.key_ctab));
    try t.expectEqual(null, res.getStringCapability(.key_dl));
    try t.expectEqual(null, res.getStringCapability(.key_eic));
    try t.expectEqual(null, res.getStringCapability(.key_enter));
    try t.expectEqual(null, res.getStringCapability(.key_eol));
    try t.expectEqual(null, res.getStringCapability(.key_eos));
    try t.expectEqual(null, res.getStringCapability(.key_exit));
    try t.expectEqual(null, res.getStringCapability(.key_f0));
    try t.expectEqual(null, res.getStringCapability(.key_find));
    try t.expectEqual(null, res.getStringCapability(.key_help));
    try t.expectEqual(null, res.getStringCapability(.key_il));
    try t.expectEqual(null, res.getStringCapability(.key_ll));
    try t.expectEqual(null, res.getStringCapability(.key_mark));
    try t.expectEqual(null, res.getStringCapability(.key_message));
    try t.expectEqual(null, res.getStringCapability(.key_move));
    try t.expectEqual(null, res.getStringCapability(.key_next));
    try t.expectEqual(null, res.getStringCapability(.key_open));
    try t.expectEqual(null, res.getStringCapability(.key_options));
    try t.expectEqual(null, res.getStringCapability(.key_previous));
    try t.expectEqual(null, res.getStringCapability(.key_print));
    try t.expectEqual(null, res.getStringCapability(.key_redo));
    try t.expectEqual(null, res.getStringCapability(.key_reference));
    try t.expectEqual(null, res.getStringCapability(.key_refresh));
    try t.expectEqual(null, res.getStringCapability(.key_replace));
    try t.expectEqual(null, res.getStringCapability(.key_restart));
    try t.expectEqual(null, res.getStringCapability(.key_resume));
    try t.expectEqual(null, res.getStringCapability(.key_save));
    try t.expectEqual(null, res.getStringCapability(.key_sbeg));
    try t.expectEqual(null, res.getStringCapability(.key_scancel));
    try t.expectEqual(null, res.getStringCapability(.key_scommand));
    try t.expectEqual(null, res.getStringCapability(.key_scopy));
    try t.expectEqual(null, res.getStringCapability(.key_screate));
    try t.expectEqual(null, res.getStringCapability(.key_sdl));
    try t.expectEqual(null, res.getStringCapability(.key_select));
    try t.expectEqual(null, res.getStringCapability(.key_seol));
    try t.expectEqual(null, res.getStringCapability(.key_sexit));
    try t.expectEqual(null, res.getStringCapability(.key_sfind));
    try t.expectEqual(null, res.getStringCapability(.key_shelp));
    try t.expectEqual(null, res.getStringCapability(.key_smessage));
    try t.expectEqual(null, res.getStringCapability(.key_smove));
    try t.expectEqual(null, res.getStringCapability(.key_soptions));
    try t.expectEqual(null, res.getStringCapability(.key_sprint));
    try t.expectEqual(null, res.getStringCapability(.key_sredo));
    try t.expectEqual(null, res.getStringCapability(.key_sreplace));
    try t.expectEqual(null, res.getStringCapability(.key_srsume));
    try t.expectEqual(null, res.getStringCapability(.key_ssave));
    try t.expectEqual(null, res.getStringCapability(.key_ssuspend));
    try t.expectEqual(null, res.getStringCapability(.key_stab));
    try t.expectEqual(null, res.getStringCapability(.key_sundo));
    try t.expectEqual(null, res.getStringCapability(.key_suspend));
    try t.expectEqual(null, res.getStringCapability(.key_undo));
    try t.expectEqual(null, res.getStringCapability(.lab_f0));
    try t.expectEqual(null, res.getStringCapability(.lab_f1));
    try t.expectEqual(null, res.getStringCapability(.lab_f10));
    try t.expectEqual(null, res.getStringCapability(.lab_f2));
    try t.expectEqual(null, res.getStringCapability(.lab_f3));
    try t.expectEqual(null, res.getStringCapability(.lab_f4));
    try t.expectEqual(null, res.getStringCapability(.lab_f5));
    try t.expectEqual(null, res.getStringCapability(.lab_f6));
    try t.expectEqual(null, res.getStringCapability(.lab_f7));
    try t.expectEqual(null, res.getStringCapability(.lab_f8));
    try t.expectEqual(null, res.getStringCapability(.lab_f9));
    try t.expectEqual(null, res.getStringCapability(.label_format));
    try t.expectEqual(null, res.getStringCapability(.label_off));
    try t.expectEqual(null, res.getStringCapability(.label_on));
    try t.expectEqual(null, res.getStringCapability(.linefeed_if_not_lf));
    try t.expectEqual(null, res.getStringCapability(.memory_lock));
    try t.expectEqual(null, res.getStringCapability(.memory_unlock));
    try t.expectEqual(null, res.getStringCapability(.meta_off));
    try t.expectEqual(null, res.getStringCapability(.meta_on));
    try t.expectEqual(null, res.getStringCapability(.micro_column_address));
    try t.expectEqual(null, res.getStringCapability(.micro_down));
    try t.expectEqual(null, res.getStringCapability(.micro_left));
    try t.expectEqual(null, res.getStringCapability(.micro_right));
    try t.expectEqual(null, res.getStringCapability(.micro_row_address));
    try t.expectEqual(null, res.getStringCapability(.micro_up));
    try t.expectEqual(null, res.getStringCapability(.mouse_info));
    try t.expectEqual(null, res.getStringCapability(.order_of_pins));
    try t.expectEqual(null, res.getStringCapability(.orig_colors));
    try t.expectEqual(null, res.getStringCapability(.other_non_function_keys));
    try t.expectEqual(null, res.getStringCapability(.pad_char));
    try t.expectEqual(null, res.getStringCapability(.parm_down_micro));
    try t.expectEqual(null, res.getStringCapability(.parm_left_micro));
    try t.expectEqual(null, res.getStringCapability(.parm_right_micro));
    try t.expectEqual(null, res.getStringCapability(.parm_rindex));
    try t.expectEqual(null, res.getStringCapability(.parm_up_micro));
    try t.expectEqual(null, res.getStringCapability(.pc_term_options));
    try t.expectEqual(null, res.getStringCapability(.pkey_key));
    try t.expectEqual(null, res.getStringCapability(.pkey_local));
    try t.expectEqual(null, res.getStringCapability(.pkey_plab));
    try t.expectEqual(null, res.getStringCapability(.pkey_xmit));
    try t.expectEqual(null, res.getStringCapability(.plab_norm));
    try t.expectEqual(null, res.getStringCapability(.print_screen));
    try t.expectEqual(null, res.getStringCapability(.prtr_non));
    try t.expectEqual(null, res.getStringCapability(.prtr_off));
    try t.expectEqual(null, res.getStringCapability(.prtr_on));
    try t.expectEqual(null, res.getStringCapability(.pulse));
    try t.expectEqual(null, res.getStringCapability(.quick_dial));
    try t.expectEqual(null, res.getStringCapability(.remove_clock));
    try t.expectEqual(null, res.getStringCapability(.repeat_char));
    try t.expectEqual(null, res.getStringCapability(.req_for_input));
    try t.expectEqual(null, res.getStringCapability(.req_mouse_pos));
    try t.expectEqual(null, res.getStringCapability(.reset_1string));
    try t.expectEqual(null, res.getStringCapability(.reset_3string));
    try t.expectEqual(null, res.getStringCapability(.reset_file));
    try t.expectEqual(null, res.getStringCapability(.scancode_escape));
    try t.expectEqual(null, res.getStringCapability(.select_char_set));
    try t.expectEqual(null, res.getStringCapability(.set0_des_seq));
    try t.expectEqual(null, res.getStringCapability(.set1_des_seq));
    try t.expectEqual(null, res.getStringCapability(.set2_des_seq));
    try t.expectEqual(null, res.getStringCapability(.set3_des_seq));
    try t.expectEqual(null, res.getStringCapability(.set_a_attributes));
    try t.expectEqual(null, res.getStringCapability(.set_background));
    try t.expectEqual(null, res.getStringCapability(.set_bottom_margin));
    try t.expectEqual(null, res.getStringCapability(.set_bottom_margin_parm));
    try t.expectEqual(null, res.getStringCapability(.set_clock));
    try t.expectEqual(null, res.getStringCapability(.set_color_band));
    try t.expectEqual(null, res.getStringCapability(.set_color_pair));
    try t.expectEqual(null, res.getStringCapability(.set_foreground));
    try t.expectEqual(null, res.getStringCapability(.set_left_margin));
    try t.expectEqual(null, res.getStringCapability(.set_left_margin_parm));
    try t.expectEqual(null, res.getStringCapability(.set_lr_margin));
    try t.expectEqual(null, res.getStringCapability(.set_page_length));
    try t.expectEqual(null, res.getStringCapability(.set_pglen_inch));
    try t.expectEqual(null, res.getStringCapability(.set_right_margin));
    try t.expectEqual(null, res.getStringCapability(.set_right_margin_parm));
    try t.expectEqual(null, res.getStringCapability(.set_tb_margin));
    try t.expectEqual(null, res.getStringCapability(.set_top_margin));
    try t.expectEqual(null, res.getStringCapability(.set_top_margin_parm));
    try t.expectEqual(null, res.getStringCapability(.set_window));
    try t.expectEqual(null, res.getStringCapability(.start_bit_image));
    try t.expectEqual(null, res.getStringCapability(.start_char_set_def));
    try t.expectEqual(null, res.getStringCapability(.stop_bit_image));
    try t.expectEqual(null, res.getStringCapability(.stop_char_set_def));
    try t.expectEqual(null, res.getStringCapability(.subscript_characters));
    try t.expectEqual(null, res.getStringCapability(.superscript_characters));
    try t.expectEqual(null, res.getStringCapability(.termcap_init2));
    try t.expectEqual(null, res.getStringCapability(.termcap_reset));
    try t.expectEqual(null, res.getStringCapability(.these_cause_cr));
    try t.expectEqual(null, res.getStringCapability(.tone));
    try t.expectEqual(null, res.getStringCapability(.underline_char));
    try t.expectEqual(null, res.getStringCapability(.up_half_line));
    try t.expectEqual(null, res.getStringCapability(.user0));
    try t.expectEqual(null, res.getStringCapability(.user1));
    try t.expectEqual(null, res.getStringCapability(.user2));
    try t.expectEqual(null, res.getStringCapability(.user3));
    try t.expectEqual(null, res.getStringCapability(.user4));
    try t.expectEqual(null, res.getStringCapability(.user5));
    try t.expectEqual(null, res.getStringCapability(.user6));
    try t.expectEqual(null, res.getStringCapability(.user7));
    try t.expectEqual(null, res.getStringCapability(.user8));
    try t.expectEqual(null, res.getStringCapability(.user9));
    try t.expectEqual(null, res.getStringCapability(.wait_tone));
    try t.expectEqual(null, res.getStringCapability(.xoff_character));
    try t.expectEqual(null, res.getStringCapability(.xon_character));
    try t.expectEqual(null, res.getStringCapability(.zero_motion));

    try t.expectEqual(3, res.ext_flags.size);
    try t.expectEqual(true, res.getExtendedFlag("AX"));
    try t.expectEqual(true, res.getExtendedFlag("G0"));
    try t.expectEqual(true, res.getExtendedFlag("XT"));

    try t.expectEqual(1, res.ext_nums.size);
    try t.expectEqual(1, res.getExtendedNumber("U8"));

    try t.expectEqual(63, res.ext_strs.size);

    try t.expectEqualSlices(u8, "\x1b]112\x07", res.getExtendedString("Cr").?);
    try t.expectEqualSlices(u8, "\x1b]12;%p1%s\x07", res.getExtendedString("Cs").?);
    try t.expectEqualSlices(u8, "\x1b(B", res.getExtendedString("E0").?);
    try t.expectEqualSlices(u8, "\x1b[3J", res.getExtendedString("E3").?);
    try t.expectEqualSlices(u8, "\x1b]52;%p1%s;%p2%s\x07", res.getExtendedString("Ms").?);
    try t.expectEqualSlices(u8, "\x1b(%p1%c", res.getExtendedString("S0").?);
    try t.expectEqualSlices(u8, "\x1b[2 q", res.getExtendedString("Se").?);
    try t.expectEqualSlices(u8, "\x1b[%p1%d q", res.getExtendedString("Ss").?);
    try t.expectEqualSlices(u8, "\x1b]0;", res.getExtendedString("TS").?);
    try t.expectEqualSlices(u8, "\x1b[3;3~", res.getExtendedString("kDC3").?);
    try t.expectEqualSlices(u8, "\x1b[3;4~", res.getExtendedString("kDC4").?);
    try t.expectEqualSlices(u8, "\x1b[3;5~", res.getExtendedString("kDC5").?);
    try t.expectEqualSlices(u8, "\x1b[3;6~", res.getExtendedString("kDC6").?);
    try t.expectEqualSlices(u8, "\x1b[3;7~", res.getExtendedString("kDC7").?);
    try t.expectEqualSlices(u8, "\x1b[1;2B", res.getExtendedString("kDN").?);
    try t.expectEqualSlices(u8, "\x1b[1;3B", res.getExtendedString("kDN3").?);
    try t.expectEqualSlices(u8, "\x1b[1;4B", res.getExtendedString("kDN4").?);
    try t.expectEqualSlices(u8, "\x1b[1;5B", res.getExtendedString("kDN5").?);
    try t.expectEqualSlices(u8, "\x1b[1;6B", res.getExtendedString("kDN6").?);
    try t.expectEqualSlices(u8, "\x1b[1;7B", res.getExtendedString("kDN7").?);
    try t.expectEqualSlices(u8, "\x1b[1;3F", res.getExtendedString("kEND3").?);
    try t.expectEqualSlices(u8, "\x1b[1;4F", res.getExtendedString("kEND4").?);
    try t.expectEqualSlices(u8, "\x1b[1;5F", res.getExtendedString("kEND5").?);
    try t.expectEqualSlices(u8, "\x1b[1;6F", res.getExtendedString("kEND6").?);
    try t.expectEqualSlices(u8, "\x1b[1;7F", res.getExtendedString("kEND7").?);
    try t.expectEqualSlices(u8, "\x1b[1;3H", res.getExtendedString("kHOM3").?);
    try t.expectEqualSlices(u8, "\x1b[1;4H", res.getExtendedString("kHOM4").?);
    try t.expectEqualSlices(u8, "\x1b[1;5H", res.getExtendedString("kHOM5").?);
    try t.expectEqualSlices(u8, "\x1b[1;6H", res.getExtendedString("kHOM6").?);
    try t.expectEqualSlices(u8, "\x1b[1;7H", res.getExtendedString("kHOM7").?);
    try t.expectEqualSlices(u8, "\x1b[2;3~", res.getExtendedString("kIC3").?);
    try t.expectEqualSlices(u8, "\x1b[2;4~", res.getExtendedString("kIC4").?);
    try t.expectEqualSlices(u8, "\x1b[2;5~", res.getExtendedString("kIC5").?);
    try t.expectEqualSlices(u8, "\x1b[2;6~", res.getExtendedString("kIC6").?);
    try t.expectEqualSlices(u8, "\x1b[2;7~", res.getExtendedString("kIC7").?);
    try t.expectEqualSlices(u8, "\x1b[1;3D", res.getExtendedString("kLFT3").?);
    try t.expectEqualSlices(u8, "\x1b[1;4D", res.getExtendedString("kLFT4").?);
    try t.expectEqualSlices(u8, "\x1b[1;5D", res.getExtendedString("kLFT5").?);
    try t.expectEqualSlices(u8, "\x1b[1;6D", res.getExtendedString("kLFT6").?);
    try t.expectEqualSlices(u8, "\x1b[1;7D", res.getExtendedString("kLFT7").?);
    try t.expectEqualSlices(u8, "\x1b[6;3~", res.getExtendedString("kNXT3").?);
    try t.expectEqualSlices(u8, "\x1b[6;4~", res.getExtendedString("kNXT4").?);
    try t.expectEqualSlices(u8, "\x1b[6;5~", res.getExtendedString("kNXT5").?);
    try t.expectEqualSlices(u8, "\x1b[6;6~", res.getExtendedString("kNXT6").?);
    try t.expectEqualSlices(u8, "\x1b[6;7~", res.getExtendedString("kNXT7").?);
    try t.expectEqualSlices(u8, "\x1b[5;3~", res.getExtendedString("kPRV3").?);
    try t.expectEqualSlices(u8, "\x1b[5;4~", res.getExtendedString("kPRV4").?);
    try t.expectEqualSlices(u8, "\x1b[5;5~", res.getExtendedString("kPRV5").?);
    try t.expectEqualSlices(u8, "\x1b[5;6~", res.getExtendedString("kPRV6").?);
    try t.expectEqualSlices(u8, "\x1b[5;7~", res.getExtendedString("kPRV7").?);
    try t.expectEqualSlices(u8, "\x1b[1;3C", res.getExtendedString("kRIT3").?);
    try t.expectEqualSlices(u8, "\x1b[1;4C", res.getExtendedString("kRIT4").?);
    try t.expectEqualSlices(u8, "\x1b[1;5C", res.getExtendedString("kRIT5").?);
    try t.expectEqualSlices(u8, "\x1b[1;6C", res.getExtendedString("kRIT6").?);
    try t.expectEqualSlices(u8, "\x1b[1;7C", res.getExtendedString("kRIT7").?);
    try t.expectEqualSlices(u8, "\x1b[1;2A", res.getExtendedString("kUP").?);
    try t.expectEqualSlices(u8, "\x1b[1;3A", res.getExtendedString("kUP3").?);
    try t.expectEqualSlices(u8, "\x1b[1;4A", res.getExtendedString("kUP4").?);
    try t.expectEqualSlices(u8, "\x1b[1;5A", res.getExtendedString("kUP5").?);
    try t.expectEqualSlices(u8, "\x1b[1;6A", res.getExtendedString("kUP6").?);
    try t.expectEqualSlices(u8, "\x1b[1;7A", res.getExtendedString("kUP7").?);
    try t.expectEqualSlices(u8, "\x1b[29m", res.getExtendedString("rmxx").?);
    try t.expectEqualSlices(u8, "\x1b[9m", res.getExtendedString("smxx").?);
}

test "xterm" {
    const x = @embedFile("descriptions/x/xterm");
    const res = try parse(ta, x);
    defer res.destroy(ta);

    try t.expectEqualStrings("xterm|xterm terminal emulator (X Window System)", res.names);

    try t.expectEqual(false, res.getFlagCapability(.auto_left_margin));
    try t.expectEqual(true, res.getFlagCapability(.auto_right_margin));
    try t.expectEqual(false, res.getFlagCapability(.no_esc_ctlc));
    try t.expectEqual(false, res.getFlagCapability(.ceol_standout_glitch));
    try t.expectEqual(true, res.getFlagCapability(.eat_newline_glitch));
    try t.expectEqual(false, res.getFlagCapability(.erase_overstrike));
    try t.expectEqual(false, res.getFlagCapability(.generic_type));
    try t.expectEqual(false, res.getFlagCapability(.hard_copy));
    try t.expectEqual(true, res.getFlagCapability(.has_meta_key));
    try t.expectEqual(false, res.getFlagCapability(.has_status_line));
    try t.expectEqual(false, res.getFlagCapability(.insert_null_glitch));
    try t.expectEqual(false, res.getFlagCapability(.memory_above));
    try t.expectEqual(false, res.getFlagCapability(.memory_below));
    try t.expectEqual(true, res.getFlagCapability(.move_insert_mode));
    try t.expectEqual(true, res.getFlagCapability(.move_standout_mode));
    try t.expectEqual(false, res.getFlagCapability(.over_strike));
    try t.expectEqual(false, res.getFlagCapability(.status_line_esc_ok));
    try t.expectEqual(false, res.getFlagCapability(.dest_tabs_magic_smso));
    try t.expectEqual(false, res.getFlagCapability(.tilde_glitch));
    try t.expectEqual(false, res.getFlagCapability(.transparent_underline));
    try t.expectEqual(false, res.getFlagCapability(.xon_xoff));
    try t.expectEqual(false, res.getFlagCapability(.needs_xon_xoff));
    try t.expectEqual(true, res.getFlagCapability(.prtr_silent));
    try t.expectEqual(false, res.getFlagCapability(.hard_cursor));
    try t.expectEqual(false, res.getFlagCapability(.non_rev_rmcup));
    try t.expectEqual(true, res.getFlagCapability(.no_pad_char));
    try t.expectEqual(false, res.getFlagCapability(.non_dest_scroll_region));
    try t.expectEqual(false, res.getFlagCapability(.can_change));
    try t.expectEqual(true, res.getFlagCapability(.back_color_erase));
    try t.expectEqual(false, res.getFlagCapability(.hue_lightness_saturation));
    try t.expectEqual(false, res.getFlagCapability(.col_addr_glitch));
    try t.expectEqual(false, res.getFlagCapability(.cr_cancels_micro_mode));
    try t.expectEqual(false, res.getFlagCapability(.has_print_wheel));
    try t.expectEqual(false, res.getFlagCapability(.row_addr_glitch));
    try t.expectEqual(false, res.getFlagCapability(.semi_auto_right_margin));
    try t.expectEqual(false, res.getFlagCapability(.cpi_changes_res));
    try t.expectEqual(false, res.getFlagCapability(.lpi_changes_res));
    try t.expectEqual(true, res.getFlagCapability(.backspaces_with_bs));
    try t.expectEqual(false, res.getFlagCapability(.crt_no_scrolling));
    try t.expectEqual(false, res.getFlagCapability(.no_correctly_working_cr));
    try t.expectEqual(false, res.getFlagCapability(.gnu_has_meta_key));
    try t.expectEqual(false, res.getFlagCapability(.linefeed_is_newline));
    try t.expectEqual(false, res.getFlagCapability(.has_hardware_tabs));
    try t.expectEqual(false, res.getFlagCapability(.return_does_clr_eol));

    try t.expectEqual(80, res.getNumberCapability(.columns));
    try t.expectEqual(8, res.getNumberCapability(.init_tabs));
    try t.expectEqual(24, res.getNumberCapability(.lines));
    try t.expectEqual(null, res.getNumberCapability(.lines_of_memory));
    try t.expectEqual(null, res.getNumberCapability(.magic_cookie_glitch));
    try t.expectEqual(null, res.getNumberCapability(.padding_baud_rate));
    try t.expectEqual(null, res.getNumberCapability(.virtual_terminal));
    try t.expectEqual(null, res.getNumberCapability(.width_status_line));
    try t.expectEqual(null, res.getNumberCapability(.num_labels));
    try t.expectEqual(null, res.getNumberCapability(.label_height));
    try t.expectEqual(null, res.getNumberCapability(.label_width));
    try t.expectEqual(null, res.getNumberCapability(.max_attributes));
    try t.expectEqual(null, res.getNumberCapability(.maximum_windows));
    try t.expectEqual(8, res.getNumberCapability(.max_colors));
    try t.expectEqual(64, res.getNumberCapability(.max_pairs));
    try t.expectEqual(null, res.getNumberCapability(.no_color_video));
    try t.expectEqual(null, res.getNumberCapability(.buffer_capacity));
    try t.expectEqual(null, res.getNumberCapability(.dot_vert_spacing));
    try t.expectEqual(null, res.getNumberCapability(.dot_horz_spacing));
    try t.expectEqual(null, res.getNumberCapability(.max_micro_address));
    try t.expectEqual(null, res.getNumberCapability(.max_micro_jump));
    try t.expectEqual(null, res.getNumberCapability(.micro_col_size));
    try t.expectEqual(null, res.getNumberCapability(.micro_line_size));
    try t.expectEqual(null, res.getNumberCapability(.number_of_pins));
    try t.expectEqual(null, res.getNumberCapability(.output_res_char));
    try t.expectEqual(null, res.getNumberCapability(.output_res_line));
    try t.expectEqual(null, res.getNumberCapability(.output_res_horz_inch));
    try t.expectEqual(null, res.getNumberCapability(.output_res_vert_inch));
    try t.expectEqual(null, res.getNumberCapability(.print_rate));
    try t.expectEqual(null, res.getNumberCapability(.wide_char_size));
    try t.expectEqual(null, res.getNumberCapability(.buttons));
    try t.expectEqual(null, res.getNumberCapability(.bit_image_entwining));
    try t.expectEqual(null, res.getNumberCapability(.bit_image_type));
    try t.expectEqual(null, res.getNumberCapability(.magic_cookie_glitch_ul));
    try t.expectEqual(null, res.getNumberCapability(.carriage_return_delay));
    try t.expectEqual(null, res.getNumberCapability(.new_line_delay));
    try t.expectEqual(null, res.getNumberCapability(.backspace_delay));
    try t.expectEqual(null, res.getNumberCapability(.horizontal_tab_delay));
    try t.expectEqual(null, res.getNumberCapability(.number_of_function_keys));

    try t.expectEqualSlices(u8, "\x1b[Z", res.getStringCapability(.back_tab).?);
    try t.expectEqualSlices(u8, "\x07", res.getStringCapability(.bell).?);
    try t.expectEqualSlices(u8, "\x0d", res.getStringCapability(.carriage_return).?);
    try t.expectEqualSlices(u8, "\x1b[%i%p1%d;%p2%dr", res.getStringCapability(.change_scroll_region).?);
    try t.expectEqualSlices(u8, "\x1b[3g", res.getStringCapability(.clear_all_tabs).?);
    try t.expectEqualSlices(u8, "\x1b[H\x1b[2J", res.getStringCapability(.clear_screen).?);
    try t.expectEqualSlices(u8, "\x1b[K", res.getStringCapability(.clr_eol).?);
    try t.expectEqualSlices(u8, "\x1b[J", res.getStringCapability(.clr_eos).?);
    try t.expectEqualSlices(u8, "\x1b[%i%p1%dG", res.getStringCapability(.column_address).?);
    try t.expectEqualSlices(u8, "\x1b[%i%p1%d;%p2%dH", res.getStringCapability(.cursor_address).?);
    try t.expectEqualSlices(u8, "\x0a", res.getStringCapability(.cursor_down).?);
    try t.expectEqualSlices(u8, "\x1b[H", res.getStringCapability(.cursor_home).?);
    try t.expectEqualSlices(u8, "\x1b[?25l", res.getStringCapability(.cursor_invisible).?);
    try t.expectEqualSlices(u8, "\x08", res.getStringCapability(.cursor_left).?);
    try t.expectEqualSlices(u8, "\x1b[?12l\x1b[?25h", res.getStringCapability(.cursor_normal).?);
    try t.expectEqualSlices(u8, "\x1b[C", res.getStringCapability(.cursor_right).?);
    try t.expectEqualSlices(u8, "\x1b[A", res.getStringCapability(.cursor_up).?);
    try t.expectEqualSlices(u8, "\x1b[?12;25h", res.getStringCapability(.cursor_visible).?);
    try t.expectEqualSlices(u8, "\x1b[P", res.getStringCapability(.delete_character).?);
    try t.expectEqualSlices(u8, "\x1b[M", res.getStringCapability(.delete_line).?);
    try t.expectEqualSlices(u8, "\x1b(0", res.getStringCapability(.enter_alt_charset_mode).?);
    try t.expectEqualSlices(u8, "\x1b[5m", res.getStringCapability(.enter_blink_mode).?);
    try t.expectEqualSlices(u8, "\x1b[1m", res.getStringCapability(.enter_bold_mode).?);
    try t.expectEqualSlices(u8, "\x1b[?1049h\x1b[22;0;0t", res.getStringCapability(.enter_ca_mode).?);
    try t.expectEqualSlices(u8, "\x1b[2m", res.getStringCapability(.enter_dim_mode).?);
    try t.expectEqualSlices(u8, "\x1b[4h", res.getStringCapability(.enter_insert_mode).?);
    try t.expectEqualSlices(u8, "\x1b[8m", res.getStringCapability(.enter_secure_mode).?);
    try t.expectEqualSlices(u8, "\x1b[7m", res.getStringCapability(.enter_reverse_mode).?);
    try t.expectEqualSlices(u8, "\x1b[7m", res.getStringCapability(.enter_standout_mode).?);
    try t.expectEqualSlices(u8, "\x1b[4m", res.getStringCapability(.enter_underline_mode).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dX", res.getStringCapability(.erase_chars).?);
    try t.expectEqualSlices(u8, "\x1b(B", res.getStringCapability(.exit_alt_charset_mode).?);
    try t.expectEqualSlices(u8, "\x1b(B\x1b[m", res.getStringCapability(.exit_attribute_mode).?);
    try t.expectEqualSlices(u8, "\x1b[?1049l\x1b[23;0;0t", res.getStringCapability(.exit_ca_mode).?);
    try t.expectEqualSlices(u8, "\x1b[4l", res.getStringCapability(.exit_insert_mode).?);
    try t.expectEqualSlices(u8, "\x1b[27m", res.getStringCapability(.exit_standout_mode).?);
    try t.expectEqualSlices(u8, "\x1b[24m", res.getStringCapability(.exit_underline_mode).?);
    try t.expectEqualSlices(u8, "\x1b[?5h$<100/>\x1b[?5l", res.getStringCapability(.flash_screen).?);
    try t.expectEqualSlices(u8, "\x1b[!p\x1b[?3;4l\x1b[4l\x1b>", res.getStringCapability(.init_2string).?);
    try t.expectEqualSlices(u8, "\x1b[L", res.getStringCapability(.insert_line).?);
    try t.expectEqualSlices(u8, "\x08", res.getStringCapability(.key_backspace).?);
    try t.expectEqualSlices(u8, "\x1b[3~", res.getStringCapability(.key_dc).?);
    try t.expectEqualSlices(u8, "\x1bOB", res.getStringCapability(.key_down).?);
    try t.expectEqualSlices(u8, "\x1bOP", res.getStringCapability(.key_f1).?);
    try t.expectEqualSlices(u8, "\x1b[21~", res.getStringCapability(.key_f10).?);
    try t.expectEqualSlices(u8, "\x1bOQ", res.getStringCapability(.key_f2).?);
    try t.expectEqualSlices(u8, "\x1bOR", res.getStringCapability(.key_f3).?);
    try t.expectEqualSlices(u8, "\x1bOS", res.getStringCapability(.key_f4).?);
    try t.expectEqualSlices(u8, "\x1b[15~", res.getStringCapability(.key_f5).?);
    try t.expectEqualSlices(u8, "\x1b[17~", res.getStringCapability(.key_f6).?);
    try t.expectEqualSlices(u8, "\x1b[18~", res.getStringCapability(.key_f7).?);
    try t.expectEqualSlices(u8, "\x1b[19~", res.getStringCapability(.key_f8).?);
    try t.expectEqualSlices(u8, "\x1b[20~", res.getStringCapability(.key_f9).?);
    try t.expectEqualSlices(u8, "\x1bOH", res.getStringCapability(.key_home).?);
    try t.expectEqualSlices(u8, "\x1b[2~", res.getStringCapability(.key_ic).?);
    try t.expectEqualSlices(u8, "\x1bOD", res.getStringCapability(.key_left).?);
    try t.expectEqualSlices(u8, "\x1b[6~", res.getStringCapability(.key_npage).?);
    try t.expectEqualSlices(u8, "\x1b[5~", res.getStringCapability(.key_ppage).?);
    try t.expectEqualSlices(u8, "\x1bOC", res.getStringCapability(.key_right).?);
    try t.expectEqualSlices(u8, "\x1b[1;2B", res.getStringCapability(.key_sf).?);
    try t.expectEqualSlices(u8, "\x1b[1;2A", res.getStringCapability(.key_sr).?);
    try t.expectEqualSlices(u8, "\x1bOA", res.getStringCapability(.key_up).?);
    try t.expectEqualSlices(u8, "\x1b[?1l\x1b>", res.getStringCapability(.keypad_local).?);
    try t.expectEqualSlices(u8, "\x1b[?1h\x1b=", res.getStringCapability(.keypad_xmit).?);
    try t.expectEqualSlices(u8, "\x1b[?1034l", res.getStringCapability(.meta_off).?);
    try t.expectEqualSlices(u8, "\x1b[?1034h", res.getStringCapability(.meta_on).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dP", res.getStringCapability(.parm_dch).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dM", res.getStringCapability(.parm_delete_line).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dB", res.getStringCapability(.parm_down_cursor).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%d@", res.getStringCapability(.parm_ich).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dS", res.getStringCapability(.parm_index).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dL", res.getStringCapability(.parm_insert_line).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dD", res.getStringCapability(.parm_left_cursor).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dC", res.getStringCapability(.parm_right_cursor).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dT", res.getStringCapability(.parm_rindex).?);
    try t.expectEqualSlices(u8, "\x1b[%p1%dA", res.getStringCapability(.parm_up_cursor).?);
    try t.expectEqualSlices(u8, "\x1b[i", res.getStringCapability(.print_screen).?);
    try t.expectEqualSlices(u8, "\x1b[4i", res.getStringCapability(.prtr_off).?);
    try t.expectEqualSlices(u8, "\x1b[5i", res.getStringCapability(.prtr_on).?);
    try t.expectEqualSlices(u8, "%p1%c\x1b[%p2%{1}%-%db", res.getStringCapability(.repeat_char).?);
    try t.expectEqualSlices(u8, "\x1bc", res.getStringCapability(.reset_1string).?);
    try t.expectEqualSlices(u8, "\x1b[!p\x1b[?3;4l\x1b[4l\x1b>", res.getStringCapability(.reset_2string).?);
    try t.expectEqualSlices(u8, "\x1b8", res.getStringCapability(.restore_cursor).?);
    try t.expectEqualSlices(u8, "\x1b[%i%p1%dd", res.getStringCapability(.row_address).?);
    try t.expectEqualSlices(u8, "\x1b7", res.getStringCapability(.save_cursor).?);
    try t.expectEqualSlices(u8, "\x0a", res.getStringCapability(.scroll_forward).?);
    try t.expectEqualSlices(u8, "\x1bM", res.getStringCapability(.scroll_reverse).?);
    try t.expectEqualSlices(u8, "%?%p9%t\x1b(0%e\x1b(B%;\x1b[0%?%p6%t;1%;%?%p5%t;2%;%?%p2%t;4%;%?%p1%p3%|%t;7%;%?%p4%t;5%;%?%p7%t;8%;m", res.getStringCapability(.set_attributes).?);
    try t.expectEqualSlices(u8, "\x1bH", res.getStringCapability(.set_tab).?);
    try t.expectEqualSlices(u8, "\x09", res.getStringCapability(.tab).?);
    try t.expectEqualSlices(u8, "\x1bOE", res.getStringCapability(.key_b2).?);
    try t.expectEqualSlices(u8, "``aaffggiijjkkllmmnnooppqqrrssttuuvvwwxxyyzz{{||}}~~", res.getStringCapability(.acs_chars).?);
    try t.expectEqualSlices(u8, "\x1b[Z", res.getStringCapability(.key_btab).?);
    try t.expectEqualSlices(u8, "\x1b[?7h", res.getStringCapability(.enter_am_mode).?);
    try t.expectEqualSlices(u8, "\x1b[?7l", res.getStringCapability(.exit_am_mode).?);
    try t.expectEqualSlices(u8, "\x1bOF", res.getStringCapability(.key_end).?);
    try t.expectEqualSlices(u8, "\x1bOM", res.getStringCapability(.key_enter).?);
    try t.expectEqualSlices(u8, "\x1b[3;2~", res.getStringCapability(.key_sdc).?);
    try t.expectEqualSlices(u8, "\x1b[1;2F", res.getStringCapability(.key_send).?);
    try t.expectEqualSlices(u8, "\x1b[1;2H", res.getStringCapability(.key_shome).?);
    try t.expectEqualSlices(u8, "\x1b[2;2~", res.getStringCapability(.key_sic).?);
    try t.expectEqualSlices(u8, "\x1b[1;2D", res.getStringCapability(.key_sleft).?);
    try t.expectEqualSlices(u8, "\x1b[6;2~", res.getStringCapability(.key_snext).?);
    try t.expectEqualSlices(u8, "\x1b[5;2~", res.getStringCapability(.key_sprevious).?);
    try t.expectEqualSlices(u8, "\x1b[1;2C", res.getStringCapability(.key_sright).?);
    try t.expectEqualSlices(u8, "\x1b[23~", res.getStringCapability(.key_f11).?);
    try t.expectEqualSlices(u8, "\x1b[24~", res.getStringCapability(.key_f12).?);
    try t.expectEqualSlices(u8, "\x1b[1;2P", res.getStringCapability(.key_f13).?);
    try t.expectEqualSlices(u8, "\x1b[1;2Q", res.getStringCapability(.key_f14).?);
    try t.expectEqualSlices(u8, "\x1b[1;2R", res.getStringCapability(.key_f15).?);
    try t.expectEqualSlices(u8, "\x1b[1;2S", res.getStringCapability(.key_f16).?);
    try t.expectEqualSlices(u8, "\x1b[15;2~", res.getStringCapability(.key_f17).?);
    try t.expectEqualSlices(u8, "\x1b[17;2~", res.getStringCapability(.key_f18).?);
    try t.expectEqualSlices(u8, "\x1b[18;2~", res.getStringCapability(.key_f19).?);
    try t.expectEqualSlices(u8, "\x1b[19;2~", res.getStringCapability(.key_f20).?);
    try t.expectEqualSlices(u8, "\x1b[20;2~", res.getStringCapability(.key_f21).?);
    try t.expectEqualSlices(u8, "\x1b[21;2~", res.getStringCapability(.key_f22).?);
    try t.expectEqualSlices(u8, "\x1b[23;2~", res.getStringCapability(.key_f23).?);
    try t.expectEqualSlices(u8, "\x1b[24;2~", res.getStringCapability(.key_f24).?);
    try t.expectEqualSlices(u8, "\x1b[1;5P", res.getStringCapability(.key_f25).?);
    try t.expectEqualSlices(u8, "\x1b[1;5Q", res.getStringCapability(.key_f26).?);
    try t.expectEqualSlices(u8, "\x1b[1;5R", res.getStringCapability(.key_f27).?);
    try t.expectEqualSlices(u8, "\x1b[1;5S", res.getStringCapability(.key_f28).?);
    try t.expectEqualSlices(u8, "\x1b[15;5~", res.getStringCapability(.key_f29).?);
    try t.expectEqualSlices(u8, "\x1b[17;5~", res.getStringCapability(.key_f30).?);
    try t.expectEqualSlices(u8, "\x1b[18;5~", res.getStringCapability(.key_f31).?);
    try t.expectEqualSlices(u8, "\x1b[19;5~", res.getStringCapability(.key_f32).?);
    try t.expectEqualSlices(u8, "\x1b[20;5~", res.getStringCapability(.key_f33).?);
    try t.expectEqualSlices(u8, "\x1b[21;5~", res.getStringCapability(.key_f34).?);
    try t.expectEqualSlices(u8, "\x1b[23;5~", res.getStringCapability(.key_f35).?);
    try t.expectEqualSlices(u8, "\x1b[24;5~", res.getStringCapability(.key_f36).?);
    try t.expectEqualSlices(u8, "\x1b[1;6P", res.getStringCapability(.key_f37).?);
    try t.expectEqualSlices(u8, "\x1b[1;6Q", res.getStringCapability(.key_f38).?);
    try t.expectEqualSlices(u8, "\x1b[1;6R", res.getStringCapability(.key_f39).?);
    try t.expectEqualSlices(u8, "\x1b[1;6S", res.getStringCapability(.key_f40).?);
    try t.expectEqualSlices(u8, "\x1b[15;6~", res.getStringCapability(.key_f41).?);
    try t.expectEqualSlices(u8, "\x1b[17;6~", res.getStringCapability(.key_f42).?);
    try t.expectEqualSlices(u8, "\x1b[18;6~", res.getStringCapability(.key_f43).?);
    try t.expectEqualSlices(u8, "\x1b[19;6~", res.getStringCapability(.key_f44).?);
    try t.expectEqualSlices(u8, "\x1b[20;6~", res.getStringCapability(.key_f45).?);
    try t.expectEqualSlices(u8, "\x1b[21;6~", res.getStringCapability(.key_f46).?);
    try t.expectEqualSlices(u8, "\x1b[23;6~", res.getStringCapability(.key_f47).?);
    try t.expectEqualSlices(u8, "\x1b[24;6~", res.getStringCapability(.key_f48).?);
    try t.expectEqualSlices(u8, "\x1b[1;3P", res.getStringCapability(.key_f49).?);
    try t.expectEqualSlices(u8, "\x1b[1;3Q", res.getStringCapability(.key_f50).?);
    try t.expectEqualSlices(u8, "\x1b[1;3R", res.getStringCapability(.key_f51).?);
    try t.expectEqualSlices(u8, "\x1b[1;3S", res.getStringCapability(.key_f52).?);
    try t.expectEqualSlices(u8, "\x1b[15;3~", res.getStringCapability(.key_f53).?);
    try t.expectEqualSlices(u8, "\x1b[17;3~", res.getStringCapability(.key_f54).?);
    try t.expectEqualSlices(u8, "\x1b[18;3~", res.getStringCapability(.key_f55).?);
    try t.expectEqualSlices(u8, "\x1b[19;3~", res.getStringCapability(.key_f56).?);
    try t.expectEqualSlices(u8, "\x1b[20;3~", res.getStringCapability(.key_f57).?);
    try t.expectEqualSlices(u8, "\x1b[21;3~", res.getStringCapability(.key_f58).?);
    try t.expectEqualSlices(u8, "\x1b[23;3~", res.getStringCapability(.key_f59).?);
    try t.expectEqualSlices(u8, "\x1b[24;3~", res.getStringCapability(.key_f60).?);
    try t.expectEqualSlices(u8, "\x1b[1;4P", res.getStringCapability(.key_f61).?);
    try t.expectEqualSlices(u8, "\x1b[1;4Q", res.getStringCapability(.key_f62).?);
    try t.expectEqualSlices(u8, "\x1b[1;4R", res.getStringCapability(.key_f63).?);
    try t.expectEqualSlices(u8, "\x1b[1K", res.getStringCapability(.clr_bol).?);
    try t.expectEqualSlices(u8, "\x1b[%i%d;%dR", res.getStringCapability(.user6).?);
    try t.expectEqualSlices(u8, "\x1b[6n", res.getStringCapability(.user7).?);
    try t.expectEqualSlices(u8, "\x1b[?%[;0123456789]c", res.getStringCapability(.user8).?);
    try t.expectEqualSlices(u8, "\x1b[c", res.getStringCapability(.user9).?);
    try t.expectEqualSlices(u8, "\x1b[39;49m", res.getStringCapability(.orig_pair).?);
    try t.expectEqualSlices(u8, "\x1b[3%?%p1%{1}%=%t4%e%p1%{3}%=%t6%e%p1%{4}%=%t1%e%p1%{6}%=%t3%e%p1%d%;m", res.getStringCapability(.set_foreground).?);
    try t.expectEqualSlices(u8, "\x1b[4%?%p1%{1}%=%t4%e%p1%{3}%=%t6%e%p1%{4}%=%t1%e%p1%{6}%=%t3%e%p1%d%;m", res.getStringCapability(.set_background).?);
    try t.expectEqualSlices(u8, "\x1b[3m", res.getStringCapability(.enter_italics_mode).?);
    try t.expectEqualSlices(u8, "\x1b[23m", res.getStringCapability(.exit_italics_mode).?);
    try t.expectEqualSlices(u8, "\x1b[<", res.getStringCapability(.key_mouse).?);
    try t.expectEqualSlices(u8, "\x1b[3%p1%dm", res.getStringCapability(.set_a_foreground).?);
    try t.expectEqualSlices(u8, "\x1b[4%p1%dm", res.getStringCapability(.set_a_background).?);
    try t.expectEqualSlices(u8, "\x1bl", res.getStringCapability(.memory_lock).?);
    try t.expectEqualSlices(u8, "\x1bm", res.getStringCapability(.memory_unlock).?);

    try t.expectEqual(null, res.getStringCapability(.command_character));
    try t.expectEqual(null, res.getStringCapability(.cursor_mem_address));
    try t.expectEqual(null, res.getStringCapability(.cursor_to_ll));
    try t.expectEqual(null, res.getStringCapability(.dis_status_line));
    try t.expectEqual(null, res.getStringCapability(.enter_delete_mode));
    try t.expectEqual(null, res.getStringCapability(.enter_protected_mode));
    try t.expectEqual(null, res.getStringCapability(.exit_delete_mode));
    try t.expectEqual(null, res.getStringCapability(.form_feed));
    try t.expectEqual(null, res.getStringCapability(.init_3string));
    try t.expectEqual(null, res.getStringCapability(.insert_padding));
    try t.expectEqual(null, res.getStringCapability(.key_catab));
    try t.expectEqual(null, res.getStringCapability(.key_dl));
    try t.expectEqual(null, res.getStringCapability(.key_eic));
    try t.expectEqual(null, res.getStringCapability(.key_il));
    try t.expectEqual(null, res.getStringCapability(.key_ll));
    try t.expectEqual(null, res.getStringCapability(.key_stab));
    try t.expectEqual(null, res.getStringCapability(.lab_f0));
    try t.expectEqual(null, res.getStringCapability(.newline));
    try t.expectEqual(null, res.getStringCapability(.pkey_key));
    try t.expectEqual(null, res.getStringCapability(.reset_3string));
    try t.expectEqual(null, res.getStringCapability(.set_window));
    try t.expectEqual(null, res.getStringCapability(.to_status_line));
    try t.expectEqual(null, res.getStringCapability(.key_c1));
    try t.expectEqual(null, res.getStringCapability(.plab_norm));
    try t.expectEqual(null, res.getStringCapability(.enter_xon_mode));
    try t.expectEqual(null, res.getStringCapability(.xon_character));
    try t.expectEqual(null, res.getStringCapability(.key_exit));
    try t.expectEqual(null, res.getStringCapability(.key_sdl));
    try t.expectEqual(null, res.getStringCapability(.key_seol));
    try t.expectEqual(null, res.getStringCapability(.key_smessage));
    try t.expectEqual(null, res.getStringCapability(.key_soptions));
    try t.expectEqual(null, res.getStringCapability(.key_sprint));
    try t.expectEqual(null, res.getStringCapability(.key_srsume));
    try t.expectEqual(null, res.getStringCapability(.clear_margins));
    try t.expectEqual(null, res.getStringCapability(.orig_colors));
    try t.expectEqual(null, res.getStringCapability(.change_char_pitch));
    try t.expectEqual(null, res.getStringCapability(.enter_leftward_mode));
    try t.expectEqual(null, res.getStringCapability(.exit_leftward_mode));
    try t.expectEqual(null, res.getStringCapability(.mouse_info));
    try t.expectEqual(null, res.getStringCapability(.pkey_plab));
    try t.expectEqual(null, res.getStringCapability(.box_chars_1));

    try t.expectEqual(3, res.ext_flags.size);
    try t.expectEqual(true, res.getExtendedFlag("AX"));
    try t.expectEqual(true, res.getExtendedFlag("G0"));
    try t.expectEqual(true, res.getExtendedFlag("XT"));

    try t.expectEqual(0, res.ext_nums.size);

    try t.expectEqual(62, res.ext_strs.size);

    try t.expectEqualSlices(u8, "\x1b]112\x07", res.getExtendedString("Cr").?);
    try t.expectEqualSlices(u8, "\x1b]12;%p1%s\x07", res.getExtendedString("Cs").?);
    try t.expectEqualSlices(u8, "\x1b[3J", res.getExtendedString("E3").?);
    try t.expectEqualSlices(u8, "\x1b]52;%p1%s;%p2%s\x07", res.getExtendedString("Ms").?);
    try t.expectEqualSlices(u8, "\x1b[2 q", res.getExtendedString("Se").?);
    try t.expectEqualSlices(u8, "\x1b[%p1%d q", res.getExtendedString("Ss").?);
    try t.expectEqualSlices(u8, "\x1b[?1006;1000%?%p1%{1}%=%th%el%;", res.getExtendedString("XM").?);
    try t.expectEqualSlices(u8, "\x1b[3;3~", res.getExtendedString("kDC3").?);
    try t.expectEqualSlices(u8, "\x1b[3;4~", res.getExtendedString("kDC4").?);
    try t.expectEqualSlices(u8, "\x1b[3;5~", res.getExtendedString("kDC5").?);
    try t.expectEqualSlices(u8, "\x1b[3;6~", res.getExtendedString("kDC6").?);
    try t.expectEqualSlices(u8, "\x1b[3;7~", res.getExtendedString("kDC7").?);
    try t.expectEqualSlices(u8, "\x1b[1;2B", res.getExtendedString("kDN").?);
    try t.expectEqualSlices(u8, "\x1b[1;3B", res.getExtendedString("kDN3").?);
    try t.expectEqualSlices(u8, "\x1b[1;4B", res.getExtendedString("kDN4").?);
    try t.expectEqualSlices(u8, "\x1b[1;5B", res.getExtendedString("kDN5").?);
    try t.expectEqualSlices(u8, "\x1b[1;6B", res.getExtendedString("kDN6").?);
    try t.expectEqualSlices(u8, "\x1b[1;7B", res.getExtendedString("kDN7").?);
    try t.expectEqualSlices(u8, "\x1b[1;3F", res.getExtendedString("kEND3").?);
    try t.expectEqualSlices(u8, "\x1b[1;4F", res.getExtendedString("kEND4").?);
    try t.expectEqualSlices(u8, "\x1b[1;5F", res.getExtendedString("kEND5").?);
    try t.expectEqualSlices(u8, "\x1b[1;6F", res.getExtendedString("kEND6").?);
    try t.expectEqualSlices(u8, "\x1b[1;7F", res.getExtendedString("kEND7").?);
    try t.expectEqualSlices(u8, "\x1b[1;3H", res.getExtendedString("kHOM3").?);
    try t.expectEqualSlices(u8, "\x1b[1;4H", res.getExtendedString("kHOM4").?);
    try t.expectEqualSlices(u8, "\x1b[1;5H", res.getExtendedString("kHOM5").?);
    try t.expectEqualSlices(u8, "\x1b[1;6H", res.getExtendedString("kHOM6").?);
    try t.expectEqualSlices(u8, "\x1b[1;7H", res.getExtendedString("kHOM7").?);
    try t.expectEqualSlices(u8, "\x1b[2;3~", res.getExtendedString("kIC3").?);
    try t.expectEqualSlices(u8, "\x1b[2;4~", res.getExtendedString("kIC4").?);
    try t.expectEqualSlices(u8, "\x1b[2;5~", res.getExtendedString("kIC5").?);
    try t.expectEqualSlices(u8, "\x1b[2;6~", res.getExtendedString("kIC6").?);
    try t.expectEqualSlices(u8, "\x1b[2;7~", res.getExtendedString("kIC7").?);
    try t.expectEqualSlices(u8, "\x1b[1;3D", res.getExtendedString("kLFT3").?);
    try t.expectEqualSlices(u8, "\x1b[1;4D", res.getExtendedString("kLFT4").?);
    try t.expectEqualSlices(u8, "\x1b[1;5D", res.getExtendedString("kLFT5").?);
    try t.expectEqualSlices(u8, "\x1b[1;6D", res.getExtendedString("kLFT6").?);
    try t.expectEqualSlices(u8, "\x1b[1;7D", res.getExtendedString("kLFT7").?);
    try t.expectEqualSlices(u8, "\x1b[6;3~", res.getExtendedString("kNXT3").?);
    try t.expectEqualSlices(u8, "\x1b[6;4~", res.getExtendedString("kNXT4").?);
    try t.expectEqualSlices(u8, "\x1b[6;5~", res.getExtendedString("kNXT5").?);
    try t.expectEqualSlices(u8, "\x1b[6;6~", res.getExtendedString("kNXT6").?);
    try t.expectEqualSlices(u8, "\x1b[6;7~", res.getExtendedString("kNXT7").?);
    try t.expectEqualSlices(u8, "\x1b[5;3~", res.getExtendedString("kPRV3").?);
    try t.expectEqualSlices(u8, "\x1b[5;4~", res.getExtendedString("kPRV4").?);
    try t.expectEqualSlices(u8, "\x1b[5;5~", res.getExtendedString("kPRV5").?);
    try t.expectEqualSlices(u8, "\x1b[5;6~", res.getExtendedString("kPRV6").?);
    try t.expectEqualSlices(u8, "\x1b[5;7~", res.getExtendedString("kPRV7").?);
    try t.expectEqualSlices(u8, "\x1b[1;3C", res.getExtendedString("kRIT3").?);
    try t.expectEqualSlices(u8, "\x1b[1;4C", res.getExtendedString("kRIT4").?);
    try t.expectEqualSlices(u8, "\x1b[1;5C", res.getExtendedString("kRIT5").?);
    try t.expectEqualSlices(u8, "\x1b[1;6C", res.getExtendedString("kRIT6").?);
    try t.expectEqualSlices(u8, "\x1b[1;7C", res.getExtendedString("kRIT7").?);
    try t.expectEqualSlices(u8, "\x1b[1;2A", res.getExtendedString("kUP").?);
    try t.expectEqualSlices(u8, "\x1b[1;3A", res.getExtendedString("kUP3").?);
    try t.expectEqualSlices(u8, "\x1b[1;4A", res.getExtendedString("kUP4").?);
    try t.expectEqualSlices(u8, "\x1b[1;5A", res.getExtendedString("kUP5").?);
    try t.expectEqualSlices(u8, "\x1b[1;6A", res.getExtendedString("kUP6").?);
    try t.expectEqualSlices(u8, "\x1b[1;7A", res.getExtendedString("kUP7").?);
    try t.expectEqualSlices(u8, "\x1b[29m", res.getExtendedString("rmxx").?);
    try t.expectEqualSlices(u8, "\x1b[9m", res.getExtendedString("smxx").?);
    try t.expectEqualSlices(u8, "\x1b[<%p1%d;%p2%d;%p3%d;%?%p4%tM%em%;", res.getExtendedString("xm").?);

    try t.expectEqual(null, res.getExtendedString("E0"));
    try t.expectEqual(null, res.getExtendedString("S0"));
    try t.expectEqual(null, res.getExtendedString("TS"));
    try t.expectEqual(null, res.getExtendedString("grbom"));
    try t.expectEqual(null, res.getExtendedString("gsbom"));
    try t.expectEqual(null, res.getExtendedString("kEND8"));
    try t.expectEqual(null, res.getExtendedString("kHOM8"));
    try t.expectEqual(null, res.getExtendedString("ka2"));
    try t.expectEqual(null, res.getExtendedString("kb1"));
    try t.expectEqual(null, res.getExtendedString("kb3"));
    try t.expectEqual(null, res.getExtendedString("kc2"));
}

// test "findTermInfoPath" {
//     {
//         const file = openTermInfoFile("st-256color").?;
//         defer file.close();

//         const buf = try file.readToEndAlloc(std.testing.allocator, 32768);
//         defer std.testing.allocator.free(buf);
//         const res = try parse(ta, buf);
//         defer res.destroy(ta);
//         try t.expectEqualStrings("st-256color| simpleterm with 256 colors", res.names);
//     }

//     {
//         const file = openTermInfoFile("xterm-256color").?;
//         defer file.close();

//         const buf = try file.readToEndAlloc(std.testing.allocator, 32768);
//         defer std.testing.allocator.free(buf);
//         const res = try parse(ta, buf);
//         defer res.destroy(ta);
//         try t.expectEqualStrings("xterm-256color|xterm with 256 colors", res.names);
//     }

//     try t.expectEqual(@as(?std.fs.File, null), openTermInfoFile(""));
//     try t.expectEqual(@as(?std.fs.File, null), openTermInfoFile("Non-extant-terminal :)"));
// }

const ParamTestContext = struct {
    list: std.ArrayList(u8) = std.ArrayList(u8).init(t.allocator),

    fn testParam(ctx: *ParamTestContext, expected: anytype, sequence: []const u8, args: anytype) !void {
        ctx.list.clearRetainingCapacity();
        const res = validateParamSequence(sequence, args.len);
        if (comptime util.isZigString(@TypeOf(expected))) {
            try res;
            try writeParamSequence(sequence, ctx.list.writer(), args);
            try t.expectEqualSlices(u8, expected, ctx.list.items);
        } else {
            try t.expectError(expected, res);
        }
    }
};

test "param string" {
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();

    try validateParamSequence("", 9);
    try validateParamSequence("%p1%4d", 1);
    try t.expectError(error.InvalidSpecifier, validateParamSequence("%pp", 1));
    try t.expectError(error.UnexpectedEndOfInput, validateParamSequence("%p", 1));
    try t.expectError(error.UnexpectedEndOfInput, validateParamSequence("%P", 1));
    try t.expectError(error.UnexpectedEndOfInput, validateParamSequence("%g", 1));
    try t.expectError(error.UnexpectedEndOfInput, validateParamSequence("%:", 1));
    try t.expectError(error.UnexpectedEndOfInput, validateParamSequence("%:-", 1));
    try t.expectError(error.UnexpectedEndOfInput, validateParamSequence("%:+", 1));
    try t.expectError(error.UnexpectedEndOfInput, validateParamSequence("%:+#", 1));
    try t.expectError(error.UnexpectedEndOfInput, validateParamSequence("%:+#1", 1));
    try t.expectError(error.UnexpectedEndOfInput, validateParamSequence("%:+#1.", 1));
    try t.expectError(error.UnexpectedEndOfInput, validateParamSequence("%:+#1.", 1));
    try t.expectError(error.UnexpectedEndOfInput, validateParamSequence("%#1", 1));
    try t.expectError(error.UnexpectedEndOfInput, validateParamSequence("%#1", 1));
}

test "Param sequence: non-formatted printing" {
    var c: ParamTestContext = .{};
    defer c.list.deinit();

    try c.testParam("", "", .{});
    try c.testParam("this is epic", "this is epic", .{});
    try c.testParam("% this is epic %", "%% this is epic %%", .{});
}

test "Param sequence: number literals" {
    var c: ParamTestContext = .{};
    defer c.list.deinit();

    try c.testParam("10", "%{10}%d", .{});
    try c.testParam("5", "%{5}%d", .{});
    try c.testParam("-1", "%{-1}%d", .{});
    try c.testParam("0", "%{0}%d", .{});
    try c.testParam("100000", "%{100000}%d", .{});
    try c.testParam(error.InvalidSpecifier, "%{}%d", .{});
    try c.testParam(error.InvalidSpecifier, "%{-}%d", .{});
    try c.testParam(error.InvalidSpecifier, "%{abc}%d", .{});
    try c.testParam(error.Overflow, "%{10000000000}%d", .{});
    try c.testParam(error.Overflow, "%{-10000000000}%d", .{});
}

test "Param sequence: character literals" {
    var c: ParamTestContext = .{};
    defer c.list.deinit();

    try c.testParam(" ", "%' '%c", .{});
    try c.testParam("x", "%'x'%c", .{});
    try c.testParam("65", "%'A'%d", .{});
    try c.testParam("32", "%' '%d", .{});
    try c.testParam(error.InvalidSpecifier, "%'ab'%c", .{});
    try c.testParam(error.InvalidSpecifier, "%''%c", .{});
}

test "Param sequence: push positional" {
    var c: ParamTestContext = .{};
    defer c.list.deinit();

    try c.testParam("39", "%p1%d", .{39});
    try c.testParam("-1", "%p1%d", .{-1});
    try c.testParam("10", "%p2%d", .{ -1, 10 });
    try c.testParam("17", "%p9%d", .{ 1, 3, 5, 7, 9, 11, 13, 15, 17 });

    // Interpreted as %p1 followed by a literal 0,
    // as only 1-9 are allowed following %p
    try c.testParam("0-1", "%p10%d", .{ -1, 5 });
}

test "Param sequence: set/get dynamic variables" {
    var c: ParamTestContext = .{};
    defer c.list.deinit();

    for ('a'..'z') |i| {
        const ch: u8 = @intCast(i);
        try c.testParam("10101", "%p1%P" ++ .{ch} ++ "%g" ++ .{ch} ++ "%d", .{10101});
        // Make sure unset variables are zeroed
        try c.testParam("0", "%ga%d", .{10101});
    }
}

test "Param sequence: set/get static variables" {
    var c: ParamTestContext = .{};
    defer c.list.deinit();

    for ('A'..'Z') |i| {
        const ch: u8 = @intCast(i);
        try c.testParam("10101", "%p1%P" ++ .{ch} ++ "%g" ++ .{ch} ++ "%d", .{10101});
        // Make sure unset variables are zeroed
        try c.testParam("0", "%gA%d", .{10101});
    }
}

test "Param sequence: hexadecimal" {
    var c: ParamTestContext = .{};
    defer c.list.deinit();

    try c.testParam("0", "%p1%x", .{0x0});
    try c.testParam("0", "%p1%X", .{0x0});

    try c.testParam("dab", "%p1%x", .{0xdab});
    try c.testParam("DAB", "%p1%X", .{0xDAB});

    try c.testParam("f", "%p1%x", .{0xf});
    try c.testParam("F", "%p1%X", .{0xF});

    try c.testParam("10", "%p1%x", .{0x10});
    try c.testParam("10", "%p1%X", .{0x10});
}

test "Param sequence: octal" {
    var c: ParamTestContext = .{};
    defer c.list.deinit();

    try c.testParam("0", "%p1%o", .{0o0});
    try c.testParam("1", "%p1%o", .{0o1});
    try c.testParam("7", "%p1%o", .{0o7});
    try c.testParam("10", "%p1%o", .{0o10});
    try c.testParam("11111", "%p1%o", .{0o11111});
}

test "Param sequence: width & flags" {
    var c: ParamTestContext = .{};
    defer c.list.deinit();

    try c.testParam("100", "%{100}%0d", .{});
    try c.testParam("100", "%{100}%1d", .{});
    try c.testParam("100", "%{100}%2d", .{});
    try c.testParam("100", "%{100}%3d", .{});
    try c.testParam(" 100", "%{100}%4d", .{});
    try c.testParam("  100", "%{100}%5d", .{});
    try c.testParam("   100", "%{100}%6d", .{});

    try c.testParam("  -100", "%{-100}%6d", .{});
    try c.testParam("  +100", "%{100}%:+6d", .{});

    try c.testParam("+100  ", "%{100}%:-+6d", .{});
    try c.testParam("-100  ", "%{-100}%:-+6d", .{});

    try c.testParam(" 100  ", "%{100}%:- 6d", .{});
    try c.testParam("-100  ", "%{-100}%:- 6d", .{});

    try c.testParam(" 100", "%{100}% d", .{});
    try c.testParam("-100", "%{-100}% d", .{});

    try c.testParam("100", "%{100}%.1d", .{});
    try c.testParam("-100", "%{-100}%.1d", .{});
    try c.testParam("100", "%{100}%.3d", .{});
    try c.testParam("-100", "%{-100}%.3d", .{});
    try c.testParam("00100", "%{100}%.5d", .{});
    try c.testParam("-00100", "%{-100}%.5d", .{});
}

test "Param sequence: strings" {
    var c: ParamTestContext = .{};
    defer c.list.deinit();

    try c.testParam("100", "%p1%s", .{100});
    try c.testParam("abc", "%p1%s", .{"abc"});
    try c.testParam("  abc", "%p1%5s", .{"abc"});
    try c.testParam("abc  ", "%p1%:-5s", .{"abc"});

    // Check for inoring of sign, space and precision
    try c.testParam("abc  ", "%p1%:-+ 5.10s", .{"abc"});
}

test "Param sequence: arithmetic" {
    var c: ParamTestContext = .{};
    defer c.list.deinit();

    // Binary operators are 'push(pop() op pop())', so %p1%p2%- would be push(p2 - p1)

    try c.testParam("10", "%p1%p2%+%d", .{ 3, 7 });
    try c.testParam("-20", "%p1%p2%+%d", .{ 10, -30 });

    try c.testParam("40", "%p1%p2%-%d", .{ 10, -30 });
    try c.testParam("20", "%p1%p2%-%d", .{ -10, -30 });

    try c.testParam("300", "%p1%p2%*%d", .{ 10, 30 });
    try c.testParam("-300", "%p1%p2%*%d", .{ 10, -30 });

    try c.testParam("0", "%p1%p2%/%d", .{ 10, 30 });
    try c.testParam("0", "%p1%p2%/%d", .{ 10, -30 });
    try c.testParam("3", "%p1%p2%/%d", .{ 30, 10 });
    try c.testParam("-3", "%p1%p2%/%d", .{ 30, -10 });

    try c.testParam("2", "%p1%p2%m%d", .{ 5, 3 });
    try c.testParam("-2", "%p1%p2%m%d", .{ -5, 3 });
}

test "Param sequence: strlen" {
    var c: ParamTestContext = .{};
    defer c.list.deinit();

    try c.testParam("3", "%p1%l%d", .{100});
    try c.testParam("4", "%p1%l%d", .{-100});
    try c.testParam("1", "%p1%l%d", .{0});
    try c.testParam("0", "%p1%l%s", .{""});
    try c.testParam("8", "%p1%l%s", .{"abcdefgh"});
    try c.testParam("4", "%p1%l%s", .{"what"});
}

test "Param sequence: if-then-else" {
    var c: ParamTestContext = .{};
    defer c.list.deinit();

    try c.testParam("100", "%?%{1}%t%{100}%d%;", .{});
    try c.testParam("100", "%?%{1}%t%{100}%e%{-1}%;%d", .{});
    try c.testParam("-1", "%?%{0}%t%{100}%e%{-1}%;%d", .{});

    try c.testParam("100", "%?%{0}%t%{3}%e%{1}%t%{100}%d%;", .{});
    try c.testParam("\x1b[48;5;239m", "\x1b[%?%p1%{8}%<%t4%p1%d%e%p1%{16}%<%t10%p1%{8}%-%d%e48;5;%p1%d%;m", .{239});

    try c.testParam(error.UnexpectedConditionTerminator, "%?%{1}%t%{100}%d%;%;", .{});
}
