const std = @import("std");
const log = @import("log.zig");
const getenv = std.os.getenv;
const sep = std.fs.path.sep;
const readInt = std.mem.readInt;
const native_endian = @import("builtin").cpu.arch.endian();
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const util = @import("util.zig");

// Maximum length of terminfo definition files
pub const max_file_length = 32768;
const Self = @This();

const Header = extern struct {
    format: Format,
    terminal_names_size: u16,
    flag_count: u16,
    number_count: u16,
    string_count: u16,
    string_table_size: u16,

    const Format = enum(i16) {
        legacy = 0o0432,
        extended = 0o1036,
    };

    fn numberCapabilitySize(header: Header) u3 {
        return switch (header.format) {
            .legacy => 2,
            .extended => 4,
        };
    }

    fn names(header: Header, bytes: []const u8) [:0]const u8 {
        return if (header.terminal_names_size == 0)
            ""
        else
            bytes[@sizeOf(Header)..][0 .. header.terminal_names_size - 1 :0];
    }

    fn flags(header: Header, bytes: []const u8) []const bool {
        return @ptrCast(
            bytes[@sizeOf(Header) + header.terminal_names_size ..][0..header.flag_count],
        );
    }

    fn Number(comptime format: Format) type {
        return switch (format) {
            .legacy => i16,
            .extended => i32,
        };
    }

    fn numSize(header: Header) u4 {
        return switch (header.format) {
            .legacy => 2,
            .extended => 4,
        };
    }

    fn numbers(header: Header, bytes: []const u8, comptime format: Format) []align(1) const Number(format) {
        std.debug.assert(header.format == format);
        const off = @sizeOf(Header) +
            header.terminal_names_size +
            header.flag_count;
        const start: [*]align(1) const Number(format) = @ptrCast(bytes[off + off % 2 ..]);
        return start[0..header.number_count];
    }

    fn strings(header: Header, bytes: []const u8) []align(1) const i16 {
        const off = @sizeOf(Header) +
            header.terminal_names_size +
            header.flag_count +
            header.number_count * header.numSize();
        const start: [*]align(1) const i16 = @ptrCast(bytes[off + off % 2 ..]);
        return start[0..header.string_count];
    }

    fn stringTable(header: Header, bytes: []const u8) []const u8 {
        const off = @sizeOf(Header) +
            header.terminal_names_size +
            header.flag_count +
            header.number_count * header.numSize() +
            header.string_count * 2;
        return bytes[off + off % 2 ..][0..header.string_table_size];
    }
};

fn searchTermInfoDirectory(term: []const u8) ?std.fs.File {
    std.debug.assert(term.len > 0);

    const path = getenv("TERMINFO") orelse return null;
    var buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
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
    var buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
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

    var buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
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

    var buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
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

fn parseHeader(bytes: []const u8) ParseError!Header {
    if (bytes.len < @sizeOf(Header))
        return error.InvalidFormat;

    const magic = std.meta.intToEnum(Header.Format, readInt(i16, bytes[0..2], .little));

    const ret: Header = .{
        .format = magic catch return error.InvalidFormat,
        .terminal_names_size = try readNonNegative(bytes[2..4]),
        .flag_count = try readNonNegative(bytes[4..6]),
        .number_count = try readNonNegative(bytes[6..8]),
        .string_count = try readNonNegative(bytes[8..10]),
        .string_table_size = try readNonNegative(bytes[10..12]),
    };

    const total_size = @sizeOf(Header) +
        ret.terminal_names_size +
        ret.flag_count +
        ret.number_count * ret.numberCapabilitySize() +
        ret.string_count * @sizeOf(i16) +
        ret.string_table_size;

    if (bytes.len < total_size)
        return error.InvalidFormat;

    // Make sure terminal names is zero terminated
    if (ret.terminal_names_size > 0 and bytes[@sizeOf(Header) + ret.terminal_names_size] != 0)
        return error.InvalidFormat;

    return ret;
}

pub fn destroy(self: *Self, allocator: Allocator) void {
    allocator.free(self.names);
    allocator.free(self.string_table);
    allocator.destroy(self);
}

pub const ParseError = error{
    InvalidFormat,
} || Allocator.Error;

pub fn parse(allocator: Allocator, bytes: []const u8) ParseError!*Self {
    const header = try parseHeader(bytes);

    const num_flags: u15 = @min(total_flags, header.flag_count);
    const num_nums: u15 = @min(total_numbers, header.number_count);
    const num_strs: u15 = @min(total_strings, header.string_count);

    const ret = try allocator.create(Self);
    errdefer allocator.destroy(ret);

    ret.names = try allocator.dupeZ(u8, header.names(bytes));
    errdefer allocator.free(ret.names);

    ret.string_table = try allocator.dupe(u8, header.stringTable(bytes));
    errdefer allocator.free(ret.string_table);

    const flags_dest: *[total_flags]bool = @ptrCast(&ret.flags);
    @memcpy(flags_dest[0..num_flags], header.flags(bytes)[0..num_flags]);
    @memset(flags_dest[num_flags..total_flags], false);

    switch (header.format) {
        inline else => |format| {
            const T = Header.Number(format);
            const nums_dest: *[total_numbers]i32 = @ptrCast(&ret.numbers);
            for (nums_dest[0..num_nums], header.numbers(bytes, format)[0..]) |*d, s| {
                d.* = std.mem.littleToNative(T, s);
            }
            @memset(nums_dest[num_nums..total_numbers], -1);
        },
    }

    const strings_dest: *[total_strings]i16 = @ptrCast(&ret.strings);
    for (strings_dest[0..num_strs], header.strings(bytes)[0..num_strs]) |*d, s| {
        d.* = std.mem.littleToNative(i16, s);
    }
    @memset(strings_dest[num_strs..total_strings], -1);

    return ret;
}

pub fn getFlagCapability(self: *const Self, tag: FlagTag) bool {
    const slice: *const [total_flags]bool = @ptrCast(&self.flags);
    return slice[@intFromEnum(tag)];
}

pub fn getNumberCapability(self: *const Self, tag: NumberTag) ?u31 {
    const slice: *const [total_numbers]i32 = @ptrCast(&self.numbers);
    const v = slice[@intFromEnum(tag)];
    return if (v < 0) null else @intCast(v);
}

pub fn getStringCapability(self: *const Self, tag: StringTag) ?[:0]const u8 {
    const slice: *const [total_strings]i16 = @ptrCast(&self.strings);
    const v = std.math.cast(u16, slice[@intFromEnum(tag)]) orelse return null;
    return std.mem.span(@as([*:0]const u8, @ptrCast(self.string_table[v..].ptr)));
}

names: [:0]const u8 = "",
string_table: []const u8 = "",

flags: Flags = .{},
numbers: Numbers = .{},
strings: Strings = .{},

// extended_flags: std.StringHashMapUnmanaged(void) = .{},
// extended_numbers: std.StringHashMapUnmanaged(u31) = .{},
// extended_strings: std.StringHashMapUnmanaged(u15) = .{},

pub const FlagTag = std.meta.FieldEnum(Flags);
pub const NumberTag = std.meta.FieldEnum(Numbers);
pub const StringTag = std.meta.FieldEnum(Strings);

const total_flags: comptime_int = @typeInfo(Flags).Struct.fields.len;
const total_numbers: comptime_int = @typeInfo(Numbers).Struct.fields.len;
const total_strings: comptime_int = @typeInfo(Strings).Struct.fields.len;

pub const Flags = extern struct {
    auto_left_margin: bool = false,
    auto_right_margin: bool = false,
    no_esc_ctlc: bool = false,
    ceol_standout_glitch: bool = false,
    eat_newline_glitch: bool = false,
    erase_overstrike: bool = false,
    generic_type: bool = false,
    hard_copy: bool = false,
    has_meta_key: bool = false,
    has_status_line: bool = false,
    insert_null_glitch: bool = false,
    memory_above: bool = false,
    memory_below: bool = false,
    move_insert_mode: bool = false,
    move_standout_mode: bool = false,
    over_strike: bool = false,
    status_line_esc_ok: bool = false,
    dest_tabs_magic_smso: bool = false,
    tilde_glitch: bool = false,
    transparent_underline: bool = false,
    xon_xoff: bool = false,
    needs_xon_xoff: bool = false,
    prtr_silent: bool = false,
    hard_cursor: bool = false,
    non_rev_rmcup: bool = false,
    no_pad_char: bool = false,
    non_dest_scroll_region: bool = false,
    can_change: bool = false,
    back_color_erase: bool = false,
    hue_lightness_saturation: bool = false,
    col_addr_glitch: bool = false,
    cr_cancels_micro_mode: bool = false,
    has_print_wheel: bool = false,
    row_addr_glitch: bool = false,
    semi_auto_right_margin: bool = false,
    cpi_changes_res: bool = false,
    lpi_changes_res: bool = false,
};

pub const Numbers = extern struct {
    columns: i32 = -1,
    init_tabs: i32 = -1,
    lines: i32 = -1,
    lines_of_memory: i32 = -1,
    magic_cookie_glitch: i32 = -1,
    padding_baud_rate: i32 = -1,
    virtual_terminal: i32 = -1,
    width_status_line: i32 = -1,
    num_labels: i32 = -1,
    label_height: i32 = -1,
    label_width: i32 = -1,
    max_attributes: i32 = -1,
    maximum_windows: i32 = -1,
    max_colors: i32 = -1,
    max_pairs: i32 = -1,
    no_color_video: i32 = -1,
    buffer_capacity: i32 = -1,
    dot_vert_spacing: i32 = -1,
    dot_horz_spacing: i32 = -1,
    max_micro_address: i32 = -1,
    max_micro_jump: i32 = -1,
    micro_col_size: i32 = -1,
    micro_line_size: i32 = -1,
    number_of_pins: i32 = -1,
    output_res_char: i32 = -1,
    output_res_line: i32 = -1,
    output_res_horz_inch: i32 = -1,
    output_res_vert_inch: i32 = -1,
    print_rate: i32 = -1,
    wide_char_size: i32 = -1,
    buttons: i32 = -1,
    bit_image_entwining: i32 = -1,
    bit_image_type: i32 = -1,
};

pub const Strings = extern struct {
    back_tab: i16 = -1,
    bell: i16 = -1,
    carriage_return: i16 = -1,
    change_scroll_region: i16 = -1,
    clear_all_tabs: i16 = -1,
    clear_screen: i16 = -1,
    clr_eol: i16 = -1,
    clr_eos: i16 = -1,
    column_address: i16 = -1,
    command_character: i16 = -1,
    cursor_address: i16 = -1,
    cursor_down: i16 = -1,
    cursor_home: i16 = -1,
    cursor_invisible: i16 = -1,
    cursor_left: i16 = -1,
    cursor_mem_address: i16 = -1,
    cursor_normal: i16 = -1,
    cursor_right: i16 = -1,
    cursor_to_ll: i16 = -1,
    cursor_up: i16 = -1,
    cursor_visible: i16 = -1,
    delete_character: i16 = -1,
    delete_line: i16 = -1,
    dis_status_line: i16 = -1,
    down_half_line: i16 = -1,
    enter_alt_charset_mode: i16 = -1,
    enter_blink_mode: i16 = -1,
    enter_bold_mode: i16 = -1,
    enter_ca_mode: i16 = -1,
    enter_delete_mode: i16 = -1,
    enter_dim_mode: i16 = -1,
    enter_insert_mode: i16 = -1,
    enter_secure_mode: i16 = -1,
    enter_protected_mode: i16 = -1,
    enter_reverse_mode: i16 = -1,
    enter_standout_mode: i16 = -1,
    enter_underline_mode: i16 = -1,
    erase_chars: i16 = -1,
    exit_alt_charset_mode: i16 = -1,
    exit_attribute_mode: i16 = -1,
    exit_ca_mode: i16 = -1,
    exit_delete_mode: i16 = -1,
    exit_insert_mode: i16 = -1,
    exit_standout_mode: i16 = -1,
    exit_underline_mode: i16 = -1,
    flash_screen: i16 = -1,
    form_feed: i16 = -1,
    from_status_line: i16 = -1,
    init_1string: i16 = -1,
    init_2string: i16 = -1,
    init_3string: i16 = -1,
    init_file: i16 = -1,
    insert_character: i16 = -1,
    insert_line: i16 = -1,
    insert_padding: i16 = -1,
    key_backspace: i16 = -1,
    key_catab: i16 = -1,
    key_clear: i16 = -1,
    key_ctab: i16 = -1,
    key_dc: i16 = -1,
    key_dl: i16 = -1,
    key_down: i16 = -1,
    key_eic: i16 = -1,
    key_eol: i16 = -1,
    key_eos: i16 = -1,
    key_f0: i16 = -1,
    key_f1: i16 = -1,
    key_f10: i16 = -1,
    key_f2: i16 = -1,
    key_f3: i16 = -1,
    key_f4: i16 = -1,
    key_f5: i16 = -1,
    key_f6: i16 = -1,
    key_f7: i16 = -1,
    key_f8: i16 = -1,
    key_f9: i16 = -1,
    key_home: i16 = -1,
    key_ic: i16 = -1,
    key_il: i16 = -1,
    key_left: i16 = -1,
    key_ll: i16 = -1,
    key_npage: i16 = -1,
    key_ppage: i16 = -1,
    key_right: i16 = -1,
    key_sf: i16 = -1,
    key_sr: i16 = -1,
    key_stab: i16 = -1,
    key_up: i16 = -1,
    keypad_local: i16 = -1,
    keypad_xmit: i16 = -1,
    lab_f0: i16 = -1,
    lab_f1: i16 = -1,
    lab_f10: i16 = -1,
    lab_f2: i16 = -1,
    lab_f3: i16 = -1,
    lab_f4: i16 = -1,
    lab_f5: i16 = -1,
    lab_f6: i16 = -1,
    lab_f7: i16 = -1,
    lab_f8: i16 = -1,
    lab_f9: i16 = -1,
    meta_off: i16 = -1,
    meta_on: i16 = -1,
    newline: i16 = -1,
    pad_char: i16 = -1,
    parm_dch: i16 = -1,
    parm_delete_line: i16 = -1,
    parm_down_cursor: i16 = -1,
    parm_ich: i16 = -1,
    parm_index: i16 = -1,
    parm_insert_line: i16 = -1,
    parm_left_cursor: i16 = -1,
    parm_right_cursor: i16 = -1,
    parm_rindex: i16 = -1,
    parm_up_cursor: i16 = -1,
    pkey_key: i16 = -1,
    pkey_local: i16 = -1,
    pkey_xmit: i16 = -1,
    print_screen: i16 = -1,
    prtr_off: i16 = -1,
    prtr_on: i16 = -1,
    repeat_char: i16 = -1,
    reset_1string: i16 = -1,
    reset_2string: i16 = -1,
    reset_3string: i16 = -1,
    reset_file: i16 = -1,
    restore_cursor: i16 = -1,
    row_address: i16 = -1,
    save_cursor: i16 = -1,
    scroll_forward: i16 = -1,
    scroll_reverse: i16 = -1,
    set_attributes: i16 = -1,
    set_tab: i16 = -1,
    set_window: i16 = -1,
    tab: i16 = -1,
    to_status_line: i16 = -1,
    underline_char: i16 = -1,
    up_half_line: i16 = -1,
    init_prog: i16 = -1,
    key_a1: i16 = -1,
    key_a3: i16 = -1,
    key_b2: i16 = -1,
    key_c1: i16 = -1,
    key_c3: i16 = -1,
    prtr_non: i16 = -1,
    char_padding: i16 = -1,
    acs_chars: i16 = -1,
    plab_norm: i16 = -1,
    key_btab: i16 = -1,
    enter_xon_mode: i16 = -1,
    exit_xon_mode: i16 = -1,
    enter_am_mode: i16 = -1,
    exit_am_mode: i16 = -1,
    xon_character: i16 = -1,
    xoff_character: i16 = -1,
    ena_acs: i16 = -1,
    label_on: i16 = -1,
    label_off: i16 = -1,
    key_beg: i16 = -1,
    key_cancel: i16 = -1,
    key_close: i16 = -1,
    key_command: i16 = -1,
    key_copy: i16 = -1,
    key_create: i16 = -1,
    key_end: i16 = -1,
    key_enter: i16 = -1,
    key_exit: i16 = -1,
    key_find: i16 = -1,
    key_help: i16 = -1,
    key_mark: i16 = -1,
    key_message: i16 = -1,
    key_move: i16 = -1,
    key_next: i16 = -1,
    key_open: i16 = -1,
    key_options: i16 = -1,
    key_previous: i16 = -1,
    key_print: i16 = -1,
    key_redo: i16 = -1,
    key_reference: i16 = -1,
    key_refresh: i16 = -1,
    key_replace: i16 = -1,
    key_restart: i16 = -1,
    key_resume: i16 = -1,
    key_save: i16 = -1,
    key_suspend: i16 = -1,
    key_undo: i16 = -1,
    key_sbeg: i16 = -1,
    key_scancel: i16 = -1,
    key_scommand: i16 = -1,
    key_scopy: i16 = -1,
    key_screate: i16 = -1,
    key_sdc: i16 = -1,
    key_sdl: i16 = -1,
    key_select: i16 = -1,
    key_send: i16 = -1,
    key_seol: i16 = -1,
    key_sexit: i16 = -1,
    key_sfind: i16 = -1,
    key_shelp: i16 = -1,
    key_shome: i16 = -1,
    key_sic: i16 = -1,
    key_sleft: i16 = -1,
    key_smessage: i16 = -1,
    key_smove: i16 = -1,
    key_snext: i16 = -1,
    key_soptions: i16 = -1,
    key_sprevious: i16 = -1,
    key_sprint: i16 = -1,
    key_sredo: i16 = -1,
    key_sreplace: i16 = -1,
    key_sright: i16 = -1,
    key_srsume: i16 = -1,
    key_ssave: i16 = -1,
    key_ssuspend: i16 = -1,
    key_sundo: i16 = -1,
    req_for_input: i16 = -1,
    key_f11: i16 = -1,
    key_f12: i16 = -1,
    key_f13: i16 = -1,
    key_f14: i16 = -1,
    key_f15: i16 = -1,
    key_f16: i16 = -1,
    key_f17: i16 = -1,
    key_f18: i16 = -1,
    key_f19: i16 = -1,
    key_f20: i16 = -1,
    key_f21: i16 = -1,
    key_f22: i16 = -1,
    key_f23: i16 = -1,
    key_f24: i16 = -1,
    key_f25: i16 = -1,
    key_f26: i16 = -1,
    key_f27: i16 = -1,
    key_f28: i16 = -1,
    key_f29: i16 = -1,
    key_f30: i16 = -1,
    key_f31: i16 = -1,
    key_f32: i16 = -1,
    key_f33: i16 = -1,
    key_f34: i16 = -1,
    key_f35: i16 = -1,
    key_f36: i16 = -1,
    key_f37: i16 = -1,
    key_f38: i16 = -1,
    key_f39: i16 = -1,
    key_f40: i16 = -1,
    key_f41: i16 = -1,
    key_f42: i16 = -1,
    key_f43: i16 = -1,
    key_f44: i16 = -1,
    key_f45: i16 = -1,
    key_f46: i16 = -1,
    key_f47: i16 = -1,
    key_f48: i16 = -1,
    key_f49: i16 = -1,
    key_f50: i16 = -1,
    key_f51: i16 = -1,
    key_f52: i16 = -1,
    key_f53: i16 = -1,
    key_f54: i16 = -1,
    key_f55: i16 = -1,
    key_f56: i16 = -1,
    key_f57: i16 = -1,
    key_f58: i16 = -1,
    key_f59: i16 = -1,
    key_f60: i16 = -1,
    key_f61: i16 = -1,
    key_f62: i16 = -1,
    key_f63: i16 = -1,
    clr_bol: i16 = -1,
    clear_margins: i16 = -1,
    set_left_margin: i16 = -1,
    set_right_margin: i16 = -1,
    label_format: i16 = -1,
    set_clock: i16 = -1,
    display_clock: i16 = -1,
    remove_clock: i16 = -1,
    create_window: i16 = -1,
    goto_window: i16 = -1,
    hangup: i16 = -1,
    dial_phone: i16 = -1,
    quick_dial: i16 = -1,
    tone: i16 = -1,
    pulse: i16 = -1,
    flash_hook: i16 = -1,
    fixed_pause: i16 = -1,
    wait_tone: i16 = -1,
    user0: i16 = -1,
    user1: i16 = -1,
    user2: i16 = -1,
    user3: i16 = -1,
    user4: i16 = -1,
    user5: i16 = -1,
    user6: i16 = -1,
    user7: i16 = -1,
    user8: i16 = -1,
    user9: i16 = -1,
    orig_pair: i16 = -1,
    orig_colors: i16 = -1,
    initialize_color: i16 = -1,
    initialize_pair: i16 = -1,
    set_color_pair: i16 = -1,
    set_foreground: i16 = -1,
    set_background: i16 = -1,
    change_char_pitch: i16 = -1,
    change_line_pitch: i16 = -1,
    change_res_horz: i16 = -1,
    change_res_vert: i16 = -1,
    define_char: i16 = -1,
    enter_doublewide_mode: i16 = -1,
    enter_draft_quality: i16 = -1,
    enter_italics_mode: i16 = -1,
    enter_leftward_mode: i16 = -1,
    enter_micro_mode: i16 = -1,
    enter_near_letter_quality: i16 = -1,
    enter_normal_quality: i16 = -1,
    enter_shadow_mode: i16 = -1,
    enter_subscript_mode: i16 = -1,
    enter_superscript_mode: i16 = -1,
    enter_upward_mode: i16 = -1,
    exit_doublewide_mode: i16 = -1,
    exit_italics_mode: i16 = -1,
    exit_leftward_mode: i16 = -1,
    exit_micro_mode: i16 = -1,
    exit_shadow_mode: i16 = -1,
    exit_subscript_mode: i16 = -1,
    exit_superscript_mode: i16 = -1,
    exit_upward_mode: i16 = -1,
    micro_column_address: i16 = -1,
    micro_down: i16 = -1,
    micro_left: i16 = -1,
    micro_right: i16 = -1,
    micro_row_address: i16 = -1,
    micro_up: i16 = -1,
    order_of_pins: i16 = -1,
    parm_down_micro: i16 = -1,
    parm_left_micro: i16 = -1,
    parm_right_micro: i16 = -1,
    parm_up_micro: i16 = -1,
    select_char_set: i16 = -1,
    set_bottom_margin: i16 = -1,
    set_bottom_margin_parm: i16 = -1,
    set_left_margin_parm: i16 = -1,
    set_right_margin_parm: i16 = -1,
    set_top_margin: i16 = -1,
    set_top_margin_parm: i16 = -1,
    start_bit_image: i16 = -1,
    start_char_set_def: i16 = -1,
    stop_bit_image: i16 = -1,
    stop_char_set_def: i16 = -1,
    subscript_characters: i16 = -1,
    superscript_characters: i16 = -1,
    these_cause_cr: i16 = -1,
    zero_motion: i16 = -1,
    char_set_names: i16 = -1,
    key_mouse: i16 = -1,
    mouse_info: i16 = -1,
    req_mouse_pos: i16 = -1,
    get_mouse: i16 = -1,
    set_a_foreground: i16 = -1,
    set_a_background: i16 = -1,
    pkey_plab: i16 = -1,
    device_type: i16 = -1,
    code_set_init: i16 = -1,
    set0_des_seq: i16 = -1,
    set1_des_seq: i16 = -1,
    set2_des_seq: i16 = -1,
    set3_des_seq: i16 = -1,
    set_lr_margin: i16 = -1,
    set_tb_margin: i16 = -1,
    bit_image_repeat: i16 = -1,
    bit_image_newline: i16 = -1,
    bit_image_carriage_return: i16 = -1,
    color_names: i16 = -1,
    define_bit_image_region: i16 = -1,
    end_bit_image_region: i16 = -1,
    set_color_band: i16 = -1,
    set_page_length: i16 = -1,
    display_pc_char: i16 = -1,
    enter_pc_charset_mode: i16 = -1,
    exit_pc_charset_mode: i16 = -1,
    enter_scancode_mode: i16 = -1,
    exit_scancode_mode: i16 = -1,
    pc_term_options: i16 = -1,
    scancode_escape: i16 = -1,
    alt_scancode_esc: i16 = -1,
    enter_horizontal_hl_mode: i16 = -1,
    enter_left_hl_mode: i16 = -1,
    enter_low_hl_mode: i16 = -1,
    enter_right_hl_mode: i16 = -1,
    enter_top_hl_mode: i16 = -1,
    enter_vertical_hl_mode: i16 = -1,
    set_a_attributes: i16 = -1,
    set_pglen_inch: i16 = -1,
};

const Parameter = union(enum) {
    number: i32,
    string: []const u8,
};

fn createParam(arg: anytype) Parameter {
    const T = @TypeOf(arg);
    return if (comptime util.isZigString(T))
        .{ .string = arg }
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
    if (nesting != 0) return error.MissingConditionTerminator;
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
    var dynamic_variables: [26]Parameter = .{.{ .number = 0 }} ** 26;
    var static_variables: [26]Parameter = .{.{ .number = 0 }} ** 26;

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
                const lhs = stack.pop().number;
                const rhs = stack.pop().number;
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
const terminfo = @embedFile("st-256color").*;

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
    try t.expectEqual(false, res.flags.auto_left_margin);
    try t.expectEqual(true, res.flags.auto_right_margin);
    try t.expectEqual(false, res.flags.no_esc_ctlc);
    try t.expectEqual(false, res.flags.ceol_standout_glitch);
    try t.expectEqual(true, res.flags.eat_newline_glitch);
    try t.expectEqual(false, res.flags.erase_overstrike);
    try t.expectEqual(false, res.flags.generic_type);
    try t.expectEqual(false, res.flags.hard_copy);
    try t.expectEqual(false, res.flags.has_meta_key);
    try t.expectEqual(true, res.flags.has_status_line);
    try t.expectEqual(false, res.flags.insert_null_glitch);
    try t.expectEqual(false, res.flags.memory_above);
    try t.expectEqual(false, res.flags.memory_below);
    try t.expectEqual(true, res.flags.move_insert_mode);
    try t.expectEqual(true, res.flags.move_standout_mode);
    try t.expectEqual(false, res.flags.over_strike);
    try t.expectEqual(false, res.flags.status_line_esc_ok);
    try t.expectEqual(false, res.flags.dest_tabs_magic_smso);
    try t.expectEqual(false, res.flags.tilde_glitch);
    try t.expectEqual(false, res.flags.transparent_underline);
    try t.expectEqual(false, res.flags.xon_xoff);
    try t.expectEqual(false, res.flags.needs_xon_xoff);
    try t.expectEqual(false, res.flags.prtr_silent);
    try t.expectEqual(false, res.flags.hard_cursor);
    try t.expectEqual(false, res.flags.non_rev_rmcup);
    try t.expectEqual(true, res.flags.no_pad_char);
    try t.expectEqual(false, res.flags.non_dest_scroll_region);
    try t.expectEqual(true, res.flags.can_change);
    try t.expectEqual(true, res.flags.back_color_erase);

    // Should all be false, as the number of flags is only 29
    try t.expectEqual(false, res.flags.hue_lightness_saturation);
    try t.expectEqual(false, res.flags.col_addr_glitch);
    try t.expectEqual(false, res.flags.cr_cancels_micro_mode);
    try t.expectEqual(false, res.flags.has_print_wheel);
    try t.expectEqual(false, res.flags.row_addr_glitch);
    try t.expectEqual(false, res.flags.semi_auto_right_margin);
    try t.expectEqual(false, res.flags.cpi_changes_res);
    try t.expectEqual(false, res.flags.lpi_changes_res);
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
    try expectNumber32(0x0050, res.numbers.columns);
    try expectNumber32(0x0008, res.numbers.init_tabs);
    try expectNumber32(0x0018, res.numbers.lines);
    try expectNumber32(0xffffffff, res.numbers.lines_of_memory);
    try expectNumber32(0xffffffff, res.numbers.magic_cookie_glitch);
    try expectNumber32(0xffffffff, res.numbers.padding_baud_rate);
    try expectNumber32(0xffffffff, res.numbers.virtual_terminal);
    try expectNumber32(0xffffffff, res.numbers.width_status_line);
    try expectNumber32(0xffffffff, res.numbers.num_labels);
    try expectNumber32(0xffffffff, res.numbers.label_height);
    try expectNumber32(0xffffffff, res.numbers.label_width);
    try expectNumber32(0xffffffff, res.numbers.max_attributes);
    try expectNumber32(0xffffffff, res.numbers.maximum_windows);
    try expectNumber32(0x0100, res.numbers.max_colors);
    try expectNumber32(0x7fff, res.numbers.max_pairs);

    // Not in terminfo file, so all -1
    try expectNumber32(0xffffffff, res.numbers.no_color_video);
    try expectNumber32(0xffffffff, res.numbers.buffer_capacity);
    try expectNumber32(0xffffffff, res.numbers.dot_vert_spacing);
    try expectNumber32(0xffffffff, res.numbers.dot_horz_spacing);
    try expectNumber32(0xffffffff, res.numbers.max_micro_address);
    try expectNumber32(0xffffffff, res.numbers.max_micro_jump);
    try expectNumber32(0xffffffff, res.numbers.micro_col_size);
    try expectNumber32(0xffffffff, res.numbers.micro_line_size);
    try expectNumber32(0xffffffff, res.numbers.number_of_pins);
    try expectNumber32(0xffffffff, res.numbers.output_res_char);
    try expectNumber32(0xffffffff, res.numbers.output_res_line);
    try expectNumber32(0xffffffff, res.numbers.output_res_horz_inch);
    try expectNumber32(0xffffffff, res.numbers.output_res_vert_inch);
    try expectNumber32(0xffffffff, res.numbers.print_rate);
    try expectNumber32(0xffffffff, res.numbers.wide_char_size);
    try expectNumber32(0xffffffff, res.numbers.buttons);
    try expectNumber32(0xffffffff, res.numbers.bit_image_entwining);
    try expectNumber32(0xffffffff, res.numbers.bit_image_type);
}

test "String capabilities" {
    const res = try parse(ta, &terminfo);
    defer res.destroy(ta);

    // Hex values taken from running `hexdump -C` on the terminfo file
    try expectNumber(0x0000, res.strings.back_tab);
    try expectNumber(0x0004, res.strings.bell);
    try expectNumber(0x0006, res.strings.carriage_return);
    try expectNumber(0x0008, res.strings.change_scroll_region);
    try expectNumber(0x0019, res.strings.clear_all_tabs);
    try expectNumber(0x001e, res.strings.clear_screen);
    try expectNumber(0x0026, res.strings.clr_eol);
    try expectNumber(0x002a, res.strings.clr_eos);
    try expectNumber(0x002e, res.strings.column_address);
    try expectNumber(0xffff, res.strings.command_character);
    try expectNumber(0x0039, res.strings.cursor_address);
    try expectNumber(0x004a, res.strings.cursor_down);
    try expectNumber(0x004c, res.strings.cursor_home);
    try expectNumber(0x0050, res.strings.cursor_invisible);
    try expectNumber(0x0057, res.strings.cursor_left);
    try expectNumber(0xffff, res.strings.cursor_mem_address);
    try expectNumber(0x0059, res.strings.cursor_normal);
    try expectNumber(0x0066, res.strings.cursor_right);
    try expectNumber(0xffff, res.strings.cursor_to_ll);
    try expectNumber(0x006a, res.strings.cursor_up);
    try expectNumber(0x006e, res.strings.cursor_visible);
    try expectNumber(0x0075, res.strings.delete_character);
    try expectNumber(0x0079, res.strings.delete_line);
    try expectNumber(0xffff, res.strings.dis_status_line);
    try expectNumber(0xffff, res.strings.down_half_line);
    try expectNumber(0x007d, res.strings.enter_alt_charset_mode);
}

fn expectStringCapability(
    capabilities: *Self,
    expected: []const u8,
    comptime e: StringTag,
) !void {
    try t.expectEqualStrings(
        expected,
        capabilities.getStringCapability(e) orelse return error.InvalidCapabilityName,
    );
}

test "getCapability" {
    const file = openTermInfoFile("st-256color") orelse return error.Fug;
    defer file.close();

    const buf = try file.readToEndAlloc(t.allocator, 32768);
    defer t.allocator.free(buf);

    const res = try parse(ta, buf);
    defer res.destroy(ta);

    try expectStringCapability(res, "\x1b[%p1%dC", .parm_right_cursor);
    try expectStringCapability(res, "\x1b[%p1%dD", .parm_left_cursor);
    try expectStringCapability(res, "\x1b[%p1%dB", .parm_down_cursor);
    try expectStringCapability(res, "\x1b[%p1%dA", .parm_up_cursor);
    try expectStringCapability(res, "\x1b[1K", .clr_bol);

    const init_tabs = res.getNumberCapability(.init_tabs) orelse return error.InvalidCapabilityName;
    try t.expectEqual(@as(u31, 8), init_tabs);

    const auto_left_margin = res.getFlagCapability(.auto_left_margin);
    const auto_right_margin = res.getFlagCapability(.auto_right_margin);
    try t.expectEqual(false, auto_left_margin);
    try t.expectEqual(true, auto_right_margin);
}

test "xterm" {
    const x = @embedFile("xterm-256color");
    const header = try parseHeader(x);
    const res = try parse(ta, x);
    defer res.destroy(ta);

    try t.expectEqual(@as(u16, 0x0025), header.terminal_names_size);
    try t.expectEqual(@as(u16, 0x0026), header.flag_count);
    try t.expectEqual(@as(u16, 0x000f), header.number_count);
    try t.expectEqual(@as(u16, 0x019d), header.string_count);
    try t.expectEqual(@as(u16, 0x065a), header.string_table_size);

    try expectNumber32(0x00000050, res.numbers.columns);

    try expectNumber(0x0000, res.strings.back_tab);
    try expectNumber(0x0004, res.strings.bell);

    try expectStringCapability(res, "\x1b[%p1%dC", .parm_right_cursor);
    try expectStringCapability(res, "\x1b[%p1%dD", .parm_left_cursor);
    try expectStringCapability(res, "\x1b[%p1%dB", .parm_down_cursor);
    try expectStringCapability(res, "\x1b[%p1%dA", .parm_up_cursor);
}

test "findTermInfoPath" {
    {
        const file = openTermInfoFile("st-256color").?;
        defer file.close();

        const buf = try file.readToEndAlloc(std.testing.allocator, 32768);
        defer std.testing.allocator.free(buf);
        const res = try parse(ta, buf);
        defer res.destroy(ta);
        try t.expectEqualStrings("st-256color| simpleterm with 256 colors", res.names);
    }

    {
        const file = openTermInfoFile("xterm-256color").?;
        defer file.close();

        const buf = try file.readToEndAlloc(std.testing.allocator, 32768);
        defer std.testing.allocator.free(buf);
        const res = try parse(ta, buf);
        defer res.destroy(ta);
        try t.expectEqualStrings("xterm-256color|xterm with 256 colors", res.names);
    }

    try t.expectEqual(@as(?std.fs.File, null), openTermInfoFile(""));
    try t.expectEqual(@as(?std.fs.File, null), openTermInfoFile("Non-extant-terminal :)"));
}

const ParamTestContext = struct {
    list: std.ArrayList(u8) = std.ArrayList(u8).init(t.allocator),

    fn testParam(ctx: *ParamTestContext, expected: anytype, sequence: []const u8, args: anytype) !void {
        ctx.list.clearRetainingCapacity();
        const res = validateParamSequence(sequence, args.len);
        if (comptime util.isZigString(@TypeOf(expected))) {
            try res;
            try writeParamSequence(sequence, ctx.list.writer(), args);
            try t.expectEqualStrings(expected, ctx.list.items);
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
    try t.expectError(error.UnpoppedStack, validateParamSequence("%p1", 1));
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

    try c.testParam("-40", "%p1%p2%-%d", .{ 10, -30 });
    try c.testParam("-20", "%p1%p2%-%d", .{ -10, -30 });

    try c.testParam("300", "%p1%p2%*%d", .{ 10, 30 });
    try c.testParam("-300", "%p1%p2%*%d", .{ 10, -30 });

    try c.testParam("3", "%p1%p2%/%d", .{ 10, 30 });
    try c.testParam("-3", "%p1%p2%/%d", .{ 10, -30 });
    try c.testParam("0", "%p1%p2%/%d", .{ 30, 10 });

    try c.testParam("2", "%p1%p2%m%d", .{ 3, 5 });
    try c.testParam("-2", "%p1%p2%m%d", .{ 3, -5 });
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

    try c.testParam(error.MissingConditionTerminator, "%?%{1}%t%{100}%d", .{});
    try c.testParam(error.UnexpectedConditionTerminator, "%?%{1}%t%{100}%d%;%;", .{});
}
