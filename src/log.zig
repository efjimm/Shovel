const std = @import("std");
const root = @import("root");
const build_options = @import("build_options");
const log = std.log.scoped(.shovel);

pub const perf = std.log.scoped(.shovel_perf);

pub const err = log.err;
pub const warn = log.warn;
pub const info = log.info;
pub const debug = log.debug;
