const std = @import("std");
const debug = std.debug;

const nob_c = @cImport({
    @cDefine("NOB_STRIP_PREFIX", "");
    @cInclude("./nob.h");
});

const self = @This();
pub const Cmd = nob_c.Cmd;
pub const cmd_run_opt = nob_c.cmd_run_opt;
pub const temp_sprintf = nob_c.temp_sprintf;

// we don't even need this tbh
fn is_valid_da(da: anytype) void {
    comptime {
        const da_type = @TypeOf(da);

        if (!@hasField(da_type, "items")) {
            @compileError("dynamic array should have items field!");
        }
        if (!@hasField(da_type, "count")) {
            @compileError("dynamic array should have count field!");
        }
        if (!@hasField(da_type, "capacity")) {
            @compileError("dynamic array should have capacity field!");
        }
    }
}

pub fn da_reserve(da: anytype, expected_capacity: usize) void {
    is_valid_da(da.*);

    if ((expected_capacity) > da.capacity) {
        if ((da).capacity == 0) {
            (da).capacity = nob_c.NOB_DA_INIT_CAP;
        }
        while ((expected_capacity) > (da).capacity) {
            (da).capacity *= 2;
        }

        (da).items = @ptrCast(@alignCast(nob_c.NOB_REALLOC(@ptrCast(da.items), (da).capacity * @sizeOf(@TypeOf(da.items)))));

        std.debug.assert(da.items != null);
    }
}
pub fn da_append(T: type, da: anytype, item: T) void {
    is_valid_da(da.*);

    da_reserve((da), (da).count + 1);
    da.items[da.count] = item;
    da.count += 1;
}

pub fn da_append_many(T: type, da: anytype, new_items: []const T) void {
    is_valid_da(da.*);

    da_reserve(da, da.count + new_items.len);
    for (new_items, 0..) |item, idx| {
        da.items[da.count + idx] = item;
    }
    (da).count += (new_items.len);
}
