const std = @import("std");
input_file: []const u8 = undefined,
const Self = @This();
pub fn init(self: *Self) void {
    self.* = .{};
}
pub fn help() void {
    std.log.debug("USAGE: 0xcc -i file", .{});
}

pub fn populate(self: *Self) void {
    var iter = std.process.ArgIterator.init();
    var flag = iter.next();
    var is_file_flag = false;
    while (flag != null) {
        if (std.mem.eql(u8, flag.?, "-i") == true) {
            const name = iter.next();
            self.input_file = name.?;
            is_file_flag = true;
        }
        flag = iter.next();
    }
    if (is_file_flag == false) {
        help();
        std.process.exit(1);
    }
}
