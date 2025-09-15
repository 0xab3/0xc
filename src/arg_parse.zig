const std = @import("std");

input_filename: []const u8 = undefined,
output_filename: []const u8 = undefined,
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
    var _output_filename: ?[]const u8 = null;
    while (flag != null) {
        if (std.mem.eql(u8, flag.?, "-i")) {
            const name = iter.next();
            self.input_filename = name.?;
            is_file_flag = true;
        } else if (std.mem.eql(u8, flag.?, "-o")) {
            const name = iter.next();
            _output_filename = name.?;
            is_file_flag = true;
        }
        flag = iter.next();
    }
    if (is_file_flag == false) {
        help();
        std.process.exit(1);
    }
    self.output_filename = _output_filename orelse "a.out";
}
