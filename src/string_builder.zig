const std = @import("std");
const ArrayListManaged = std.array_list.Managed;
const Allocator = std.mem.Allocator;

string: ArrayListManaged(u8),
const Self = @This();
pub fn init(allocator: Allocator) Self {
    return .{ .string = .init(allocator) };
}
pub fn append_fmt(self: *Self, comptime fmt: []const u8, args: anytype) ![] const u8 {
    const format_required_size = std.fmt.count(fmt, args);
    const fmt_buff: []u8 = try self.string.addManyAsSlice(format_required_size);
    return try std.fmt.bufPrint(fmt_buff, fmt, args);
}

pub fn print_fmt(self: *Self, comptime fmt: []const u8, args: anytype) ![]u8 {
    const format_required_size = std.fmt.count(fmt, args);
    try self.string.resize(format_required_size);
    return try std.fmt.bufPrint(self.string.items, fmt, args);
}
