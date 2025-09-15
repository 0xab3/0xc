const std = @import("std");
const Allocator = std.mem.Allocator;

string: std.ArrayList(u8),
const Self = @This();
pub fn init(allocator: Allocator) Self {
    return .{ .string = .init(allocator) };
}
pub fn append_fmt(self: *Self, comptime fmt: []const u8, args: anytype) !void {
    const format_required_size = std.fmt.count(fmt, args);
    const fmt_buff: []u8 = try self.string.addManyAsSlice(format_required_size);
    _ = try std.fmt.bufPrint(fmt_buff, fmt, args);
    return;
}
