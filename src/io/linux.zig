const std = @import("std");
const fs = std.fs;
const Allocator = std.mem.Allocator;
const IoError = fs.File.OpenError | fs.File.StatError;

pub fn read_entire_file(allocator: Allocator, filename: []const u8) ![]u8 {
    // @TODO(shahzad)!: this assumes that the file path is relative
    var file = try fs.cwd().openFile(filename, .{});
    const stat = try file.stat();
    const size = stat.size;
    const memory = try allocator.alloc(u8, size);
    const bytes_read = try file.readAll(memory);
    std.debug.assert(bytes_read == size);
    return memory;
}
pub fn write_entire_file(filename: []const u8, buffer: []const u8) !void {
    var file = try fs.cwd().createFile(filename, .{});
    try file.writeAll(buffer);
}
