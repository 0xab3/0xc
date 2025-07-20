// NOTE(shahzad): throw away shit to work with strings
//

const std = @import("std");
const ascii = std.ascii;
pub fn split_once(buffer: *[]u8, delim: u8) []u8 {
    for (buffer, 0..) |elem, i| {
        if (elem == delim) {
            buffer = buffer[i + 1 ..];
            return buffer[0..i];
        }
    }
    return buffer[0..0];
}
pub fn get_line(buffer: []const u8) []const u8 {
    const idx = std.mem.indexOfAny(u8, buffer, "\r\n") orelse buffer.len;
    return buffer[0..idx];
}

pub fn is_symbol(c: u8) bool {
    const symbols: []const u8 = "!@#$%^&*()-_=+[{]}\\|;:,<.>/?";
    if (std.mem.indexOfScalar(u8, symbols, c) != null) return true else return false;
}
// raw dogged parsing implementation
pub fn parse_int(buffer: []const u8, comptime base: u8) struct { usize, u64 } {
    comptime std.debug.assert(base == 0); // base is not supported
    var idx: usize = 0;
    var literal: u64 = 0;
    while (idx < buffer.len) {
        if (!ascii.isDigit(buffer[idx])) {
            break;
        }

        literal *= 10;
        literal += buffer[idx] - 0x30;

        idx += 1;
    }
    return .{idx, literal};
}
