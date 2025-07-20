const std = @import("std");
const arg_parse = @import("arg_parse.zig");
const io = @import("io/linux.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const SourceContext = @import("lexer.zig").SourceContext;
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var args: arg_parse = .{};

    args.init();
    args.populate();

    std.debug.print("input file {s}\n", .{args.input_file});

    const bytes = try io.read_entire_file(allocator, args.input_file);

    var lexer: Lexer = undefined;

    const source_ctx = SourceContext.init(args.input_file, bytes);

    lexer.init(allocator, source_ctx);

    try lexer.parse();
}
