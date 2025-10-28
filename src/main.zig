const std = @import("std");
const builtin = @import("builtin");
const io = @import("io/linux.zig");
const nob = @import("nob.zig");

const ArgParse = @import("arg_parse.zig");

const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const SourceContext = @import("ast.zig").SourceContext;
const TypeCheck = @import("./type_check.zig");
const CodeGen = @import("./codegen/x64_nasm_linux.zig");

pub fn build_asm_file(file_path: []const u8, out_path: []const u8, is_object_only: bool, object_files: std.array_list.Managed([]const u8)) void {
    var cmd: nob.Cmd = nob.Cmd{};
    const obj_filename = nob.temp_sprintf("%s.o", file_path.ptr);
    nob.da_append_many([*c]const u8, &cmd, &[_][*c]const u8{ "nasm","-g", "-f", "elf64", file_path.ptr, "-o", if (is_object_only) out_path.ptr else obj_filename });
    _ = nob.cmd_run_opt(&cmd, .{});
    if (is_object_only) return;
    nob.da_append_many([*c]const u8, &cmd, &[_][*c]const u8{ "gcc", "-g", obj_filename });
    for (object_files.items) |object_file| {
        nob.da_append([*c]const u8, &cmd, object_file.ptr);
    }
    nob.da_append_many([*c]const u8, &cmd, &[_][*c]const u8{ "-o", out_path.ptr });
    _ = nob.cmd_run_opt(&cmd, .{});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var args: ArgParse = .{};

    args.init();
    try args.populate();

    std.debug.print("input file {s}\n", .{args.input_filename});

    const bytes = try io.read_entire_file(allocator, args.input_filename);

    var lexer: Lexer = undefined;

    const source_ctx = SourceContext.init(args.input_filename, bytes);

    lexer.init(allocator, source_ctx);

    try lexer.tokenize();

    var parser = Parser.init(allocator, lexer.tokens.items);
    var module = try parser.parse(source_ctx);

    var type_checker = TypeCheck.init(allocator, module.context);
    type_checker.type_check_mod(&module) catch |err| {
        std.log.debug("Error Occured {}", .{err});
        return;
    };
    var code_gen = CodeGen.init(allocator);
    try code_gen.compile_mod(&module);

    var buff: [1024]u8 = undefined;
    const asm_filename = try std.fmt.bufPrintZ(&buff, "{s}.asm", .{args.output_filename});

    try io.write_entire_file(asm_filename, code_gen.program_builder.string.items);

    build_asm_file(asm_filename, args.output_filename, args.object_only, args.link_object_filename);
}
