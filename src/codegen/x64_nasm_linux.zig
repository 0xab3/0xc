const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Ast = @import("../ast.zig");
const StringBuilder = @import("../string_builder.zig");

program_builder: StringBuilder,
const Self = @This();

pub fn init(allocator: Allocator) Self {
    return .{ .program_builder = .init(allocator) };
}
fn get_size_identifier_based_on_size(size: u32) []const u8 {
    return switch (size) {
        1 => "BYTE",
        2 => "WORD",
        4 => "DWORD",
        8 => "QWORD",
        else => unreachable,
    };
}

// @UNIMPORTANT(shahzad): yes we are duplicating the code... First defined at  type_check.zig:44
pub fn get_size_of_int_literal(int_literal: u64) u16 {
    const n_bits: u64 = std.math.log2(int_literal) + 1;
    const n_bytes: u16 = std.math.divCeil(u16, @intCast(n_bits), 8) catch unreachable;
    assert(n_bytes <= 8);
    return if ((n_bytes & n_bytes - 1) == 0) n_bytes else @as(u16, @intCast(1)) << @intCast(std.math.log2(n_bytes) + 1);
}

// generate code here and return the ident that specifies the location?
// for the variable
// @TODO(shahzad): this is shitty idea but by default we just put anything we've compiled to rax
pub fn compile_expr(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef, expr: *const Ast.Expression) ![]u8 {
    switch (expr.*) {
        .LiteralInt => |expr_as_int_lit| {
            // @TODO(shahzad): use the size identifier
            const size_ident = get_size_identifier_based_on_size(get_size_of_int_literal(expr_as_int_lit));
            _ = size_ident;

            try self.program_builder.append_fmt("   mov rax, {}\n", .{expr_as_int_lit});
        },
        .NoOp => {},
        .Var => |expr_as_var| {
            const stack_var = procedure.get_variable(expr_as_var);
            const stack_offset = stack_var.?.offset;
            try self.program_builder.append_fmt("   mov r8,rbp\n", .{});
            try self.program_builder.append_fmt("   sub r8,{}\n", .{stack_offset});
        },
        .Call => |call_expr| {
            // @TODO(shahzad)!!!!!: we don't support any function with arity more than one
            assert(call_expr.params.items.len == 1);
            for (call_expr.params.items) |param_expr| {
                _ = try self.compile_expr(module, procedure, &param_expr);
                switch (param_expr) {
                    .Var => {
                        try self.program_builder.append_fmt("   mov rdi, [r8]\n", .{});
                    },
                    .LiteralInt => {
                        try self.program_builder.append_fmt("   mov rdi, rax\n", .{});
                    },
                    else => unreachable,
                }
                // @TODO(shahzad): this is a hack cause we can also define our own put char
                if (module.get_proc_decl(call_expr.name) != null) {
                    try self.program_builder.append_fmt("   call {s} wrt ..plt\n", .{call_expr.name});
                } else {
                    // @NOTE(shahzad): if the proc is not in decl_array that means it is not extern, which means
                    // that it can only be a defined proc, calling undeclared proc is handled by the parser
                    assert(module.get_proc_def(call_expr.name) != null);

                    try self.program_builder.append_fmt("   call {s}\n", .{call_expr.name});
                }
                if (std.mem.eql(u8, call_expr.name, "putchar")) {} else {}
            }
        },
        // @TODO(shahzad): figure out what to do with this shit
        .Tuple => {},
        .Assign => |*expr_assign| {
            try self.compile_expr_assign(module, procedure, expr_assign);
        },
    }
    return "";
}
pub fn compile_expr_assign(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef, expr: *const Ast.Expression.Assignment) anyerror!void {
    _ = try self.compile_expr(module, procedure, expr.rhs);
    _ = try self.compile_expr(module, procedure, expr.lhs);

    switch (expr.lhs.*) {
        .Var => {
            const stack_var = procedure.get_variable(expr.lhs.Var);
            const stack_var_size = stack_var.?.meta.size;
            switch (stack_var_size) {
                4 => {
                    try self.program_builder.append_fmt("   mov [r8], eax\n", .{});
                },
                else => unreachable,
            }
        },
        .Call => unreachable, // ?
        .LiteralInt => unreachable,
        .NoOp => {},
        else => unreachable,
    }
}

pub fn compile_stmt(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef, statement: *Ast.Statement) !void {
    switch (statement.*) {
        .VarDefStack, .VarDefStackMut => {},
        .Expr => |*stmt_as_expr| {
            _ = try self.compile_expr(module, procedure, stmt_as_expr);
        },
        else => {},
    }
}
fn compile_proc_prelude(self: *Self, procedure: *Ast.ProcDef) !void {
    // @TODO(shahzad): what about the arguments :sob::sob:
    try self.program_builder.append_fmt("{s}:\n", .{procedure.decl.name});
    try self.program_builder.append_fmt("   mov rbp, rsp\n", .{});
    try self.program_builder.append_fmt("   sub rsp, {}\n", .{procedure.total_stack_var_offset});
}
pub fn compile_proc(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef) !void {
    try self.compile_proc_prelude(procedure);
    for (procedure.block.items) |*statement| {
        try self.compile_stmt(module, procedure, statement);
    }
    try self.compile_proc_ending(procedure);
}
fn compile_proc_ending(self: *Self, procedure: *Ast.ProcDef) !void {
    try self.program_builder.append_fmt("   add rsp, {}\n", .{procedure.total_stack_var_offset});
    std.log.debug("@TODO(shahzad): add return value :sob:", .{});
    try self.program_builder.append_fmt("   xor rax, rax\n", .{});
    try self.program_builder.append_fmt("   ret\n", .{});
}
pub fn compile_data_section(self: *Self) !void {
    _ = self;
    @panic("unimplemented!");
}

pub fn compile_mod(self: *Self, module: *Ast.Module) !void {
    try self.program_builder.append_fmt("section .text\n", .{});
    if (module.has_main_proc) {
        try self.program_builder.append_fmt("global main\n", .{});
        // @NOTE(shahzad): we linking with crt so we don't care much about anything for now
    }
    for (module.proc_decls.items) |*proc_decl| {
        try self.program_builder.append_fmt("extern {s}\n", .{proc_decl.name});
    }
    for (module.proc_defs.items) |*proc| {
        try self.compile_proc(module, proc);
    }
    std.debug.print("generated assembly", .{});
    std.debug.print("--------------------------------------------------\n", .{});
    std.debug.print("{s}\n", .{self.program_builder.string.items});
    std.debug.print("--------------------------------------------------\n", .{});
}
