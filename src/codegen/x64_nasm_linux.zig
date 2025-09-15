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
fn get_inst_based_on_size(size: u32) []const u8 {
    return switch (size) {
        1 => "BYTE",
        2 => "WORD",
        4 => "DWORD",
        8 => "QWORD",
        else => unreachable,
    };
}

// generate code here and return the ident that specifies the location?
// for the variable
// @TODO(shahzad): this is shitty idea but by default we just put anything we've compiled to rax
pub fn compile_expr(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef, expr: *const Ast.Expression) ![]u8 {
    switch (expr.*) {
        .LiteralInt => |expr_as_int_lit| {
            try self.program_builder.append_fmt("   mov rax, {}\n", .{expr_as_int_lit});
        },
        .NoOp => {},
        .Var => |expr_as_var| {
            const stack_var = procedure.get_variable(expr_as_var);
            const stack_offset = stack_var.?.offset;
            try self.program_builder.append_fmt("   mov r8,rbp\n", .{});
            try self.program_builder.append_fmt("   sub r8,{}\n", .{stack_offset});
            std.log.debug("@TODO!(shahzad): compile_expr for var is not implemented!", .{});
        },
        .Call => |call_expr| {
            // we don't support any function with arity more than one
            assert(call_expr.params.items.len == 1);
            for (call_expr.params.items) |param_expr| {
                _ = try self.compile_expr(module, procedure, &param_expr);
                assert(param_expr == .Var);
                try self.program_builder.append_fmt("   mov rdi, [r8]\n", .{});
                try self.program_builder.append_fmt("   call {s} wrt ..plt\n", .{call_expr.name});
            }
        },
    }
    return "";
}
pub fn compile_stmt_assign(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef, statement: *const Ast.Statement.Assignment) !void {
    _ = try self.compile_expr(module, procedure, &statement.rhs);
    _ = try self.compile_expr(module, procedure, &statement.lhs);

    switch (statement.lhs) {
        .Var => {
            const stack_var = procedure.get_variable(statement.lhs.Var);
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
    }
}

pub fn compile_stmt(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef, statement: *Ast.Statement) !void {
    switch (statement.*) {
        .VarDefStack, .VarDefStackMut => {},
        .Assign => |*stmt_assign| {
            try self.compile_stmt_assign(module, procedure, stmt_assign);
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
    if (module.has_main_func) {
        try self.program_builder.append_fmt("global main\n", .{});
        // we linking with crt so we don't care much about anything for now
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
