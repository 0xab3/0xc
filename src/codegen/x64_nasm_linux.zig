const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Ast = @import("../ast.zig");
const StringBuilder = @import("../string_builder.zig");
const common = @import("./common.zig");
const CompiledExpression = common.CompiledExpression;

program_builder: StringBuilder,
scratch_buffer: StringBuilder,
string_arena: StringBuilder,
const Self = @This();

pub fn init(allocator: Allocator) Self {
    var arena = std.heap.ArenaAllocator.init(allocator);
    return .{ .program_builder = .init(allocator), .scratch_buffer = .init(allocator), .string_arena = .init(arena.allocator()) };
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
fn _get_size_of_register(reg: []const u8) u16 {
    return switch (reg.len) {
        1 => unreachable,
        2 => if (reg[0] != 'r') return 8 else return 2,
        3 => if (reg[0] == 'r') return 8 else return 4,
        else => unreachable,
    };
}
const LinuxCallingConvRegisters = [_][]const u8{
    "edi", "rdi",
    "esi", "rsi",
    "edx", "rdx",
    "ecx", "rcx",
    "r8d", "r8",
    "r9d", "r9",
};

pub fn get_callcov_arg_register(self: *Self, idx: usize, size: u32) []const u8 {
    _ = self;
    if (idx < LinuxCallingConvRegisters.len) {
        return LinuxCallingConvRegisters[(idx * 2) + (size / 8)];
    }
    unreachable;
}

//register should be a,b,c,d
fn _get_regiter_based_on_size(comptime register: []const u8, size: usize) []const u8 {
    comptime assert(register.len == 1);
    comptime switch (register[0]) {
        'a', 'b', 'c', 'd' => {},
        else => std.debug.panic("idk if register {s} exists or not", register),
    };
    return switch (size) {
        inline 1 => register ++ "l",
        inline 2 => register ++ "x",
        inline 4 => "e" ++ register ++ "x",
        inline 8 => "r" ++ register ++ "x",
        else => unreachable,
    };
}

fn _clear_register(self: *Self, register: []const u8) !void {
    _ = try self.program_builder.append_fmt("   xor %{s}, %{s}\n", .{ register, register });
}

pub fn get_size_of_int_literal(int_literal: u64) u32 {
    if (int_literal == 0) return 1;
    const n_bits: u16 = 64 - @clz(int_literal);
    const n_bytes: u16 = std.math.divCeil(u16, @intCast(n_bits), 8) catch unreachable;
    return n_bytes;
}
// generate code here and return the ident that specifies the location?
// for the variable
// @TODO(shahzad): this is shitty idea but by default we just put anything we've compiled to rax
pub fn compile_expr(self: *Self, module: *Ast.Module, block: *Ast.Block, expr: *const Ast.Expression) !CompiledExpression {
    switch (expr.*) {
        .LiteralInt => |expr_as_int_lit| {
            const int_lit_size: u32 = if (get_size_of_int_literal(expr_as_int_lit) <= 4) 4 else 8;
            return .{ .LitInt = .{ .literal = expr_as_int_lit, .size = int_lit_size } };
        },
        .LiteralString => |lit| {
            const str_lit = module.find_string_literal(lit);
            return .{ .LitStr = .{ .expr = str_lit.label, .size = 8 } };
        },
        .NoOp => {},
        .Var => |expr_as_var| {
            const stack_var = block.find_variable(expr_as_var);
            const stack_offset = stack_var.?.offset;

            return .{ .Var = .{ .offset = stack_offset, .size = stack_var.?.meta.size } };
        },
        .Call => |call_expr| {
            // @TODO(shahzad)!!!!!: we don't support any function with arity more than one

            for (call_expr.params.items, 0..) |param_expr, idx| {
                const expr_compiled_to_reg = try self.compile_expr(module, block, &param_expr);

                switch (expr_compiled_to_reg) {
                    .LitInt => |compiled_expr| {
                        const register = self.get_callcov_arg_register(idx, compiled_expr.size);
                        _ = try self.program_builder.append_fmt("   mov ${}, %{s}\n", .{ compiled_expr.literal, register });
                    },

                    .Var,
                    => |compiled_expr| {
                        const register = self.get_callcov_arg_register(idx, compiled_expr.size);
                        _ = try self.program_builder.append_fmt("   mov -{}(%rbp), %{s}\n", .{ compiled_expr.offset, register });
                    },
                    .Register, .Call => |compiled_expr| {
                        const register = self.get_callcov_arg_register(idx, compiled_expr.size);
                        _ = try self.program_builder.append_fmt("   mov %{s}, %{s}\n", .{ compiled_expr.expr, register });
                    },
                    .LitStr => |compiled_expr| {
                        const register = self.get_callcov_arg_register(idx, compiled_expr.size);
                        if (idx * 2 < LinuxCallingConvRegisters.len) {
                            _ = try self.program_builder.append_fmt("   leaq {s}(%rip), %{s}\n", .{ compiled_expr.expr, register }); 
                        } else {
                            unreachable; // unimplemented
                            // _ = try self.program_builder.append_fmt("   mov %{s}, %{s}\n", .{ compiled_expr.expr, register });
                        }
                    },
                }
            }

            // @TODO(shahzad): this is a hack cause we can also define our own put char
            if (module.get_proc_decl(call_expr.name) != null) {
                // c abi expect number of vector register used in rax if a function with
                // va args is called, we don't support that anyways to just zeroing out rax
                _ = try self.program_builder.append_fmt("   xor %rax, %rax\n", .{});
                _ = try self.program_builder.append_fmt("   call {s}@PLT\n", .{call_expr.name});
            } else {
                // @NOTE(shahzad): if the proc is not in decl_array that means it is not extern, which means
                // that it can only be a defined proc, calling undeclared proc is handled by the parser
                assert(module.get_proc_def(call_expr.name) != null);

                _ = try self.program_builder.append_fmt("   xor %rax, %rax\n", .{});
                _ = try self.program_builder.append_fmt("   call {s}\n", .{call_expr.name});
            }
            return .{ .Call = .{ .expr = "%rax", .size = 8 } };
            // x64 linux c convention specifies that return value should be in rax... we probably will have to change this

        },
        .Block => |blk| {
            try self.compile_block(module, blk);
            return .{ .Register = .{ .expr = "assignment from compiled block is not implemented!", .size = 8 } };
        },
        .IfCondition => |if_condition| {
            const compiled_expr = try self.compile_expr(module, block, if_condition.condition);
            switch (compiled_expr) {
                .Register => {
                    _ = try self.program_builder.append_fmt("   jnz ", .{});
                },
                else => unreachable,
            }
            return .{ .Register = .{ .expr = "assignment from if conditions is not implemented!", .size = 8 } };
        },
        // @TODO(shahzad): figure out what to do with this shit
        .Tuple => {},
        .BinOp => |*expr_as_bin_op| {
            return try self.compile_expr_bin_op(module, block, expr_as_bin_op);
        },
    }
    unreachable;
}

//      =
//      /\
//     x  +
//        /\
//      34  35
//
//
//      2 int literals = lea rax + intlit, 2nd intlit
//      1var 1int literals = load variable in rax and add rax, int lit
//      2var = load variable in rax, load variable in rdx add
fn load_int_literal_to_register(self: *Self, register: []const u8, literal: u64) void {
    self.program_builder.append_fmt("   mov ${}, %{}", .{ literal, register });
}

pub fn compile_expr_bin_op(self: *Self, module: *Ast.Module, block: *Ast.Block, bin_op: *const Ast.BinaryOperation) anyerror!CompiledExpression {
    const lhs = try self.compile_expr(module, block, bin_op.lhs);
    const rhs = try self.compile_expr(module, block, bin_op.rhs);
    // @TODO(shahzad): @pretty change this to if

    var lhs_compiled: []const u8 = undefined;
    var rhs_compiled: []const u8 = undefined;
    var ret: CompiledExpression = undefined;
    switch (bin_op.op) {
        .Add => {
            switch (lhs) {
                .LitInt => |expr| {
                    const register = _get_regiter_based_on_size("a", expr.size);
                    _ = try self.program_builder.append_fmt("   mov ${}, %{s}\n", .{ expr.literal, register });
                    lhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .Var => |expr| {
                    const register_size: u32 = if (expr.size <= 4) 4 else 8;
                    const register = _get_regiter_based_on_size("a", register_size);
                    _ = try self.program_builder.append_fmt("   mov -{}(%rbp), %{s} \n", .{ expr.offset, register });
                    lhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .Register, .Call => |expr| {
                    lhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{expr.expr});
                },
                .LitStr => {
                    unreachable;
                },
            }

            switch (rhs) {
                .LitInt => |expr| {
                    const register = _get_regiter_based_on_size("d", expr.size);
                    _ = try self.program_builder.append_fmt("   mov ${}, %{s}\n", .{ expr.literal, register });
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                    ret = .{ .Register = .{ .expr = register, .size = expr.size } };
                },
                .Var => |expr| {
                    const register = _get_regiter_based_on_size("d", expr.size);
                    _ = try self.program_builder.append_fmt("   mov -{}(%rbp), %{s}\n", .{ expr.offset, register });
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                    ret = .{ .Register = .{ .expr = register, .size = expr.size } };
                },
                .Register, .Call => |expr| {
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{expr.expr});
                    ret = .{ .Register = .{ .expr = expr.expr, .size = expr.size } };
                },
                .LitStr => {
                    unreachable;
                },
            }
            _ = try self.program_builder.append_fmt("   add {s}, {s}\n", .{ lhs_compiled, rhs_compiled });
            self.scratch_buffer.reset();
            return ret;
        },
        .Ass => {
            switch (lhs) {
                .Var => |expr| {
                    _ = try self.program_builder.append_fmt("   leaq -{}(%rbp), %rax\n", .{expr.offset});
                    lhs_compiled = try self.scratch_buffer.append_fmt("(%rax)", .{});
                    ret = .{ .Register = .{ .expr = "rax", .size = 8 } };
                },
                else => unreachable, // we don't give a shit as of now
            }
            // lhs is memory pointer so if you don't put rhs in register it requires a size mnemonic
            switch (rhs) {
                .LitInt => |expr| {
                    const register = _get_regiter_based_on_size("d", expr.size);
                    _ = try self.program_builder.append_fmt("   mov ${}, %{s}\n", .{ expr.literal, register });
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                    ret = .{ .Register = .{ .expr = register, .size = expr.size } };
                },
                .Var => |expr| {
                    const register_size: u32 = if (expr.size <= 4) 4 else 8;
                    const register = _get_regiter_based_on_size("d", register_size);
                    _ = try self.program_builder.append_fmt("   mov - {}(%rbp), ${s}\n", .{ expr.offset, register });
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                    ret = .{ .Register = .{ .expr = register, .size = register_size } };
                },
                .LitStr => |expr| {
                    const register = _get_regiter_based_on_size("d", 8);
                    _ = try self.program_builder.append_fmt("   leaq {s}(%rip), %{s}\n", .{ expr.expr,register });
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .Register,
                .Call,
                => |expr| {
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{expr.expr});
                },
            }
            _ = try self.program_builder.append_fmt("   mov {s}, {s}\n", .{ rhs_compiled, lhs_compiled });
            self.scratch_buffer.reset();
            return .{ .Register = .{ .expr = "assignmente cannot resolve to a type", .size = 100000 } };
        },
        else => std.debug.panic("compile_expr_bin_op for {} is not implemented!", .{bin_op}),
    }
}
pub fn compile_stmt(self: *Self, module: *Ast.Module, block: *Ast.Block, statement: *Ast.Statement) !void {
    switch (statement.*) {
        .VarDefStack, .VarDefStackMut => |stmt| {
            if (!std.meta.eql(stmt.expr, .NoOp)) {
                var var_as_expr: Ast.Expression = .{ .Var = stmt.name };
                var rhs_as_expr = stmt.expr;
                const bin_op_expr: Ast.BinaryOperation = .{
                    .op = .Ass,
                    .lhs = &var_as_expr,
                    .rhs = &rhs_as_expr,
                };
                _ = try self.compile_expr_bin_op(module, block, &bin_op_expr);
            }
        },
        .Expr => |*stmt_as_expr| {
            _ = try self.compile_expr(module, block, stmt_as_expr);
        },
        else => {},
    }
}
fn compile_proc_prelude(self: *Self, procedure: *Ast.ProcDef) !void {
    // @TODO(shahzad): what about the arguments :sob::sob:
    _ = try self.program_builder.append_fmt("{s}:\n", .{procedure.decl.name});
    _ = try self.program_builder.append_fmt("   mov %rsp, %rbp\n", .{});
    _ = try self.program_builder.append_fmt("   sub ${}, %rsp\n", .{procedure.total_stack_var_offset});
}
fn compile_block(self: *Self, module: *Ast.Module, block: *Ast.Block) anyerror!void {
    for (block.stmts.items) |*statement| {
        try self.compile_stmt(module, block, statement);
    }
}
pub fn compile_proc(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef) !void {
    try self.compile_proc_prelude(procedure);
    try self.compile_block(module, procedure.block);
    try self.compile_proc_ending(procedure);
}
fn compile_proc_ending(self: *Self, procedure: *Ast.ProcDef) !void {
    _ = try self.program_builder.append_fmt("   add ${}, %rsp\n", .{procedure.total_stack_var_offset});
    std.log.debug("@TODO(shahzad): add return value :sob:", .{});
    _ = try self.program_builder.append_fmt("   xor %rax, %rax\n", .{});
    _ = try self.program_builder.append_fmt("   ret\n", .{});
}
pub fn compile_data_section(self: *Self, module: *Ast.Module) !void {
    var label_no: usize = 0;
    _ = try self.program_builder.append_fmt(".section .rodata\n", .{});
    for (module.string_literals.items) |*str_lit| {
        str_lit.label = try self.string_arena.append_fmt("LD{d:0>2}", .{label_no});
        _ = try self.program_builder.append_fmt("{s}:\n", .{str_lit.label});
        _ = try self.program_builder.append_fmt(".string \"{s}\"\n", .{str_lit.string});
        label_no += 1;
    }
}

pub fn compile_mod(self: *Self, module: *Ast.Module) !void {
    try self.compile_data_section(module);
    _ = try self.program_builder.append_fmt(".section .text\n", .{});
    if (module.has_main_proc) {
        _ = try self.program_builder.append_fmt(".global main\n", .{});
        // @NOTE(shahzad): we linking with crt so we don't care much about anything for now
    }
    for (module.proc_decls.items) |*proc_decl| {
        _ = try self.program_builder.append_fmt(".extern {s}\n", .{proc_decl.name});
    }
    for (module.proc_defs.items) |*proc| {
        try self.compile_proc(module, proc);
    }
    std.debug.print("generated assembly", .{});
    std.debug.print("--------------------------------------------------\n", .{});
    std.debug.print("{s}\n", .{self.program_builder.string.items});
    std.debug.print("--------------------------------------------------\n", .{});
}
