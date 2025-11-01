const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Ast = @import("../ast.zig");
const StringBuilder = @import("../string_builder.zig");
const common = @import("./common.zig");
const CompiledExpression = common.CompiledExpression;
const CompiledExprStack = CompiledExpression.CompiledExprStack;
const CompiledExprLiteral = CompiledExpression.CompiledExprLiteral;

program_builder: StringBuilder,
scratch_buffer: StringBuilder,
string_arena: StringBuilder,
const Self = @This();
pub const Storage = struct {
    pub const Type = enum { Stack, CallArg };
    storage_type: Type,
    offset: u64,
    pub fn init(storage_type: Type, offset: u64) Storage {
        return .{ .storage_type = storage_type, .offset = offset };
    }
    pub fn get_next(self: *Storage, scratch_buffer: *StringBuilder, size: u8) ![]const u8 {
        assert(1 <= size and size <= 8);
        switch (self.storage_type) {
            .Stack => {
                const prev_offset = self.offset;
                self.offset -= size;
                return try scratch_buffer.append_fmt("-{}(%rbp)", .{prev_offset});
            },
            .CallArg => {
                unreachable;
            },
        }
    }
};

pub fn init(allocator: Allocator) Self {
    var arena = std.heap.ArenaAllocator.init(allocator);
    return .{ .program_builder = .init(allocator), .scratch_buffer = .init(allocator), .string_arena = .init(arena.allocator()) };
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
fn get_register_based_on_size(self: *Self, register: []const u8, size: usize) []const u8 {
    switch (register[0]) {
        'a', 'b', 'c', 'd' => {},
        else => std.debug.panic("idk if register {s} exists or not", .{register}),
    }
    return switch (size) {
        1 => self.scratch_buffer.append_fmt("{s}l", .{register}) catch unreachable,
        2 => self.scratch_buffer.append_fmt("{s}x", .{register}) catch unreachable,
        4 => self.scratch_buffer.append_fmt("e{s}x", .{register}) catch unreachable,
        8 => self.scratch_buffer.append_fmt("r{s}x", .{register}) catch unreachable,
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
pub fn get_inst_postfix_based_on_size(size: u8) []const u8 {
    return switch (size) {
        1 => "b",
        2 => "w",
        4 => "l",
        8 => "q",
        else => unreachable,
    };
}
pub fn compile_plex(self: *Self, module: *Ast.Module, block: *Ast.Block, plex: *const Ast.Expression.PlexDef, storage: *Storage) anyerror!void {
    if (storage.storage_type == .CallArg) @panic("we are cooked");
    const plex_decl = module.get_plex_decl(plex.name);
    for (plex.members.items, 0..) |*member, i| {
        const compiled_expr = try self.compile_expr(module, block, member, storage);
        // TODO(shahzad): @bug recursive plex compilation
        const field_size: u8 = @intCast(plex_decl.?.fields.items[i].size);
        const byte_storage = try storage.get_next(&self.scratch_buffer, field_size);
        const postfix = get_inst_postfix_based_on_size(field_size);
        var lhs_compiled: []const u8 = undefined;
        switch (compiled_expr) {
            .Var => |expr| {
                var register = try self.load_variable_to_register(expr, "d");
                if (field_size < expr.size) {
                    const smol_register = try self.load_variable_to_register(expr, "d");
                    _ = try self.program_builder.append_fmt("   movz{s}l %{s}, %{s}", .{ postfix, smol_register, register });
                    register = smol_register;
                }
                lhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
            },
            .LitInt => |expr| {
                lhs_compiled = try self.scratch_buffer.append_fmt("${}", .{expr.literal});
            },
            .Register, .Call => |expr| {
                lhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{expr.expr});
            },
            .LitStr => |expr| {
                std.debug.print("field_size {}\n", .{field_size});
                assert(field_size == 8);
                const register = self.get_register_based_on_size("d", 8);
                _ = try self.program_builder.append_fmt("   leaq {s}(%rip), %{s}\n", .{ expr.expr, register });
                lhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
            },
            // TODO(shahzad): this should be that hard but i am tired as shit
            .PlexLiteral => @panic("recursive plex literals are not supported as of now"),
        }
        _ = try self.program_builder.append_fmt("   mov{s} {s}, {s}\n", .{ postfix, lhs_compiled, byte_storage });
    }
}

// TODO(shahzad): generate code here and return the ident that specifies the location?
pub fn compile_expr(self: *Self, module: *Ast.Module, block: *Ast.Block, expr: *const Ast.Expression, storage: ?*Storage) anyerror!CompiledExpression {
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
        .Plex => |plex| {
            assert(storage != null);
            try self.compile_plex(module, block, &plex, storage.?);
            // TODO(shahzad): @bug support struct literals initialization
            return .PlexLiteral;
        },
        .Call => |call_expr| {
            // @TODO(shahzad)!!!!!: we don't support any function with arity more than one

            for (call_expr.params.items, 0..) |param_expr, idx| {
                var local_storage: Storage = .init(.CallArg, 0);
                const expr_compiled_to_reg = try self.compile_expr(module, block, &param_expr, storage orelse &local_storage);

                switch (expr_compiled_to_reg) {
                    .LitInt => |compiled_expr| {
                        const register = self.get_callcov_arg_register(idx, compiled_expr.size);
                        _ = try self.program_builder.append_fmt("   mov ${}, %{s}\n", .{ compiled_expr.literal, register });
                    },

                    .Var => |compiled_expr| {
                        const register = self.get_callcov_arg_register(idx, compiled_expr.size);
                        _ = try self.program_builder.append_fmt("   mov -{}(%rbp), %{s}\n", .{ compiled_expr.offset, register });
                    },
                    .Register, .Call => |compiled_expr| {
                        const callconv_register = self.get_callcov_arg_register(idx, compiled_expr.size);
                        const compiled_register = self.get_register_based_on_size(compiled_expr.expr, compiled_expr.size);
                        _ = try self.program_builder.append_fmt("   mov %{s}, %{s}\n", .{ compiled_register, callconv_register });
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
                    .PlexLiteral => {
                        unreachable;
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
            return .{ .Call = .{ .expr = "a", .size = 8 } };
            // x64 linux c convention specifies that return value should be in rax... we probably will have to change this

        },
        .Block => |blk| {
            try self.compile_block(module, blk);
            return .{ .Register = .{ .expr = "assignment from compiled block is not implemented!", .size = 8 } };
        },
        .IfCondition => |if_condition| {
            module.total_branches += 1;

            var if_skip_label: [32]u8 = undefined;
            const if_skip_fmt = try std.fmt.bufPrint(&if_skip_label, "LB{d:0>2}", .{module.total_branches});

            module.total_branches += 1;
            var else_skip_label: [32]u8 = undefined;
            const else_skip_fmt = try std.fmt.bufPrint(&else_skip_label, "LB{d:0>2}", .{module.total_branches});

            const compiled_expr = try self.compile_expr(module, block, if_condition.if_.condition, null);
            switch (compiled_expr) {
                .Register => |reg| {
                    const register = self.get_register_based_on_size(reg.expr, reg.size);

                    _ = try self.program_builder.append_fmt("   test %{s}, %{s}\n", .{ register, register });
                    _ = try self.program_builder.append_fmt("   jz {s}\n", .{if_skip_fmt});
                },
                .Call => {
                    _ = try self.program_builder.append_fmt("   test %rax, %rax\n", .{});
                    _ = try self.program_builder.append_fmt("   jz {s}\n", .{if_skip_fmt});
                },

                else => {
                    std.debug.print("{} is not supported while compiling if conditions\n", .{compiled_expr});
                    unreachable;
                },
            }
            _ = try self.compile_expr(module, block, if_condition.if_.expression, null);
            _ = try self.program_builder.append_fmt("   jmp {s}\n", .{else_skip_fmt});
            _ = try self.program_builder.append_fmt("{s}:\n", .{if_skip_fmt});
            if (if_condition.else_expr) |else_expr| {
                _ = try self.compile_expr(module, block, else_expr, null);
            }
            _ = try self.program_builder.append_fmt("{s}:\n", .{else_skip_fmt});
            return .{ .Register = .{ .expr = "assignment from if conditions is not implemented!", .size = 8 } };
        },
        .WhileLoop => |while_loop| {
            module.total_branches += 1;
            const n_branch = module.total_branches;

            var label: [32]u8 = undefined;
            const label_fmt = try std.fmt.bufPrint(&label, "LB{d:0>2}", .{n_branch});
            _ = try self.program_builder.append_fmt("{s}:\n", .{label_fmt});

            _ = try self.compile_expr(module, block, while_loop.expression, null);
            const compiled_expr = try self.compile_expr(module, block, while_loop.condition, null);

            switch (compiled_expr) {
                .Register => |reg| {
                    const register = self.get_register_based_on_size(reg.expr, reg.size);
                    _ = try self.program_builder.append_fmt("   test %{s}, %{s}\n", .{ register, register });
                    _ = try self.program_builder.append_fmt("   jnz {s}\n", .{label_fmt});
                },
                .Call => {
                    _ = try self.program_builder.append_fmt("   test %rax, %rax\n", .{});
                    _ = try self.program_builder.append_fmt("   jnz {s}\n", .{label_fmt});
                },
                else => unreachable,
            }

            return .{ .Register = .{ .expr = "assignment from if conditions is not implemented!", .size = 8 } };
        },
        // @TODO(shahzad): figure out what to do with this shit
        .Tuple => {},
        .BinOp => |*expr_as_bin_op| {
            return try self.compile_expr_bin_op(module, block, expr_as_bin_op, storage);
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
fn load_int_literal_to_register(self: *Self, expr: CompiledExprLiteral, comptime register: []const u8) ![]const u8 {
    const reg = self.get_register_based_on_size(register, expr.size);
    _ = try self.program_builder.append_fmt("   mov ${}, %{s}\n", .{ expr.literal, reg });
    return reg;
}
pub fn load_variable_to_register(self: *Self, expr: CompiledExprStack, comptime register: []const u8) ![]const u8 {
    const register_size: u32 = if (expr.size <= 4) 4 else 8;
    const reg = self.get_register_based_on_size(register, register_size);
    _ = try self.program_builder.append_fmt("   mov -{}(%rbp), %{s} \n", .{ expr.offset, reg });
    return reg;
}

fn get_compare_store_inst_based_on_op(bin_op: *const Ast.BinaryOperation) []const u8 {
    switch (bin_op.op) {
        .Eq => return "sete",
        .Lt => return "setl",
        else => @panic("unimplemented!"),
    }
}
pub fn compile_expr_bin_op(self: *Self, module: *Ast.Module, block: *Ast.Block, bin_op: *const Ast.BinaryOperation, storage: ?*Storage) anyerror!CompiledExpression {
    // TODO(shahzad): @bug 64 bits bin ops are fucked :sob:
    const lhs = try self.compile_expr(module, block, bin_op.lhs, storage);
    const rhs = try self.compile_expr(module, block, bin_op.rhs, storage);
    // @TODO(shahzad): @pretty change this to if

    var lhs_compiled: []const u8 = undefined;
    var rhs_compiled: []const u8 = undefined;
    switch (bin_op.op) {
        .Add, .Sub, .Mul, .Div => {
            var ret: CompiledExpression = undefined;
            switch (lhs) {
                .LitInt => |expr| {
                    const register = try self.load_int_literal_to_register(expr, "a");
                    lhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .Var => |expr| {
                    const register = try self.load_variable_to_register(expr, "a");
                    lhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .Register, .Call => |expr| {
                    const register = self.get_register_based_on_size(expr.expr, expr.size);
                    lhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .LitStr, .PlexLiteral => {
                    unreachable;
                },
            }

            var rhs_size: u32 = 0;
            switch (rhs) {
                .LitInt => |expr| {
                    const register = try self.load_int_literal_to_register(expr, "d");
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                    rhs_size = expr.size;
                    ret = .{ .Register = .{ .expr = "d", .size = expr.size } };
                },
                .Var => |expr| {
                    const register = try self.load_variable_to_register(expr, "d");
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                    rhs_size = expr.size;
                    ret = .{ .Register = .{ .expr = "d", .size = expr.size } };
                },
                .Register, .Call => |expr| {
                    const register = self.get_register_based_on_size(expr.expr, expr.size);
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                    rhs_size = expr.size;
                    ret = .{ .Register = .{ .expr = "d", .size = expr.size } };
                },
                .LitStr, .PlexLiteral => {
                    unreachable;
                },
            }
            switch (bin_op.op) {
                .Add => {
                    _ = try self.program_builder.append_fmt("   add {s}, {s}\n", .{ lhs_compiled, rhs_compiled });
                },
                .Sub => {
                    _ = try self.program_builder.append_fmt("   sub {s}, {s}\n", .{ lhs_compiled, rhs_compiled });
                },
                .Mul => {
                    _ = try self.program_builder.append_fmt("   imul {s}, {s}\n", .{ lhs_compiled, rhs_compiled });
                },
                .Div => {
                    // TODO(shahzad): support floating point divides
                    const mnemonic = get_inst_postfix_based_on_size(@intCast(rhs_size));
                    const b_reg = self.get_register_based_on_size("b", rhs_size);
                    _ = try self.program_builder.append_fmt("   #-----divide------\n", .{});
                    _ = try self.program_builder.append_fmt("   mov{s} {s}, %{s}\n", .{ mnemonic, rhs_compiled, b_reg });
                    _ = try self.program_builder.append_fmt("   xor {s}, {s}\n", .{ rhs_compiled, rhs_compiled });
                    _ = try self.program_builder.append_fmt("   idiv %{s}\n", .{b_reg});
                    _ = try self.program_builder.append_fmt("   mov{s} {s}, {s}\n", .{ mnemonic, lhs_compiled, rhs_compiled });
                },
                else => unreachable,
            }
            self.scratch_buffer.reset();
            return ret;
        },
        .Ass => {
            var ret: CompiledExpression = undefined;
            var lhs_size: u32 = 0;
            switch (lhs) {
                .Var => |expr| {
                    lhs_size = if (expr.size <= 4) 4 else 8;
                    lhs_compiled = try self.scratch_buffer.append_fmt("-{}(%rbp)", .{expr.offset});
                    ret = .{ .Var = .{ .offset = expr.offset, .size = expr.size } };
                },
                else => unreachable, // we don't give a shit as of now
            }
            // lhs is memory pointer so if you don't put rhs in register it requires a size mnemonic
            switch (rhs) {
                .LitInt => |expr| {
                    const register = try self.load_int_literal_to_register(expr, "d");
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .Var => |expr| {
                    const register = try self.load_variable_to_register(expr, "d");
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .LitStr => |expr| {
                    const register = self.get_register_based_on_size("d", 8);
                    _ = try self.program_builder.append_fmt("   leaq {s}(%rip), %{s}\n", .{ expr.expr, register });
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .PlexLiteral => {
                    // TODO(shahzad): @bug bin op assignment is not implemented
                    // NOTE(shahzad): @hack clear the rax filled for lhs
                    _ = try self.program_builder.append_fmt("   xor %rax,%rax # fix this hack\n", .{});
                    self.scratch_buffer.reset();
                    return .PlexLiteral;
                },
                .Register, .Call => |expr| {
                    const register = self.get_register_based_on_size(expr.expr, lhs_size);
                    if (lhs == .Var and lhs_size <= 4) {
                        const lhs_mnemonic = get_inst_postfix_based_on_size(@intCast(lhs_size));
                        _ = try self.program_builder.append_fmt("   mov{s} %{s}, %{s} # extending\n", .{ lhs_mnemonic, register, register });
                    }
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
            }

            const lhs_mnemonic = get_inst_postfix_based_on_size(@intCast(lhs_size));
            _ = try self.program_builder.append_fmt("   mov{s} {s}, {s}\n", .{ lhs_mnemonic, rhs_compiled, lhs_compiled });
            self.scratch_buffer.reset();
            return ret;
        },
        .Eq, .Lt => {
            switch (lhs) {
                .Var => |expr| {
                    const register = try self.load_variable_to_register(expr, "a");
                    lhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .LitInt => |expr| {
                    const register = try self.load_int_literal_to_register(expr, "a");
                    lhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .Register, .Call => |expr| {
                    const register = self.get_register_based_on_size(expr.expr, expr.size);
                    lhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .LitStr, .PlexLiteral => unreachable,
            }

            switch (rhs) {
                .Var => |expr| {
                    const register = try self.load_variable_to_register(expr, "d");
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .LitInt => |expr| {
                    const register = try self.load_int_literal_to_register(expr, "d");
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .Register, .Call => |expr| {
                    const register = self.get_register_based_on_size(expr.expr, expr.size);
                    rhs_compiled = try self.scratch_buffer.append_fmt("%{s}", .{register});
                },
                .LitStr, .PlexLiteral => unreachable,
            }

            _ = try self.program_builder.append_fmt("   cmpl {s}, {s}\n", .{ rhs_compiled, lhs_compiled });
            const cmp_inst = get_compare_store_inst_based_on_op(bin_op);
            _ = try self.program_builder.append_fmt("   {s} %dl\n", .{cmp_inst});

            self.scratch_buffer.reset();
            // NOTE(shahzad): @bug we expect that any op with respect to a comparison has to be
            // 32 bits wide is not always the case
            _ = try self.program_builder.append_fmt("   movzbl %dl, %edx\n", .{});
            return .{ .Register = .{ .expr = "d", .size = 4 } };
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
                const stack_var = block.find_variable(stmt.name);
                var storage: Storage = .init(.Stack, stack_var.?.offset);
                _ = try self.compile_expr_bin_op(module, block, &bin_op_expr, &storage);
            }
        },
        .Expr => |*stmt_as_expr| {
            _ = try self.compile_expr(module, block, stmt_as_expr, null);
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
