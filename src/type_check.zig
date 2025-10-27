const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("ast.zig");
const assert = std.debug.assert;
const SourceContext = Ast.SourceContext;

allocator: Allocator,
context: SourceContext,
const Self = @This();

const Error = error{
    TypeNotFound,
    VariableRedefinition,
    VariableNotDefined,
    ProcedureNotDefined,
    ProcedureCallArgsMismatch,
    TypeMisMatch,
};
//TODO(shahzad): add a formatter here
const ExprType = struct {
    type: []const u8,
    info: union(enum) {
        PtrDepth: usize,
        IntLiteral: u64,
    },
    pub fn from_var_type(var_type: Ast.VarType, is_int_lit: bool, int_lit: usize) ExprType {
        if (is_int_lit) {
            return .{ .type = var_type.type, .info = .{ .IntLiteral = int_lit } };
        }
        return .{ .type = var_type.type, .info = .{ .PtrDepth = var_type.ptr_depth } };
    }
};
// NOTE(shahzad): we use this as IntLiteralType ++ int_literal
const IntLiteralType = "\x00intlit";

const PrimitiveTypes = [_]struct { []const u8, u16 }{
    .{ "s8", 1 },
    .{ "x8", 1 },

    .{ "s16", 2 },
    .{ "x16", 2 },

    .{ "s32", 4 },
    .{ "x32", 4 },

    .{ "s64", 8 },
    .{ "x64", 8 },
};

pub fn init(allocator: Allocator, context: SourceContext) Self {
    return .{ .context = context, .allocator = allocator };
}
fn get_size_for_type(@"type": ExprType) Self.Error!u16 {
    if (@"type".info == .PtrDepth and @"type".info.PtrDepth > 0) return 8;
    if (@"type".info == .IntLiteral) {
        return @intCast(get_size_of_int_literal(@"type".info.IntLiteral));
    }
    for (PrimitiveTypes) |it| {
        if (std.mem.eql(u8, it[0], @"type".type)) {
            return it[1];
        }
    }
    return Error.TypeNotFound;
}
pub fn get_size_of_int_literal(int_literal: u64) u32 {
    if (int_literal == 0) return 1;
    const n_bits: u16 = 64 - @clz(int_literal);
    const n_bytes: u16 = std.math.divCeil(u16, @intCast(n_bits), 8) catch unreachable;
    assert(n_bytes <= 8);
    return if ((n_bytes & n_bytes - 1) == 0) n_bytes else @as(u16, @intCast(1)) << @intCast(std.math.log2(n_bytes) + 1);
}

fn get_unsigned_int_for_size(size: u16) []const u8 {
    for (PrimitiveTypes) |it| {
        if (it[0][0] == 'x' and it[1] == size) {
            return it[0];
        }
    }
    unreachable;
}

pub fn type_check_proc_args(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef, proc_call: *const Ast.Expression.ProcCall, proc_decl: *const Ast.ProcDecl) bool {
    // @TODO(shahzad): better error reporting
    const params = proc_decl.get_required_params();
    const n_lines, _ = self.context.get_loc(proc_call.name);
    if (params.items.len > proc_call.params.items.len) {
        std.log.err("{s}:{}:{}: too few arguments to procedure '{s}' expected {}, have {}", .{ self.context.filename, n_lines, 0, proc_call.name, params.items.len, proc_call.params.items.len });
        return false;
    } else if (params.items.len < proc_call.params.items.len) {
        std.log.debug("@TODO(shahzad): {s}:{}:{}: procedure call '{s}' contains more arguments than required! implement named args!!!!", .{ self.context.filename, n_lines, 0, proc_call.name });
    }
    for (params.items, 0..) |param, idx| {
        const resolved_type = self.type_check_expr(module, procedure, &proc_call.params.items[idx]) catch {
            std.log.err("{s}:{}:{}: type of argument in procedure {s} on postion {} is '{s}', but given {s}", .{ self.context.filename, n_lines, 0, proc_call.name, idx, param.decl.type.?.type, "unimplemented!" });
            return false;
        };

        const param_expr_type = ExprType.from_var_type(param.decl.type.?, false, 0);
        if (!can_type_resolve(param_expr_type, resolved_type)) {
            std.log.err("{s}:{}:{}: type of argument in procedure {s} on postion {} is '{s}', but given {s}", .{ self.context.filename, n_lines, 0, proc_call.name, idx, param.decl.type.?.type, resolved_type.type });
            return false;
        }
    }
    return true;
}

pub fn can_type_resolve(concrete_: ?ExprType, abstract_: ?ExprType) bool {
    if (concrete_ == null and abstract_ == null) {
        return false;
    } else if (concrete_ == null or abstract_ == null) {
        return true;
    }

    const concrete = concrete_.?.type;
    var abstract = abstract_.?.type;

    const is_abstract_int_lit = abstract_.?.info == .IntLiteral;
    const is_concrete_int_lit = concrete_.?.info == .IntLiteral;

    if (is_abstract_int_lit and is_concrete_int_lit) {
        return true;
    } else if (is_concrete_int_lit) {
        return can_type_resolve(abstract_, concrete_);
    } else if (is_abstract_int_lit) {
        // this means that concrete is not a literal so check if it's a pointer
        if (concrete_.?.info.PtrDepth > 0) return false; // idk if this is right or nah
        const abstract_int_lit = abstract_.?.info.IntLiteral;
        const abstract_int_lit_size: u16 = @intCast(get_size_of_int_literal(abstract_int_lit));
        abstract = get_unsigned_int_for_size(abstract_int_lit_size);
    } else if (abstract_.?.info.PtrDepth != concrete_.?.info.PtrDepth) {
        return false;
    }

    if (std.mem.eql(u8, concrete, abstract)) {
        return true;
    }
    const concrete_size = get_size_for_type(concrete_.?) catch return false;
    const abstract_size = get_size_for_type(abstract_.?) catch return false;
    if (concrete_size >= abstract_size and concrete[0] == abstract[0]) return true; // don't break signed numbers and only cast to bigger size
    return false;
}
// check if the expr is correct with types and shit and also check if it can resolve to the given type
pub fn type_check_expr(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef, expression: *const Ast.Expression) !ExprType {
    //@TODO(shahzad): this is ass brother please fix it
    switch (expression.*) {
        .Var => |expr_as_var| {
            const variable = procedure.get_variable(expr_as_var);
            const n_lines, _ = self.context.get_loc(expr_as_var);
            if (variable == null) {
                std.log.err("{s}:{}:{}: use of undefined variable '{s}'", .{ self.context.filename, n_lines, 0, expr_as_var });
                self.context.print_loc(expr_as_var);
                return Error.VariableNotDefined;
            }

            return .from_var_type(variable.?.decl.type.?, false, 0);
        },
        .BinOp => |expr_as_bin_op| {
            // @NOTE(shahzad)!!: PRECEDENCE IS REQUIRED FOR TYPE CHECKING TO PROPERLY WORK!!!!
            const asignee_type = try self.type_check_expr(module, procedure, expr_as_bin_op.lhs);
            if (asignee_type.info == .IntLiteral) {
                const int_lit = asignee_type.info.IntLiteral;
                std.debug.print("asignee_type int_literal, literal: {}\n", .{int_lit});
            } else std.debug.print("asignee_type {}\n", .{asignee_type});

            const asigner_type = try self.type_check_expr(module, procedure, expr_as_bin_op.rhs);
            if (asignee_type.info == .IntLiteral) {
                const int_lit = asigner_type.info.IntLiteral;
                std.debug.print("asigner_type int_literal, literal: {}\n", .{int_lit});
            } else std.debug.print("asigner_type {}\n", .{asigner_type});

            if (!can_type_resolve(asignee_type, asigner_type)) {
                std.log.err("unable to resolve type {} to {}\n", .{ asigner_type, asignee_type });
                return error.TypeMisMatch;
            }
            const return_type =
                if (try get_size_for_type(asignee_type) > try get_size_for_type(asigner_type))
                    asignee_type
                else
                    asigner_type;
            if (return_type.info == .IntLiteral) {
                const int_lit = return_type.info.IntLiteral;
                std.debug.print("BinOP returning int_literal literal: {}\n", .{int_lit});
            } else std.debug.print("BinOP returning {}\n", .{return_type});
            return return_type;
        },
        .LiteralString => |str_lit| {
            try module.string_literals.append(.{ .string = str_lit, .label = undefined });
            return .{ .type = "x8", .info = .{ .PtrDepth = 1 } }; // hack

        },

        .Call => |*expr_as_call| {
            // @TODO(shahzad): check if the return statement matches with the proc_decl.return_type
            const proc_decl = module.get_proc(expr_as_call.name);
            if (proc_decl == null) {
                const n_lines, _ = self.context.get_loc(expr_as_call.name);

                std.log.err("{s}:{}:{}: use of undefined procedure '{s}'", .{ self.context.filename, n_lines, 0, expr_as_call.name });
                self.context.print_loc(expr_as_call.name);
                return Error.ProcedureNotDefined;
            }
            if (!self.type_check_proc_args(module, procedure, expr_as_call, &proc_decl.?)) {
                return Error.ProcedureCallArgsMismatch;
            }
            return .from_var_type(proc_decl.?.return_type, false, 0);
        },
        .NoOp, .Tuple => {
            // @TODO(shahzad)!!!!!: this smells bad
            // return "";
            unreachable;
        },
        .LiteralInt => |expr_as_int_lit| {
            // @TODO(shahzad): add something  in the literal int source to we can get the loc of it
            // @TODO(shahzad)!!: add comptime overflow checks on maths ops
            // @TODO(shahzad)!: add run time overflow checks on maths ops

            // @NOTE(shahzad)!: Literal int is by default unsigned

            return .{ .type = IntLiteralType, .info = .{ .IntLiteral = expr_as_int_lit } };
        },
        // else => |unhandled| {
        //     std.log.err("type_check_expr is not implemented for {}\n", .{unhandled});
        //
        // },
    }
    unreachable;
}

//todo(shahzad): can we print the propagating error?
pub fn type_check_stmt(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef, statement: *Ast.Statement) !void {
    // @TODO(shahzad)!!!!: check mutability on assignments
    switch (statement.*) {
        .VarDefStack, .VarDefStackMut => |stmt_var_def_stack| {
            const variable = procedure.get_variable(stmt_var_def_stack.name);
            if (variable != null) {
                const n_lines, const line = self.context.get_loc(variable.?.decl.name);

                std.log.err("{s}:{}:{}: redeclaration of variable '{s}'", .{ self.context.filename, n_lines, 0, variable.?.decl.name });
                std.log.debug("{s}:{}: {s}", .{ self.context.filename, n_lines, line });

                std.log.err("first declared here", .{});
                self.context.print_loc(stmt_var_def_stack.name);
                return Error.VariableRedefinition;
            }
            //@TODO(shahzad): after we add assignment at initialization we should check type of that shit here
        },
        .VarDefGlobal, .VarDefGlobalMut => {
            unreachable; // @NOTE(shahzad): this is ONLY for static variables inside proc def

        },
        .Expr => |*stmt_expr| {
            _ = try self.type_check_expr(module, procedure, stmt_expr);
        },
        .Return => |stmt_return| {
            if (!std.mem.eql(u8, procedure.decl.return_type.type, "void")) {
                if (stmt_return.expr == null) {
                    std.log.err("@TODO(shahzad): add something in return to get the location!!!", .{});
                    std.log.err("{s}: caller expects '{s}' but procedure '{s}' returns void", .{
                        self.context.filename,
                        procedure.decl.return_type.type,
                        procedure.decl.name,
                    });
                }
            }
            if (stmt_return.expr != null) {
                _ = try self.type_check_expr(
                    module,
                    procedure,
                    &stmt_return.expr.?,
                );
            }
            //@TODO(shahzad): check all the variables in return value is defined or nah
        },
    }
}
pub fn type_check_proc(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef) !void {
    var stack_variable_offset: u16 = 0;
    for (procedure.block.items) |*statement| {
        try self.type_check_stmt(module, procedure, statement);
        if (statement.* == .VarDefStack or statement.* == .VarDefStackMut) {
            const var_def = if (statement.* == .VarDefStack) statement.VarDefStack else statement.VarDefStackMut;
            const as_expr_type: ExprType = .{
                .type = var_def.type.?.type,
                .info = .{ .PtrDepth = var_def.type.?.ptr_depth },
            };
            const size = get_size_for_type(as_expr_type) catch |err| {
                std.log.debug("user defined types are not supported!", .{});
                return err;
            };
            stack_variable_offset += if (size <= 4) 4 else 8;
            var stack_var: Ast.StackVar = undefined;
            stack_var.init(var_def.name, stack_variable_offset, size, var_def.type, statement.* == .VarDefStackMut);
            if (!std.meta.eql(var_def.expr, .NoOp)) {
                const expr_type = try self.type_check_expr(module, procedure, &var_def.expr);
                const var_def_type_as_expr_type: ExprType = .from_var_type(var_def.type.?, false, 0);
                if (!can_type_resolve(var_def_type_as_expr_type, expr_type)) {
                    std.log.err("unable to resolve type {any} to {any}\n", .{ var_def.type , expr_type.type });
                    return error.TypeMisMatch;
                }
            }

            try procedure.stack_vars.append(stack_var);
        }
    }
    procedure.total_stack_var_offset = stack_variable_offset;
}
pub fn type_check_argument_list(self: *Self, proc_decl: *Ast.ProcDecl) bool {
    var err = false;
    for (0..proc_decl.args_list.items.len) |idx| {
        for (idx..proc_decl.args_list.items.len) |idx2| {
            const arg = &proc_decl.args_list.items[idx];
            const arg2 = &proc_decl.args_list.items[idx2];
            if (arg != arg2 and std.mem.eql(u8, arg.*.decl.name, arg2.*.decl.name)) {
                const n_lines, const line = self.context.get_loc(arg.*.decl.name);
                std.log.err("{s}:{}:{}: redeclaration of argument '{s}'", .{ self.context.filename, n_lines, 0, arg.*.decl.name });
                std.log.debug("{s}:{}: {s}", .{ self.context.filename, n_lines, line });

                std.log.err("first declared here", .{});
                self.context.print_loc(arg2.*.decl.name);
                err = true;
            }
        }
    }
    return err;
}
pub fn type_check_proc_decl(self: *Self, proc_decl: *Ast.ProcDecl) !void {
    if (self.type_check_argument_list(proc_decl)) return Error.VariableRedefinition;
}
// @TODO(shahzad): typecheck proc calls
pub fn type_check_mod(self: *Self, module: *Ast.Module) !void {
    // @TODO(shahzad): type check declarations only
    for (module.proc_decls.items) |*proc_decl| {
        try self.type_check_proc_decl(proc_decl);
    }
    for (module.proc_defs.items) |*proc| {
        try self.type_check_proc(module, proc);
    }
}
