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
    .{ "^", 8 },
};

pub fn init(allocator: Allocator, context: SourceContext) Self {
    return .{ .context = context, .allocator = allocator };
}
fn get_size_for_type(@"type": []const u8) Self.Error!u16 {
    for (PrimitiveTypes) |it| {
        if (std.mem.eql(u8, it[0], @"type")) {
            return it[1];
        }
    }
    if (std.mem.startsWith(u8, @"type", IntLiteralType)) {
        return @intCast(get_size_of_int_literal(extract_int_lit_from_type(@"type")));
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

// NOTE(shahzad): i hate zig
pub fn extract_int_lit_from_type(int_lit_type: []const u8) u64 {
    var extended: []const u8 = undefined;
    extended.ptr = int_lit_type.ptr;
    extended.len = int_lit_type.len + 8;

    return std.mem.bytesAsValue(u64, extended[int_lit_type.len..extended.len]).*;
    // return @bitCast(extended[int_lit_type.len..extended.len]);
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
            std.log.err("{s}:{}:{}: type of argument in procedure {s} on postion {} is '{s}', but given {s}", .{ self.context.filename, n_lines, 0, proc_call.name, idx, param.decl.type.?, "unimplemented!" });
            return false;
        };

        if (!can_type_resolve(param.decl.type, resolved_type)) {
            std.log.err("{s}:{}:{}: type of argument in procedure {s} on postion {} is '{s}', but given {s}", .{ self.context.filename, n_lines, 0, proc_call.name, idx, param.decl.type.?, resolved_type });
            return false;
        }
    }
    return true;
}

pub fn can_type_resolve_int_literal(concrete: []const u8, abstract: []const u8) bool {
    const is_concrete_int_lit = std.mem.startsWith(u8, concrete, IntLiteralType);
    const is_abstract_int_lit = std.mem.startsWith(u8, abstract, IntLiteralType);

    if (@intFromBool(is_concrete_int_lit) ^ @intFromBool(is_abstract_int_lit) == 1) {
        if (is_concrete_int_lit) {
            const concrete_int_lit = extract_int_lit_from_type(concrete);
            const concrete_int_lit_type = get_size_for_type(get_size_of_int_literal(concrete_int_lit)) catch return false;
            _ = concrete_int_lit_type;
        } else if (is_abstract_int_lit) {
            const abstract_int_lit = extract_int_lit_from_type(abstract);
            const abstract_int_lit_type = get_size_for_type(get_size_of_int_literal(abstract_int_lit)) catch return false;
            _ = abstract_int_lit_type;
        }
        unreachable;
    }
    if (is_concrete_int_lit or is_abstract_int_lit) {
        return true;
    }
    unreachable;
}

pub fn can_type_resolve(concrete_: ?[]const u8, abstract_: ?[]const u8) bool {
    const concrete = concrete_;
    var abstract = abstract_;
    if (concrete == null and abstract == null) {
        return false;
    } else if (concrete == null or abstract == null) {
        return true;
    }
    const is_abstract_int_lit = std.mem.startsWith(u8, abstract.?, IntLiteralType);
    const is_concrete_int_lit = std.mem.startsWith(u8, concrete.?, IntLiteralType);
    if (is_abstract_int_lit and is_concrete_int_lit) {
        return true;
    } else if (is_concrete_int_lit) {
        return can_type_resolve(abstract, concrete);
    } else if (is_abstract_int_lit) {
        const abstract_int_lit = extract_int_lit_from_type(abstract.?);
        const abstract_int_lit_size: u16 = @intCast(get_size_of_int_literal(abstract_int_lit));
        abstract = get_unsigned_int_for_size(abstract_int_lit_size);
    }
    // const is_concrete_int_lit = std.mem.startsWith(u8, concrete, IntLiteralType);
    // if (is_concrete_int_lit or is_abstract_int_lit){
    // return can_type_resolve_int_literal(concrete.?, abstract.?);
    // }

    if (std.mem.eql(u8, concrete.?, abstract.?)) {
        return true;
    } else if (std.mem.eql(u8, concrete.?, "^") and std.mem.eql(u8, abstract.?, "^")) {
        return true; // pointers can resolve to any type
    } else if (std.mem.eql(u8, concrete.?, "^") or std.mem.eql(u8, abstract.?, "^")) {
        return false;
    }
    const concrete_size = get_size_for_type(concrete.?) catch return false;
    const abstract_size = get_size_for_type(abstract.?) catch return false;
    if (concrete_size >= abstract_size and concrete.?[0] == abstract.?[0]) return true; // don't break signed numbers and only cast to bigger size
    return false;
}
// check if the expr is correct with types and shit and also check if it can resolve to the given type
pub fn type_check_expr(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef, expression: *const Ast.Expression) ![]const u8 {
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
            return variable.?.decl.type.?;

            // @NOTE(shahzad): if we can't resolve the type to given type
            // if (!can_type_resolve(can_resolve_to_type, variable.?.decl.type)) {
            //     std.log.err("{s}:{}:{}: cannot cast variable '{s}' of type '{s}' to type '{s}'", .{
            //         self.context.filename,
            //         n_lines,
            //         0,
            //         expr_as_var,
            //         variable.?.decl.type orelse "(unknown)",
            //         can_resolve_to_type orelse "(unknown)",
            //     });
            //     self.context.print_loc(expr_as_var);
            //     return Error.TypeMisMatch;
            // }

        },
        .BinOp => |expr_as_bin_op| {
            // @NOTE(shahzad)!!: PRECEDENCE IS REQUIRED FOR TYPE CHECKING TO PROPERLY WORK!!!!
            const asignee_type = try self.type_check_expr(module, procedure, expr_as_bin_op.lhs);
            if (std.mem.startsWith(u8, asignee_type, IntLiteralType)) {
                const int_lit = extract_int_lit_from_type(asignee_type);
                std.debug.print("asignee_type {s}, literal: {}\n", .{ asignee_type, int_lit });
            } else std.debug.print("asignee_type {s}\n", .{asignee_type});

            const asigner_type = try self.type_check_expr(module, procedure, expr_as_bin_op.rhs);
            if (std.mem.startsWith(u8, asigner_type, IntLiteralType)) {
                const int_lit = extract_int_lit_from_type(asigner_type);
                std.debug.print("asigner_type {s}, literal: {}\n", .{ asigner_type, int_lit });
            } else std.debug.print("asigner_type {s}\n", .{asigner_type});

            // if (std.meta.eql(expr_as_bin_op.op, .Ass)) {
            if (!can_type_resolve(asignee_type, asigner_type)) {
                std.log.err("unable to resolve type {s} to {s}\n", .{ asigner_type, asignee_type });
                return error.TypeMisMatch;
            }
            const return_type =
                if (try get_size_for_type(asignee_type) > try get_size_for_type(asigner_type))
                    asignee_type
                else
                    asigner_type;
            if (std.mem.startsWith(u8, return_type, IntLiteralType)) {
                const int_lit = extract_int_lit_from_type(asigner_type);
                std.debug.print("BinOP returning {s} literal: {}\n", .{ return_type, int_lit });
            } else std.debug.print("BinOP returning {s}\n", .{return_type});
            return return_type;

            // if (!can_type_resolve(can_resolve_to_type, return_type)) {
            //     std.log.err("{s}:{}:{}: cannot cast expression '{}' of type '{s}' to type '{s}'", .{
            //         self.context.filename,
            //         0,
            //         0,
            //         expr_as_bin_op,
            //         return_type,
            //         can_resolve_to_type orelse "(unknown)",
            //     });
            //     return Error.TypeMisMatch;
            // }
            // return return_type;
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
            return proc_decl.?.return_type;
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

            const format_required_size = IntLiteralType.len + 8;
            var buf = try self.allocator.alloc(u8, format_required_size);
            var expr_as_int_lit_as_u8_slice = expr_as_int_lit;
            const int_lit_as_slice: []u8 = @ptrCast((&expr_as_int_lit_as_u8_slice)[0..1]);
            @memcpy(buf[0..IntLiteralType.len], IntLiteralType);
            @memcpy(buf[IntLiteralType.len..], int_lit_as_slice);
            return buf[0..IntLiteralType.len];
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
            if (!std.mem.eql(u8, procedure.decl.return_type, "void")) {
                if (stmt_return.expr == null) {
                    std.log.err("@TODO(shahzad): add something in return to get the location!!!", .{});
                    std.log.err("{s}: caller expects '{s}' but procedure '{s}' returns void", .{
                        self.context.filename,
                        procedure.decl.return_type,
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
            const size = get_size_for_type(var_def.type.?) catch |err| {
                std.log.debug("user defined types are not supported!", .{});
                return err;
            };
            stack_variable_offset += if (size < 4) 4 else 8;
            var stack_var: Ast.StackVar = undefined;
            stack_var.init(var_def.name, stack_variable_offset, size, var_def.type, statement.* == .VarDefStackMut);

            try procedure.stack_vars.append(stack_var);
        }
    }
    procedure.total_stack_var_offset = stack_variable_offset;
}
pub fn type_check_proc_decl(self: *Self, proc_decl: *Ast.ProcDecl) !void {
    _ = self;
    _ = proc_decl;
}
// @TODO(shahzad): typecheck proc calls
pub fn type_check_mod(self: *Self, module: *Ast.Module) !void {
    // @TODO(shahzad): type check declarations only
    for (module.proc_decls.items) |*proc_decl| {
        std.log.debug("args list of proc decl {s}", .{proc_decl.name});
        std.log.debug("{f}", .{std.json.fmt(proc_decl.args_list.items, .{})});
    }
    for (module.proc_defs.items) |*proc| {
        std.log.debug("args list of proc def {s}", .{proc.decl.name});
        std.log.debug("{f}", .{std.json.fmt(proc.decl.args_list.items, .{})});
        try self.type_check_proc(module, proc);
    }
}
