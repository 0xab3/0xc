const std = @import("std");
const Ast = @import("ast.zig");
const assert = std.debug.assert;
const SourceContext = Ast.SourceContext;

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

pub fn init(context: SourceContext) Self {
    return .{ .context = context };
}
fn get_size_for_type(@"type": []const u8) Self.Error!u16 {
    for (PrimitiveTypes) |it| {
        if (std.mem.eql(u8, it[0], @"type")) {
            return it[1];
        }
    }
    return Error.TypeNotFound;
}
pub fn type_check_proc_args(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef, proc_call: *const Ast.Expression.ProcCall, proc_decl: *const Ast.ProcDecl) bool {
    const params = proc_decl.get_required_params();
    // const n_total_params_provided = proc_decl.args_list.items.len - proc_decl.args_list.items.len;
    if (params.items.len > proc_call.params.items.len) {
        const n_lines, _ = self.context.get_loc(proc_call.name);
        std.log.err("{s}:{}:{}: too few arguments to procedure '{s}' expected {}, have {}", .{ self.context.filename, n_lines, 0, proc_call.name, params.items.len, proc_call.params.items.len });
        return false;
    }
    for (params.items, 0..) |param, idx| {
        self.type_check_expr(module, procedure, &proc_call.params.items[idx], param.decl.type) catch return false;
    }
    return true;
}

pub fn can_type_resolve(concrete: ?[]const u8, abstract: ?[]const u8) bool {
    if (concrete == null and abstract == null) {
        return false;
    } else if (concrete == null or abstract == null) {
        return true;
    }
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
pub fn type_check_expr(self: *Self, module: *Ast.Module, procedure: *Ast.ProcDef, expression: *const Ast.Expression, can_resolve_to_type: ?[]const u8) !void {
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

            if (!can_type_resolve(can_resolve_to_type, variable.?.decl.type)) {
                std.log.err("{s}:{}:{}: cannot cast variable '{s}' of type '{s}' to type '{s}'", .{
                    self.context.filename,
                    n_lines,
                    0,
                    expr_as_var,
                    variable.?.decl.type orelse "(unknown)",
                    can_resolve_to_type orelse "(unknown)",
                });
                self.context.print_loc(expr_as_var);
                return Error.TypeMisMatch;
            }
        },
        .Call => |*expr_as_call| {
            const proc_decl = module.get_proc(expr_as_call.name);
            if (proc_decl == null) {
                const n_lines, _ = self.context.get_loc(expr_as_call.name);

                std.log.err("{s}:{}:{}: use of undefined procedure '{}'", .{ self.context.filename, n_lines, 0, expr_as_call });
                self.context.print_loc(expr_as_call.name);
                return Error.ProcedureNotDefined;
            }
            if (!self.type_check_proc_args(module, procedure, expr_as_call, &proc_decl.?)) {
                return Error.ProcedureCallArgsMismatch;
            }
        },
        .NoOp => {},
        else => {
            std.log.debug("type_check_expr for type '{}' is not implemented!", .{expression.*});
        },
    }
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
        .Assign => |stmt_assign| {
            try self.type_check_expr(module, procedure, &stmt_assign.lhs, null);
            try self.type_check_expr(module, procedure, &stmt_assign.rhs, null);
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
                try self.type_check_expr(
                    module,
                    procedure,
                    &stmt_return.expr.?,
                    procedure.decl.return_type,
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
            stack_variable_offset += size;
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
        std.log.debug("{s}", .{std.json.fmt(proc_decl.args_list.items, .{})});
    }
    for (module.proc_defs.items) |*proc| {
        std.log.debug("args list of proc def {s}", .{proc.decl.name});
        std.log.debug("{s}", .{std.json.fmt(proc.decl.args_list.items, .{})});
        try self.type_check_proc(module, proc);
    }
}
