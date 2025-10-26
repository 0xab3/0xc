const std = @import("std");

const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const ArrayListManaged = std.array_list.Managed;

const strings = @import("./strings.zig");
const BinOp = @import("lexer.zig").BinOp;
pub const Error = error{UnexpectedToken};

pub const StringLiteral = struct {
    string: []const u8,
    label: []const u8,
};
pub const Module = struct {
    allocator: Allocator,
    context: SourceContext,
    // i could use a hashmap but no
    string_literals: ArrayListManaged(StringLiteral),
    proc_decls: ArrayListManaged(ProcDecl),
    proc_defs: ArrayListManaged(ProcDef),
    has_main_proc: bool = false, // cries in alignment :sob:
    const Self = @This();
    pub fn init(self: *Self, allocator: Allocator, context: SourceContext) void {
        self.* = .{ .allocator = allocator, .proc_defs = .init(allocator), .proc_decls = .init(allocator), .context = context, .string_literals = .init(allocator) };
    }
    pub fn get_proc_decl(self: *Self, name: []const u8) ?ProcDecl {
        for (self.proc_decls.items) |proc_decl| {
            if (std.mem.eql(u8, proc_decl.name, name)) {
                return proc_decl;
            }
        }
        return null;
    }
    pub fn get_proc_def(self: *Self, name: []const u8) ?ProcDef {
        for (self.proc_defs.items) |proc_def| {
            if (std.mem.eql(u8, proc_def.decl.name, name)) {
                return proc_def;
            }
        }
        return null;
    }
    pub fn get_proc(self: *Self, name: []const u8) ?ProcDecl {
        var proc_decl = self.get_proc_decl(name);
        if (proc_decl == null) {
            const proc_def = self.get_proc_def(name);
            if (proc_def == null) {
                return null;
            }
            proc_decl = proc_def.?.decl;
        }
        return proc_decl;
    }
    pub fn find_string_literal(self: *Self, literal: []const u8) ?StringLiteral {
        for (self.string_literals.items) |it| {
            if (it.string.ptr == literal.ptr) {
                return it;
            }
        }
        unreachable;
    }
};
pub const BinaryOperation = struct {
    op: BinOp,
    lhs: *Expression,
    rhs: *Expression,
    const Self = @This();
    pub fn init(allocator: Allocator, op_type: BinOp, lhs_expr: Expression, rhs_expr: Expression) !Self {
        const lhs = try allocator.create(Expression);
        const rhs = try allocator.create(Expression);
        lhs.* = lhs_expr;
        rhs.* = rhs_expr;
        return .{ .op = op_type, .lhs = lhs, .rhs = rhs };
    }
};
// @TODO(shahzad): add source here so we can do error reporting
pub const Expression = union(enum) {
    pub const ProcCall = struct { name: []const u8, params: ArrayListManaged(Expression) };
    NoOp: void,
    Var: []const u8,
    LiteralInt: u64, // this should go away
    LiteralString: []const u8,
    Call: ProcCall,
    Tuple: ArrayListManaged(Expression),
    BinOp: BinaryOperation,
};

// @TODO(shahzad): add source in every field here so we can do error reporting
pub const Statement = union(enum) {
    VarDefStack: VarDecl,
    VarDefStackMut: VarDecl,
    VarDefGlobal: []u8,
    VarDefGlobalMut: []u8,
    Expr: Expression,
    Return: struct {
        expr: ?Expression,
    },
};

pub const VarDecl = struct {
    name: []const u8,
    type: ?[]const u8, // this should be in meta but fuck it
    expr: Expression,
    pub fn init(name: []const u8, @"type": ?[]const u8) @This() {
        return .{ .name = name, .type = @"type", .expr = .NoOp };
    }
};
pub const VarMetaData = struct {
    size: u31,
    is_mut: u1,
    pub fn init(size: u31, is_mut: bool) @This() {
        return .{ .size = size, .is_mut = @bitCast(is_mut) };
    }
};

pub const StackVar = struct {
    decl: VarDecl,
    meta: VarMetaData,
    offset: u32,

    const Self = @This();
    pub fn init(self: *Self, name: []const u8, offset: u32, size: u31, @"type": ?[]const u8, is_mutable: bool) void {
        self.* = .{ .decl = .init(name, @"type"), .meta = .init(size, is_mutable), .offset = offset };
    }
};

pub const Argument = struct { // @TODO(shahzad): do we really need this? Aren't arguments just variables on stack/register??
    decl: VarDecl,
    meta: VarMetaData, // unknown before type checking

    // todo(shahzad): bruh there is too much unknown
    // should this shit contain offset?
    // how does procedure args get passed *on stack?

    const Self = @This();
    pub fn init(self: *Self, name: []const u8, size: u31, @"type": ?[]const u8, is_mutable: bool) void {
        self.* = .{ .decl = .init(name, @"type"), .meta = .init(size, is_mutable) };
    }
};
pub const ProcDecl = struct {
    name: []const u8,
    args_list: ArrayListManaged(Argument),
    return_type: []const u8, // concrete type?
    const Self = @This();
    pub fn init(name: []const u8, args: ArrayListManaged(Argument), return_type: []const u8) Self {
        return .{ .name = name, .args_list = args, .return_type = return_type };
    }
    pub fn get_required_params(self: *const Self) ArrayListManaged(Argument) {
        // @TODO(shahzad): impl this function frfr
        return self.args_list;
    }
};

pub const ProcDef = struct {
    decl: ProcDecl,
    total_stack_var_offset: usize = 0,
    stack_vars: ArrayListManaged(StackVar), // populated in type checking phase
    block: ArrayListManaged(Statement),
    const Self = @This();

    pub fn init(allocator: Allocator, decl: ProcDecl, block: ArrayListManaged(Statement)) Self {
        return .{ .decl = decl, .stack_vars = .init(allocator), .block = block };
    }
    pub fn get_variable(self: *Self, var_name: []const u8) ?StackVar {
        const stack_vars = self.stack_vars.items;
        for (stack_vars) |item| {
            const var_decl = item.decl;
            if (std.mem.eql(u8, var_decl.name, var_name)) {
                return item;
            }
        }
        return null;
    }
};

pub const SourceContext = struct {
    filename: []const u8,
    file: []const u8,
    const Self = @This();
    pub fn init(filename: []const u8, file: []const u8) SourceContext {
        return .{
            .filename = filename,
            .file = file,
        };
    }
    pub fn count_lines_until_source_found(self: Self, source: []const u8) u32 {
        const start_ptr = self.file;

        assert(@intFromPtr(source.ptr) >= @intFromPtr(start_ptr.ptr) and source.len <= start_ptr.len);
        const buf = start_ptr[0 .. source.ptr - start_ptr.ptr];
        var count: u32 = 1;
        for (buf) |val| {
            switch (val) {
                '\r', '\n' => {
                    count += 1;
                },
                else => {},
            }
        }
        return count;
    }
    pub fn get_line_start_for_source(self: Self, source: []const u8) []const u8 {
        const start_ptr = self.file;
        assert(@intFromPtr(source.ptr) >= @intFromPtr(start_ptr.ptr) and source.len <= start_ptr.len);
        const buf = start_ptr[0 .. source.ptr - start_ptr.ptr];
        const line: []const u8 = blk: {
            var idx = buf.len - 1;
            while (idx >= 0) : (idx -= 1) {
                if (idx == 0) return strings.get_line(start_ptr[0..]);
                switch (buf[idx]) {
                    '\r', '\n' => {
                        const line_start_idx = if (idx + 1 < buf.len) idx + 1 else buf.len;
                        break :blk strings.get_line(start_ptr[line_start_idx..]);
                    },
                    else => {},
                }
            }

            break :blk strings.get_line(start_ptr[0..]);
        };

        return line;
    }
    pub fn get_loc(self: Self, source: []const u8) struct { u32, []const u8 } {
        const n_lines = self.count_lines_until_source_found(source);
        const line = self.get_line_start_for_source(source);
        return .{ n_lines, line };
    }

    // this function takes a pointer inside file buffer and prints the line (with
    // the source highlighted? i guess idk tho)
    pub fn print_loc(self: Self, source: []const u8) void {
        const n_lines: u32 = self.count_lines_until_source_found(source);
        const line = self.get_line_start_for_source(source);
        std.log.debug("{s}:{}: {s}", .{ self.filename, n_lines, line });
    }
};
