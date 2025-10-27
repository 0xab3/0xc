const std = @import("std");
const assert = std.debug.assert;
const ArrayListManaged = std.array_list.Managed;

const Ast = @import("ast.zig");
const Token = @import("lexer.zig").Token;
const BinOps = @import("lexer.zig").BinOp;
const TokenKind = @import("lexer.zig").TokenKind;

pub fn Peekable(T: type) type { // oop ahh :sob:
    return struct {
        comptime {
            std.debug.assert(@typeInfo(T) == .pointer);
            std.debug.assert(@typeInfo(T).pointer.size == .slice);
        }
        items: T,
        const Self = @This();
        pub fn init(item: T) Self {
            return .{ .items = item };
        }

        pub fn consume(self: *Self) ?Token {
            if (self.items.len > 0) {
                const copy = self.items[0];
                self.items = self.items[1..];
                return copy;
            } else {
                return null;
            }
        }
        pub fn peek(self: *Self, offset: usize) ?Token {
            if (self.items.len > 0 and offset < self.items.len) return self.items[offset] else return null;
        }
        pub fn advance(self: *Self, by: usize) void {
            self.items = self.items[by..];
        }
    };
}

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: Peekable([]Token),
    const Self = @This();
    pub fn init(allocator: std.mem.Allocator, tokens: []Token) Self {
        return .{ .allocator = allocator, .tokens = Peekable([]Token).init(tokens) };
    }
    pub fn parse(self: *Self, source_context: Ast.SourceContext) !Ast.Module {
        var module: Ast.Module = undefined;
        var has_main_procedure: bool = false;

        module.init(self.allocator, source_context);
        while (self.tokens.peek(0).?.kind != .Eof) {
            const token: Token = self.tokens.peek(0).?;
            switch (token.kind) {
                .ProcDecl => {
                    const proc_decl = try self.parse_proc(&module);
                    if (std.mem.eql(u8, proc_decl.name, "main")) {
                        has_main_procedure = true;
                    }
                },
                .VarDef => { // global variable
                    std.log.err("global variables are not supported!", .{});
                    self.tokens.peek(0).?.print_loc();
                },
                .Ident => {
                    std.log.err("unidentified identifier!", .{});
                    self.tokens.peek(0).?.print_loc();
                    return error.UnexpectedToken;
                },
                .Eof => {
                    unreachable;
                },
                else => {
                    std.log.debug("{}", .{token.kind});
                    unreachable;
                },
            }
        }
        assert(self.tokens.peek(0).?.kind == .Eof);

        module.has_main_proc = has_main_procedure;
        return module;
    }
    // parse the procedure and return only the declaration for some use
    pub fn parse_proc(self: *Self, module: *Ast.Module) !Ast.ProcDecl {
        const proc_decl = self.parse_proc_decl() catch |err| {
            self.tokens.peek(0).?.print_loc();
            return err;
        };
        switch (self.tokens.peek(0).?.kind) {
            .Semi => {
                try module.proc_decls.append(proc_decl);
                self.tokens.advance(1);
            },
            .CurlyOpen => {
                // @TODO(shahzad)!!!!!: instead of duplicating the procedure declaration
                // only store it in the ProcDecl array and attach an index to it that specifies
                // the procDef
                const code_block = try self.parse_block();
                try module.proc_defs.append(.init(module.allocator, proc_decl, code_block));
            },
            else => {
                _ = self.expect(.CurlyOpen, "'{' or ';'") catch |err| {
                    self.tokens.peek(0).?.print_loc();
                    return err;
                };
            },
        }
        return proc_decl;
    }

    fn parse_proc_decl(self: *Self) !Ast.ProcDecl {
        _ = try self.expect(.ProcDecl, "procedure definition");
        const proc_name = try self.expect(.Ident, null);

        const proc_args = try self.parse_proc_args();
        _ = try self.expect(.Arrow, null);
        var ptr_depth: usize = 0;
        while (self.tokens.peek(0).?.kind == .Pointer) : (self.tokens.advance(1)) {
            ptr_depth += 1;
        }
        const return_type = (try self.expect(.Ident, "'return type'")).source;
        return .init(proc_name.source, proc_args, return_type, ptr_depth);
    }
    fn parse_arg(self: *Self) !Ast.Argument {
        var token = self.tokens.peek(0).?;
        const mutable = blk: {
            if (token.kind == .Mut) {
                self.tokens.advance(1);
                break :blk true;
            } else {
                break :blk false;
            }
        };
        token = try self.expect(.Ident, "variable name");
        const var_name = token.source;
        _ = try self.expect(.Colon, null);

        var ptr_depth: usize = 0;
        while (self.tokens.peek(0).?.kind == .Pointer) : (self.tokens.advance(1)) {
            ptr_depth += 1;
        }

        token = try self.expect(.Ident, "type");
        const var_type = token.source;
        var arg_def: Ast.Argument = undefined;
        arg_def.init(var_name, undefined, var_type, ptr_depth, mutable);
        return arg_def;
    }

    fn parse_proc_args(self: *Self) !ArrayListManaged(Ast.Argument) {
        var param_defs = ArrayListManaged(Ast.Argument).init(self.allocator);
        _ = try self.expect(.ParenOpen, null);
        while (self.tokens.peek(0).?.kind != .ParenClose) {
            const arg = try self.parse_arg();
            try param_defs.append(arg);
            const next = self.tokens.peek(0).?;
            switch (next.kind) {
                .Comma => {
                    self.tokens.advance(1);
                    continue;
                },
                .ParenClose => {},
                else => {
                    _ = try self.expect(.ParenClose, null);
                },
            }
        }

        _ = self.expect(.ParenClose, null) catch |err| {
            self.tokens.peek(0).?.print_loc();
            return err;
        };
        return param_defs;
    }

    fn parse_block(self: *Self) !ArrayListManaged(Ast.Statement) {
        _ = try self.expect(.CurlyOpen, null);

        var statements = ArrayListManaged(Ast.Statement).init(self.allocator);
        errdefer statements.deinit();

        while (!std.meta.eql(self.tokens.peek(0).?.kind, .CurlyClose)) {
            const statement = try self.parse_stmt();
            try statements.append(statement);
            std.log.debug("{}", .{statement});
        }
        self.tokens.advance(1);

        return statements;
    }

    fn parse_stmt(self: *Self) !Ast.Statement {
        var token = self.tokens.peek(0);
        assert(token != null);
        switch (token.?.kind) {
            .VarDef => {
                self.tokens.advance(1);
                const next_tok = self.tokens.peek(0);

                const is_mut = blk: {
                    if (next_tok.?.kind == .Mut) {
                        self.tokens.advance(1);
                        break :blk true;
                    }
                    break :blk false;
                };
                const var_name = (try self.expect(.Ident, "'variable name'")).source;
                token = self.tokens.peek(0);
                assert(token != null);

                var var_type: ?Ast.VarType = null;

                if (std.meta.eql(token.?.kind, .Colon)) {
                    self.tokens.advance(1);

                    var ptr_depth: usize = 0;

                    while (self.tokens.peek(0).?.kind == .Pointer) : (self.tokens.advance(1)) {
                        ptr_depth += 1;
                    }

                    var_type = .{ .type = (try self.expect(.Ident, "'type definition'")).source, .ptr_depth = ptr_depth };
                }

                token = self.tokens.peek(0);
                assert(token != null);

                var expr: Ast.Expression = .NoOp;
                if (std.meta.eql(token.?.kind, .{ .Op = .Ass })) {
                    self.tokens.advance(1);
                    expr = try self.parse_expr();
                }

                // @TODO(shahzad): add support for assignment during initialization of variable
                _ = try self.expect(.Semi, null);
                if (is_mut) {
                    return .{ .VarDefStackMut = .{ .name = var_name, .type = var_type, .expr = expr } };
                } else {
                    return .{ .VarDefStack = .{ .name = var_name, .type = var_type, .expr = expr } };
                }
            },
            .Ident => {
                // @TODO(shahzad): implement correct parsing of assignments, i.e. Literals shouldn't get assigned
                const lhs = try self.parse_expr();
                _ = self.expect(.Semi, null) catch |err| {
                    self.tokens.peek(0).?.print_loc();
                    return err;
                };
                switch (lhs) {
                    .BinOp, .Call => {},
                    else => {
                        std.debug.panic("parse_statement only implemented for variable assignment and call!", .{});
                    },
                }

                return .{ .Expr = lhs };
            },
            .Return => {
                self.tokens.advance(1);
                var expr: ?Ast.Expression = null;
                if (!std.meta.eql(self.tokens.peek(0).?.kind, .Semi)) {
                    expr = try self.parse_expr();
                }
                _ = try self.expect(.Semi, null);
                return .{ .Return = .{ .expr = expr } };
            },
            else => {
                std.debug.panic("parse_statement for {} is not implemented!", .{token.?.kind});
            },
        }
    }
    fn parse_tuple(self: *Self) !ArrayListManaged(Ast.Expression) {
        var params = ArrayListManaged(Ast.Expression).init(self.allocator);
        _ = try self.expect(.ParenOpen, null);
        const token_kind = self.tokens.peek(0).?.kind;
        if (token_kind == .ParenClose) {
            self.tokens.advance(1);
            return params;
        }
        while (true) {
            const expr = try self.parse_expr();
            try params.append(expr);
            const next = self.tokens.peek(0).?;
            switch (next.kind) {
                .Comma => {
                    self.tokens.advance(1);
                    continue;
                },
                .ParenClose => {
                    self.tokens.advance(1);
                    break;
                },
                else => {
                    self.tokens.peek(0).?.print_loc();
                    return Ast.Error.UnexpectedToken;
                },
            }
        }
        return params;
    }
    // @NOTE(shahzad): parser doesn't give a shit about precedence  which is not good
    // if we have expression x + 43 = 34 + 35;
    // it will generate the below ast
    //
    //     +
    //    /\
    //  x   =
    //      /\
    //    43  +
    //        /\
    //      34  35

    // but we need need
    //       =
    //      / \
    //     /   \
    //   +      +
    //  /\      /\
    // x  43   34  35
    //
    // @TODO(shahzad): make it so this respect precedence

    fn parse_expr(self: *Self) anyerror!Ast.Expression {
        var expr: Ast.Expression = undefined;
        const lhs_expr = try self.parse_unit_expr();

        const token = self.tokens.peek(0);

        switch (token.?.kind) {
            .Op => |kind| {
                self.tokens.advance(1); // skip the token
                const rhs_expr = try self.parse_expr();
                expr = .{ .BinOp = try Ast.BinaryOperation.init(self.allocator, kind, lhs_expr, rhs_expr) };
            },
            .ParenClose, .Semi, .Comma => {
                expr = lhs_expr;
            },
            else => {
                std.log.err("parse_expr for token kind {} is not implemented!\n", .{token.?.kind});
                unreachable;
            },
        }
        return expr;
    }
    fn parse_unit_expr(self: *Self) anyerror!Ast.Expression {
        var expr: Ast.Expression = undefined;
        const token = self.tokens.peek(0);
        assert(token != null);
        switch (token.?.kind) {
            .Ident => {
                expr = .{ .Var = token.?.source };
                self.tokens.advance(1); // skip the previous token

                if (std.meta.eql(self.tokens.peek(0).?.kind, .ParenOpen)) {
                    const next_parsed = try self.parse_expr();
                    const params = next_parsed.Tuple; // if there is paren open then it has to be tuple
                    expr = .{ .Call = .{ .name = token.?.source, .params = params } };
                }
            },
            .ParenOpen => {
                const tuple = try self.parse_tuple(); // we don't skip the token here as it is '('
                expr = .{ .Tuple = tuple };
            },
            .LiteralInt => {
                self.tokens.advance(1); // skip the token
                expr = .{ .LiteralInt = token.?.kind.LiteralInt };
            },
            .LiteralString => |str| {
                self.tokens.advance(1); // skip the token
                expr = .{ .LiteralString = str };
            },
            // @TODO(shahzad): i don't think we need this anymore
            // .Semi, .ParenClose => {
            //     expr = .NoOp;
            // },
            else => {
                self.tokens.advance(1); // skip the token
                token.?.print_loc();
                std.debug.panic("expression parsing for {} is not implemented!", .{token.?.kind});
            },
        }

        return expr;
    }

    pub fn expect(self: *Self, expected: TokenKind, context: ?[]const u8) !Token {
        const token = self.tokens.peek(0);
        assert(token != null);

        if (std.meta.eql(token.?.kind, expected)) {
            self.tokens.advance(1);
            return token.?;
        } else {
            std.log.debug("expected {s} but got {s}", .{ context orelse expected.to_str(), token.?.kind.to_str() });

            return Ast.Error.UnexpectedToken;
        }
    }
};
