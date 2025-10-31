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
    module: Ast.Module = undefined,
    const Self = @This();
    pub fn init(allocator: std.mem.Allocator, tokens: []Token) Self {
        return .{ .allocator = allocator, .tokens = Peekable([]Token).init(tokens) };
    }
    pub fn parse(self: *Self, source_context: Ast.SourceContext) !Ast.Module {
        var has_main_procedure: bool = false;

        self.module.init(self.allocator, source_context);
        while (self.tokens.peek(0).?.kind != .Eof) {
            const token: Token = self.tokens.peek(0).?;
            switch (token.kind) {
                .ProcDecl => {
                    const proc_decl = try self.parse_proc();
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
                .Plex => {
                    const plex = try self.parse_plex();
                    try self.module.plex_decl.append(plex);
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

        self.module.has_main_proc = has_main_procedure;
        return self.module;
    }
    // parse the procedure and return only the declaration for some use
    pub fn parse_plex(self: *Self) !Ast.PlexDecl {
        _ = try self.expect(.Plex, null);
        const plex_name = try self.expect(.Ident, "plex name");
        _ = try self.expect(.CurlyOpen, "{");

        var block = ArrayListManaged(Ast.PlexField).init(self.allocator);
        errdefer block.deinit();

        while (!std.meta.eql(self.tokens.peek(0).?.kind, .CurlyClose)) {
            const field_name = try self.expect(.Ident, "field name");
            _ = try self.expect(.Colon, ":");

            var ptr_depth: usize = 0;

            while (self.tokens.peek(0).?.kind == .Pointer) : (self.tokens.advance(1)) {
                ptr_depth += 1;
            }

            // TODO(shahzad): @feat add support for default struct fields
            const type_name = try self.expect(.Ident, "field name");
            const field_type: Ast.ExprType = .{ .type = type_name.source, .info = .{ .ptr_depth = ptr_depth } };

            if (self.tokens.peek(0).?.kind == .Comma) {
                self.tokens.advance(1);
            }

            const plex_field: Ast.PlexField = .{ .name = field_name.source, .type = field_type, .expr = .NoOp, .size = 0, .offset = 0 };
            _ = try block.append(plex_field);
        }
        self.tokens.advance(1); // we got curly close
        return .{ .name = plex_name.source, .fields = block, .size = null };
    }
    pub fn parse_proc(self: *Self) !Ast.ProcDecl {
        const proc_decl = self.parse_proc_decl() catch |err| {
            self.tokens.peek(0).?.print_loc();
            return err;
        };
        switch (self.tokens.peek(0).?.kind) {
            .Semi => {
                try self.module.proc_decls.append(proc_decl);
                self.tokens.advance(1);
            },
            .CurlyOpen => {
                // @TODO(shahzad)!!!!!: instead of duplicating the procedure declaration
                // only store it in the ProcDecl array and attach an index to it that specifies
                // the procDef
                const code_block = try self.parse_block();
                try self.module.proc_defs.append(.init(proc_decl, code_block));
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
        arg_def.init(var_name, 0, var_type, ptr_depth, mutable);
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

    fn set_outer_if_type_contains_block(expr: *const Ast.Expression, outer: *Ast.Block) void {
        switch (expr.*) {
            .Block => |*block| {
                block.*.outer = outer;
            },
            .IfCondition, .WhileLoop => |*condition| {
                condition.block.outer = outer;
            },

            .Plex, .NoOp, .Var, .LiteralInt, .LiteralString, .Call, .Tuple, .BinOp => {},
        }
    }
    fn parse_block(self: *Self) anyerror!*Ast.Block {
        _ = try self.expect(.CurlyOpen, null);

        const block = try self.module.blocks.create(Ast.Block);

        var statements = ArrayListManaged(Ast.Statement).init(self.allocator);
        errdefer statements.deinit();

        while (!std.meta.eql(self.tokens.peek(0).?.kind, .CurlyClose)) {
            const statement = try self.parse_stmt();
            switch (statement) {
                .VarDefStackMut, .VarDefStack => |var_def| {
                    set_outer_if_type_contains_block(&var_def.expr, block);
                },
                .Expr => {
                    set_outer_if_type_contains_block(&statement.Expr, block);
                },
                .Return => {
                    @panic("this is unimplemented!");
                },
                else => unreachable,
            }
            try statements.append(statement);
            std.log.debug("{}\n", .{statement});
        }
        self.tokens.advance(1);
        block.* = .{ .stmts = statements, .stack_vars = .init(self.module.allocator), .outer = null };
        return block;
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

                var var_type: ?Ast.ExprType = null;

                if (std.meta.eql(token.?.kind, .Colon)) {
                    self.tokens.advance(1);

                    var ptr_depth: usize = 0;

                    while (self.tokens.peek(0).?.kind == .Pointer) : (self.tokens.advance(1)) {
                        ptr_depth += 1;
                    }

                    var_type = .{ .type = (try self.expect(.Ident, "'type definition'")).source, .info = .{ .ptr_depth = ptr_depth } };
                }

                token = self.tokens.peek(0);
                assert(token != null);

                var expr: Ast.Expression = .NoOp;
                if (std.meta.eql(token.?.kind, .{ .Op = .Ass })) {
                    self.tokens.advance(1);
                    expr = try self.parse_expr();
                }

                // @TODO(shahzad): add support for assignment during initialization of variable

                _ = self.expect(.Semi, null) catch |err| {
                    self.tokens.peek(0).?.print_loc();
                    return err;
                };
                if (is_mut) {
                    return .{ .VarDefStackMut = .{ .name = var_name, .type = var_type, .expr = expr } };
                } else {
                    return .{ .VarDefStack = .{ .name = var_name, .type = var_type, .expr = expr } };
                }
            },
            .CurlyOpen => {
                const block = try self.parse_block();
                return .{ .Expr = .{ .Block = block } };
            },
            .Ident => {
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
            .If => {
                _ = self.tokens.consume();
                // parse the condition
                const expr = try self.parse_expr();
                const block = try self.parse_block();

                const expr_duped = try self.allocator.create(Ast.Expression);
                expr_duped.* = expr;

                return .{ .Expr = .{ .IfCondition = .{ .condition = expr_duped, .block = block } } };
            },
            .While => {
                _ = self.tokens.consume();
                // parse the condition
                const expr = try self.parse_expr();
                const block = try self.parse_block();

                const expr_duped = try self.allocator.create(Ast.Expression);
                expr_duped.* = expr;

                return .{ .Expr = .{ .WhileLoop = .{ .condition = expr_duped, .block = block } } };
            },
            .Return => {
                self.tokens.advance(1);
                var expr: Ast.Expression = .NoOp;
                if (!std.meta.eql(self.tokens.peek(0).?.kind, .Semi)) {
                    expr = try self.parse_expr();
                }
                _ = try self.expect(.Semi, null);
                return .{ .Return = expr };
            },
            else => {
                std.debug.panic("parse_statement for {} is not implemented!", .{token.?.kind});
            },
        }
    }
    fn get_paren_close_for_tok(paren_start: TokenKind) TokenKind {
        switch (paren_start) {
            .ParenOpen => return .ParenClose,
            .CurlyOpen => return .CurlyClose,
            else => unreachable,
        }
    }
    fn parse_exprs_between(self: *Self, paren_start: TokenKind) !ArrayListManaged(Ast.Expression) {
        const paren_end = get_paren_close_for_tok(paren_start);
        var params = ArrayListManaged(Ast.Expression).init(self.allocator);
        _ = try self.expect(paren_start, null);
        const token_kind = self.tokens.peek(0).?.kind;
        if (std.meta.eql(token_kind, paren_end)) {
            self.tokens.advance(1);
            return params;
        }
        while (true) {
            const expr = try self.parse_expr();
            try params.append(expr);
            const next = self.tokens.peek(0).?;
            if (std.meta.eql(next.kind, paren_end)) {
                self.tokens.advance(1);
                break;
            }

            switch (next.kind) {
                .Comma => {
                    self.tokens.advance(1);
                    continue;
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
            .ParenOpen, .CurlyOpen, .ParenClose, .CurlyClose, .Semi, .Comma => {
                expr = lhs_expr;
            },
            else => {
                std.log.err("expected expression found {}\n", .{token.?.kind});
                token.?.print_loc();
                return error.UnexpectedToken;
            },
        }
        return expr;
    }
    fn parse_plex_def_fields(self: *Self) anyerror!Ast.Expression.PlexDef {
        const exprs = try self.parse_exprs_between(.CurlyOpen);
        return .{ .name = undefined, .members = exprs };
    }
    fn is_ident_proc_name(self: *Self, ident: []const u8) bool {
        // useless
        if (self.module.get_proc(ident)) |proc| return std.mem.eql(u8, proc.name, ident);
        return false;
    }
    fn parse_unit_expr(self: *Self) anyerror!Ast.Expression {
        var expr: Ast.Expression = undefined;
        const token = self.tokens.peek(0);
        assert(token != null);
        switch (token.?.kind) {
            .Ident => blk: {
                expr = .{ .Var = token.?.source };
                self.tokens.advance(1); // skip the previous token
                const next = self.tokens.peek(0).?;

                if (next.kind == .ParenOpen and
                    self.is_ident_proc_name(token.?.source))
                {
                    std.debug.print("proc name is {s}\n", .{token.?.source});
                    const next_parsed = try self.parse_expr();
                    const params = next_parsed.Tuple; // if there is paren open then it has to be tuple
                    expr = .{ .Call = .{ .name = token.?.source, .params = params } };
                } else if (next.kind == .CurlyOpen) {
                    const prev_tokens = self.tokens;
                    std.debug.print("plex name is {s}\n", .{token.?.source});
                    var plex_literal = self.parse_plex_def_fields() catch {
                        self.tokens = prev_tokens;
                        expr = .{ .Var = token.?.source };
                        break :blk;
                    };
                    plex_literal.name = token.?.source;
                    expr = .{ .Plex = plex_literal };
                }
            },
            .ParenOpen => {
                const tuple = try self.parse_exprs_between(.ParenOpen); // we don't skip the token here as it is '('
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
            .CurlyOpen => {
                expr = .{ .Block = try self.parse_block() };
            },
            .ParenClose, .CurlyClose => expr = .NoOp,

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
            token.?.print_loc();

            return Ast.Error.UnexpectedToken;
        }
    }
};
