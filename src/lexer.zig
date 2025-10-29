const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ascii = std.ascii;

const ArrayListManaged = std.array_list.Managed;

const strings = @import("strings.zig");
const SourceContext = @import("ast.zig").SourceContext;
const libc = @cImport({
    @cInclude("stdio.h");
});

const keywords = [_]struct { []const u8, TokenKind }{
    .{ "proc", .ProcDecl },
    .{ "let", .VarDef },
    .{ "return", .Return },
    .{ "if", .If },
    .{ "else", .Else },
    .{ "while", .While },
};

// @TODO(shahzad): this should be a common thingy
pub const BinOp = enum {
    Ass,
    Add,
    Sub,
    Mul,
    Div,
    AddAss,
    SubAss,
    MulAss,
    DivAss,
    Eq,
    Lt,
    Gt,
    LtEq,
    GtEq,
};

pub const TokenKind = union(enum) {
    // literals
    LiteralInt: u64, // we don't care about bigInts
    LiteralString: []const u8,
    LiteralFloat: f64,

    Op: BinOp,

    //identifier
    Ident: void,
    Pointer: void,

    // native?
    ParenOpen: void,
    ParenClose: void,
    CurlyOpen: void,
    CurlyClose: void,
    Arrow: void,
    Semi: void,
    Colon: void,
    Comma: void,

    // keywords
    ProcDecl: void,
    VarDef: void,
    Mut: void,

    If: void,
    Else: void,
    While: void,
    For: void,
    Not: void,

    Return: void,

    //end of file
    Eof: void,
    const Self = @This();

    pub fn from_str(tok: []const u8) !struct { usize, TokenKind } {
        //@TODO(shahzad)!: add function to parse float
        return switch (tok[0]) {
            '(' => .{ 1, .ParenOpen },
            ')' => .{ 1, .ParenClose },
            '{' => .{ 1, .CurlyOpen },
            '}' => .{ 1, .CurlyClose },
            '=' => if (tok.len > 1 and tok[1] == '=') .{ 2, .{ .Op = .Eq } } else .{ 1, .{ .Op = .Ass } },
            ':' => .{ 1, .Colon },
            ';' => .{ 1, .Semi },
            ',' => .{ 1, .Comma },

            '-' => blk: {
                if (tok.len > 1) {
                    switch (tok[1]) {
                        '>' => break :blk .{ 2, .Arrow },
                        '=' => break :blk .{ 2, .{ .Op = .SubAss } },
                        else => {
                            std.log.err("unidentified token \"{s}\"\n", .{tok[1..]});
                            break :blk .{ 1, .{ .Op = .Sub } };
                        },
                    }
                } else break :blk .{ 1, .{ .Op = .Sub } };
            },

            '+' => if (tok.len > 1 and tok[1] == '=') .{ 2, .{ .Op = .AddAss } } else .{ 1, .{ .Op = .Add } },
            '*' => if (tok.len > 1 and tok[1] == '=') .{ 2, .{ .Op = .MulAss } } else .{ 1, .{ .Op = .Mul } },
            '/' => if (tok.len > 1 and tok[1] == '=') .{ 2, .{ .Op = .DivAss } } else .{ 1, .{ .Op = .Div } },
            '^' => .{ 1, .Pointer },

            'a'...'z', 'A'...'Z', '_' => blk: {
                var ident_idx: usize = 0;
                var ident: []const u8 = tok;
                while (ident_idx < ident.len) {
                    if (!(ascii.isAlphanumeric(ident[ident_idx]) or ident[ident_idx] == '_')) {
                        break;
                    }
                    ident_idx += 1;
                }
                ident.len = ident_idx;
                for (keywords) |keyword| {
                    if (std.mem.eql(u8, ident, keyword[0])) {
                        break :blk .{ ident.len, keyword[1] };
                    }
                }
                break :blk .{ ident.len, .Ident };
            },
            '0'...'9' => blk: {
                const literal_str_length, const literal = strings.parse_int(tok, 0);
                break :blk .{ literal_str_length, .{ .LiteralInt = literal } };
            },
            '"', '\'' => blk: {
                const literal = try strings.parse_string_literal(tok);
                break :blk .{ literal.len + 1, .{ .LiteralString = literal[0 .. literal.len - 1] } };
            },
            '!' => .{ 1, .Not },
            '<' => .{ 1, .{ .Op = .Lt } },

            else => {
                std.log.err("unidentified token \"{c}\"\n", .{tok[0]});
                unreachable;
            },
        };
    }
    pub fn to_str(self: Self) []const u8 {
        return switch (self) {
            .LiteralInt => "int_lit",
            .LiteralString => "string_lit",
            .LiteralFloat => "float_lit",
            .Ident => "ident",

            .Op => |op| blk: switch (op) {
                .Add => break :blk "+",
                .Sub => break :blk "-",
                .Mul => break :blk "*",
                .Div => break :blk "/",
                .Ass => break :blk "=",
                .AddAss => break :blk "+=",
                .SubAss => break :blk "-=",
                .MulAss => break :blk "*=",
                .DivAss => break :blk "/=",
                .Eq => "==",
                .Lt => "<",
                .Gt => ">",
                .LtEq => "<=",
                .GtEq => ">=",
            },
            .Pointer => "^",
            .ParenOpen => "(",
            .ParenClose => ")",
            .CurlyOpen => "{",
            .CurlyClose => "}",
            .Arrow => "->",
            .Semi => ";",
            .Colon => ":",
            .Comma => ",",
            .ProcDecl => "proc",
            .VarDef => "let",
            .Mut => "mut",
            .If => "if",
            .Else => "else",
            .While => "while",
            .For => "for",
            .Not => "!",
            .Return => "return",
            .Eof => "eof",
        };
    }
};
pub const Token = struct {
    kind: TokenKind,
    line: []const u8,
    source: []const u8,

    const Self = @This();
    fn init(token_kind: TokenKind, line: []const u8, source: []const u8) Token {
        return .{
            .kind = token_kind,
            .line = line,
            .source = source,
        };
    }
    pub fn print_loc(self: Self) void {
        const current_line = self.line;
        const current_token_start = (self.source.ptr - current_line.ptr);

        std.debug.print("error: unexpected token: \"{s}\"\n", .{current_line});
        _ = libc.printf("error:%*s^\n", 20 + (current_token_start), "");
    }
};

pub const Lexer = struct {
    tokens: ArrayListManaged(Token),
    context: SourceContext,

    const Self = @This();
    pub fn init(self: *Self, allocator: Allocator, context: SourceContext) void {
        self.* = .{
            .tokens = ArrayListManaged(Token).init(allocator),
            .context = context,
        };
    }
    pub fn tokenize(self: *Self) !void {
        var program: []const u8 = self.context.file;
        var current_line: []const u8 = strings.get_line(program);

        while (program.len > 0) {
            program = std.mem.trimLeft(u8, program, " \t");
            if (std.mem.startsWith(u8, program, "//")) {
                const comment_end: usize = std.mem.indexOfAny(u8, program, "\r\n") orelse program.len;
                assert(comment_end <= program.len);

                program = program[comment_end..];
                current_line = strings.get_line(program);
                continue;
            }

            if (program[0] == '\r' or program[0] == '\n') {
                // @TODO(shahzad): this fks up multiple new lines
                program = std.mem.trimLeft(u8, program, "\r\n");
                current_line = strings.get_line(program);
                continue;
            }
            const current_token_start = (program.ptr - current_line.ptr);

            const consumed_length, const token_kind = TokenKind.from_str(program) catch |err| {
                switch (err) {
                    else => {

                        // @TODO(shahazd): better squigly line error reporting shit
                        std.debug.print("error: failed to parse token: \"{s}\"\n", .{current_line});
                        _ = libc.printf("error:%*s^\n", 25 + (current_token_start), "");
                        return err;
                    },
                }
            };
            const source = current_line[current_token_start .. current_token_start + consumed_length];

            const token = Token.init(
                token_kind,
                current_line,
                source,
            );

            program = program[consumed_length..];

            try self.tokens.append(token);
        }
        try self.tokens.append(.{ .kind = .Eof, .line = current_line, .source = program[program.len..] });
    }
};
