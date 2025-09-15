const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ascii = std.ascii;
const strings = @import("strings.zig");
const SourceContext = @import("ast.zig").SourceContext;
const libc = @cImport({
    @cInclude("stdio.h");
});

pub const TokenKind = union(enum) {
    // literals
    LiteralInt: u64, // we don't care about bigInts
    LiteralString,
    LiteralFloat: f64,

    //identifier
    Ident,

    //maths
    OpAdd: void,
    OpSub: void,
    OpMul: void,
    OpDiv: void,

    OpAss: void,
    OpEq: void,
    OpLt: void,
    OpGt: void,
    OpLtEq: void,
    OpGtEq: void,

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
            '-' => if (tok.len > 1 and tok[1] == '>') .{ 2, .Arrow } else .{ 1, .OpSub },
            '=' => .{ 1, .OpAss },
            ':' => .{ 1, .Colon },
            ';' => .{ 1, .Semi },
            ',' => .{ 1, .Comma },
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
                if (std.mem.eql(u8, ident, "proc")) {
                    break :blk .{ ident.len, .ProcDecl };
                } else if (std.mem.eql(u8, ident, "let")) {
                    break :blk .{ ident.len, .VarDef };
                } else if (std.mem.eql(u8, ident, "mut")) {
                    break :blk .{ ident.len, .Mut };
                } else if (std.mem.eql(u8, ident, "return")) {
                    break :blk .{ ident.len, .Return };
                }
                break :blk .{ ident.len, .Ident };
            },
            '0'...'9' => blk: {
                const literal_str_length, const literal = strings.parse_int(tok, 0);
                break :blk .{ literal_str_length, .{ .LiteralInt = literal } };
            },
            '"', '\'' => {
                @panic("implemtent string literal parsing");
            },

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
            .OpAdd => "+",
            .OpSub => "-",
            .OpMul => "*",
            .OpDiv => "/",
            .OpAss => "=",
            .OpEq => "==",
            .OpLt => "<",
            .OpGt => ">",
            .OpLtEq => "<=",
            .OpGtEq => ">=",
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
    tokens: std.ArrayList(Token),
    context: SourceContext,

    const Self = @This();
    pub fn init(self: *Self, allocator: Allocator, context: SourceContext) void {
        self.* = .{
            .tokens = std.ArrayList(Token).init(allocator),
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
                    error.InvalidCharacter => {
                        // @TODO(shahazd): better squigly line error reporting shit
                        std.debug.print("error: failed to parse token: \"{s}\"\n", .{current_line});
                        _ = libc.printf("error:%*s^\n", 25 + (current_token_start), "");
                        return;
                    },
                    else => {
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
        try self.tokens.append(.{ .kind = .Eof, .line = undefined, .source = undefined });
    }
};
