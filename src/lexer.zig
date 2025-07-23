const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ascii = std.ascii;
const strings = @import("strings.zig");
const libc = @cImport({
    @cInclude("stdio.h");
});

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
};

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

    // keywords
    ProcDef: void,
    VarDef: void,

    If: void,
    Else: void,
    While: void,
    For: void,

    Return: void,

    //end of file
    Eof: void,

    pub fn from_str(tok: []const u8) !struct { usize, TokenKind } {
        //TODO(shahzad)!: add function to parse float
        switch (tok[0]) {
            '(' => {
                return .{ 1, .ParenOpen };
            },
            ')' => {
                return .{ 1, .ParenClose };
            },
            '{' => {
                return .{ 1, .CurlyOpen };
            },
            '}' => {
                return .{ 1, .CurlyClose };
            },
            '-' => {
                if (tok.len > 1 and tok[1] == '>') {
                    return .{ 2, .Arrow };
                }
                return .{ 1, .OpSub };
            },
            '=' => {
                return .{ 1, .OpAss };
            },
            ':' => {
                return .{ 1, .Colon };
            },
            ';' => {
                return .{ 1, .Semi };
            },
            'a'...'z', 'A'...'Z', '_' => {
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
                    return .{ ident.len, .ProcDef };
                } else if (std.mem.eql(u8, ident, "let")) {
                    return .{ ident.len, .VarDef };
                } else if (std.mem.eql(u8, ident, "return")) {
                    return .{ ident.len, .Return };
                }

                return .{ ident.len, .Ident };
            },
            '0'...'9' => {
                const literal_str_length, const literal = strings.parse_int(tok, 0);
                return .{ literal_str_length, .{ .LiteralInt = literal } };
            },
            '"', '\'' => {
                @panic("implemtent string literal parsing");
            },

            else => {
                std.log.err("unidentified token \"{c}\"\n", .{tok[0]});
                unreachable;
            },
        }
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
    pub fn print_location(self: Self) void {
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
    pub fn parse(self: *Self) !void {
        var program: []const u8 = self.context.file;
        var current_line: []const u8 = strings.get_line(program);

        while (program.len > 0) {
            program = std.mem.trimLeft(u8, program, " \t");

            if (program[0] == '\r' or program[0] == '\n') {
                //TODO(shahzad): this fks up multiple new lines
                program = std.mem.trimLeft(u8, program, "\r\n");
                current_line = strings.get_line(program);
                continue;
            }
            const current_token_start = (program.ptr - current_line.ptr);

            const consumed_length, const token_kind = TokenKind.from_str(program) catch |err| {
                switch (err) {
                    error.InvalidCharacter => {
                        //TODO(shahazd): better squigly line error reporting shit
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
