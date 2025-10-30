pub const CompiledExpression = union(enum) {
    pub const CompiledExpCommon = struct {
        expr: []const u8,
        size: u32,
    };
    pub const CompiledExprStack = struct {
        offset: usize,
        size: u32,
    };
    pub const CompiledExprLiteral = struct {
        literal: u64,
        size: u32,
    };

    Var: CompiledExprStack,
    PlexLiteral: void, // compile_plex takes storage as argument so we don't need to pass shit
    LitInt: CompiledExprLiteral,
    LitStr: CompiledExpCommon,
    Register: CompiledExpCommon,
    Call: CompiledExpCommon,
    // i've piled garbage onto garbage so much that i have to
    // do the oop :sob: we will have to refactor ts
    pub fn get_size(self: @This()) u32 {
        switch (self) {
            inline .Var, .PlexLiteral => |compiled_expr| return compiled_expr.size,
            inline .LitInt => |compiled_expr| return compiled_expr.size,
            inline .LitStr, .Register, .Call => |compiled_expr| return compiled_expr.size,
        }
    }
};
