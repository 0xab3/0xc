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
    pub const CompiledExprField = struct {
        initial_expr: *CompiledExpression,
        size: u32,
        offset: u32,
    };

    Var: CompiledExprStack,
    FieldAccess: CompiledExprField,
    PlexLiteral: void, // compile_plex takes storage as argument so we don't need to pass shit
    LitInt: CompiledExprLiteral,
    LitStr: CompiledExpCommon,
    Register: CompiledExpCommon,
    Call: CompiledExpCommon,
    // TODO(shahzad): @refactor i've piled garbage onto garbage so much that i have to
    // do the oop :sob: we will have to refactor ts
    pub fn get_size(self: @This()) u32 {
        switch (self) {
            inline .Var => |compiled_expr| return compiled_expr.size,
            inline .LitInt => |compiled_expr| return compiled_expr.size,
            inline .LitStr, .Register, .Call => |compiled_expr| return compiled_expr.size,
            inline .FieldAccess => |compiled_expr| return compiled_expr.size,
            inline .PlexLiteral => unreachable,
        }
    }
};
