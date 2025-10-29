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
    LitInt: CompiledExprLiteral,
    LitStr: CompiledExpCommon,
    Register: CompiledExpCommon,
    Call: CompiledExpCommon,
};
