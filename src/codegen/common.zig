pub const CompiledExpression = union(enum) {
    const CompiledExpCommon = struct {
        expr: []const u8,
        size: u32,
    };
    const CompiledExprStack = struct {
        offset: usize,
        size: u32,
    };
    const CompiledExprLiteral = struct {
        literal: u64,
        size: u32,
    };

    Var: CompiledExprStack,
    LitInt: CompiledExprLiteral,
    LitStr: CompiledExpCommon,
    Register: CompiledExpCommon,
    Call: CompiledExpCommon,
};
