pub const CompiledExpression = union(enum) {
    const CompiledExpCommon = struct {
        expr: []const u8,
        size: u32,
    };
    const CompiledExprStack = struct {
        offset: usize,
        size: u32,
    };

    Var: CompiledExprStack,
    LitInt: CompiledExprStack,
    LitStr: CompiledExprStack,
    Register: CompiledExpCommon,
    Call: CompiledExpCommon,
};
