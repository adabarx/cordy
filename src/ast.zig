const BinaryOperator = @import("lexer.zig").BinaryOperator;

pub const ASTNode = union(enum) {
    definition: Definition,
    expression: Expression,
    eof,
};

pub const Definition = union(enum) {
    variable: struct {
        identifier: []const u8,
        mutable: bool = false,
        expression: Expression,
    },
};

pub const Expression = union(enum) { literal: Literal, binary: struct {
    lhs: Expression,
    operator: BinaryOperator,
    rhs: Expression,
} };

pub const Literal = union(enum) {
    int: isize,
    flt: []const u8,
    str: []const u8,
    boolean: bool,
};
