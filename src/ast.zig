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

pub const Expression = union(enum) {
    literal: Literal,
};

pub const Literal = union(enum) {
    int: isize,
    flt: f32,
    str: []const u8,
    boolean: bool,
};
