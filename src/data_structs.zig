const std = @import("std");

pub const ASTNode = union(enum) {
    definition: Definition,
    expression: Expression,
    eof: void,

    const Self = @This();
    pub fn assert_eq(self: *const Self, other: *const ASTNode) !void {
        try switch (self.*) {
            .definition => |def_self| switch (other.*) {
                .definition => |def_other| try def_self.assert_eq(&def_other),
                else => error.NotEqual,
            },
            .expression => |expr_self| switch (other.*) {
                .expression => |expr_other| try expr_self.assert_eq(&expr_other),
                else => error.NotEqual,
            },
            .eof => switch (other.*) {
                .eof => {},
                else => error.NotEqual,
            }
        };
    }
};

pub const Definition = union(enum) {
    variable: struct {
        identifier: []const u8,
        mutable: bool = false,
        expression: Expression,
    },
    
    const Self = @This();
    pub fn assert_eq(self: *const Self, other: *const Definition) !void {
        const expect = std.testing.expect;
        const expectEqualStrings = std.testing.expectEqualStrings;
        switch (self.*) {
            .variable => |var_self| switch (other.*) {
                .variable => |var_other| {
                    try expectEqualStrings(var_self.identifier, var_other.identifier);
                    try expect(var_self.mutable == var_other.mutable);
                    try var_self.expression.assert_eq(&var_other.expression);
                },
            },
        }
    }
};

pub const Expression = union(enum) {
    literal: Literal,
    identifier: []const u8,
    binary: struct {
        lhs: *const Expression,
        operator: BinaryOperator,
        rhs: *const Expression,
    },
    
    const Self = @This();
    pub fn assert_eq(self: *const Self, other: *const Expression) !void {
        const expectEqualDeep = std.testing.expectEqualDeep;
        const expectEqualStrings = std.testing.expectEqualStrings;
        try switch (self.*) {
            .literal => |lit_self| switch (other.*) {
                .literal => |lit_other| try lit_self.assert_eq(&lit_other),
                else => error.NotEqual,
            },
            .identifier => |id_self| switch (other.*) {
                .identifier => |id_other| try expectEqualStrings(id_self, id_other),
                else => error.NotEqual,
            },
            .binary => |bin_self| switch (other.*) {
                .binary => |bin_other| {
                    try expectEqualDeep(bin_self.operator, bin_other.operator);
                    try bin_self.lhs.assert_eq(bin_other.lhs);
                    try bin_self.rhs.assert_eq(bin_other.rhs);
                },
                else => error.NotEqual,
            },
        };
    }
};

pub const Literal = union(enum) {
    int: isize,
    flt: []const u8,
    str: []const u8,
    boolean: bool,
    
    const Self = @This();
    pub fn assert_eq(self: *const Self, other: *const Literal) !void {
        try std.testing.expectEqualDeep(self, other);
    }
};

pub const Token = union(enum) {
    ident: []const u8,
    int: []const u8,
    flt: []const u8,
    str: []const u8,
    boolean: bool,

    binary_operator: BinaryOperator,
    unary_prefix_operator: UnaryPrefixOperator,

    let,
    mut,
    dash,
    assign,
    newline,
    cantTouchThis,
    eof,
    illegal,

    const Self = @This();

    pub fn keyword(ident: []const u8) ?Token {
        const map = std.ComptimeStringMap(Token, .{
            .{ "let", .let },
            .{ "mut", .mut },
            .{ "ctt", .cantTouchThis },
            .{ "True", .{ .boolean = true } },
            .{ "False", .{ .boolean = false } },
        });
        return map.get(ident);
    }

    pub fn get_binary_operator(self: *const Self) ?BinaryOperator {
        return switch (self.*) {
            .binary_operator => |op| op,
            else => null,
        };
    }
};

pub const UnaryPrefixOperator = union(enum) {
    not,
    negative,
};

pub const BinaryOperator = union(enum) {
    add,
    subtract,
    multiply,
    divide,

    pub fn precedence(self: *BinaryOperator) u8 {
        return switch (self.*) {
            .multiply, .divide => 2,
            .add, .subtract => 1,
        };
    }
};
