const std = @import("std");
const expectEqualDeep = std.testing.expectEqualDeep;
const print = std.debug.print;

const AssertError = error {
    Statement__SelfExpression_OtherDefinition,
    Statement__SelfExpression_OtherEOF,
    Statement__SelfEOF_OtherDefinition,
    Statement__SelfEOF_OtherExpression,
    Statement__SelfDefinition_OtherExpression,
    Statement__SelfDefinition_OtherEOF,
    Expression__SelfLiteral_OtherIdent,
    Expression__SelfLiteral_OtherBinary,
    Expression__SelfIdent_OtherLiteral,
    Expression__SelfIdent_OtherBinary,
    Expression__SelfBinary_OtherLiteral,
    Expression__SelfBinary_OtherIdent,
};

pub const ASTNode = union(enum) {
    definition: Definition,
    expression: Expression,
    eof: void,

    const Self = @This();
    pub fn prittyprint(self: *const Self) void {
        print("\n\nStatement:\n", .{});

        switch (self.*) {
            .definition => |def| def.prittyprint(2),
            .expression => |exp| exp.prittyprint(2),
            .eof => print("EOF\n", .{}),
        }
    }

    pub fn assert_eq(self: *const Self, other: *const ASTNode) !void {
        switch (self.*) {
            .definition => |def_self| switch (other.*) {
                .definition => |def_other| try def_self.assert_eq(&def_other),
                .expression => return AssertError.Statement__SelfDefinition_OtherExpression,
                .eof => return AssertError.Statement__SelfDefinition_OtherEOF,
            },
            .expression => |expr_self| switch (other.*) {
                .expression => |expr_other| try expr_self.assert_eq(&expr_other),
                .definition => return AssertError.Statement__SelfExpression_OtherDefinition,
                .eof => return AssertError.Statement__SelfExpression_OtherEOF,
            },
            .eof => switch (other.*) {
                .eof => {},
                .definition => return AssertError.Statement__SelfEOF_OtherDefinition,
                .expression => return AssertError.Statement__SelfEOF_OtherExpression,
            }
        }
    }
};

pub const Definition = union(enum) {
    variable: struct {
        identifier: []const u8,
        mutable: bool = false,
        expression: Expression,
    },
    
    const Self = @This();
    pub fn prittyprint(self: *const Self, spaces: u8) void {
        p_spaces(spaces);
        std.debug.print("Definition\n", .{});
        switch (self.*) {
            .variable => |vari| {
                p_spaces(spaces + 2);
                print("Variable:\n", .{});
                p_spaces(spaces + 4);
                print("Ident: {s}\n", .{vari.identifier});
                p_spaces(spaces + 4);
                print("Mutable: {s}\n", .{if (vari.mutable) "True" else "False"});
                vari.expression.prittyprint(spaces + 4);
            }
        }
    }

    pub fn assert_eq(self: *const Self, other: *const Definition) !void {
        switch (self.*) {
            .variable => |var_self| switch (other.*) {
                .variable => |var_other| {
                    try expectEqualDeep(var_self.identifier, var_other.identifier);
                    try expectEqualDeep(var_self.mutable, var_other.mutable);
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
    pub fn prittyprint(self: *const Self, spaces: u8) void {
        p_spaces(spaces);
        print("Expression:\n", .{});
        switch (self.*) {
            .literal => |lit| lit.prittyprint(spaces + 2), 
            .identifier => |ident| {
                defer print("\n", .{});
                p_spaces(spaces + 2);
                print("Ident: {s}", .{ident});
            },
            .binary => |bin| {
                defer print("\n", .{});
                p_spaces(spaces + 2);
                print("Binary:\n", .{});
                p_spaces(spaces + 4);
                print("Operator: {s}\n", .{switch (bin.operator) {
                    .add => "+",
                    .subtract => "-",
                    .multiply => "*",
                    .divide => "/",
                }});
                p_spaces(spaces + 4);
                print("Left:\n", .{});
                bin.lhs.prittyprint(spaces + 6);
                p_spaces(spaces + 4);
                print("Right:\n", .{});
                bin.lhs.prittyprint(spaces + 6);
            }
        }
    }

    pub fn assert_eq(self: *const Self, other: *const Expression) !void {
        switch (self.*) {
            .literal => |lit_self| switch (other.*) {
                .literal => |lit_other| try lit_self.assert_eq(&lit_other),
                .identifier => return AssertError.Expression__SelfLiteral_OtherIdent,
                .binary => return AssertError.Expression__SelfLiteral_OtherBinary,
            },
            .identifier => |id_self| switch (other.*) {
                .identifier => |id_other| try expectEqualDeep(id_self, id_other),
                .literal => return AssertError.Expression__SelfIdent_OtherLiteral,
                .binary => return AssertError.Expression__SelfIdent_OtherBinary,
            },
            .binary => |bin_self| switch (other.*) {
                .binary => |bin_other| {
                    try expectEqualDeep(bin_self.operator, bin_other.operator);
                    try bin_self.lhs.assert_eq(bin_other.lhs);
                    try bin_self.rhs.assert_eq(bin_other.rhs);
                },
                .literal => return AssertError.Expression__SelfBinary_OtherLiteral,
                .identifier => return AssertError.Expression__SelfBinary_OtherIdent,
            },
        }
    }
};

pub const Literal = union(enum) {
    int: isize,
    flt: []const u8,
    str: []const u8,
    boolean: bool,
    
    const Self = @This();
    pub fn prittyprint(self: *const Self, spaces: u8) void {
        p_spaces(spaces);
        print("Literal:\n", .{});
        p_spaces(spaces + 2);
        switch (self.*) {
            .int => |i| print("Int: {}", .{i}),
            .flt => |f|print("Flt: {s}", .{f}),
            .str => |s| print("Str: {s}", .{s}),
            .boolean => |b| print("Bool: {s}", .{if (b) "True" else "False"}),
        }
        print("\n", .{});
    }

    pub fn assert_eq(self: *const Self, other: *const Literal) !void {
        std.debug.print("\nexpected: {any}; found: {any}\n", .{self, other});
        try expectEqualDeep(self, other);
    }
};

const as = std.zig.Ast;

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

    pub fn precedence(self: *const BinaryOperator) u8 {
        return switch (self.*) {
            .multiply, .divide => 1,
            .add, .subtract => 0,
        };
    }
};

fn p_spaces(num: u8) void {
    for (0..num) |_| {
        std.debug.print(" ", .{});
    }
}
