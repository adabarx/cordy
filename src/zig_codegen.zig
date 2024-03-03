const std = @import("std");

const data_structs = @import("data_structs.zig");
const ASTNode = data_structs.ASTNode;
const Definition = data_structs.Definition;
const Expression = data_structs.Expression;
const Literal = data_structs.Literal;
const BinaryOperator = data_structs.BinaryOperator;

pub fn generate(allocator: std.mem.Allocator, input: []const ASTNode) ![]const u8 {
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    for (input) |statement| {
        switch (statement.node) {
            .definition => |def| {
                const rv = try gen_definition(allocator, def);
                defer allocator.free(rv);
                try output.appendSlice(rv);
            },
            .expression => |exp| {
                const rv = try gen_expression(allocator, exp);
                defer allocator.free(rv);
                try output.appendSlice(rv);
            },
            .eof => break,
        }
        try output.appendSlice(";\n");
    }
    return output.toOwnedSlice();
}

fn gen_definition(allocator: std.mem.Allocator, input: Definition) ![]const u8 {
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    switch (input) {
        .variable => |variable| {
            if (variable.mutable) {
                try output.appendSlice("var ");
            } else {
                try output.appendSlice("const ");
            }

            try output.appendSlice(variable.identifier);
            try output.appendSlice(" = ");

            const rv = try gen_expression(allocator, variable.expression);
            defer allocator.free(rv);
            try output.appendSlice(rv);
        },
    }

    return output.toOwnedSlice();
}

fn gen_expression(allocator: std.mem.Allocator, input: *const Expression) ![]const u8 {
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    switch (input.*) {
        .identifier => |id| try output.appendSlice(id),
        .literal => |lit| {
            const rv = try gen_literal(allocator, lit);
            defer allocator.free(rv);

            try output.appendSlice(rv);
        },
        .binary => |expr| {
            const lhs = try gen_expression(allocator, expr.lhs);
            const rhs = try gen_expression(allocator, expr.rhs);
            defer allocator.free(lhs);
            defer allocator.free(rhs);

            try output.appendSlice(lhs);
            try output.appendSlice(gen_operator(expr.operator));
            try output.appendSlice(rhs);
        },
    }

    return output.toOwnedSlice();
}

fn gen_operator(op: BinaryOperator) []const u8 {
    return switch (op) {
        .assign => " = ",

        .and_tok => " and ",
        .or_tok => " or ",

        .equal => " == ",
        .not_equal => " != ",
        .greater_than => " > ",
        .lesser_than => " < ",
        .greater_equal => " >= ",
        .lesser_equal => " <= ",

        .subtract => " - ",
        .add => " + ",
        .multiply => " * ",
        .divide => " / ",

        .struct_access => ".",
    };
}

fn gen_literal(allocator: std.mem.Allocator, input: Literal) ![]const u8 {
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    switch (input) {
        .int => |int| {
            const rv = try std.fmt.allocPrint(allocator, "{d}", .{int});
            defer allocator.free(rv);
            try output.appendSlice(rv);
        },
        .flt => |flt| try output.appendSlice(flt),
        .str => |str| {
            try output.append('"');
            try output.appendSlice(str);
            try output.append('"');
        },
        .boolean => |boolean| {
            if (boolean) {
                try output.appendSlice("true");
            } else {
                try output.appendSlice("false");
            }
        },
    }

    return output.toOwnedSlice();
}

const expectEqualDeep = std.testing.expectEqualDeep;
test "Generate Assignment" {
    const input = [_]ASTNode{
        ASTNode{ .definition = .{ .variable = .{ .identifier = "five", .mutable = false, .expression = .{ .literal = .{ .int = 5 } } } } },
        ASTNode{ .definition = .{ .variable = .{ .identifier = "neg_ten", .mutable = false, .expression = .{ .literal = .{ .int = -10 } } } } },
        ASTNode{ .definition = .{ .variable = .{ .identifier = "pi", .mutable = true, .expression = .{ .literal = .{ .flt = "3.14" } } } } },
        ASTNode{ .definition = .{ .variable = .{ .identifier = "neg_e", .mutable = true, .expression = .{ .literal = .{ .flt = "-2.72" } } } } },
        ASTNode{ .definition = .{ .variable = .{ .identifier = "hello", .mutable = false, .expression = .{ .literal = .{ .str = "world" } } } } },
    };

    const expected: []const u8 =
        \\const five = 5;
        \\const neg_ten = -10;
        \\var pi = 3.14;
        \\var neg_e = -2.72;
        \\const hello = "world";
        \\
    ;

    const output = try generate(std.testing.allocator, &input);

    // std.debug.print("{s}\n", .{output});

    try expectEqualDeep(expected, output);
    std.testing.allocator.free(output);
}
