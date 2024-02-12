const std = @import("std");

const ast_mod = @import("./ast.zig");
const ASTNode = ast_mod.ASTNode;
const Definition = ast_mod.Definition;
const Expression = ast_mod.Expression;
const Literal = ast_mod.Literal;

pub fn generate(allocator: std.mem.Allocator, input: []const ASTNode) ![]const u8 {
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    for (input) |statement| {
        try output.appendSlice(switch (statement) {
            .definition => |def| try gen_definition(allocator, def),
            .expression => |exp| try gen_expression(allocator, exp),
            .eof => break,
        });
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

            try output.appendSlice(try gen_expression(allocator, variable.expression));
        },
    }

    return output.toOwnedSlice();
}

fn gen_expression(allocator: std.mem.Allocator, input: Expression) ![]const u8 {
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    switch (input) {
        .literal => |lit| try output.appendSlice(try gen_literal(allocator, lit)),
    }

    return output.toOwnedSlice();
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
test "Generate Zig Code" {
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

    std.debug.print("{s}\n", .{output});

    try expectEqualDeep(expected, output);
}
