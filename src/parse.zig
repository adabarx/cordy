const std = @import("std");

const ast_mod = @import("ast.zig");
const ASTNode = ast_mod.ASTNode;
const Expression = ast_mod.Expression;
const Literal = ast_mod.Literal;

const Token = @import("lexer.zig").Token;
const BinaryOperator = @import("lexer.zig").BinaryOperator;

const Parser = struct {
    position: usize = 0,
    input: []const Token,

    const ParseError = error{
        IllegalStatement,
        IllegalLetIdent,
        IllegalLetAssign,
        IllegalExpression,
        IllegalLiteral,
    };

    const Self = @This();

    fn read_token(self: *Self) Token {
        if (self.position >= self.input.len) return .eof;
        return self.input[self.position];
    }

    fn next_token(self: *Self) Token {
        self.position += 1;
        if (self.position >= self.input.len) return .eof;
        return self.input[self.position];
    }

    fn peek_token(self: *Self, spaces: usize) Token {
        if (self.position + spaces >= self.input.len) return .eof;
        return self.input[self.position + spaces];
    }

    pub fn parse_statement(self: *Self) !ASTNode {
        while (self.read_token() == .newline) _ = self.next_token();

        return switch (self.read_token()) {
            .let => try self.parse_let_statement(),
            .int, .flt, .str, .boolean => {
                return ASTNode{ .expression = try self.parse_expression(0) };
            },
            .eof => return ASTNode.eof,
            else => ParseError.IllegalStatement,
        };
    }

    fn parse_let_statement(self: *Self) !ASTNode {
        var mutt = false;
        if (self.next_token() == .mut) {
            mutt = true;
            _ = self.next_token();
        }
        const ident: []const u8 = try switch (self.read_token()) {
            .ident => |name| name,
            else => ParseError.IllegalLetIdent,
        };
        return switch (self.next_token()) {
            .assign => {
                _ = self.next_token();
                return ASTNode{ .definition = .{ .variable = .{
                    .identifier = ident,
                    .mutable = mutt,
                    .expression = try self.parse_expression(0),
                } } };
            },
            else => ParseError.IllegalLetAssign,
        };
    }

    fn parse_expr_leaf(self: *Self) !Expression {
        std.debug.print("\nparse leaf\n", .{});
        return switch (self.read_token()) {
            .int, .flt, .str, .boolean => Expression{ .literal = try self.parse_literal() },
            else => ParseError.IllegalExpression,
        };
    }

    fn parse_expression(self: *Self, precedence: u8) !Expression {
        var leaf = try self.parse_expr_leaf();
        while (true) {
            var bin_exp = try self.parse_binary_expression(leaf, precedence);
            if (std.meta.eql(leaf, bin_exp)) break;
            leaf = bin_exp;
        }
        return leaf;
    }

    fn parse_binary_expression(self: *Self, lhs: Expression, precedence: u8) !Expression {
        // if precedence decreases: left to right (non recursive)
        // if precedence increases: right to left (recursive)
        var curr_operator: BinaryOperator =
            try if (self.next_token().get_binary_operator()) |op| op
            else return lhs;

        _ = self.next_token();
        if (curr_operator.precedence() <= precedence) {
            return .{
                .binary = .{
                    .lhs = lhs,
                    .operator = curr_operator,
                    .rhs = self.parse_expr_leaf(),
                }
            };
        } else {
            return try self.parse_expression(curr_operator.precedence());
        }
    }

    fn parse_literal(self: *Self) !Literal {
        defer _ = self.next_token();
        return switch (self.read_token()) {
            .int => |val| Literal{ .int = try std.fmt.parseInt(isize, val, 10) },
            .flt => |val| {
                // see if it parses into a float and store as str
                // this stops a three digit literal from exploding
                // into a long string of digits at codegen
                _ = try std.fmt.parseFloat(f32, val);
                return Literal{ .flt = val };
            },
            .str => |val| Literal{ .str = val },
            .boolean => |val| Literal{ .boolean = val },
            else => ParseError.IllegalLiteral,
        };
    }
};

pub fn parse_tokens(allocator: std.mem.Allocator, tokens: []const Token) []const ASTNode {
    var parser: Parser = .{ .input = tokens };

    var ast = std.ArrayList(ASTNode).init(allocator);
    defer ast.deinit();

    while (true) {
        const node = parser.parse_statement() catch |err| {
            std.log.err("Error: {}", .{err});
            break;
        };
        if (node == .eof) break;
        ast.append(node) catch unreachable;
    }

    return ast.toOwnedSlice() catch unreachable;
}

// const expectEqualDeep = std.testing.expectEqualDeep;
// test "Parse into AST" {
//     const input = [_]Token{
//         .let, .{ .ident = "five" }, .assign, .{ .int = "5" }, .newline,
//         .let, .{ .ident = "neg_ten" }, .assign, .{ .int = "-10" }, .newline,
//         .let, .mut, .{ .ident = "pi" }, .assign, .{ .flt = "3.14" }, .newline,
//         .let, .mut, .{ .ident = "neg_e" }, .assign, .{ .flt = "-2.72" }, .newline,
//         .let, .{ .ident = "hello" }, .assign, .{ .str = "world" }, .newline,
//
//         .eof,
//     };
//
//     const expected = [_]ASTNode {
//         ASTNode {
//             .definition = .{
//                 .variable = .{
//                     .identifier = "five",
//                     .mutable = false,
//                     .expression = .{ .literal = .{ .int = 5 } }
//                 }
//             }
//         },
//         ASTNode {
//             .definition = .{
//                 .variable = .{
//                     .identifier = "neg_ten",
//                     .mutable = false,
//                     .expression = .{ .literal = .{ .int = -10 } }
//                 }
//             }
//         },
//         ASTNode {
//             .definition = .{
//                 .variable = .{
//                     .identifier = "pi",
//                     .mutable = true,
//                     .expression = .{ .literal = .{ .flt = "3.14" } }
//                 }
//             }
//         },
//         ASTNode {
//             .definition = .{
//                 .variable = .{
//                     .identifier = "neg_e",
//                     .mutable = true,
//                     .expression = .{ .literal = .{ .flt = "-2.72" } }
//                 }
//             }
//         },
//         ASTNode {
//             .definition = .{
//                 .variable = .{
//                     .identifier = "hello",
//                     .mutable = false,
//                     .expression = .{ .literal = .{ .str = "world" } }
//                 }
//             }
//         },
//     };
//
//     const ast = parse_tokens(std.testing.allocator, &input);
//
//     for (ast, 0..) |node, i| try expectEqualDeep(node, expected[i]);
//
//     std.testing.allocator.free(ast);
// }

// test "single binary operator" {
//     const input = [_]Token{
//         .let,
//         .{ .ident = "five" },
//         .assign,
//         .{ .int = "5" },
//         .{ .binary_operator = .add },
//         .{ .int = "7" },
//         .newline, 
//
//         .eof,
//     };
//     const expected = [_]ASTNode {
//         ASTNode {
//             .definition {
//                 .variable {
//                     .identifier = "five",
//                     .mutable = false,
//                     .expression = Expression {
//                         .binary = .{
//                             .lhs = Expression {
//                                 .literal = .{
//                                     .int = 5
//                                 }
//                             },
//                             .rhs = Expression {
//                                 .literal = .{
//                                     .int = 7
//                                 }
//                             },
//                             .operator = .{ .binary_operator = .add },
//                         }
//                     }
//                 }
//             }
//         }
//     };
//
//     const ast = parse_tokens(std.testing.allocator, input);
//     defer std.testing.allocator.free(ast);
//
//     for (ast, 0..) |node, i| try expectEqualDeep(node, expected[i]);
// }

// test "multiple binary operator same precendence" {
//     const input = [_]Token{
//         .let,
//         .{ .ident = "five" },
//         .assign,
//         .{ .int = "5" },
//         .{ .binary_operator = .add },
//         .{ .int = "7" },
//         .{ .binary_operator = .add },
//         .{ .int = "3" },
//         .{ .binary_operator = .add },
//         .{ .int = "9" },
//         .{ .binary_operator = .add },
//         .{ .int = "1" },
//         .newline, 
//
//         .eof,
//     };
//     const expected = [_]ASTNode {
//         ASTNode {
//             .definition {
//                 .variable {
//                     .identifier = "five",
//                     .mutable = false,
//                     .expression = .{
//                         .binary = .{
//                             .lhs = .{
//                                 .binary = .{
//                                     .lhs = .{
//                                         .binary = .{
//                                             .lhs = .{
//                                                 .binary = .{
//                                                     .lhs = .{ .literal = .{ .int = 5 } },
//                                                     .rhs = .{ .literal = .{ .int = 7 } },
//                                                     .operator = .{ .binary_operator = .add },
//                                                 }
//                                             },
//                                             .rhs = .{ .literal = .{ .int = 3 } },
//                                             .operator = .{ .binary_operator = .add },
//                                         }
//                                     },
//                                     .rhs = .{ .literal = .{ .int = 9 } },
//                                     .operator = .{ .binary_operator = .add },
//                                 }
//                             },
//                             .rhs = .{ .literal = .{ .int = 1 } },
//                             .operator = .{ .binary_operator = .add },
//                         }
//                     }
//                 }
//             }
//         }
//     };
//
//     const ast = parse_tokens(std.testing.allocator, input);
//     defer std.testing.allocator.free(ast);
//
//     for (ast, 0..) |node, i| try expectEqualDeep(node, expected[i]);
// }

// test "multiple binary operator different precendence" {
//     // let five = 5 + 7 * 7 - 3 / 9 + 1
//     //            5 + (7 * 7) - (3 / 9) + 1
//     //            ((5 + (7 * 7)) - (3 / 9)) + 1
//     //
//     // tree rep:
//     //
//     //            +
//     //           / \
//     //          -   1
//     //         / \
//     //        /   \
//     //       +      %
//     //      / \    / \
//     //     5   *  3   9
//     //        / \
//     //       7   7
//     const input = [_]Token{
//         .let,
//         .{ .ident = "five" },
//         .assign,
//         .{ .int = "5" },
//         .{ .binary_operator = .add },
//         .{ .int = "7" },
//         .{ .binary_operator = .multiply },
//         .{ .int = "7" },
//         .{ .binary_operator = .subtract },
//         .{ .int = "3" },
//         .{ .binary_operator = .divide },
//         .{ .int = "9" },
//         .{ .binary_operator = .add },
//         .{ .int = "1" },
//         .newline, 
//
//         .eof,
//     };
//     const expected = [_]ASTNode {
//         ASTNode{
//             .definition {
//                 .variable {
//                     .identifier = "five",
//                     .mutable = false,
//                     .expression = .{
//                         .binary = .{
//                             .lhs = .{
//                                 .binary = .{
//                                     .lhs = .{
//                                         .binary = .{
//                                             .lhs = .{ .literal = .{ .int = "5" } },
//                                             .operator = .{ .add },
//                                             .rhs = .{
//                                                 .binary = .{
//                                                     .lhs = .{ .literal = .{ .int = "7" } },
//                                                     .operator = .{ .multiply },
//                                                     .rhs = .{ .literal = .{ .int = "7" } },
//                                                 }
//                                         },
//                                         }
//                                     },
//                                     .operator = .{ .subtract },
//                                     .rhs = .{
//                                         .binary = .{
//                                             .lhs = .{ .literal = .{ .int = "3" } },
//                                             .operator = .{ .divide },
//                                             .rhs = .{ .literal = .{ .int = "9" } }
//                                         }
//                                 },
//                                 }
//                             },
//                             .operator = .{ .add },
//                             .rhs = .{ .literal = .{ .int = "1" }},
//                         }
//                     }
//                 }
//             }
//         }
//     };
//
//     const ast = parse_tokens(std.testing.allocator, input);
//     defer std.testing.allocator.free(ast);
//
//     for (ast, 0..) |node, i| try expectEqualDeep(node, expected[i]);
// }
//
