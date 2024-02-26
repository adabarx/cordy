const std = @import("std");

const data_structs = @import("data_structs.zig");
const ASTNode = data_structs.ASTNode;
const Expression = data_structs.Expression;
const Literal = data_structs.Literal;
const Token = data_structs.Token;
const BinaryOperator = data_structs.BinaryOperator;

const Parser = struct {
    position: usize = 0,
    input: []const Token,

    const ParseError = error{
        IllegalStatement,
        IllegalLetIdent,
        IllegalLetAssign,
        IllegalExpression,
        IllegalLiteral,

        CantParseFloat,
        CantParseInt,
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

    pub fn parse_statement(self: *Self) ParseError!ASTNode {
        std.debug.print("\nstatement\n", .{});
        while (self.read_token() == .newline) _ = self.next_token();

        return switch (self.read_token()) {
            .let => self.parse_let_statement(),
            .int, .flt, .str, .boolean => ASTNode{ .expression = (try self.parse_expression(0)).* },
            .eof => ASTNode.eof,
            else => ParseError.IllegalStatement,
        };
    }

    fn parse_let_statement(self: *Self) ParseError!ASTNode {
        std.debug.print("let\n", .{});
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
                return .{ .definition = .{ .variable = .{
                    .identifier = ident,
                    .mutable = mutt,
                    .expression = (try self.parse_expression(0)).*,
                } } };
            },
            else => ParseError.IllegalLetAssign,
        };
    }

    fn parse_leaf(self: *Self) ParseError!Expression {
        std.debug.print("leaf\n", .{});
        return switch (self.read_token()) {
            .int, .flt, .str, .boolean => {
                return Expression{ .literal = try self.parse_literal() };
            },
            .ident => |id| Expression{ .identifier = id },
            else => ParseError.IllegalExpression,
        };
    }

    fn parse_expression(self: *Self, precedence: u8) ParseError!*const Expression {
        std.debug.print("expression\n", .{});
        var called_prec = precedence;
        var left = try self.parse_leaf();
        left.prittyprint(0);
        // check for binary op
        while (self.next_token().get_binary_operator()) |curr_op| {
            std.debug.print("binary\n", .{});
            _ = self.next_token();
            if (curr_op.precedence() <= called_prec) {
                // if precedence is equal or decreases: left to right and loop
                std.debug.print("{} <= {}\n", .{curr_op.precedence(), called_prec});
                const leaf = left;
                left = .{
                    .binary = .{
                        .lhs = &leaf,
                        .operator = curr_op,
                        .rhs = &(try self.parse_leaf()),
                    }
                };
                leaf.prittyprint(0);
                called_prec = curr_op.precedence();
            } else {
                // else precedence increases: right to left and recurse
                std.debug.print("{} > {}\n", .{curr_op.precedence(), called_prec});
                const leaf = left;
                left = .{
                    .binary = .{
                        .lhs = &leaf,
                        .operator = curr_op,
                        .rhs = try self.parse_expression(curr_op.precedence()),
                    }
                };
                leaf.prittyprint(0);
            }
        }
        left.prittyprint(0);
        return &left;
    }

    fn parse_literal(self: *Self) ParseError!Literal {
        std.debug.print("literal\n", .{});
        return switch (self.read_token()) {
            .int => |val| {
                std.debug.print("int\n", .{});
                const int = std.fmt.parseInt(isize, val, 10) catch return ParseError.CantParseInt;
                return Literal{ .int = int };
            },
            .flt => |val| {
                std.debug.print("flt\n", .{});
                // see if it parses into a float and store as str
                // this stops a three digit literal from exploding
                // into a long string of digits at codegen
                _ = std.fmt.parseFloat(f32, val) catch return ParseError.CantParseFloat;
                return Literal{ .flt = val };
            },
            .str => |val| {
                std.debug.print("str\n", .{});
                return Literal{ .str = val };
            },
            .boolean => |val| {
                std.debug.print("bool\n", .{});
                return Literal{ .boolean = val };
            },
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
        node.prittyprint();
        ast.append(node) catch unreachable;
    }

    return ast.toOwnedSlice() catch unreachable;
}

const expectEqualDeep = std.testing.expectEqualDeep;
test "Parse into AST" {
    const input = [_]Token{
        .let, .{ .ident = "five" }, .assign, .{ .int = "5" }, .newline,
        .let, .{ .ident = "neg_ten" }, .assign, .{ .int = "-10" }, .newline,
        .let, .mut, .{ .ident = "pi" }, .assign, .{ .flt = "3.14" }, .newline,
        .let, .mut, .{ .ident = "neg_e" }, .assign, .{ .flt = "-2.72" }, .newline,
        .let, .{ .ident = "hello" }, .assign, .{ .str = "world" }, .newline,

        .eof,
    };

    const expected = [_]ASTNode {
        ASTNode {
            .definition = .{
                .variable = .{
                    .identifier = "five",
                    .mutable = false,
                    .expression = .{ .literal = .{ .int = 5 } }
                }
            }
        },
        ASTNode {
            .definition = .{
                .variable = .{
                    .identifier = "neg_ten",
                    .mutable = false,
                    .expression = .{ .literal = .{ .int = -10 } }
                }
            }
        },
        ASTNode {
            .definition = .{
                .variable = .{
                    .identifier = "pi",
                    .mutable = true,
                    .expression = .{ .literal = .{ .flt = "3.14" } }
                }
            }
        },
        ASTNode {
            .definition = .{
                .variable = .{
                    .identifier = "neg_e",
                    .mutable = true,
                    .expression = .{ .literal = .{ .flt = "-2.72" } }
                }
            }
        },
        ASTNode {
            .definition = .{
                .variable = .{
                    .identifier = "hello",
                    .mutable = false,
                    .expression = .{ .literal = .{ .str = "world" } }
                }
            }
        },
    };

    const ast = parse_tokens(std.testing.allocator, &input);
    defer std.testing.allocator.free(ast);

    // for (ast) |node| node.prittyprint();
    for (ast, 0..) |node, i| try node.assert_eq(&expected[i]);
}

test "single binary operator" {
    const input = [_]Token{
        .let,
        .{ .ident = "five" },
        .assign,
        .{ .int = "5" },
        .{ .binary_operator = .add },
        .{ .int = "7" },
        .newline, 

        .eof,
    };

    const bin_expr: Expression = .{
        .binary = .{
            .lhs = &.{ .literal = .{ .int = 5 } },
            .operator = .add,
            .rhs = &.{ .literal = .{ .int = 7 } },
        }
    };

    const expected: ASTNode = .{
        .definition = .{
            .variable = .{
                .identifier = "five",
                .mutable = false,
                .expression = bin_expr,
            }
        }
    };

    const ast = parse_tokens(std.testing.allocator, &input);
    defer std.testing.allocator.free(ast);

    ast[0].prittyprint();
    try expected.assert_eq(&ast[0]);
}

test "multiple binary operator same precendence" {
    const input = [_]Token{
        .let,
        .{ .ident = "five" },
        .assign,
        .{ .int = "5" },
        .{ .binary_operator = .add },
        .{ .int = "7" },
        .{ .binary_operator = .add },
        .{ .int = "3" },
        .{ .binary_operator = .add },
        .{ .int = "9" },
        .{ .binary_operator = .add },
        .{ .int = "1" },
        .newline, 

        .eof,
    };

    const first: Expression = .{
        .binary = .{
            .lhs = &.{ .literal = .{ .int = 5 } },
            .rhs = &.{ .literal = .{ .int = 3 } },
            .operator = .add 
        }
    };
    const second: Expression = .{
        .binary = .{
            .lhs = &first,
            .rhs = &.{ .literal = .{ .int = 3 } },
            .operator = .add 
        }
    };
    const third: Expression = .{
        .binary = .{
            .lhs = &second,
            .rhs = &.{ .literal = .{ .int = 9 } },
            .operator = .add 
        }
    };
    const forth: Expression = .{
        .binary = .{
            .lhs = &third,
            .rhs = &.{ .literal = .{ .int = 1 } },
            .operator = .add 
        }
    };

    const expected: ASTNode = .{
        .definition = .{
            .variable = .{
                .identifier = "five",
                .mutable = false,
                .expression = forth
            }
        }
    };

    const ast = parse_tokens(std.testing.allocator, &input);
    defer std.testing.allocator.free(ast);

    ast[0].prittyprint();
    try expected.assert_eq(&ast[0]);
}

test "multiple binary operator different precendence" {
    // let five = 5 + 7 * 6 - 3 / 9 + 1
    //            5 + (7 * 6) - (3 / 9) + 1
    //            ((5 + (7 * 6)) - (3 / 9)) + 1
    //
    // tree rep:
    //
    //            +
    //           / \
    //          -   1
    //         / \
    //        /   \
    //       +      %
    //      / \    / \
    //     5   *  3   9
    //        / \
    //       7   6
    const input = [_]Token{
        .let,
        .{ .ident = "five" },
        .assign,
        .{ .int = "5" },
        .{ .binary_operator = .add },
        .{ .int = "7" },
        .{ .binary_operator = .multiply },
        .{ .int = "7" },
        .{ .binary_operator = .subtract },
        .{ .int = "3" },
        .{ .binary_operator = .divide },
        .{ .int = "9" },
        .{ .binary_operator = .add },
        .{ .int = "1" },
        .newline, 

        .eof,
    };

    const first: Expression = .{
        .binary = .{
            .lhs = &.{ .literal = .{ .int = 7 } },
            .rhs = &.{ .literal = .{ .int = 7 } },
            .operator = .multiply 
        }
    };
    const second: Expression = .{
        .binary = .{
            .lhs = &.{ .literal = .{ .int = 5 } },
            .rhs = &first,
            .operator = .add
        }
    };
    const third: Expression = .{
        .binary = .{
            .lhs = &.{ .literal = .{ .int = 3 } },
            .rhs = &.{ .literal = .{ .int = 9 } },
            .operator = .divide,
        }
    };
    const forth: Expression = .{
        .binary = .{
            .lhs = &second,
            .rhs = &third,
            .operator = .subtract 
        }
    };
    const fifth: Expression = .{
        .binary = .{
            .lhs = &forth,
            .rhs = &.{ .literal = .{ .int = 1 } },
            .operator = .add 
        }
    };

    const expected: ASTNode = .{
        .definition = .{
            .variable = .{
                .identifier = "five",
                .mutable = false,
                .expression = fifth
            }
        }
    };

    const ast = parse_tokens(std.testing.allocator, &input);
    defer std.testing.allocator.free(ast);

    ast[0].prittyprint();
    try expected.assert_eq(&ast[0]);
}


