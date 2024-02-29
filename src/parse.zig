const std = @import("std");
const Allocator = std.mem.Allocator;

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
        IllegalLetNotBinOp,
        IllegalLetNotAssign,
        IllegalExpression,
        IllegalLiteral,

        CantParseFloat,
        CantParseInt,

        OutOfMemory,
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

    pub fn parse_statement(self: *Self, allocator: Allocator) ParseError!ASTNode {
        while (self.read_token() == .newline) _ = self.next_token();

        return switch (self.read_token()) {
            .ident => |id| blk: {
                if (self.next_token() == .colon) {
                    break :blk self.parse_assign(allocator, id);
                }
            },
            .int, .flt, .str, .boolean => .{
                .alloc = allocator,
                .node = .{
                    .expression = try self.parse_expression(allocator)
                }
            },
            .eof => .{
                .alloc = allocator,
                .node = .eof,
            },
            else => ParseError.IllegalStatement,
        };
    }

    fn parse_assign(self: *Self, allocator: Allocator, ident: []const u8) ParseError!ASTNode {
        var mutt = false;
        if (self.next_token() == .mut) {
            mutt = true;
            _ = self.next_token();
        }

        const operator = self.read_token().get_binary_operator() orelse return ParseError.IllegalLetNotBinOp;
        if (operator != .assign) return ParseError.IllegalLetNotAssign;
        _ = self.next_token();
        return .{
            .alloc = allocator,
            .node = .{ .definition = .{
                .variable = .{
                    .identifier = ident,
                    .mutable = mutt,
                    .expression = try self.parse_expression(allocator),
                }
            } }
        };
    }

    fn parse_leaf(self: *Self, allocator: Allocator) ParseError!*Expression {
        var leaf = allocator.create(Expression)
            catch return ParseError.OutOfMemory;
        leaf.* = switch (self.read_token()) {
            .int, .flt, .str, .boolean => Expression{ .literal = try self.parse_literal() },
            .ident => |id| Expression{ .identifier = id },
            else => {
                std.debug.print("IllegalExpression: {}\n", .{self.read_token()});
                return ParseError.IllegalExpression;
            },
        };
        _ = self.next_token();
        return leaf;
    }

    fn parse_expression(self: *Self, allocator: Allocator) ParseError!*Expression {
        var leaf = try self.parse_leaf(allocator);
        // check for binary op
        while (self.read_token().get_binary_operator()) |curr_op| {
            _ = self.next_token();
            var left = allocator.create(Expression)
                catch return ParseError.OutOfMemory;
            left.* = leaf.*;

            var right = try self.parse_leaf(allocator);
            const next_op = self.read_token().get_binary_operator();

            if (next_op == null or next_op.?.precedence() <= curr_op.precedence()) {
                leaf.* = .{
                    .binary = .{
                        .lhs = left,
                        .operator = curr_op,
                        .rhs = right,
                    }
                };
            } else {
                _ = self.next_token(); // move off of BinaryOperator
                leaf.* = .{
                    .binary = .{
                        .lhs = left,
                        .operator = curr_op,
                        .rhs = try self.parse_increasing_prec(allocator, right, next_op.?),
                    }
                };
            }
        }
        return leaf;
    }

    fn parse_increasing_prec(
        self: *Self,
        allocator: Allocator,
        left: *Expression,
        op: BinaryOperator
    ) ParseError!*Expression {
        var rv = allocator.create(Expression) catch return ParseError.OutOfMemory;
        const right = try self.parse_leaf(allocator);
        const next_op = self.read_token().get_binary_operator();

        if (next_op == null or next_op.?.precedence() <= op.precedence()) {
            rv.* = .{
                .binary = .{
                    .lhs = left,
                    .operator = op,
                    .rhs = right,
                }
            };
        } else {
            _ = self.next_token(); // move off of BinaryOperator
            rv.* = .{
                .binary = .{
                    .lhs = left,
                    .operator = op,
                    .rhs = try self.parse_increasing_prec(allocator, right, next_op.?),
                }
            };
        }
        return rv;
    }

    fn parse_literal(self: *Self) ParseError!Literal {
        return switch (self.read_token()) {
            .int => |val| {
                const int = std.fmt.parseInt(isize, val, 10)
                    catch return ParseError.CantParseInt;
                return Literal{ .int = int };
            },
            .flt => |val| {
                // see if it parses into a float and store as str
                // this stops a three digit literal from exploding
                // into a long string of digits at codegen
                _ = std.fmt.parseFloat(f32, val)
                    catch return ParseError.CantParseFloat;
                return Literal{ .flt = val };
            },
            .str => |val| {
                return Literal{ .str = val };
            },
            .boolean => |val| {
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
        const astnode = parser.parse_statement(allocator) catch |err| {
            std.log.err("Error: {}", .{err});
            break;
        };
        if (astnode.node == .eof) break;
        ast.append(astnode) catch unreachable;
    }

    return ast.toOwnedSlice() catch unreachable;
}

const expectEqualDeep = std.testing.expectEqualDeep;
test "Parse into AST" {
    const input = [_]Token{
        .{ .ident = "five" }, .colon, .{ .binary_operator = .assign }, .{ .int = "5" }, .newline,
        .{ .ident = "neg_ten" }, .colon, .{ .binary_operator = .assign }, .{ .int = "-10" }, .newline,
        .{ .ident = "pi" }, .colon, .mut, .{ .binary_operator = .assign }, .{ .flt = "3.14" }, .newline,
        .{ .ident = "neg_e" }, .colon, .mut, .{ .binary_operator = .assign }, .{ .flt = "-2.72" }, .newline,
        .{ .ident = "hello" }, .colon, .{ .binary_operator = .assign }, .{ .str = "world" }, .newline,

        .eof,
    };

    const expected = [_]ASTNode {
        ASTNode {
            .alloc = std.testing.allocator,
            .node = .{
                .definition = .{
                    .variable = .{
                        .identifier = "five",
                        .mutable = false,
                        .expression = &.{ .literal = .{ .int = 5 } }
                    }
                }
            }
        },
        ASTNode {
            .alloc = std.testing.allocator,
            .node = .{
                .definition = .{
                    .variable = .{
                        .identifier = "neg_ten",
                        .mutable = false,
                        .expression = &.{ .literal = .{ .int = -10 } }
                    }
                }
            }
        },
        ASTNode {
            .alloc = std.testing.allocator,
            .node = .{
                .definition = .{
                    .variable = .{
                        .identifier = "pi",
                        .mutable = true,
                        .expression = &.{ .literal = .{ .flt = "3.14" } }
                    }
                }
            }
        },
        ASTNode {
            .alloc = std.testing.allocator,
            .node = .{
                .definition = .{
                    .variable = .{
                        .identifier = "neg_e",
                        .mutable = true,
                        .expression = &.{ .literal = .{ .flt = "-2.72" } }
                    }
                }
            }
        },
        ASTNode {
            .alloc = std.testing.allocator,
            .node = .{
                .definition = .{
                    .variable = .{
                        .identifier = "hello",
                        .mutable = false,
                        .expression = &.{ .literal = .{ .str = "world" } }
                    }
                }
            }
        },
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const ast = parse_tokens(arena.allocator(), &input);
    defer arena.deinit();
    // defer {
    //     for (ast) |astnode| astnode.deinit();
    //     std.testing.allocator.free(ast);
    // }

    for (ast, 0..) |astnode, i| try astnode.assert_eq(&expected[i]);
}

test "single binary operator" {
    const input = [_]Token{
        .{ .ident = "five" },
        .colon,
        .{ .binary_operator = .assign },
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
        .alloc = std.testing.allocator,
        .node = .{
            .definition = .{
                .variable = .{
                    .identifier = "five",
                    .mutable = false,
                    .expression = &bin_expr,
                }
            }
        }
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const ast = parse_tokens(arena.allocator(), &input);
    defer arena.deinit();
    // defer {
    //     for (ast) |astnode| astnode.deinit();
    //     std.testing.allocator.free(ast);
    // }

    try expected.assert_eq(&ast[0]);
}

test "multiple binary operator same precendence" {
    const input = [_]Token{
        .{ .ident = "five" },
        .colon,
        .{ .binary_operator = .assign },
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
            .rhs = &.{ .literal = .{ .int = 7 } },
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
        .alloc = std.testing.allocator,
        .node = .{
            .definition = .{
                .variable = .{
                    .identifier = "five",
                    .mutable = false,
                    .expression = &forth
                }
            }
        }
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const ast = parse_tokens(arena.allocator(), &input);
    defer arena.deinit();
    // defer {
    //     for (ast) |astnode| astnode.deinit();
    //     std.testing.allocator.free(ast);
    // }

    try expected.assert_eq(&ast[0]);
}

test "multiple binary operator two levels of precendence" {
    // let five = 5 + 7 * 7 - 3 / 9 + 1
    //            5 + (7 * 7) - (3 / 9) + 1
    //            ((5 + (7 * 7)) - (3 / 9)) + 1
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
    //       7   7
    const input = [_]Token{
        .{ .ident = "five" },
        .colon,
        .{ .binary_operator = .assign },
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
        .alloc = std.testing.allocator,
        .node = .{
            .definition = .{
                .variable = .{
                    .identifier = "five",
                    .mutable = false,
                    .expression = &fifth
                }
            }
        }
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const ast = parse_tokens(arena.allocator(), &input);
    defer arena.deinit();
    // defer {
    //     for (ast) |astnode| astnode.deinit();
    //     std.testing.allocator.free(ast);
    // }

    std.debug.print("\nExpected:", .{});
    expected.prittyprint();
    std.debug.print("Output:", .{});
    ast[0].prittyprint();
    try expected.assert_eq(&ast[0]);
}

