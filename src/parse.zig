const std = @import("std");

const ast_mod = @import("ast.zig");
const ASTNode = ast_mod.ASTNode;
const Expression = ast_mod.Expression;
const Literal = ast_mod.Literal;

const Token = @import("lexer.zig").Token;

const ParseError = error{
    IllegalStatement,
    IllegalLetIdent,
    IllegalLetAssign,
    IllegalExpression,
    IllegalLiteral,
};

const Parser = struct {
    position: usize = 0,
    input: []const Token,

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
                const lit = try self.parse_literal();
                return ASTNode{ .expression = .{ .literal = lit } };
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
                    .expression = try self.parse_expression(),
                } } };
            },
            else => ParseError.IllegalLetAssign,
        };
    }

    fn parse_expression(self: *Self) !Expression {
        return switch (self.read_token()) {
            .int, .flt, .str, .boolean => Expression{ .literal = try self.parse_literal() },
            else => ParseError.IllegalExpression,
        };
    }

    fn parse_literal(self: *Self) !Literal {
        defer _ = self.next_token();
        return switch (self.read_token()) {
            .int => |val| Literal{ .int = try std.fmt.parseInt(isize, val, 10) },
            .flt => |val| {
                // see if it does parse into a float and store as str
                // this stops a three digit literal from exploding into
                // a string of digits at codegen
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

    while (true) main_loop: {
        const node = parser.parse_statement() catch |err| {
            std.log.err("Error: {}", .{err});
            break :main_loop;
        };
        if (node == .eof) break;
        ast.append(node) catch unreachable;
    }

    return ast.toOwnedSlice() catch unreachable;
}

const expectEqualDeep = std.testing.expectEqualDeep;
test "Parse into AST" {
    const input = [_]Token{
        .let,
        .{ .ident = "five" },
        .assign,
        .{ .int = "5" },
        .newline,
        .let,
        .{ .ident = "neg_ten" },
        .assign,
        .{ .int = "-10" },
        .newline,
        .let,
        .mut,
        .{ .ident = "pi" },
        .assign,
        .{ .flt = "3.14" },
        .newline,
        .let,
        .mut,
        .{ .ident = "neg_e" },
        .assign,
        .{ .flt = "-2.72" },
        .newline,
        .let,
        .{ .ident = "hello" },
        .assign,
        .{ .str = "world" },

        .eof,
    };

    const expected = [_]ASTNode{
        ASTNode{ .definition = .{ .variable = .{ .identifier = "five", .mutable = false, .expression = .{ .literal = .{ .int = 5 } } } } },
        ASTNode{ .definition = .{ .variable = .{ .identifier = "neg_ten", .mutable = false, .expression = .{ .literal = .{ .int = -10 } } } } },
        ASTNode{ .definition = .{ .variable = .{ .identifier = "pi", .mutable = true, .expression = .{ .literal = .{ .flt = "3.14" } } } } },
        ASTNode{ .definition = .{ .variable = .{ .identifier = "neg_e", .mutable = true, .expression = .{ .literal = .{ .flt = "-2.72" } } } } },
        ASTNode{ .definition = .{ .variable = .{ .identifier = "hello", .mutable = false, .expression = .{ .literal = .{ .str = "world" } } } } },
    };

    const ast = parse_tokens(std.testing.allocator, &input);

    for (ast, 0..) |node, i| {
        try expectEqualDeep(node, expected[i]);
    }

    std.testing.allocator.free(ast);
}
