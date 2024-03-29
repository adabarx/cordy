const std = @import("std");

const data_structs = @import("data_structs.zig");
const Token = data_structs.Token;
const BinaryOperator = data_structs.BinaryOperator;

fn isLetter(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

fn isNumber(ch: u8) bool {
    return std.ascii.isDigit(ch);
}

pub const Lexer = struct {
    const Self = @This();

    read_position: usize = 0,
    position: usize = 0,
    ch: u8 = 0,
    input: []const u8,

    pub fn init(input: []const u8) Self {
        var lexer = Self{ .input = input };
        lexer.read_char();
        return lexer;
    }

    pub fn has_tokens(self: *Self) bool {
        return self.ch != 0;
    }

    pub fn next_token(self: *Self) Token {
        self.skip_whitespace();
        const token: Token = switch (self.ch) {
            '\n' => .newline,
            ':' => .colon,
            '=' => .{ .binary_operator = .assign },
            '"' => .{ .str = self.read_str() },
            '!' => .{ .unary_prefix_operator = .not },
            '-' => 
                if (isNumber(self.peek_char())) {
                    return self.read_number();
                }
                else .{ .binary_operator = .subtract },
            '+' =>
                if (self.peek_char() == '=') eq: {
                    self.read_char();
                    break :eq .{ .binary_operator = .add_assign };
                } else op: {
                    break :op .{ .binary_operator = .add };
                },
            '*' => .{ .binary_operator = .multiply },
            '/' => .{ .binary_operator = .divide },
            '0'...'9' => return self.read_number(),
            'a'...'z', 'A'...'Z', '_' => {
                const ident = self.read_identifier();
                if (Token.keyword(ident)) |token| {
                    return token;
                }
                return .{ .ident = ident };
            },
            0 => .eof,
            else => .illegal,
        };

        self.read_char();
        return token;
    }

    fn peek_char(self: *Self) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_position];
        }
    }

    fn read_char(self: *Self) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(self: *Self) []const u8 {
        const position = self.position;
        while (isLetter(self.ch)) {
            self.read_char();
        }
        return self.input[position..self.position];
    }

    fn read_number(self: *Self) Token {
        const position = self.position;
        var decimalPoint = false;

        if (self.ch == '-') self.read_char();

        while (isNumber(self.ch) or (self.ch == '.' and !decimalPoint)) {
            if (self.ch == '.') decimalPoint = true;

            self.read_char();
        }

        if (decimalPoint) {
            return .{ .flt = self.input[position..self.position] };
        } else {
            return .{ .int = self.input[position..self.position] };
        }
    }

    fn read_str(self: *Self) []const u8 {
        self.read_char(); // get past first quote
        const position = self.position;
        while (true) {
            self.read_char();
            if (self.ch == '"') break;
        }
        const endPosition = self.position;
        self.read_char(); // get past second quote
        return self.input[position..endPosition];
    }

    fn skip_whitespace(self: *Self) void {
        while (self.ch == ' ' or self.ch == '\t') self.read_char();
    }
};

pub fn tokenize(allocator: std.mem.Allocator, input: []const u8) ![]const Token {
    var lex = Lexer.init(input);
    var rv = std.ArrayList(Token).init(allocator);
    defer rv.deinit();

    var curr_token = lex.next_token();
    while (true) {
        var next_token = lex.next_token();

        if (curr_token == .newline and next_token == .newline) {
            curr_token = next_token;
            next_token = lex.next_token();
        }
        if (curr_token == .newline and next_token == .binary_operator) {
            curr_token = next_token;
            next_token = lex.next_token();
        }
        if (curr_token == .binary_operator and next_token == .newline) {
            next_token = lex.next_token();
        }
        // if (curr_token == .{ .unary_prefix_operator = .not }) {
        //     if (next_token.get_binary_operator()) |op| {
        //         switch (op) {
        //             .and_tok => {
        //                 curr_token = .{ .binary_operator = .not_and };
        //                 next_token = lex.next_token();
        //             },
        //             .or_tok => {
        //                 curr_token = .{ .binary_operator = .not_or };
        //                 next_token = lex.next_token();
        //             },
        //             .xor => {
        //                 curr_token = .{ .binary_operator = .not_xor };
        //                 next_token = lex.next_token();
        //             },
        //         }
        //     }
        // }

        try rv.append(curr_token);
        if (curr_token == .eof) break;
        curr_token = next_token;
    }
    return rv.toOwnedSlice();
}

const expectEqualDeep = std.testing.expectEqualDeep;
test "Lexer" {
    const input =
        \\let five = 5
        \\let neg_ten = -10
        \\let mut pi = 3.14
        \\let mut neg_e = -2.72
        \\let hello = "world"
    ;

    var lex = Lexer.init(input);

    const expected = [_]Token{
        .let,
        .{ .ident = "five" },
        .assign,
        .{ .int = "5" },
        .newline,
        .let,
        .{ .ident = "neg_ten" },
        .assign,
        .{ .unary_prefix_operator = .negative },
        .{ .int = "10" },
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
        .{ .unary_prefix_operator = .negative },
        .{ .flt = "2.72" },
        .newline,
        .let,
        .{ .ident = "hello" },
        .assign,
        .{ .str = "world" },

        .eof,
    };

    for (expected) |token| {
        const tok = lex.next_token();
        try expectEqualDeep(token, tok);
    }
}

test "tokenize function" {
    const input =
        \\let five = 5
        \\let neg_ten = -10
        \\let mut pi = 3.14
        \\let mut neg_e = -2.72
        \\let hello = "world"
    ;

    const expected = [_]Token{
        .let,
        .{ .ident = "five" },
        .assign,
        .{ .int = "5" },
        .newline,
        .let,
        .{ .ident = "neg_ten" },
        .assign,
        .{ .unary_prefix_operator = .negative },
        .{ .int = "10" },
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
        .{ .unary_prefix_operator = .negative },
        .{ .flt = "2.72" },
        .newline,
        .let,
        .{ .ident = "hello" },
        .assign,
        .{ .str = "world" },

        .eof,
    };

    const output: []const Token = try tokenize(std.testing.allocator, input);
    defer std.testing.allocator.free(output);

    try expectEqualDeep(@as([]const Token, &expected), output);
}

test "remove excessive newlines" {
    const input =
        \\let five = 5
        \\
        \\
        \\
        \\
        \\
        \\
        \\
        \\
        \\
        \\
        \\
        \\
        \\
        \\let neg_ten = -10
        \\let mut pi = 3.14
        \\let mut neg_e = -2.72
        \\let hello = "world"
    ;

    const expected = [_]Token{
        .let,
        .{ .ident = "five" },
        .assign,
        .{ .int = "5" },
        .newline,
        .let,
        .{ .ident = "neg_ten" },
        .assign,
        .{ .unary_prefix_operator = .negative },
        .{ .int = "10" },
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
        .{ .unary_prefix_operator = .negative },
        .{ .flt = "2.72" },
        .newline,
        .let,
        .{ .ident = "hello" },
        .assign,
        .{ .str = "world" },

        .eof,
    };

    const output: []const Token = try tokenize(std.testing.allocator, input);
    defer std.testing.allocator.free(output);

    try expectEqualDeep(@as([]const Token, &expected), output);
}

test "remove newlines between binary operators" {
    const input =
        \\let five = 5
        \\    + 2
        \\    - 4
        \\let six = 5 +
        \\    2 -
        \\
        \\
        \\
        \\    4
        \\
    ;

    const expected = [_]Token{
        .let,
        .{ .ident = "five" },
        .assign,
        .{ .int = "5" },
        .{ .binary_operator = .add },
        .{ .int = "2" },
        .{ .binary_operator = .subtract },
        .{ .int = "4" },
        .newline,
        .let,
        .{ .ident = "six" },
        .assign,
        .{ .int = "5" },
        .{ .binary_operator = .add },
        .{ .int = "2" },
        .{ .binary_operator = .subtract },
        .{ .int = "4" },
        .newline,

        .eof,
    };

    const output: []const Token = try tokenize(std.testing.allocator, input);
    defer std.testing.allocator.free(output);

    try expectEqualDeep(@as([]const Token, &expected), output);
}
