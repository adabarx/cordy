const std = @import("std");

pub const Token = union(enum) {
    ident: []const u8,
    int: []const u8,
    flt: []const u8,
    str: []const u8,
    boolean: bool,

    let,
    mut,
    dash,
    assign,
    newline,
    cantTouchThis,
    eof,
    illegal,

    fn keyword(ident: []const u8) ?Token {
        const map = std.ComptimeStringMap(Token, .{
            .{ "let", .let },
            .{ "mut", .mut },
            .{ "ctt", .cantTouchThis },
            .{ "True", .{ .boolean = true } },
            .{ "False", .{ .boolean = false } },
        });
        return map.get(ident);
    }
};

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
            '=' => .assign,
            '"' => .{ .str = self.read_str() },
            '-' => {
                if (isNumber(self.peek_char())) {
                    return self.read_number();
                } else {
                    return .dash;
                }
            },
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
        while (self.ch == ' ' or self.ch == '\t') {
            self.read_char();
        }
    }
};

// pub fn tokenize(allocator: std.mem.Allocator, lex: Lexer) []const Token {
//
// }

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

    for (expected) |token| {
        const tok = lex.next_token();

        try expectEqualDeep(token, tok);
    }
}
