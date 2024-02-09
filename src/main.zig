const std = @import("std");
const lexer = @import("lexer.zig");

pub fn main() !void {
    var args = std.process.args();
    const path: []const u8 = args[1];

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const contents = try std.fs.cwd().readFileAlloc(gpa.allocator(), path, std.math.maxInt(usize));

    var lex = lexer.Lexer.init(contents);
    _ = lex;
}
