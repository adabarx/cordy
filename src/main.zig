const std = @import("std");
const fs = std.fs;

const lexer = @import("lexer.zig");
const parser = @import("parse.zig");
const codegen = @import("zig_codegen.zig");

pub fn main() !void {
    // grab the path from the cli args
    var args = std.process.args();
    const path: []const u8 = args[1];

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer gpa.deinit();

    // read the input file
    const input = try fs.cwd().readFileAlloc(allocator, path, std.math.maxInt(usize));
    defer allocator.free(input);

    // parse text into tokens
    var lex = lexer.Lexer.init(input);
    const tokens = lexer.tokenize(allocator, lex);
    defer allocator.free(tokens);

    // parse tokens into abstract syntax tree
    const ast = parser.parse_tokens(allocator, tokens);
    defer allocator.free(ast);

    // NOTE: this is where we will scan the AST for errors

    // generate output code from AST
    const output = codegen.generate(allocator, ast);
    defer allocator.free(output);

    // write to output file
    const output_file = try fs.cwd().createFile(
        "output.zig",
        .{ .read = true },
    );
    defer output_file.close();

    _ = try output_file.writeAll(output);
}
