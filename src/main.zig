const std = @import("std");
const fs = std.fs;

const lexer = @import("lexer.zig");
const parser = @import("parse.zig");
const codegen = @import("zig_codegen.zig");

pub fn main() !void {
    // grab the path from the cli args
    var arg = std.os.argv[1];
    const path: []const u8 = std.mem.sliceTo(arg, 0);

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    // defer _ = gpa.deinit();
    defer _ = arena.deinit();

    // read the input file
    const input = try fs.cwd().readFileAlloc(allocator, path, std.math.maxInt(usize));
    defer allocator.free(input);

    // parse text into tokens
    const tokens = try lexer.tokenize(allocator, input);
    defer allocator.free(tokens);

    // parse tokens into abstract syntax tree
    const ast = parser.parse_tokens(allocator, tokens);
    defer allocator.free(ast);

    // NOTE: this is where we will scan the AST for errors

    // generate output code from AST
    const output = try codegen.generate(allocator, ast);
    defer allocator.free(output);

    // write to output file
    const output_file = try fs.cwd().createFile(
        "output.zig",
        .{ .read = true },
    );
    defer output_file.close();

    _ = try output_file.writeAll(output);
}

test "Main" {
    std.testing.refAllDecls(@This());
}
