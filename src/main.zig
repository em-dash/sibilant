pub fn main() !u8 {
    const stdout = std.io.getStdOut().writer();

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    // const source = "(bork (1.5 2.5 3) (a b c d e) (lmao (lol (mdr))))";
    // const source = "(/ (+ 1 (- 6 3)) 2 (* 3 6))";
    // const source = "(quote (add 5 (multiply 3 8)))";
    // const source = "(quote (* (+ (*) (*)) (+ (*) (*) (*))))";
    // const source = "(quote (a b c d e f))";
    const source = "((lambda (a) (multiply a a)) 5)" ++ "((lambda (a b c) (add (multiply a b) c)) 4 5 3)";
    const tokens = try tokenization.tokenize(allocator, source);
    defer allocator.free(tokens);
    var tree = try Tree.parse(allocator, source, tokens);
    defer tree.deinit();

    try stdout.print("input: {}\n", .{tree});
    try tree.eval(allocator);
    try stdout.print("evaluation: {any}\n", .{tree});

    return 0;
}

const std = @import("std");

const tokenization = @import("tokenization.zig");
const Tree = @import("Tree.zig");

test {
    // std.testing.refAllDeclsRecursive(@This());
    _ = @import("tokenization.zig");
    _ = @import("Tree.zig");
}
