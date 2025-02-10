pub fn main() !u8 {
    const stdout = std.io.getStdOut().writer();

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    // const source = "(bork (1.5 2.5 3) (a b c d e) (lmao (lol (mdr))))";
    const source = "(add 1 2 -5)";
    // const source = "(add 5 (add 3 1))";
    const tokens = try tokenization.tokenize(allocator, source);
    defer allocator.free(tokens);
    var tree = try Tree.parse(allocator, source, tokens);
    defer tree.deinit();

    try stdout.print("input: {}\n", .{tree});
    try eval.eval(allocator, &tree);
    try stdout.print("evaluation: {any}\n", .{tree});

    return 0;
}

const std = @import("std");

const tokenization = @import("tokenization.zig");
const Tree = @import("Tree.zig");
const eval = @import("eval.zig");

test {
    // std.testing.refAllDeclsRecursive(@This());
    _ = @import("tokenization.zig");
    _ = @import("Tree.zig");
    _ = @import("eval.zig");
}
