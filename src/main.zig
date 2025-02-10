pub fn main() !u8 {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const source = "(add 123 123 123 123  234 3 4 234 12 3 123 123  234 235)";
    // const source = "(add 123 123)";
    const tokens = try tokenization.tokenize(allocator, source);
    defer allocator.free(tokens);
    var tree = try Tree.parse(allocator, source, tokens);
    defer tree.deinit();

    std.debug.print("{any}\n", .{tree.getNode(.root)});
    // std.debug.print("{any}\n", .{tree.nodes.items(.data).len});
    // try std.io.getStdOut().writer().print("{any}\n", .{tree});

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
