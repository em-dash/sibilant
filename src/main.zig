pub fn main() !u8 {
    return 0;
}

const std = @import("std");

const tokenization = @import("tokenization.zig");
const Tree = @import("Tree.zig");
const eval = @import("eval.zig");

test {
    _ = std.testing.refAllDecls(tokenization);
    _ = std.testing.refAllDecls(Tree);
    _ = std.testing.refAllDecls(eval);
}
