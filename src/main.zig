pub fn main() !u8 {
    return 0;
}

const std = @import("std");

const tokens = @import("tokens.zig");
const tree = @import("tree.zig");

test {
    inline for (.{ "tokens", "tree" }) |namespace|
        std.testing.refAllDeclsRecursive(@field(@This(), namespace));
}
