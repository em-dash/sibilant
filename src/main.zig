const Result = enum(u8) {
    ok = 0,
    eval_error = 10,

    fn int(self: Result) u8 {
        return @intFromEnum(self);
    }
};

pub fn main() !u8 {
    const stdout = std.io.getStdOut().writer();
    // const stderr = std.io.getStdErr().writer();
    const stdin = std.io.getStdIn().reader();

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var input = std.ArrayList(u8).init(allocator);
    defer input.deinit();
    try stdin.readAllArrayList(&input, std.math.maxInt(u32));

    const tokens = try tokenization.tokenize(allocator, input.items);
    defer allocator.free(tokens);
    var tree = try Tree.parse(allocator, input.items, tokens);
    defer tree.deinit();

    // tree.eval(allocator) catch |e| switch (e) {
    //     inline error.DivideByZero,
    //     error.NotImplemented,
    //     error.TypeError,
    //     error.IncorrectArgumentCount,
    //     error.VariableNotBound,
    //     => |ev| {
    //         try stderr.print("error: {s}\n", .{@errorName(ev)});
    //         return Result.eval_error.int();
    //     },
    //     else => |other| return other,
    // };
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
