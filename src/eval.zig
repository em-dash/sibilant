const Builtin = enum {
    quote,
    add,
    subtract,
    lambda,
};

const builtin_map: std.StaticStringMap(Builtin) = .initComptime(.{
    .{ "quote", .quote },
    .{ "add", .add },
    .{ "subtract", .subtract },
    .{ "lambda", .lambda },
    .{ "λ", .lambda },
});

pub fn eval(allocator: std.mem.Allocator, tree: *Tree, index: Tree.NodeIndex) !void {
    // var temp_allocator = std.heap.stackFallback(64, allocator);

    switch (tree.getNode(index)) {
        .sexpr => |sexpr| {
            switch (tree.getNode(sexpr.value)) {
                .number => |_| return error.ThatTypeDoesntGoAtTheStartOfASexprBruh,
                .string => |_| return error.ThatTypeDoesntGoAtTheStartOfASexprBruh,
                .sexpr => |_| return error.NotImplemented,
                .identifier => |i| {
                    if (builtin_map.get(tree.getIdentifier(i))) |builtin| switch (builtin) {
                        .quote => return error.NotImplemented,
                        .add => {
                            var sum: f64 = 0;
                            var current = index;
                            while (true) {
                                current = sexpr.next;
                                if (current != .none) {
                                    try eval(allocator, tree, current);
                                    switch (tree.getNode(current)) {
                                        .identifier => |_| return error.NotImplemented,
                                        .sexpr => |_| return error.CantEvalThisThing,
                                        .string => |_| return error.TypeIsWrongHere,
                                        .number => |number| sum += number,
                                    }
                                } else break;
                            }
                        },
                        .subtract => return error.NotImplemented,
                        .lambda => return error.NotImplemented,
                    };
                },
            }
        },
        .number => return,
        .string => return,
        .identifier => {
            @panic("lol");
        },
    }
}

test eval {
    // {
    //     const source = "(add 69 420)";
    //     const tokens = try @import("tokenization.zig").tokenize(std.testing.allocator, source);
    //     defer std.testing.allocator.free(tokens);
    //     var tree = try Tree.parse(std.testing.allocator, source, tokens);
    //     defer tree.deinit();

    //     try eval(std.testing.allocator, &tree, .root);
    //     // try std.testing.expectEqual(tree.getNode(@enumFromInt(1)).number, @as(f64, 123.0));
    // }
}

const std = @import("std");

const Tree = @import("Tree.zig");
