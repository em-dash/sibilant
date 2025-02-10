const builtin_map: std.StaticStringMap(Tree.Node) = .initComptime(.{
    .{ "add", .builtin_add },
    .{ "subtract", .builtin_subtract },
    .{ "multiply", .builtin_multiply },
    .{ "divide", .builtin_divide },
    .{ "quote", .builtin_quote },
    .{ "lambda", .builtin_lambda },
    .{ "λ", .builtin_lambda },
});

pub fn eval(allocator: std.mem.Allocator, tree: *Tree) !void {
    for (tree.roots.items) |root| try evalFromNode(allocator, tree, root);
}

fn evalFromNode(allocator: std.mem.Allocator, tree: *Tree, index: Tree.NodeIndex) !void {
    // var temp_allocator = std.heap.stackFallback(64, allocator);

    switch (tree.getNode(index)) {
        .sexpr_head => |sexpr_head| {
            {
                try evalFromNode(allocator, tree, sexpr_head.value);
                var current = sexpr_head.next;
                while (current != .none) {
                    const node = tree.getNode(current);
                    try evalFromNode(allocator, tree, node.sexpr_tail.value);
                    current = node.sexpr_tail.next;
                }
            }

            const new_head = tree.getNode(index).sexpr_head;
            switch (tree.getNode(new_head.value)) {
                .builtin_add => {
                    var current = new_head.next;
                    var sum: f64 = 0;
                    while (current != .none) {
                        const node = tree.getNode(current).sexpr_tail;
                        const summand = tree.getNode(node.value);
                        if (summand == .number)
                            sum += summand.number
                        else
                            return error.TypeError;
                        current = node.next;
                    }
                    tree.setNode(index, .{ .number = sum });
                },
                else => return error.NotImplemented,
            }
        },
        .sexpr_tail => unreachable,
        .number => {},
        .string => {},
        .identifier => |identifier| {
            // If this is a bulitin, set it.
            const string = tree.getIdentifierString(identifier);
            if (builtin_map.get(string)) |builtin|
                tree.setNode(index, builtin)
            else
                return error.NotImplemented;
        },
        // These only appear in a branch that has already been parsed.
        .builtin_add,
        .builtin_subtract,
        .builtin_divide,
        .builtin_multiply,
        .builtin_quote,
        .builtin_lambda,
        => unreachable,
    }
}

test eval {
    // {
    //     const source = "(add 69 420)";
    //     const tokens = try @import("tokenization.zig").tokenize(std.testing.allocator, source);
    //     defer std.testing.allocator.free(tokens);
    //     var tree = try Tree.parse(std.testing.allocator, source, tokens);
    //     defer tree.deinit();

    //     try evalFromRoot(std.testing.allocator, &tree, .root);
    //     // try std.testing.expectEqual(tree.getNode(@enumFromInt(1)).number, @as(f64, 123.0));
    // }
}

const std = @import("std");

const Tree = @import("Tree.zig");
