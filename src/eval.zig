pub fn eval(tree: *Tree, index: Tree.NodeIndex) void {
    switch (tree.getNode(index)) {
        .sexpr => {
            @panic("lol");
        },
        .number => return,
        .string => return,
        .identifier => {
            @panic("lol");
        },
    }
}

const std = @import("std");

const Tree = @import("Tree.zig");
