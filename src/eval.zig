// pub fn eval(tree: *tree.Tree, index: tree.NodeIndex) void {
//     switch (tree.getNode(index)) {
//         .sexpr => {
//             @compileError("lol");
//         },
//         .number => return,
//         .string => return,
//         .identifier => {
//             @compileError("lol");
//         },
//     }
// }

const std = @import("std");

const tree = @import("tree");
