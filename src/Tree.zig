allocator: std.mem.Allocator,
nodes: std.MultiArrayList(Node),
identifiers: std.ArrayListUnmanaged([]const u8),
root: NodeIndex,
// strings: []?[]u8,

// pub fn format(value: ?, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void
pub fn format(
    self: Tree,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    var list = std.ArrayList(NodeIndex).init(self.allocator);
    defer list.deinit();
    try self.recurseWriteCode(&list, writer, .root);
}

fn recurseWriteCode(self: Tree, list: *std.ArrayList(NodeIndex), writer: anytype, index: NodeIndex) !void {
    for (list.items) |seen| {
        if (seen == index) @panic("trying to print an AST with a loop in it");
    } else try list.append(index);

    switch (self.getNode(index)) {
        .sexpr => |_| {
            if (index != .root) _ = try writer.write("(");
            var current = index;
            while (true) {
                try self.recurseWriteCode(list, writer, self.getNode(current).sexpr.value);
                if (self.getNode(current).sexpr.next != .none) {
                    _ = try writer.write(" ");
                    current = self.getNode(current).sexpr.next;
                } else break;
            }
            if (index != .root) _ = try writer.write(")");
        },
        .number => |number| try std.fmt.format(writer, "{any}", .{number}),
        .string => |_| {
            @panic("bruh");
        },
        .identifier => |identifier| try std.fmt.format(
            writer,
            "{s}",
            .{self.getIdentifierString(identifier)},
        ),
    }
}

fn getOrPutIdentifier(
    self: *Tree,
    string: []const u8,
) !IdentifierIndex {
    for (self.identifiers.items, 0..) |item, index| {
        if (std.mem.eql(u8, string, item)) return @enumFromInt(index);
    } else {
        try self.identifiers.append(self.allocator, string);
        return @enumFromInt(self.identifiers.items.len - 1);
    }
}

pub fn getIdentifierString(self: Tree, index: IdentifierIndex) []const u8 {
    return self.identifiers.items[@intFromEnum(index)];
}

const Tree = @This();

fn init(allocator: std.mem.Allocator) Tree {
    return .{
        .allocator = allocator,
        .nodes = .empty,
        .identifiers = .empty,
        .root = .none,
        // .strings
    };
}

pub fn deinit(self: *Tree) void {
    self.nodes.deinit(self.allocator);
    self.identifiers.deinit(self.allocator);
    // self.allocator.free(self.strings);
}

pub fn setNode(self: *Tree, index: NodeIndex, element: Node) void {
    self.nodes.set(@intFromEnum(index), element);
}

pub fn getNode(self: Tree, index: NodeIndex) Node {
    std.debug.assert(index != .none);
    std.debug.assert(@intFromEnum(index) < self.nodes.items(.data).len);
    return self.nodes.get(@intFromEnum(index));
}

pub fn addNode(self: *Tree, item: Node) !NodeIndex {
    try self.nodes.append(self.allocator, item);
    return @enumFromInt(self.nodes.items(.tags).len - 1);
}

pub const StringIndex = enum(u32) { _ };
pub const NodeIndex = enum(u32) {
    root = 0,
    none = std.math.maxInt(u32),
    _,

    // const root: NodeIndex = @enumFromInt(0);
};
pub const IdentifierIndex = enum(u32) { _ };

pub const Sexpr = struct {
    value: NodeIndex,
    next: NodeIndex,

    const empty: Sexpr = .{ .value = .none, .next = .none };
};

pub const Node = union(enum) {
    sexpr: Sexpr,
    number: f64,
    string: StringIndex,
    identifier: IdentifierIndex,

    const empty_sexpr: Node = .{ .sexpr = .empty };

    comptime {
        for (@typeInfo(Node).@"union".fields) |field| if (@sizeOf(field.type) > 8)
            @compileError("The " ++ field.name ++ " node body shouldn't be bigger than 8 bytes!");
    }
};

const TokenIterator = struct {
    index: u32 = 0,
    slice: []const tokenization.Token,

    fn next(self: *TokenIterator) ?*const tokenization.Token {
        if (self.index < self.slice.len) {
            const result = &self.slice[self.index];
            self.index += 1;
            return result;
        } else return null;
    }

    fn peek(self: *TokenIterator) ?*const tokenization.Token {
        if (self.index < self.slice.len) {
            return &self.slice[self.index];
        } else return null;
    }
};

fn recurseSexprs(
    source: []const u8,
    iterator: *TokenIterator,
    tree: *Tree,
) !NodeIndex {
    const token = if (iterator.next()) |t| t else return error.UnexpectedEof;
    std.log.debug("parsing... token.tag == {any}", .{token.tag});
    switch (token.tag) {
        .open => {
            const root = try tree.addNode(.{ .sexpr = .{ .value = undefined, .next = .none } });
            tree.nodes.slice().items(.data)[@intFromEnum(root)].sexpr.value =
                try recurseSexprs(source, iterator, tree);

            var current = root;
            while (iterator.peek()) |t| {
                std.debug.print("=========================================================\n", .{});
                if (t.tag == .close) {
                    _ = iterator.next();
                    break;
                }
                const next = try tree.addNode(.{ .sexpr = .{ .value = undefined, .next = .none } });
                // tree.nodes.slice().items(.data)[@intFromEnum(current)].sexpr.next = next;
                {
                    var temp = tree.getNode(current);
                    temp.sexpr.next = next;
                    tree.setNode(current, temp);
                }
                current = next;
                // tree.nodes.slice().items(.data)[@intFromEnum(current)].sexpr.value =
                //     try recurseSexprs(source, iterator, tree);
                {
                    var temp = tree.getNode(current);
                    temp.sexpr.value = try recurseSexprs(source, iterator, tree);
                    tree.setNode(current, temp);
                }
            } else return error.UnexpectedEof;
            return root;
        },
        .close => {
            return error.DontPutAClosingParenHerePls;
        },
        .string => {
            @panic("i didn't implement this yet sry");
        },
        .number => {
            const number = try std.fmt.parseFloat(f64, source[token.start..token.end]);
            const node = try tree.addNode(.{ .number = number });
            return node;
        },
        .identifier => {
            const identifier = try tree.getOrPutIdentifier(source[token.start..token.end]);
            const node = try tree.addNode(.{ .identifier = identifier });
            return node;
        },
    }

    unreachable;
}

pub fn parse(
    allocator: std.mem.Allocator,
    source: []const u8,
    tokens: []const tokenization.Token,
) !Tree {
    var tree = Tree.init(allocator);
    errdefer tree.deinit();
    var iterator: TokenIterator = .{ .slice = tokens };

    if (tokens.len == 0) {
        tree.root = try tree.addNode(.{ .sexpr = .{ .value = .none, .next = .none } });
        return tree;
    }

    const root = try tree.addNode(.{ .sexpr = .{ .value = undefined, .next = .none } });
    tree.nodes.slice().items(.data)[@intFromEnum(root)].sexpr.value =
        try recurseSexprs(source, &iterator, &tree);

    var current = root;
    while (iterator.peek()) |_| {
        const next = try tree.addNode(.{ .sexpr = .{ .value = undefined, .next = .none } });
        tree.nodes.slice().items(.data)[@intFromEnum(current)].sexpr.next = next;
        current = next;
        tree.nodes.slice().items(.data)[@intFromEnum(current)].sexpr.value =
            try recurseSexprs(source, &iterator, &tree);
    }

    tree.root = root;
    return tree;
}

test parse {
    // const source = "(add 420 69 (lol lmao))";
    const source = "(add 123 123 12 3235 345 324)";
    const tokens = try tokenization.tokenize(std.testing.allocator, source);
    defer std.testing.allocator.free(tokens);
    var tree = try parse(std.testing.allocator, source, tokens);
    defer tree.deinit();

    try std.io.getStdOut().writer().print("{any}\n", .{tree});
}

const std = @import("std");

const tokenization = @import("tokenization.zig");
