const StringIndex = enum(u32) { _ };
const NodeIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,
};
const IdentifierIndex = enum(u32) { _ };

const IdentifierMap = struct {
    hashmap: std.StringHashMapUnmanaged(u32),
    next: u32,

    const empty: IdentifierMap = .{ .hashmap = .empty, .next = 0 };

    fn getOrPut(
        self: *IdentifierMap,
        allocator: std.mem.Allocator,
        key: []const u8,
    ) !IdentifierIndex {
        const maybe_index = self.hashmap.get(key);
        if (maybe_index) |index| {
            return @enumFromInt(index);
        } else {
            try self.hashmap.put(allocator, key, self.next);
            const result = self.next;
            self.next += 1;
            return @enumFromInt(result);
        }
    }

    fn deinit(self: *IdentifierMap, allocator: std.mem.Allocator) void {
        self.hashmap.deinit(allocator);
    }
};

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
};

pub const Tree = struct {
    allocator: std.mem.Allocator,
    nodes: []Node,
    roots: []NodeIndex,
    identifiers: IdentifierMap,
    // strings: []?[]u8,

    fn init(allocator: std.mem.Allocator) Tree {
        return .{
            .allocator = allocator,
            .nodes = &.{},
            .roots = &.{},
            .identifiers = .empty,
            // .strings
        };
    }

    pub fn deinit(self: *Tree) void {
        self.allocator.free(self.nodes);
        self.allocator.free(self.roots);
        self.identifiers.deinit(self.allocator);
        // self.allocator.free(self.strings);
    }

    fn getNodeMutable(self: Tree, index: NodeIndex) *Node {
        return &self.nodes[@intFromEnum(index)];
    }

    pub fn getNode(self: Tree, index: NodeIndex) *const Node {
        return self.getNodeMutable(index);
    }

    fn addNode(self: *Tree, comptime tag: std.meta.Tag(Node)) !NodeIndex {
        var temp = std.ArrayList(Node).fromOwnedSlice(self.allocator, self.nodes);
        try temp.append(@unionInit(Node, @tagName(tag), undefined));
        self.nodes = try temp.toOwnedSlice();
        return @enumFromInt(self.nodes.len - 1);
    }
};

const TokenIterator = struct {
    index: u32 = 0,
    slice: []const tokens.Token,

    fn next(self: *TokenIterator) ?*const tokens.Token {
        if (self.index < self.slice.len) {
            const result = &self.slice[self.index];
            self.index += 1;
            return result;
        } else return null;
    }

    fn peek(self: *TokenIterator) ?*const tokens.Token {
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
    switch (token.tag) {
        .l => {
            const root = try tree.addNode(.sexpr);
            tree.getNodeMutable(root).sexpr.value =
                try recurseSexprs(source, iterator, tree);
            var current = root;
            while (iterator.peek()) |t| {
                if (t.tag == .r) {
                    _ = iterator.next();
                    break;
                }
                const next = try tree.addNode(.sexpr);
                tree.getNodeMutable(current).sexpr.next = next;
                current = next;
                tree.getNodeMutable(current).sexpr.value =
                    try recurseSexprs(source, iterator, tree);
            } else return error.UnexpectedEof;
            return root;
        },
        .r => {
            return error.DontPutARightParenHerePls;
        },
        .string => {
            @panic("i didn't implement this yet sry");
        },
        .number => {
            const number = try std.fmt.parseFloat(f64, source[token.start..token.end]);
            const node = try tree.addNode(.number);
            tree.getNodeMutable(node).number = number;
            return node;
        },
        .identifier => {
            const identifier = try tree.identifiers.getOrPut(
                tree.allocator,
                source[token.start..token.end],
            );
            const node = try tree.addNode(.identifier);
            tree.getNodeMutable(node).identifier = identifier;
            return node;
        },
    }

    unreachable;
}

pub fn parse(
    allocator: std.mem.Allocator,
    source: []const u8,
    tokenized: []const tokens.Token,
) !Tree {
    var tree = Tree.init(allocator);
    errdefer tree.deinit();
    var roots: std.ArrayListUnmanaged(NodeIndex) = .empty;
    errdefer roots.deinit(allocator);
    var iterator: TokenIterator = .{ .slice = tokenized };

    while (iterator.peek()) |_| {
        const node = try recurseSexprs(source, &iterator, &tree);
        try roots.append(allocator, node);
    }

    tree.roots = try roots.toOwnedSlice(allocator);
    return tree;
}

test parse {
    const source = "(lol lmao)";
    const tokenized = try tokens.tokenize(std.testing.allocator, source);
    defer std.testing.allocator.free(tokenized);
    var tree = try parse(std.testing.allocator, source, tokenized);
    defer tree.deinit();
}

const std = @import("std");

const tokens = @import("tokens.zig");
