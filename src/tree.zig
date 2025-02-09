const StringIndex = enum(u32) { _ };
const NodeIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    const root: NodeIndex = @enumFromInt(0);
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

    comptime {
        for (@typeInfo(Node).@"union".fields) |field| if (@sizeOf(field.type) > 8)
            @compileError("The " ++ field.name ++ " node body shouldn't be bigger than 8 bytes!");
    }
};

pub const Tree = struct {
    allocator: std.mem.Allocator,
    nodes: std.MultiArrayList(Node),
    identifiers: IdentifierMap,
    root: NodeIndex,
    // strings: []?[]u8,

    fn init(allocator: std.mem.Allocator) Tree {
        return .{
            .allocator = allocator,
            .nodes = .{},
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
        return self.nodes.get(@intFromEnum(index));
    }

    pub fn addNode(self: *Tree, item: Node) !NodeIndex {
        try self.nodes.append(self.allocator, item);
        return @enumFromInt(self.nodes.items(.tags).len - 1);
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
        .open => {
            const root = try tree.addNode(.{ .sexpr = .{ .value = undefined, .next = .none } });
            tree.nodes.slice().items(.data)[@intFromEnum(root)].sexpr.value =
                try recurseSexprs(source, iterator, tree);

            var current = root;
            while (iterator.peek()) |t| {
                if (t.tag == .close) {
                    _ = iterator.next();
                    break;
                }
                const next = try tree.addNode(.{ .sexpr = .{ .value = undefined, .next = .none } });
                tree.nodes.slice().items(.data)[@intFromEnum(current)].sexpr.next = next;
                current = next;
                tree.nodes.slice().items(.data)[@intFromEnum(current)].sexpr.value =
                    try recurseSexprs(source, iterator, tree);
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
            const identifier = try tree.identifiers.getOrPut(
                tree.allocator,
                source[token.start..token.end],
            );
            const node = try tree.addNode(.{ .identifier = identifier });
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
    var iterator: TokenIterator = .{ .slice = tokenized };

    if (tokenized.len == 0) {
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
    const source = "(lol lmao)";
    const tokenized = try tokens.tokenize(std.testing.allocator, source);
    defer std.testing.allocator.free(tokenized);
    var tree = try parse(std.testing.allocator, source, tokenized);
    defer tree.deinit();

    return error.lol;
}

const std = @import("std");

const tokens = @import("tokens.zig");
