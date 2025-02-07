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
    ) IdentifierIndex {
        const maybe_index = self.hashmap.get(key);
        if (maybe_index) |index| {
            return @enumFromInt(index);
        } else {
            try self.hashmap.put(allocator, []const u8, self.next);
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

    const nones: Sexpr = .{ .value = .none, .next = .none };
};

pub const Node = union(enum) {
    sexpr: Sexpr,
    number: f64,
    string: StringIndex,
    identifier: IdentifierIndex,
};

pub const Tree = struct {
    nodes: []Node,
    roots: []NodeIndex,
    strings: []?[]u8,
};

const TokenIterator = struct {
    index: u32 = 0,
    slice: []const tokens.Token,

    fn next(self: TokenIterator) ?*tokens.Token {
        if (self.index < self.slice.len) {
            const result = &self.slice[self.index];
            self.index += 1;
            return result;
        } else return null;
    }

    fn peek(self: TokenIterator) ?*tokens.Token {
        if (self.index < self.slice.len) {
            return &self.slice[self.index];
        } else return null;
    }
};

fn parseNode(
    allocator: std.mem.Allocator,
    source: []const u8,
    iterator: *TokenIterator,
    nodes: *std.ArrayListUnmanaged(Node),
    strings: *std.ArrayListUnmanaged([]u8),
    identifiers: *IdentifierMap,
) !NodeIndex {
    const token = if (iterator.next()) |t| t;
    switch (token.tag) {
        .l => {
            try nodes.append(allocator, .nones);
            const root = nodes.items.len - 1;
            nodes.items[root] =
                .{try parseNode(allocator, source, iterator, nodes, strings, identifiers)};
            while (iterator.peek().tag != .r) {
                // next
            }
        },
        .string => {},
        .number => {
            const number = try std.fmt.parseFloat(f64, source[token.start..token.end]);
            try nodes.append(allocator, .{ .number = number });
            return nodes.len - 1;
        },
        .identifier => {
            const identifier = try identifiers.getOrPut(allocator, source[token.start..token.end]);
            return .{ .identifier = identifier };
        },
    }
}

pub fn parse(
    allocator: std.mem.Allocator,
    source: []const u8,
    tokenized: []const tokens.Token,
) !Tree {
    var nodes: std.ArrayListUnmanaged(Node) = .empty;
    var roots: std.ArrayListUnmanaged(NodeIndex) = .empty;
    var strings: std.ArrayListUnmanaged([]u8) = .empty;
    var iterator: TokenIterator = .{ .slice = tokenized };
    var identifiers: *IdentifierMap = .{};

    while (iterator.peek()) |_| {
        const node = try parseNode(source, &iterator, &nodes, &strings, &identifiers);
        try roots.append(allocator, node);
    }
}

const std = @import("std");
const tokens = @import("tokens.zig");
