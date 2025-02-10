allocator: std.mem.Allocator,
nodes: std.MultiArrayList(Node),
identifiers: std.ArrayListUnmanaged([]const u8),
roots: std.ArrayListUnmanaged(NodeIndex),
// strings: []?[]u8,

const builtin_map: std.StaticStringMap(Node) = .initComptime(.{
    .{ "add", .builtin_add },
    .{ "+", .builtin_add },
    .{ "subtract", .builtin_subtract },
    .{ "-", .builtin_subtract },
    .{ "multiply", .builtin_multiply },
    .{ "*", .builtin_multiply },
    .{ "divide", .builtin_divide },
    .{ "/", .builtin_divide },
    .{ "quote", .builtin_quote },
    .{ "lambda", .builtin_lambda },
    .{ "λ", .builtin_lambda },
});

pub fn eval(self: *Tree, allocator: std.mem.Allocator) !void {
    for (self.roots.items) |root| try self.evalFromNode(allocator, root);
}

fn evalFromNode(self: *Tree, allocator: std.mem.Allocator, index: NodeIndex) !void {
    // var temp_allocator = std.heap.stackFallback(64, allocator);

    switch (self.getNode(index)) {
        .sexpr_head => |sexpr_head| {
            {
                try self.evalFromNode(allocator, sexpr_head.value);
                var current = sexpr_head.next;
                while (current != .none) {
                    const node = self.getNode(current);
                    try self.evalFromNode(allocator, node.sexpr_tail.value);
                    current = node.sexpr_tail.next;
                }
            }

            const new_head = self.getNode(index).sexpr_head;
            switch (self.getNode(new_head.value)) {
                .builtin_add => {
                    var current = new_head.next;
                    var sum: f64 = 0;
                    while (current != .none) {
                        const node = self.getNode(current).sexpr_tail;
                        const summand = self.getNode(node.value);
                        if (summand == .number)
                            sum += summand.number
                        else
                            return error.TypeError;
                        current = node.next;
                    }
                    self.setNode(index, .{ .number = sum });
                },
                else => return error.NotImplemented,
            }
        },
        .sexpr_tail => unreachable,
        .number => {},
        .string => {},
        .identifier => |identifier| {
            // If this is a bulitin, set it.
            const string = self.getIdentifierString(identifier);
            if (builtin_map.get(string)) |builtin|
                self.setNode(index, builtin)
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

pub fn format(
    self: Tree,
    comptime fmt: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    var options: WriteOptions = .{};
    if (fmt.len > 0) {
        if (std.mem.count(u8, fmt, "t") > 0) options.type_annotation = true;
    }
    for (self.roots.items) |root| {
        try self.recurseWriteCode(writer, root, options);
    }
}

const WriteOptions = struct {
    type_annotation: bool = false,
};

fn recurseWriteCode(self: Tree, writer: anytype, index: NodeIndex, options: WriteOptions) !void {
    if (options.type_annotation and self.getNode(index) != .sexpr_head) {
        _ = try writer.write("[");
        _ = try writer.write(@tagName(self.getNode(index)));
        _ = try writer.write("]: ");
    }

    const node = self.getNode(index);
    switch (node) {
        .sexpr_head => |_| {
            _ = try writer.write("(");
            var current = index;
            try self.recurseWriteCode(writer, self.getNode(current).sexpr_head.value, options);
            current = self.getNode(current).sexpr_head.next;
            while (current != .none) {
                _ = try writer.write(" ");
                try self.recurseWriteCode(writer, self.getNode(current).sexpr_tail.value, options);
                current = self.getNode(current).sexpr_tail.next;
            }
            _ = try writer.write(")");
        },
        .sexpr_tail => |_| unreachable,
        .number => |number| try std.fmt.format(writer, "{d}", .{number}),
        .string => |_| {
            @panic("bruh");
        },
        .identifier => |identifier| try std.fmt.format(
            writer,
            "{s}",
            .{self.getIdentifierString(identifier)},
        ),
        .builtin_add,
        .builtin_subtract,
        .builtin_divide,
        .builtin_multiply,
        .builtin_quote,
        .builtin_lambda,
        => {
            const tag_name = @tagName(node);
            try std.fmt.format(writer, "{s}", .{tag_name[8..]});
        },
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
        .roots = .empty,
        // .strings
    };
}

pub fn deinit(self: *Tree) void {
    self.nodes.deinit(self.allocator);
    self.identifiers.deinit(self.allocator);
    self.roots.deinit(self.allocator);
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
    // root = 0,
    none = std.math.maxInt(u32),
    _,

    // const root: NodeIndex = @enumFromInt(0);
};
pub const IdentifierIndex = enum(u32) { _ };

pub const SexprHead = struct {
    value: NodeIndex,
    next: NodeIndex,

    const empty: SexprHead = .{ .value = .none, .next = .none };
};

pub const SexprTail = struct {
    value: NodeIndex,
    next: NodeIndex,

    const empty: SexprTail = .{ .value = .none, .next = .none };
};

pub const Node = union(enum) {
    sexpr_head: SexprHead,
    sexpr_tail: SexprTail,
    number: f64,
    string: StringIndex,
    identifier: IdentifierIndex,

    builtin_add,
    builtin_subtract,
    builtin_multiply,
    builtin_divide,
    builtin_quote,
    builtin_lambda,

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

fn recurseParse(
    source: []const u8,
    iterator: *TokenIterator,
    tree: *Tree,
) !NodeIndex {
    const token = if (iterator.next()) |t| t else return error.UnexpectedEof;
    switch (token.tag) {
        .open => {
            const root = try tree.addNode(
                .{ .sexpr_head = .{ .value = undefined, .next = .none } },
            );
            {
                // Use an intermediate to avoid writing to an invalidated pointer after a potential
                // realloc.
                const value = try recurseParse(source, iterator, tree);
                tree.nodes.slice().items(.data)[@intFromEnum(root)].sexpr_head.value = value;
            }

            var current = root;
            while (iterator.peek()) |t| {
                if (t.tag == .close) {
                    _ = iterator.next();
                    break;
                }
                const next = try tree.addNode(
                    .{ .sexpr_tail = .{ .value = undefined, .next = .none } },
                );
                if (tree.getNode(current) == .sexpr_head)
                    tree.nodes.slice().items(.data)[@intFromEnum(current)].sexpr_head.next = next
                else
                    tree.nodes.slice().items(.data)[@intFromEnum(current)].sexpr_tail.next = next;
                current = next;
                // Same realloc danger as above.
                const value = try recurseParse(source, iterator, tree);
                tree.nodes.slice().items(.data)[@intFromEnum(current)].sexpr_tail.value = value;
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
        return tree;
    }

    while (iterator.peek()) |_| {
        const root = try recurseParse(source, &iterator, &tree);
        try tree.roots.append(allocator, root);
    }

    return tree;
}

test parse {
    const source = "(add 123 123 12 3235 345 324)";
    const tokens = try tokenization.tokenize(std.testing.allocator, source);
    defer std.testing.allocator.free(tokens);
    var tree = try parse(std.testing.allocator, source, tokens);
    defer tree.deinit();

    try std.io.getStdOut().writer().print("{any}\n", .{tree});
}

const std = @import("std");

const tokenization = @import("tokenization.zig");
