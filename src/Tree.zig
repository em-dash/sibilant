allocator: std.mem.Allocator,
nodes: std.MultiArrayList(Node),
identifiers: std.ArrayListUnmanaged([]const u8),
roots: std.ArrayListUnmanaged(NodeIndex),
defines: std.AutoHashMapUnmanaged(IdentifierIndex, Node),

const builtin_map: std.StaticStringMap(Node) = .initComptime(.{
    .{ "add", .builtin_add },
    .{ "+", .builtin_add },
    .{ "subtract", .builtin_subtract },
    // .{ "-", .builtin_subtract },
    .{ "multiply", .builtin_multiply },
    // .{ "*", .builtin_multiply },
    .{ "divide", .builtin_divide },
    // .{ "/", .builtin_divide },
    .{ "quote", .builtin_quote },
    .{ "lambda", .builtin_lambda },
    .{ "λ", .builtin_lambda },
    .{ "not", .builtin_not },
    .{ "true", .builtin_true },
    .{ "false", .builtin_false },
    .{ "and", .builtin_and },
    .{ "or", .builtin_or },
    .{ "if", .builtin_if },
    .{ "define", .builtin_define },
    .{ "nil", .builtin_nil },
    .{ "null", .builtin_nil },
    .{ "equals", .builtin_equals },
    // .{ "=", .builtin_equals },
    .{ "condition", .builtin_condition },
    .{ "append", .builtin_append },
});

pub const EvalError = error{
    DivideByZero,
    NotImplemented,
    TypeError,
    IncorrectArgumentCount,
    VariableNotBound,
};

const SexprIterator = struct {
    tree: *const Tree,
    node: NodeIndex,

    fn peek(self: SexprIterator) ?Node {
        if (self.node == .none) return null;

        const node = self.tree.getNode(self.node);
        switch (node) {
            .sexpr_item => |item| {
                return self.tree.getNode(item.value);
            },
            else => unreachable,
        }
    }

    fn peekIndex(self: SexprIterator) ?NodeIndex {
        if (self.node == .none) return null;

        const node = self.tree.getNode(self.node);
        switch (node) {
            .sexpr_item => |item| return item.value,
            else => unreachable,
        }
    }

    fn next(self: *SexprIterator) ?Node {
        if (self.node == .none) return null;

        const node = self.tree.getNode(self.node);
        switch (node) {
            .sexpr_item => |item| {
                const result = self.tree.getNode(item.value);
                self.node = item.next;
                return result;
            },
            else => return null,
        }
    }

    fn nextIndex(self: *SexprIterator) ?NodeIndex {
        if (self.node == .none) return null;

        const node = self.tree.getNode(self.node);
        switch (node) {
            .sexpr_item => |item| {
                const result = item.value;
                self.node = item.next;
                return result;
            },
            else => return null,
        }
    }
};

/// Evaluate the AST in place.  Requires an allocator for some operations but cleans up after
/// itself; any permanent memory will be allocated with `self.allocator` and cleaned up with
/// `.deinit()`.
pub fn eval(self: *Tree, allocator: std.mem.Allocator) (EvalError || std.mem.Allocator.Error)!void {
    for (self.roots.items) |root| {
        try self.evalFromNode(allocator, root);
        // TODO garbage collect
    }
}

/// Evaluates an S-Expression from the second item onwards; does not affect the head.
fn evalTheRestOfTheFuckingSexpr(
    self: *Tree,
    allocator: std.mem.Allocator,
    index: NodeIndex,
) (EvalError || std.mem.Allocator.Error)!void {
    var iterator: SexprIterator = .{ .tree = self, .node = index };
    _ = iterator.next(); // Skip the head.
    while (iterator.nextIndex()) |node| try self.evalFromNode(allocator, node);
}

/// Asserts that `index` points to a `.sexpr_item` node.
fn recurseSubstituteIdentifiers(
    self: *Tree,
    index: NodeIndex,
    defines: std.AutoHashMapUnmanaged(IdentifierIndex, Node),
) std.mem.Allocator.Error!void {
    std.debug.assert(self.getNode(index) == .sexpr_item);
    var iterator: SexprIterator = .{ .tree = self, .node = index };
    while (iterator.peekIndex()) |i| {
        const node = self.getNode(i);
        switch (node) {
            .sexpr_item => try self.recurseSubstituteIdentifiers(i, defines),
            .identifier => |identifier| {
                var defines_iterator = defines.iterator();
                while (defines_iterator.next()) |entry| {
                    if (entry.key_ptr.* == identifier) {
                        switch (self.getNode(iterator.node)) {
                            .sexpr_item => {
                                const new = try self.deepCopyNode(entry.value_ptr.*);
                                var temp = self.getNode(iterator.node);
                                temp.sexpr_item.value = try self.addNode(new);
                                self.setNode(iterator.node, temp);
                            },
                            else => unreachable,
                        }
                    }
                }
            },
            else => {},
        }
        _ = iterator.nextIndex();
    }
}

fn evalFromNode(
    self: *Tree,
    allocator: std.mem.Allocator,
    index: NodeIndex,
) (EvalError || std.mem.Allocator.Error)!void {
    switch (self.getNode(index)) {
        .sexpr_item => |sexpr_item| {
            try self.evalFromNode(allocator, sexpr_item.value);

            switch (self.getNode(sexpr_item.value)) {
                .builtin_add => {
                    try self.evalTheRestOfTheFuckingSexpr(allocator, index);

                    var sum: f64 = 0;
                    var iterator: SexprIterator = .{ .tree = self, .node = index };
                    _ = iterator.next(); // Skip builtin name node.
                    while (iterator.next()) |node| {
                        if (node == .number)
                            sum += node.number
                        else
                            return error.TypeError;
                    }
                    self.setNode(index, .{ .number = sum });
                },
                .builtin_subtract => {
                    try self.evalTheRestOfTheFuckingSexpr(allocator, index);

                    var difference: f64 = 0;
                    var iterator: SexprIterator = .{ .tree = self, .node = index };
                    _ = iterator.next(); // Skip builtin name node.
                    // First argument is added rather than subtracted.
                    if (iterator.next()) |node| {
                        if (node == .number)
                            difference += node.number
                        else
                            return error.TypeError;
                    }
                    while (iterator.next()) |node| {
                        if (node == .number)
                            difference -= node.number
                        else
                            return error.TypeError;
                    }
                    self.setNode(index, .{ .number = difference });
                },
                .builtin_multiply => {
                    try self.evalTheRestOfTheFuckingSexpr(allocator, index);

                    var product: f64 = 1;
                    var iterator: SexprIterator = .{ .tree = self, .node = index };
                    _ = iterator.next(); // Skip builtin name node.
                    while (iterator.next()) |node| {
                        if (node == .number)
                            product *= node.number
                        else
                            return error.TypeError;
                    }
                    self.setNode(index, .{ .number = product });
                },
                .builtin_divide => {
                    try self.evalTheRestOfTheFuckingSexpr(allocator, index);

                    var quotient: f64 = 1;
                    var iterator: SexprIterator = .{ .tree = self, .node = index };
                    _ = iterator.next(); // Skip builtin name node.
                    // First argument is added rather than subtracted.
                    if (iterator.next()) |node| {
                        if (node == .number)
                            quotient *= node.number
                        else
                            return error.TypeError;
                    }
                    while (iterator.next()) |node| {
                        if (node == .number) {
                            if (node.number == 0.0) return error.DivideByZero;
                            quotient /= node.number;
                        } else return error.TypeError;
                    }
                    self.setNode(index, .{ .number = quotient });
                },
                .builtin_quote => {
                    if (sexpr_item.next == .none) return error.IncorrectArgumentCount;
                    const next = self.getNode(sexpr_item.next);
                    if (next.sexpr_item.next != .none) return error.IncorrectArgumentCount;
                    const quoted = self.getNode(next.sexpr_item.value);
                    self.setNode(index, quoted);
                },
                .builtin_lambda => {
                    // No processing needed here; if this gets applied to something it will get
                    // resolved in the parent sexpr.
                },
                .sexpr_item => {
                    // This sexpr's head should resolve to a lambda expression.
                    const lambda = self.getNode(sexpr_item.value);
                    if (self.getNode(lambda.sexpr_item.value) != .builtin_lambda)
                        return error.TypeError;

                    // Collect variable names.
                    var variable_iterator: SexprIterator = .{
                        .tree = self,
                        .node = self.getNode(lambda.sexpr_item.next).sexpr_item.value,
                    };
                    var substitution_iterator: SexprIterator = .{ .tree = self, .node = index };
                    _ = substitution_iterator.next(); // Skip lambda keyword.
                    var defines: std.AutoHashMapUnmanaged(IdentifierIndex, Node) = .empty;
                    defer defines.deinit(allocator);
                    while (true) {
                        const variable = variable_iterator.next();
                        const substitution = substitution_iterator.next();
                        if (variable == null and substitution == null)
                            break
                        else if (variable != null and substitution != null)
                            try defines.put(allocator, variable.?.identifier, substitution.?)
                        else
                            return error.IncorrectArgumentCount;
                    }

                    const expression = self.getNode(lambda.sexpr_item.next).sexpr_item.next;
                    try self.recurseSubstituteIdentifiers(
                        self.getNode(expression).sexpr_item.value,
                        defines,
                    );

                    const expression_data = self.getNode(self.getNode(expression).sexpr_item.value);
                    self.setNode(index, expression_data);
                    try self.evalFromNode(allocator, index);
                },
                .builtin_true, .builtin_false, .builtin_nil => return error.TypeError,
                .number, .string => return error.TypeError,
                .identifier => {
                    try self.recurseSubstituteIdentifiers(index, self.defines);
                    try self.evalFromNode(allocator, index);
                },
                .builtin_not => {
                    try self.evalTheRestOfTheFuckingSexpr(allocator, index);

                    var iterator: SexprIterator = .{ .tree = self, .node = index };
                    _ = iterator.next(); // Skip builtin name node.
                    const operand = iterator.next() orelse return error.IncorrectArgumentCount;
                    if (iterator.next() != null) return error.IncorrectArgumentCount;
                    self.setNode(index, switch (operand) {
                        .builtin_true => .builtin_false,
                        .builtin_false => .builtin_true,
                        else => return error.TypeError,
                    });
                },
                .builtin_and => {
                    try self.evalTheRestOfTheFuckingSexpr(allocator, index);

                    var iterator: SexprIterator = .{ .tree = self, .node = index };
                    _ = iterator.next(); // Skip builtin name node.
                    var result = true;
                    while (iterator.next()) |node| {
                        switch (node) {
                            .builtin_true => {},
                            .builtin_false => result = false,
                            else => return error.TypeError,
                        }
                    }
                    self.setNode(index, switch (result) {
                        true => .builtin_true,
                        false => .builtin_false,
                    });
                },
                .builtin_or => {
                    try self.evalTheRestOfTheFuckingSexpr(allocator, index);

                    var iterator: SexprIterator = .{ .tree = self, .node = index };
                    _ = iterator.next(); // Skip builtin name node.
                    var result = false;
                    while (iterator.next()) |node| {
                        switch (node) {
                            .builtin_true => result = true,
                            .builtin_false => {},
                            else => return error.TypeError,
                        }
                    }
                    self.setNode(index, switch (result) {
                        true => .builtin_true,
                        false => .builtin_false,
                    });
                },
                .builtin_if => {
                    var iterator: SexprIterator = .{ .tree = self, .node = index };
                    _ = iterator.next(); // Skip builtin name node.
                    const condition = iterator.nextIndex() orelse return error.IncorrectArgumentCount;
                    try self.evalFromNode(allocator, condition);
                    const if_branch = iterator.nextIndex() orelse return error.IncorrectArgumentCount;
                    const else_branch = iterator.nextIndex();
                    switch (self.getNode(condition)) {
                        .builtin_true => {
                            try self.evalFromNode(allocator, if_branch);
                            self.setNode(index, self.getNode(if_branch));
                        },
                        .builtin_false => {
                            if (else_branch) |else_index| {
                                try self.evalFromNode(allocator, else_index);
                                self.setNode(index, self.getNode(else_index));
                            }
                        },
                        else => return error.TypeError,
                    }
                },
                .builtin_define => {
                    var iterator: SexprIterator = .{ .tree = self, .node = index };
                    _ = iterator.next(); // Skip builtin name node.
                    const identifier = iterator.next();
                    if (identifier == null) return error.IncorrectArgumentCount;
                    if (identifier.? != .identifier) return error.TypeError;
                    const value_index = iterator.nextIndex() orelse
                        return error.IncorrectArgumentCount;
                    try self.evalFromNode(allocator, value_index);
                    const value = self.getNode(value_index);
                    try self.defines.put(
                        self.allocator,
                        identifier.?.identifier,
                        try self.deepCopyNode(value),
                    );

                    self.setNode(index, .builtin_nil);
                },
                .builtin_equals => {
                    try self.evalTheRestOfTheFuckingSexpr(allocator, index);

                    var iterator: SexprIterator = .{ .tree = self, .node = index };
                    _ = iterator.next(); // Skip builtin name node.
                    var result = true;
                    const value = iterator.next() orelse return error.IncorrectArgumentCount;
                    while (iterator.next()) |item| result = switch (value) {
                        .builtin_nil => false,
                        inline else => std.meta.eql(value, item),
                    };

                    self.setNode(index, switch (result) {
                        true => .builtin_true,
                        false => .builtin_false,
                    });
                },
                .builtin_condition => {
                    var iterator: SexprIterator = .{ .tree = self, .node = index };
                    _ = iterator.next(); // Skip builtin name node.

                    while (iterator.nextIndex()) |node| {
                        if (self.getNode(node) != .sexpr_item) return error.TypeError;
                        var condition_iterator: SexprIterator = .{ .tree = self, .node = node };
                        const condition = condition_iterator.nextIndex() orelse
                            return error.IncorrectArgumentCount;
                        const consequent = condition_iterator.nextIndex() orelse
                            return error.IncorrectArgumentCount;
                        try self.evalFromNode(allocator, condition);
                        switch (self.getNode(condition)) {
                            .builtin_true => {
                                try self.evalFromNode(allocator, consequent);
                                self.setNode(index, self.getNode(consequent));
                                break;
                            },
                            .builtin_false => {},
                            else => return error.TypeError,
                        }
                    } else {
                        // No conditions were true.
                        self.setNode(index, .builtin_nil);
                    }
                },
                .builtin_append => {
                    try self.evalTheRestOfTheFuckingSexpr(allocator, index);

                    var iterator: SexprIterator = .{ .tree = self, .node = index };
                    _ = iterator.next(); // Skip builtin name node.

                    const root = if (iterator.next()) |node|
                        try self.deepCopyNode(node)
                    else
                        return error.IncorrectArgumentCount;

                    while (iterator.nextIndex()) |node| switch (self.getNode(node)) {
                        .sexpr_item => {
                            var current = root.sexpr_item.next;
                            while (true) {
                                const current_node = self.getNode(current);
                                if (current_node.sexpr_item.next == .none) break;
                                current = current_node.sexpr_item.next;
                            }
                            self.nodes.slice().items(.data)[@intFromEnum(current)]
                                .sexpr_item.next = node;
                        },
                        else => return error.TypeError,
                    };

                    self.setNode(index, root);
                },
            }
        },
        .number => {},
        .string => {},
        .identifier => |identifier| {
            // If this is a bulitin, set it.
            const string = self.getIdentifierString(identifier);
            if (builtin_map.get(string)) |builtin|
                self.setNode(index, builtin);
        },
        // These only appear in a branch that has already been parsed.
        .builtin_nil,
        .builtin_add,
        .builtin_subtract,
        .builtin_divide,
        .builtin_multiply,
        .builtin_quote,
        .builtin_lambda,
        .builtin_not,
        .builtin_true,
        .builtin_false,
        .builtin_and,
        .builtin_or,
        .builtin_if,
        .builtin_define,
        .builtin_equals,
        .builtin_condition,
        // .builtin_greater_than,
        // .builtin_less_than,
        .builtin_append,
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
        _ = try writer.write(" ");
    }
}

const WriteOptions = struct {
    type_annotation: bool = false,
};

fn recurseWriteCode(self: Tree, writer: anytype, index: NodeIndex, options: WriteOptions) !void {
    if (options.type_annotation and self.getNode(index) != .sexpr_item) {
        _ = try writer.write("[");
        _ = try writer.write(@tagName(self.getNode(index)));
        _ = try writer.write("]: ");
    }

    const node = self.getNode(index);
    switch (node) {
        .sexpr_item => |_| {
            _ = try writer.write("(");
            var iterator: SexprIterator = .{ .tree = &self, .node = index };
            try self.recurseWriteCode(
                writer,
                iterator.nextIndex().?,
                options,
            );
            while (iterator.nextIndex()) |current| {
                _ = try writer.write(" ");
                try self.recurseWriteCode(writer, current, options);
            }
            _ = try writer.write(")");
        },
        .number => |number| try std.fmt.format(writer, "{d}", .{number}),
        .string => |_| {
            @panic("bruh");
        },
        .identifier => |identifier| try std.fmt.format(
            writer,
            "{s}",
            .{self.getIdentifierString(identifier)},
        ),
        .builtin_nil => {}, // nil writes nothing
        // zig fmt: off
        .builtin_add, .builtin_subtract, .builtin_divide, .builtin_multiply, .builtin_quote,
        .builtin_lambda, .builtin_not, .builtin_true, .builtin_false, .builtin_or, .builtin_and,
        .builtin_if, .builtin_define, .builtin_equals, .builtin_condition, .builtin_append => { 
            // zig fmt: on
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

// Returns a literal `Node` value because we're overwriting existing values with the result.
pub fn deepCopyNode(self: *Tree, node: Node) std.mem.Allocator.Error!Node {
    return switch (node) {
        .sexpr_item => |head| .{
            .sexpr_item = .{
                .value = try self.addNode(try self.deepCopyNode(self.getNode(head.value))),
                .next = if (head.next == .none)
                    .none
                else
                    try self.addNode(try self.deepCopyNode(self.getNode(head.next))),
            },
        },
        else => node,
    };
}

const Tree = @This();

fn init(allocator: std.mem.Allocator) Tree {
    return .{
        .allocator = allocator,
        .nodes = .empty,
        .identifiers = .empty,
        .roots = .empty,
        .defines = .empty,
        // .strings
    };
}

pub fn deinit(self: *Tree) void {
    self.nodes.deinit(self.allocator);
    self.identifiers.deinit(self.allocator);
    self.roots.deinit(self.allocator);
    self.defines.deinit(self.allocator);
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
    none = std.math.maxInt(u32),
    _,
};
pub const IdentifierIndex = enum(u32) { _ };

pub const SexprItem = struct {
    value: NodeIndex,
    next: NodeIndex,

    const empty: SexprItem = .{ .value = .none, .next = .none };
};

pub const Node = union(enum) {
    sexpr_item: SexprItem,
    number: f64,
    string: StringIndex,
    identifier: IdentifierIndex,

    builtin_add,
    builtin_subtract,
    builtin_multiply,
    builtin_divide,
    builtin_quote,
    builtin_lambda,
    builtin_define,
    builtin_not,
    builtin_true,
    builtin_false,
    builtin_and,
    builtin_or,
    builtin_if,
    builtin_nil,
    builtin_equals,
    builtin_condition,
    builtin_append,

    const empty_sexpr: Node = .{ .sexpr = .empty };

    comptime {
        if (@sizeOf(Node) > 16)
            @compileError("You made `Node` too big!  It should be 16 bytes, found " ++
                std.fmt.comptimePrint("{}", .{@sizeOf(Node)}));
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
                .{ .sexpr_item = .{ .value = undefined, .next = .none } },
            );
            {
                // Use an intermediate to avoid writing to an invalidated pointer after a potential
                // realloc.
                const value = try recurseParse(source, iterator, tree);
                tree.nodes.slice().items(.data)[@intFromEnum(root)].sexpr_item.value = value;
            }

            var current = root;
            while (iterator.peek()) |t| {
                if (t.tag == .close) {
                    _ = iterator.next();
                    break;
                }
                const next = try tree.addNode(
                    .{ .sexpr_item = .{ .value = undefined, .next = .none } },
                );
                tree.nodes.slice().items(.data)[@intFromEnum(current)].sexpr_item.next = next;
                current = next;
                // Same realloc danger as above.
                const value = try recurseParse(source, iterator, tree);
                tree.nodes.slice().items(.data)[@intFromEnum(current)].sexpr_item.value = value;
            } else return error.UnexpectedEof;
            return root;
        },
        .quote => {
            const value = try recurseParse(source, iterator, tree);
            const next = try tree.addNode(
                .{ .sexpr_item = .{ .value = value, .next = .none } },
            );
            const identifier = try tree.getOrPutIdentifier("quote");
            const quote = try tree.addNode(.{ .identifier = identifier });
            const root = try tree.addNode(
                .{ .sexpr_item = .{ .value = quote, .next = next } },
            );
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

const std = @import("std");

const tokenization = @import("tokenization.zig");
