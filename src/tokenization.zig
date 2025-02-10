/// Such that `source[start..end]` gives a slice of the token.
pub const Token = struct {
    start: u32,
    end: u32,
    tag: Tag,

    const Tag = enum {
        open,
        close,
        // quote,
        string,
        identifier,
        number,
    };
};

fn isNumber(word: []const u8, props_data: PropsData) bool {
    if (std.mem.eql(u8, word, ".")) return false;
    if (word.len >= 3) {
        if (std.mem.eql(u8, "0x", word[0..2])) {
            var iterator = code_point.Iterator{ .bytes = word[2..] };
            while (iterator.next()) |cp| {
                if (!(props_data.isHexDigit(cp.code) or cp.code == '.' or cp.code == '_'))
                    return false;
            } else return true;
        }
    }
    {
        var iterator = code_point.Iterator{ .bytes = word };
        var point_count: u32 = 0;
        while (iterator.next()) |cp| {
            if (cp.code == '.') {
                point_count += 1;
                if (point_count > 1) return false;
            } else if (!(props_data.isDecimal(cp.code) or cp.code == '_')) return false;
        } else return true;
    }
}

/// Caller owns returned memory.
pub fn tokenize(allocator: std.mem.Allocator, source: []const u8) ![]Token {
    const props_data = try PropsData.init(allocator);
    defer props_data.deinit();
    const gen_cat_data = try GenCatData.init(allocator);
    defer gen_cat_data.deinit();

    if (source.len == 0) return &[_]Token{};

    var token_list = std.ArrayList(Token).init(allocator);
    // errdefer token_list.deinit();

    var iterator = code_point.Iterator{ .bytes = source };
    source_loop: while (true) {
        var token: Token = undefined;
        token.start = iterator.i;
        var state: enum {
            start,
            @"continue",
        } = .start;
        _ = &state;

        token_loop: while (true) {
            switch (state) {
                .start => if (iterator.peek()) |cp| {
                    if (cp.code == '(') {
                        _ = iterator.next();
                        token.end = iterator.i;
                        token.tag = .open;
                        try token_list.append(token);
                        break :token_loop;
                    } else if (cp.code == ')') {
                        _ = iterator.next();
                        token.end = iterator.i;
                        token.tag = .close;
                        try token_list.append(token);
                        break :token_loop;
                    } else if (gen_cat_data.isLetter(cp.code) or
                        gen_cat_data.isMark(cp.code) or
                        props_data.isDecimal(cp.code) or
                        gen_cat_data.isPunctuation(cp.code) or
                        gen_cat_data.isSymbol(cp.code))
                    {
                        state = .@"continue";
                        _ = iterator.next();
                    } else if (props_data.isWhitespace(cp.code)) {
                        _ = iterator.next();
                        token.start = iterator.i;
                    } else @panic("lol");
                } else break :source_loop,
                .@"continue" => if (iterator.peek()) |cp| {
                    if (gen_cat_data.isLetter(cp.code) or
                        gen_cat_data.isMark(cp.code) or
                        props_data.isDecimal(cp.code) or
                        // gen_cat_data.isPunctuation(cp.code) or
                        gen_cat_data.isSymbol(cp.code) or
                        cp.code == '.')
                        _ = iterator.next()
                    else {
                        token.end = iterator.i;
                        token.tag = if (isNumber(source[token.start..token.end], props_data))
                            .number
                        else
                            .identifier;
                        state = .start;
                        try token_list.append(token);
                        break :token_loop;
                    }
                } else {
                    token.end = iterator.i;
                    token.tag = if (isNumber(source[token.start..token.end], props_data))
                        .number
                    else
                        .identifier;
                    state = .start;
                    try token_list.append(token);
                    break :source_loop;
                },
            }
        }
    }

    return try token_list.toOwnedSlice();
}

test tokenize {
    {
        const actual = try tokenize(std.testing.allocator, "()");
        defer std.testing.allocator.free(actual);
        const expected = &[_]Token{
            .{ .start = 0, .end = 1, .tag = .open },
            .{ .start = 1, .end = 2, .tag = .close },
        };
        try std.testing.expectEqualSlices(Token, expected, actual);
    }
    {
        const actual = try tokenize(std.testing.allocator, "(blep)");
        defer std.testing.allocator.free(actual);
        const expected = &[_]Token{
            .{ .start = 0, .end = 1, .tag = .open },
            .{ .start = 1, .end = 5, .tag = .identifier },
            .{ .start = 5, .end = 6, .tag = .close },
        };
        try std.testing.expectEqualSlices(Token, expected, actual);
    }
    {
        const actual = try tokenize(std.testing.allocator, ".( ) xyz");
        defer std.testing.allocator.free(actual);
        const expected = &[_]Token{
            .{ .start = 0, .end = 1, .tag = .identifier },
            .{ .start = 1, .end = 2, .tag = .open },
            .{ .start = 3, .end = 4, .tag = .close },
            .{ .start = 5, .end = 8, .tag = .identifier },
        };
        try std.testing.expectEqualSlices(Token, expected, actual);
    }
}

const std = @import("std");

const code_point = @import("code_point");
const PropsData = @import("PropsData");
const GenCatData = @import("GenCatData");
