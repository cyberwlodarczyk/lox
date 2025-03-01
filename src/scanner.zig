const std = @import("std");
const ascii = std.ascii;
const StringHashMap = std.StringHashMap;
const Allocator = std.mem.Allocator;

pub const TokenKind = enum {
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,
    bang,
    bang_equal,
    equal,
    equal_equal,
    grater,
    greater_equal,
    less,
    less_equal,
    identifier,
    string,
    number,
    @"and",
    class,
    @"else",
    false,
    @"for",
    fun,
    @"if",
    nil,
    @"or",
    print,
    @"return",
    super,
    this,
    true,
    @"var",
    @"while",
    eof,
};

pub const Token = struct {
    kind: TokenKind,
    lexeme: []const u8,
    line: u32,
};

pub const Scanner = struct {
    const Self = @This();
    pub const Error = error{
        UnexpectedCharacter,
        UnterminatedString,
    };

    line: u32,
    start: usize,
    current: usize,
    source: []const u8,
    identifiers: StringHashMap(TokenKind),

    pub fn init(source: []const u8, allocator: Allocator) !Self {
        var i = StringHashMap(TokenKind).init(allocator);
        try i.put("and", .@"and");
        try i.put("class", .class);
        try i.put("else", .@"else");
        try i.put("false", .false);
        try i.put("for", .@"for");
        try i.put("fun", .fun);
        try i.put("if", .@"if");
        try i.put("nil", .nil);
        try i.put("or", .@"or");
        try i.put("print", .print);
        try i.put("return", .@"return");
        try i.put("super", .super);
        try i.put("this", .this);
        try i.put("true", .true);
        try i.put("var", .@"var");
        try i.put("while", .@"while");
        return Self{
            .line = 1,
            .start = 0,
            .current = 0,
            .source = source,
            .identifiers = i,
        };
    }

    fn isAtEnd(self: Self) bool {
        return self.source.len == self.current;
    }

    fn peek(self: Self) u8 {
        return self.source[self.current];
    }

    fn peekNext(self: Self) ?u8 {
        if (self.source.len >= self.current + 1) {
            return null;
        }
        return self.source[self.current + 1];
    }

    fn makeLexeme(self: Self) []const u8 {
        return self.source[self.start..self.current];
    }

    fn makeToken(self: Self, kind: TokenKind) Token {
        return Token{
            .kind = kind,
            .lexeme = self.makeLexeme(),
            .line = self.line,
        };
    }

    fn makeIdentifierToken(self: Self) Token {
        const lexeme = self.makeLexeme();
        return Token{
            .kind = self.identifiers.get(lexeme) orelse .identifier,
            .lexeme = lexeme,
            .line = self.line,
        };
    }

    fn next(self: *Self) void {
        self.current += 1;
    }

    fn nextLine(self: *Self) void {
        self.line += 1;
    }

    fn advance(self: *Self) u8 {
        defer self.next();
        return self.peek();
    }

    fn match(self: *Self, char: u8) bool {
        if (self.isAtEnd()) {
            return false;
        }
        if (self.peek() != char) {
            return false;
        }
        self.next();
        return true;
    }

    fn string(self: *Self) !Token {
        while (!self.isAtEnd() and self.peek() != '"') {
            if (self.peek() == '\n') {
                self.nextLine();
            }
            self.next();
        }
        if (self.isAtEnd()) {
            return Error.UnterminatedString;
        }
        self.next();
        return self.makeToken(.string);
    }

    fn number(self: *Self) Token {
        while (ascii.isDigit(self.peek())) {
            self.next();
        }
        if (self.peek() == '.' and ascii.isDigit(self.peekNext() orelse 'a')) {
            self.next();
            while (ascii.isDigit(self.peek())) {
                self.next();
            }
        }
        return self.makeToken(.number);
    }

    fn identifier(self: *Self) Token {
        while (true) {
            const char = self.peek();
            if (ascii.isAlphanumeric(char) or char == '_') {
                self.next();
            } else {
                break;
            }
        }
        return self.makeIdentifierToken();
    }

    fn skipWhitespace(self: *Self) void {
        while (!self.isAtEnd()) {
            switch (self.peek()) {
                ' ', '\r', '\t' => {
                    self.next();
                },
                '\n' => {
                    self.next();
                    self.nextLine();
                },
                '/' => {
                    if (self.peekNext()) |char| {
                        if (char != '/') {
                            return;
                        }
                        while (!self.isAtEnd()) {
                            if (self.peek() == '\n') {
                                break;
                            } else {
                                self.next();
                            }
                        }
                    } else {
                        return;
                    }
                },
                else => {
                    return;
                },
            }
        }
    }

    pub fn scan(self: *Self) !Token {
        self.skipWhitespace();
        self.start = self.current;
        if (self.isAtEnd()) {
            return self.makeToken(.eof);
        }
        const char = self.advance();
        if (ascii.isAlphabetic(char) or char == '_') {
            return self.identifier();
        }
        if (ascii.isDigit(char)) {
            return self.number();
        }
        if (char == '"') {
            return self.string();
        }
        const kind: ?TokenKind = switch (char) {
            '(' => .left_paren,
            ')' => .right_paren,
            '{' => .left_brace,
            '}' => .right_brace,
            ';' => .semicolon,
            ',' => .comma,
            '.' => .dot,
            '-' => .minus,
            '+' => .plus,
            '/' => .slash,
            '*' => .star,
            '!' => x: {
                if (self.match('=')) {
                    break :x .bang_equal;
                } else {
                    break :x .bang;
                }
            },
            '=' => x: {
                if (self.match('=')) {
                    break :x .equal_equal;
                } else {
                    break :x .bang;
                }
            },
            '<' => x: {
                if (self.match('=')) {
                    break :x .less_equal;
                } else {
                    break :x .less;
                }
            },
            '>' => x: {
                if (self.match('=')) {
                    break :x .greater_equal;
                } else {
                    break :x .grater;
                }
            },
            else => null,
        };
        if (kind) |k| {
            return self.makeToken(k);
        }
        return Error.UnexpectedCharacter;
    }

    pub fn deinit(self: *Self) void {
        self.identifiers.deinit();
    }
};
