const std = @import("std");
const ascii = std.ascii;
const StaticStringMap = std.static_string_map.StaticStringMap;

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
    greater,
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
    line: u16,
};

pub const Scanner = struct {
    const Self = @This();
    pub const Error = error{
        UnexpectedCharacter,
        UnterminatedString,
    };
    const keywords = StaticStringMap(TokenKind).initComptime(.{
        .{ "and", .@"and" },
        .{ "class", .class },
        .{ "else", .@"else" },
        .{ "false", .false },
        .{ "for", .@"for" },
        .{ "fun", .fun },
        .{ "if", .@"if" },
        .{ "nil", .nil },
        .{ "or", .@"or" },
        .{ "print", .print },
        .{ "return", .@"return" },
        .{ "super", .super },
        .{ "this", .this },
        .{ "true", .true },
        .{ "var", .@"var" },
        .{ "while", .@"while" },
    });

    line: u16,
    start: usize,
    current: usize,
    source: []const u8,

    pub fn init(source: []const u8) Self {
        return Self{
            .line = 1,
            .start = 0,
            .current = 0,
            .source = source,
        };
    }

    fn isDigit(char: ?u8) bool {
        if (char) |c| {
            return ascii.isDigit(c);
        } else {
            return false;
        }
    }

    fn isIdentifier(char: ?u8) bool {
        if (char) |c| {
            return ascii.isAlphabetic(c) or c == '_';
        } else {
            return false;
        }
    }

    fn peekAt(self: Self, delta: usize) ?u8 {
        const i = self.current + delta;
        if (i == self.source.len) {
            return null;
        }
        return self.source[i];
    }

    fn peek(self: Self) ?u8 {
        return self.peekAt(0);
    }

    fn peekNext(self: Self) ?u8 {
        return self.peekAt(1);
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
            .kind = keywords.get(lexeme) orelse .identifier,
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

    fn match(self: *Self, char: u8) bool {
        if (self.peek() != char) {
            return false;
        }
        self.next();
        return true;
    }

    fn string(self: *Self) !Token {
        while (true) {
            const char = self.peek();
            if (char == null) {
                return Error.UnterminatedString;
            }
            if (char == '"') {
                break;
            }
            if (char == '\n') {
                self.nextLine();
            }
            self.next();
        }
        self.next();
        return self.makeToken(.string);
    }

    fn number(self: *Self) Token {
        while (isDigit(self.peek())) {
            self.next();
        }
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            self.next();
            while (isDigit(self.peek())) {
                self.next();
            }
        }
        return self.makeToken(.number);
    }

    fn identifier(self: *Self) Token {
        while (isIdentifier(self.peek())) {
            self.next();
        }
        return self.makeIdentifierToken();
    }

    fn skipWhitespace(self: *Self) void {
        while (true) {
            switch (self.peek() orelse return) {
                ' ', '\r', '\t' => {
                    self.next();
                },
                '\n' => {
                    self.next();
                    self.nextLine();
                },
                '/' => {
                    if (self.peekNext()) |c1| {
                        if (c1 != '/') {
                            return;
                        }
                        while (true) {
                            const c0 = self.peek();
                            if (c0 == null or c0 == '\n') {
                                break;
                            }
                            self.next();
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

    pub fn token(self: *Self) !Token {
        self.skipWhitespace();
        self.start = self.current;
        const char = self.peek() orelse return self.makeToken(.eof);
        self.next();
        if (isIdentifier(char)) {
            return self.identifier();
        }
        if (isDigit(char)) {
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
                    break :x .equal;
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
                    break :x .greater;
                }
            },
            else => null,
        };
        if (kind) |k| {
            return self.makeToken(k);
        }
        return Error.UnexpectedCharacter;
    }
};
