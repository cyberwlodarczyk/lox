const std = @import("std");
const print = std.debug.print;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const EnumArray = std.enums.EnumArray;
const Token = @import("scanner.zig").Token;
const TokenKind = @import("scanner.zig").TokenKind;
const Scanner = @import("scanner.zig").Scanner;

pub const Operation = enum(u8) {
    constant,
    nil,
    true,
    false,
    equal,
    greater,
    less,
    add,
    subtract,
    multiply,
    divide,
    not,
    negate,
    @"return",
};

pub const Value = union(enum) {
    const Self = @This();
    const Tag = std.meta.Tag(Self);

    bool: bool,
    nil,
    number: f64,
    string: []const u8,

    pub fn debug(self: Self) void {
        switch (self) {
            .bool => |b| {
                if (b) {
                    print("true", .{});
                } else {
                    print("false", .{});
                }
            },
            .nil => {
                print("nil", .{});
            },
            .number => |n| {
                print("{d}", .{n});
            },
            .string => |s| {
                print("{s}", .{s});
            },
        }
    }

    pub fn is(self: Self, tag: Tag) bool {
        return @as(Tag, self) == tag;
    }

    pub fn eql(self: Self, other: Value) bool {
        if (@as(Tag, self) != @as(Tag, other)) {
            return false;
        }
        return switch (self) {
            .bool => |x| x == other.bool,
            .nil => true,
            .number => |x| x == other.number,
            .string => |s| std.mem.eql(u8, s, other.string),
        };
    }

    pub fn isFalsy(self: Self) bool {
        return switch (self) {
            .bool => |x| !x,
            .nil => true,
            else => false,
        };
    }
};

pub const Chunk = struct {
    const Self = @This();
    const Bytecode = ArrayList(u8);
    const Constants = ArrayList(Value);
    const Lines = ArrayList(u32);

    code: Bytecode,
    constants: Constants,
    lines: Lines,

    pub fn init(allocator: Allocator) Self {
        return Chunk{
            .code = Bytecode.init(allocator),
            .constants = Constants.init(allocator),
            .lines = Lines.init(allocator),
        };
    }

    fn write(self: *Self, byte: u8, line: u32) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn disassemble(self: Chunk, name: []const u8) void {
        print("== {s} ==\n", .{name});
        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = self.disassembleInstruction(offset);
        }
    }

    pub fn disassembleInstruction(self: Chunk, offset: usize) usize {
        print("{d:0>4} ", .{offset});
        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            print("   | ", .{});
        } else {
            print("{d: >4} ", .{self.lines.items[offset]});
        }
        const operation: Operation = @enumFromInt(self.code.items[offset]);
        return switch (operation) {
            .constant => self.constantInstruction("constant", offset),
            .nil => simpleInstruction("nil", offset),
            .true => simpleInstruction("true", offset),
            .false => simpleInstruction("false", offset),
            .equal => simpleInstruction("equal", offset),
            .greater => simpleInstruction("greater", offset),
            .less => simpleInstruction("less", offset),
            .add => simpleInstruction("add", offset),
            .subtract => simpleInstruction("subtract", offset),
            .multiply => simpleInstruction("multiply", offset),
            .divide => simpleInstruction("divide", offset),
            .not => simpleInstruction("not", offset),
            .negate => simpleInstruction("negate", offset),
            .@"return" => simpleInstruction("return", offset),
        };
    }

    fn simpleInstruction(name: []const u8, offset: usize) usize {
        print("{s}\n", .{name});
        return offset + 1;
    }

    fn constantInstruction(self: Chunk, name: []const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];
        const value = self.constants.items[constant];
        print("{s: <16} {d:>4} '\n", .{ name, constant });
        value.debug();
        print("'\n", .{});
        return offset + 2;
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }
};

pub const Compiler = struct {
    const Self = @This();
    const Precedence = enum(u8) {
        none,
        assignment,
        @"or",
        @"and",
        equality,
        comparison,
        term,
        factor,
        unary,
        call,
        primary,
    };
    const ParseFn = *const fn (self: *Self) anyerror!void;
    const ParseRule = struct {
        prefix: ?ParseFn = null,
        infix: ?ParseFn = null,
        precedence: Precedence = .none,
    };
    const ParseRules = EnumArray(TokenKind, ParseRule);
    pub const Error = error{
        MissingExpressionRightParen,
        MissingExpression,
        MissingExpressionEnd,
        TooManyConstants,
    };

    scanner: Scanner,
    current: Token,
    previous: Token,
    chunk: *Chunk,
    rules: ParseRules,
    allocator: Allocator,

    pub fn init(allocator: Allocator, source: []const u8, chunk: *Chunk) Self {
        return Self{
            .allocator = allocator,
            .scanner = Scanner.init(source),
            .current = undefined,
            .previous = undefined,
            .chunk = chunk,
            .rules = ParseRules.initDefault(.{}, .{
                .left_paren = .{ .prefix = grouping },
                .minus = .{ .prefix = unary, .infix = binary, .precedence = .term },
                .plus = .{ .infix = binary, .precedence = .term },
                .slash = .{ .infix = binary, .precedence = .factor },
                .star = .{ .infix = binary, .precedence = .factor },
                .number = .{ .prefix = number },
                .nil = .{ .prefix = literal },
                .true = .{ .prefix = literal },
                .false = .{ .prefix = literal },
                .bang = .{ .prefix = unary },
                .bang_equal = .{ .infix = binary, .precedence = .equality },
                .equal_equal = .{ .infix = binary, .precedence = .equality },
                .greater_equal = .{ .infix = binary, .precedence = .comparison },
                .greater = .{ .infix = binary, .precedence = .comparison },
                .less_equal = .{ .infix = binary, .precedence = .comparison },
                .less = .{ .infix = binary, .precedence = .comparison },
                .string = .{ .prefix = string },
            }),
        };
    }

    fn advance(self: *Self) !void {
        self.previous = self.current;
        self.current = try self.scanner.token();
    }

    fn consume(self: *Self, kind: TokenKind, @"error": Error) !void {
        if (self.current.kind == kind) {
            try self.advance();
        } else {
            return @"error";
        }
    }

    fn emit(self: *Self, byte: u8) !void {
        try self.chunk.write(byte, self.previous.line);
    }

    fn emitOperation(self: *Self, operation: Operation) !void {
        try self.emit(@intFromEnum(operation));
    }

    fn emitConstant(self: *Self, value: Value) !void {
        if (self.chunk.constants.items.len == std.math.maxInt(u8) + 1) {
            return Error.TooManyConstants;
        }
        try self.chunk.constants.append(value);
        try self.emitOperation(.constant);
        try self.emit(@intCast(self.chunk.constants.items.len - 1));
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) !void {
        try self.advance();
        const rule = self.rules.get(self.previous.kind);
        const prefix = rule.prefix orelse return Error.MissingExpression;
        try prefix(self);
        while (@intFromEnum(precedence) <= @intFromEnum(self.rules.get(self.current.kind).precedence)) {
            try self.advance();
            const infix = self.rules.get(self.previous.kind).infix.?;
            try infix(self);
        }
    }

    fn expression(self: *Self) !void {
        try self.parsePrecedence(.assignment);
    }

    fn number(self: *Self) !void {
        const value = try std.fmt.parseFloat(f64, self.previous.lexeme);
        try self.emitConstant(.{ .number = value });
    }

    fn string(self: *Self) !void {
        try self.emitConstant(.{
            .string = try self.allocator.dupe(
                u8,
                self.previous.lexeme[1 .. self.previous.lexeme.len - 1],
            ),
        });
    }

    fn unary(self: *Self) !void {
        const kind = self.previous.kind;
        try self.parsePrecedence(.unary);
        try self.emitOperation(switch (kind) {
            .bang => .not,
            .minus => .negate,
            else => unreachable,
        });
    }

    fn binary(self: *Self) !void {
        const kind = self.previous.kind;
        const rule = self.rules.get(kind);
        try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));
        try self.emitOperation(switch (kind) {
            .plus => .add,
            .minus => .subtract,
            .star => .multiply,
            .slash => .divide,
            .bang_equal => .equal,
            .equal_equal => .equal,
            .greater => .greater,
            .greater_equal => .less,
            .less => .less,
            .less_equal => .greater,
            else => unreachable,
        });
        switch (kind) {
            .bang_equal, .greater_equal, .less_equal => {
                try self.emitOperation(.not);
            },
            else => {},
        }
    }

    fn literal(self: *Self) !void {
        try self.emitOperation(switch (self.previous.kind) {
            .false => .false,
            .nil => .nil,
            .true => .true,
            else => unreachable,
        });
    }

    fn grouping(self: *Self) !void {
        try self.expression();
        try self.consume(.right_paren, Error.MissingExpressionRightParen);
    }

    pub fn run(self: *Self) !void {
        try self.advance();
        try self.expression();
        try self.consume(.eof, Error.MissingExpressionEnd);
        try self.emitOperation(.@"return");
    }
};
