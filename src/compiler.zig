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
    add,
    subtract,
    multiply,
    divide,
    negate,
    @"return",
};

pub const Value = f64;

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
            .add => simpleInstruction("add", offset),
            .subtract => simpleInstruction("subtract", offset),
            .multiply => simpleInstruction("multiply", offset),
            .divide => simpleInstruction("divide", offset),
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
        print(
            "{s: <16} {d:>4} '{d}'\n",
            .{ name, constant, value },
        );
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

    pub fn init(source: []const u8, chunk: *Chunk) Self {
        return Self{
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
        try self.emitConstant(value);
    }

    fn unary(self: *Self) !void {
        const kind = self.previous.kind;
        try self.parsePrecedence(.unary);
        switch (kind) {
            .minus => {
                try self.emitOperation(.negate);
            },
            else => {},
        }
    }

    fn binary(self: *Self) !void {
        const kind = self.previous.kind;
        const rule = self.rules.get(kind);
        try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));
        const operation: ?Operation = switch (kind) {
            .plus => .add,
            .minus => .subtract,
            .star => .multiply,
            .slash => .divide,
            else => null,
        };
        try self.emitOperation(operation.?);
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
