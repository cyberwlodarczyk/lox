const std = @import("std");
const print = std.debug.print;
const maxInt = std.math.maxInt;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const EnumArray = std.enums.EnumArray;
const Config = @import("lox.zig").Config;
const Token = @import("scanner.zig").Token;
const TokenKind = @import("scanner.zig").TokenKind;
const Scanner = @import("scanner.zig").Scanner;

pub const Operation = enum(u8) {
    constant,
    nil,
    true,
    false,
    pop,
    get_local,
    set_local,
    get_global,
    define_global,
    set_global,
    equal,
    greater,
    less,
    add,
    subtract,
    multiply,
    divide,
    not,
    negate,
    print,
    jump,
    jump_if_false,
    loop,
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

    pub fn debugAt(self: Chunk, offset: usize) usize {
        print("{d:0>4} ", .{offset});
        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            print("   | ", .{});
        } else {
            print("{d: >4} ", .{self.lines.items[offset]});
        }
        const operation: Operation = @enumFromInt(self.code.items[offset]);
        const name = @tagName(operation);
        return switch (operation) {
            .constant, .get_global, .define_global, .set_global => o: {
                const constant = self.code.items[offset + 1];
                const value = self.constants.items[constant];
                print("{s: <16} {d:>4} '", .{ name, constant });
                value.debug();
                print("'\n", .{});
                break :o offset + 2;
            },
            .get_local, .set_local => o: {
                print("{s: <16} {d:>4}\n", .{ name, self.code.items[offset + 1] });
                break :o offset + 2;
            },
            .jump, .jump_if_false, .loop => o: {
                var jump = @as(u16, @intCast(self.code.items[offset + 1])) << 8;
                jump |= @as(u16, @intCast(self.code.items[offset + 2]));
                var dest = offset + 3;
                if (operation == .loop) {
                    dest -= jump;
                } else {
                    dest += jump;
                }
                print("{s: <16} {d:>4} -> {d}\n", .{ name, offset, dest });
                break :o offset + 3;
            },
            else => o: {
                print("{s}\n", .{name});
                break :o offset + 1;
            },
        };
    }

    pub fn debug(self: Chunk, name: []const u8) void {
        print("== {s} ==\n", .{name});
        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = self.debugAt(offset);
        }
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
    const Local = struct {
        name: Token,
        depth: ?u32,
    };
    const Locals = ArrayList(Local);
    pub const Error = error{
        MissingExpressionRightParen,
        MissingExpression,
        MissingExpressionEnd,
        MissingExpressionSemicolon,
        MissingVarDeclarationSemicolon,
        MissingVarName,
        TooManyConstants,
        TooManyLocals,
        InvalidAssignmentTarget,
        ExpectedBlockRightBrace,
        LocalAlreadyExists,
        LocalOwnInitializer,
        ExpectedIfLeftParen,
        ExpectedIfRightParen,
        ExpectedWhileLeftParen,
        ExpectedWhileRightParen,
        ExpectedLeftParen,
        ExpectedRightParen,
        ExpectedSemicolon,
        TooBigJump,
        TooBigLoop,
    };

    allocator: Allocator,
    config: Config,
    scanner: Scanner,
    current: Token,
    previous: Token,
    chunk: *Chunk,
    rules: ParseRules,
    can_assign: bool,
    scope_depth: u32,
    locals: Locals,

    pub fn init(allocator: Allocator, config: Config, source: []const u8, chunk: *Chunk) Self {
        return Self{
            .allocator = allocator,
            .config = config,
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
                .identifier = .{ .prefix = variable },
                .@"and" = .{ .infix = @"and", .precedence = .@"and" },
                .@"or" = .{ .infix = @"or", .precedence = .@"or" },
            }),
            .can_assign = false,
            .scope_depth = 0,
            .locals = Locals.init(allocator),
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

    fn check(self: Self, kind: TokenKind) bool {
        return self.current.kind == kind;
    }

    fn match(self: *Self, kind: TokenKind) !bool {
        if (!self.check(kind)) {
            return false;
        }
        try self.advance();
        return true;
    }

    fn makeConstant(self: *Self, value: Value) !u8 {
        const constants = &self.chunk.constants;
        if (constants.items.len == maxInt(u8) + 1) {
            return Error.TooManyConstants;
        }
        try constants.append(value);
        return @intCast(constants.items.len - 1);
    }

    fn emit(self: *Self, byte: u8) !void {
        try self.chunk.write(byte, self.previous.line);
    }

    fn emitOperation(self: *Self, operation: Operation) !void {
        try self.emit(@intFromEnum(operation));
    }

    fn emitConstant(self: *Self, value: Value) !void {
        const i = try self.makeConstant(value);
        try self.emitOperation(.constant);
        try self.emit(i);
    }

    fn emitJump(self: *Self, operation: Operation) !usize {
        try self.emitOperation(operation);
        try self.emit(0xff);
        try self.emit(0xff);
        return self.chunk.code.items.len - 2;
    }

    fn emitLoop(self: *Self, start: usize) !void {
        try self.emitOperation(.loop);
        const offset = self.chunk.code.items.len - start + 2;
        if (offset > maxInt(u16)) {
            return Error.TooBigLoop;
        }
        try self.emit(@intCast((offset >> 8) & 0xff));
        try self.emit(@intCast(offset & 0xff));
    }

    fn patchJump(self: *Self, offset: usize) !void {
        const jump = self.chunk.code.items.len - offset - 2;
        if (jump > maxInt(u16)) {
            return Error.TooBigJump;
        }
        const code = &self.chunk.code;
        code.items[offset] = @intCast((jump >> 8) & 0xff);
        code.items[offset + 1] = @intCast(jump & 0xff);
    }

    fn beginScope(self: *Self) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Self) !void {
        self.scope_depth -= 1;
        const len = self.locals.items.len;
        if (len != 0) {
            var i = len - 1;
            while (i != 0) : (i -= 1) {
                if (self.locals.items[i].depth.? > self.scope_depth) {
                    _ = self.locals.orderedRemove(i);
                } else {
                    break;
                }
                try self.emitOperation(.pop);
                if (i == 0) {
                    break;
                }
            }
        }
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) !void {
        try self.advance();
        const rule = self.rules.get(self.previous.kind);
        const prefix = rule.prefix orelse return Error.MissingExpression;
        self.can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.assignment);
        try prefix(self);
        while (@intFromEnum(precedence) <= @intFromEnum(self.rules.get(self.current.kind).precedence)) {
            try self.advance();
            const infix = self.rules.get(self.previous.kind).infix.?;
            try infix(self);
        }
        if (self.can_assign and try self.match(.equal)) {
            return Error.InvalidAssignmentTarget;
        }
    }

    fn @"and"(self: *Self) !void {
        const end_jump = try self.emitJump(.jump_if_false);
        try self.emitOperation(.pop);
        try self.parsePrecedence(.@"and");
        try self.patchJump(end_jump);
    }

    fn @"or"(self: *Self) !void {
        const else_jump = try self.emitJump(.jump_if_false);
        const end_jump = try self.emitJump(.jump);
        try self.patchJump(else_jump);
        try self.emitOperation(.pop);
        try self.parsePrecedence(.@"or");
        try self.patchJump(end_jump);
    }

    fn identifierConstant(self: *Self, token: Token) !u8 {
        return self.makeConstant(.{ .string = try self.allocator.dupe(u8, token.lexeme) });
    }

    fn expression(self: *Self) !void {
        try self.parsePrecedence(.assignment);
    }

    fn block(self: *Self) !void {
        while (!self.check(.right_brace) and !self.check(.eof)) {
            try self.declaration();
        }
        try self.consume(.right_brace, Error.ExpectedBlockRightBrace);
    }

    fn expressionStatement(self: *Self) !void {
        try self.expression();
        try self.consume(.semicolon, Error.MissingExpressionSemicolon);
        try self.emitOperation(.pop);
    }

    fn ifStatement(self: *Self) !void {
        try self.consume(.left_paren, Error.ExpectedIfLeftParen);
        try self.expression();
        try self.consume(.right_paren, Error.ExpectedIfRightParen);
        const then_jump = try self.emitJump(.jump_if_false);
        try self.emitOperation(.pop);
        try self.statement();
        const else_jump = try self.emitJump(.jump);
        try self.patchJump(then_jump);
        try self.emitOperation(.pop);
        if (try self.match(.@"else")) {
            try self.statement();
        }
        try self.patchJump(else_jump);
    }

    fn printStatement(self: *Self) !void {
        try self.expression();
        try self.consume(.semicolon, Error.MissingExpressionSemicolon);
        try self.emitOperation(.print);
    }

    fn whileStatement(self: *Self) !void {
        const loop_start = self.chunk.code.items.len;
        try self.consume(.left_paren, Error.ExpectedWhileLeftParen);
        try self.expression();
        try self.consume(.right_paren, Error.ExpectedWhileRightParen);
        const exit_jump = try self.emitJump(.jump_if_false);
        try self.emitOperation(.pop);
        try self.statement();
        try self.emitLoop(loop_start);
        try self.patchJump(exit_jump);
        try self.emitOperation(.pop);
    }

    fn forStatement(self: *Self) !void {
        self.beginScope();
        try self.consume(.left_paren, Error.ExpectedLeftParen);
        if (!try self.match(.semicolon)) {
            if (try self.match(.@"var")) {
                try self.varDeclaration();
            } else {
                try self.expressionStatement();
            }
        }
        var loop_start = self.chunk.code.items.len;
        var exit_jump: ?usize = null;
        if (!try self.match(.semicolon)) {
            try self.expression();
            try self.consume(.semicolon, Error.ExpectedSemicolon);
            exit_jump = try self.emitJump(.jump_if_false);
            try self.emitOperation(.pop);
        }
        if (!try self.match(.right_paren)) {
            const body_jump = try self.emitJump(.jump);
            const increment_start = self.chunk.code.items.len;
            try self.expression();
            try self.emitOperation(.pop);
            try self.consume(.right_paren, Error.ExpectedRightParen);
            try self.emitLoop(loop_start);
            loop_start = increment_start;
            try self.patchJump(body_jump);
        }
        try self.statement();
        try self.emitLoop(loop_start);
        if (exit_jump) |offset| {
            try self.patchJump(offset);
            try self.emitOperation(.pop);
        }
        try self.endScope();
    }

    fn statement(self: *Self) anyerror!void {
        if (try self.match(.print)) {
            try self.printStatement();
        } else if (try self.match(.@"if")) {
            try self.ifStatement();
        } else if (try self.match(.@"while")) {
            try self.whileStatement();
        } else if (try self.match(.@"for")) {
            try self.forStatement();
        } else if (try self.match(.left_brace)) {
            self.beginScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }

    fn varInitializer(self: *Self) !void {
        if (try self.match(.equal)) {
            try self.expression();
        } else {
            try self.emitOperation(.nil);
        }
        try self.consume(.semicolon, Error.MissingVarDeclarationSemicolon);
    }

    fn varLocal(self: *Self) !void {
        const name = self.previous;
        const len = self.locals.items.len;
        if (len != 0) {
            var i = len - 1;
            while (true) : (i -= 1) {
                const local = self.locals.items[i];
                if (local.depth.? < self.scope_depth) {
                    break;
                }
                if (std.mem.eql(u8, local.name.lexeme, name.lexeme)) {
                    return Error.LocalAlreadyExists;
                }
                if (i == 0) {
                    break;
                }
            }
        }
        if (len == maxInt(u8) + 1) {
            return Error.TooManyLocals;
        }
        try self.locals.append(.{ .name = name, .depth = null });
        try self.varInitializer();
        self.locals.items[self.locals.items.len - 1].depth = self.scope_depth;
    }

    fn varGlobal(self: *Self) !void {
        const i = try self.identifierConstant(self.previous);
        try self.varInitializer();
        try self.emitOperation(.define_global);
        try self.emit(i);
    }

    fn varDeclaration(self: *Self) !void {
        try self.consume(.identifier, Error.MissingVarName);
        if (self.scope_depth == 0) {
            try self.varGlobal();
        } else {
            try self.varLocal();
        }
    }

    fn declaration(self: *Self) anyerror!void {
        if (try self.match(.@"var")) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }
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

    fn resolveLocal(self: *Self, name: Token) !?u8 {
        const len = self.locals.items.len;
        if (len == 0) {
            return null;
        }
        var i = len - 1;
        while (true) : (i -= 1) {
            const local = self.locals.items[i];
            if (std.mem.eql(u8, name.lexeme, local.name.lexeme)) {
                if (local.depth) |_| {
                    return @intCast(i);
                } else {
                    return Error.LocalOwnInitializer;
                }
            }
            if (i == 0) {
                break;
            }
        }
        return null;
    }

    fn namedVariable(self: *Self, name: Token) !void {
        const i = try self.resolveLocal(name);
        const get: Operation = if (i) |_| .get_local else .get_global;
        const set: Operation = if (i) |_| .set_local else .set_global;
        const j = i orelse try self.identifierConstant(name);
        if (self.can_assign and try self.match(.equal)) {
            try self.expression();
            try self.emitOperation(set);
        } else {
            try self.emitOperation(get);
        }
        try self.emit(j);
    }

    fn variable(self: *Self) !void {
        try self.namedVariable(self.previous);
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
        while (!try self.match(.eof)) {
            try self.declaration();
        }
        try self.emitOperation(.@"return");
        if (self.config.debug.print_code) {
            self.chunk.debug("code");
        }
    }
};
