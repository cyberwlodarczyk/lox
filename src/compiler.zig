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
const Operation = @import("chunk.zig").Operation;
const Chunk = @import("chunk.zig").Chunk;
const RawChunk = @import("chunk.zig").RawChunk;
const Function = @import("value.zig").Function;
const Value = @import("value.zig").Value;

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

        fn int(self: Precedence) u8 {
            return @intFromEnum(self);
        }

        fn lessEqual(self: Precedence, other: Precedence) bool {
            return self.int() <= other.int();
        }

        fn increment(self: Precedence) Precedence {
            return @enumFromInt(self.int() + 1);
        }
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
        is_captured: bool,
    };
    const Locals = ArrayList(Local);
    const Upvalue = struct {
        is_local: bool,
        index: u8,
    };
    const Upvalues = ArrayList(Upvalue);
    const Node = struct {
        parent: ?*Node,
        chunk: Chunk,
        locals: Locals,
        upvalues: Upvalues,

        fn init(allocator: Allocator, parent: ?*Node, start_line: u16) !*Node {
            const nodes = try allocator.alloc(Node, 1);
            const self = &nodes[0];
            self.parent = parent;
            self.chunk = try Chunk.init(allocator, start_line);
            self.locals = Locals.init(allocator);
            self.upvalues = Upvalues.init(allocator);
            _ = try self.locals.addOne();
            return self;
        }
    };
    pub const Error = error{
        ExpectedGroupingRightParen,
        ExpectedExpression,
        ExpectedExpressionSemicolon,
        ExpectedVarDeclarationSemicolon,
        ExpectedVarName,
        ExpectedBlockLeftBrace,
        ExpectedBlockRightBrace,
        ExpectedIfLeftParen,
        ExpectedIfRightParen,
        ExpectedWhileLeftParen,
        ExpectedWhileRightParen,
        ExpectedFunLeftParen,
        ExpectedFunRightParen,
        ExpectedForLeftParen,
        ExpectedForRightParen,
        ExpectedCallRightParen,
        ExpectedSemicolon,
        ExpectedFunName,
        ExpectedParamName,
        ExpectedReturnSemicolon,
        InvalidAssignmentTarget,
        LocalAlreadyExists,
        LocalOwnInitializer,
        TopLevelReturn,
        TooBigJump,
        TooBigLoop,
        TooManyConstants,
        TooManyLocals,
        TooManyParams,
        TooManyArgs,
        TooManyUpvalues,
    };

    rules: ParseRules,
    allocator: Allocator,
    config: Config,
    scanner: Scanner,
    current: Token,
    previous: Token,
    can_assign: bool,
    scope_depth: u8,
    node: *Node,

    pub fn init(allocator: Allocator, config: Config, source: []const u8) !Self {
        return Self{
            .rules = ParseRules.initDefault(.{}, .{
                .left_paren = .{
                    .prefix = grouping,
                    .infix = call,
                    .precedence = .call,
                },
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
            .allocator = allocator,
            .config = config,
            .scanner = Scanner.init(source),
            .current = undefined,
            .previous = undefined,
            .can_assign = false,
            .scope_depth = 0,
            .node = try Node.init(allocator, null, 1),
        };
    }

    fn copy(self: *Self, buf: []const u8) ![]const u8 {
        return self.allocator.dupe(u8, buf);
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
        const constants = &self.node.chunk.constants;
        if (constants.items.len == maxInt(u8) + 1) {
            return Error.TooManyConstants;
        }
        try constants.append(value);
        return @intCast(constants.items.len - 1);
    }

    fn makeGlobal(self: *Self) !u8 {
        return self.makeConstant(.{ .string = try self.copy(self.previous.lexeme) });
    }

    fn emit(self: *Self, byte: u8) !void {
        try self.node.chunk.write(byte, self.previous.line);
    }

    fn emitOperation(self: *Self, operation: Operation) !void {
        try self.emit(@intFromEnum(operation));
    }

    fn emitPair(self: *Self, operation: Operation, byte: u8) !void {
        try self.emitOperation(operation);
        try self.emit(byte);
    }

    fn emitConstant(self: *Self, value: Value) !void {
        const i = try self.makeConstant(value);
        try self.emitPair(.constant, i);
    }

    fn emitJump(self: *Self, operation: Operation) !usize {
        try self.emitOperation(operation);
        try self.emit(0xff);
        try self.emit(0xff);
        return self.node.chunk.code.items.len - 2;
    }

    fn emitLoop(self: *Self, start: usize) !void {
        try self.emitOperation(.loop);
        const offset = self.node.chunk.code.items.len - start + 2;
        if (offset > maxInt(u16)) {
            return Error.TooBigLoop;
        }
        try self.emit(@intCast((offset >> 8) & 0xff));
        try self.emit(@intCast(offset & 0xff));
    }

    fn emitReturn(self: *Self) !void {
        try self.emitOperation(.nil);
        try self.emitOperation(.@"return");
    }

    fn patchJump(self: *Self, offset: usize) !void {
        const code = &self.node.chunk.code;
        const jump = code.items.len - offset - 2;
        if (jump > maxInt(u16)) {
            return Error.TooBigJump;
        }
        code.items[offset] = @intCast((jump >> 8) & 0xff);
        code.items[offset + 1] = @intCast(jump & 0xff);
    }

    fn beginScope(self: *Self) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Self) !void {
        self.scope_depth -= 1;
        const locals = &self.node.locals;
        const len = locals.items.len;
        if (len == 0) {
            return;
        }
        var i = len - 1;
        while (true) : (i -= 1) {
            if (locals.items[i].depth.? <= self.scope_depth) {
                break;
            }
            if (locals.items[i].is_captured) {
                try self.emitOperation(.close_upvalue);
            } else {
                try self.emitOperation(.pop);
            }
            _ = locals.orderedRemove(i);
            if (i == 0) {
                break;
            }
        }
    }

    fn verifyLocal(self: *Self) !void {
        const name = self.previous;
        const locals = self.node.locals.items;
        const len = locals.len;
        if (len == 0) {
            return;
        }
        if (len == maxInt(u8) + 1) {
            return Error.TooManyLocals;
        }
        var i = len - 1;
        while (true) : (i -= 1) {
            if (locals[i].depth.? < self.scope_depth) {
                break;
            }
            if (std.mem.eql(u8, locals[i].name.lexeme, name.lexeme)) {
                return Error.LocalAlreadyExists;
            }
            if (i == 0) {
                break;
            }
        }
    }

    fn addLocal(self: *Self, depth: ?u32) !void {
        try self.node.locals.append(.{ .name = self.previous, .depth = depth, .is_captured = false });
    }

    fn markLocal(self: *Self) void {
        const locals = self.node.locals.items;
        locals[locals.len - 1].depth = self.scope_depth;
    }

    fn resolveLocal(self: *Self, node: ?*Node) !?u8 {
        const n = node orelse return null;
        const name = self.previous;
        const locals = n.locals.items;
        const len = locals.len;
        if (len == 0) {
            return null;
        }
        var i = len - 1;
        while (true) : (i -= 1) {
            if (std.mem.eql(u8, name.lexeme, locals[i].name.lexeme)) {
                if (locals[i].depth) |_| {
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

    fn addUpvalue(node: ?*Node, index: u8, is_local: bool) !?u8 {
        const n = node orelse return null;
        for (n.upvalues.items, 0..) |u, i| {
            if (u.is_local == is_local and u.index == index) {
                return @intCast(i);
            }
        }
        if (n.upvalues.items.len == maxInt(u8)) {
            return Error.TooManyUpvalues;
        }
        try n.upvalues.append(.{ .is_local = is_local, .index = index });
        return @intCast(n.upvalues.items.len - 1);
    }

    fn resolveUpvalue(self: *Self, node: ?*Node) !?u8 {
        const n = node orelse return null;
        if (try self.resolveLocal(n.parent)) |i| {
            n.parent.?.locals.items[i].is_captured = true;
            return addUpvalue(n, i, true);
        }
        if (try self.resolveUpvalue(n.parent)) |i| {
            return addUpvalue(n, i, false);
        }
        return null;
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

    fn parsePrecedence(self: *Self, precedence: Precedence) !void {
        try self.advance();
        const rule = self.rules.get(self.previous.kind);
        const prefix = rule.prefix orelse return Error.ExpectedExpression;
        self.can_assign = precedence.lessEqual(.assignment);
        try prefix(self);
        while (precedence.lessEqual(self.rules.get(self.current.kind).precedence)) {
            try self.advance();
            const infix = self.rules.get(self.previous.kind).infix.?;
            try infix(self);
        }
        if (self.can_assign and try self.match(.equal)) {
            return Error.InvalidAssignmentTarget;
        }
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
        try self.consume(.semicolon, Error.ExpectedExpressionSemicolon);
        try self.emitOperation(.pop);
    }

    fn printStatement(self: *Self) !void {
        try self.expression();
        try self.consume(.semicolon, Error.ExpectedExpressionSemicolon);
        try self.emitOperation(.print);
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

    fn returnStatement(self: *Self) !void {
        if (self.node.parent == null) {
            return Error.TopLevelReturn;
        }
        if (try self.match(.semicolon)) {
            try self.emitReturn();
        } else {
            try self.expression();
            try self.consume(.semicolon, Error.ExpectedReturnSemicolon);
            try self.emitOperation(.@"return");
        }
    }

    fn whileStatement(self: *Self) !void {
        const loop_start = self.node.chunk.code.items.len;
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
        try self.consume(.left_paren, Error.ExpectedForLeftParen);
        if (!try self.match(.semicolon)) {
            if (try self.match(.@"var")) {
                try self.varDeclaration();
            } else {
                try self.expressionStatement();
            }
        }
        var loop_start = self.node.chunk.code.items.len;
        var exit_jump: ?usize = null;
        if (!try self.match(.semicolon)) {
            try self.expression();
            try self.consume(.semicolon, Error.ExpectedSemicolon);
            exit_jump = try self.emitJump(.jump_if_false);
            try self.emitOperation(.pop);
        }
        if (!try self.match(.right_paren)) {
            const body_jump = try self.emitJump(.jump);
            const increment_start = self.node.chunk.code.items.len;
            try self.expression();
            try self.emitOperation(.pop);
            try self.consume(.right_paren, Error.ExpectedForRightParen);
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
        } else if (try self.match(.@"return")) {
            try self.returnStatement();
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
        try self.consume(.semicolon, Error.ExpectedVarDeclarationSemicolon);
    }

    fn varLocal(self: *Self) !void {
        try self.verifyLocal();
        try self.addLocal(null);
        try self.varInitializer();
        self.markLocal();
    }

    fn varGlobal(self: *Self) !void {
        const i = try self.makeGlobal();
        try self.varInitializer();
        try self.emitPair(.define_global, i);
    }

    fn varDeclaration(self: *Self) !void {
        try self.consume(.identifier, Error.ExpectedVarName);
        if (self.scope_depth == 0) {
            try self.varGlobal();
        } else {
            try self.varLocal();
        }
    }

    fn function(self: *Self) !void {
        self.beginScope();
        self.node = try Node.init(self.allocator, self.node, self.previous.line);
        var arity: u8 = 0;
        const name = try self.copy(self.previous.lexeme);
        try self.consume(.left_paren, Error.ExpectedFunLeftParen);
        if (!self.check(.right_paren)) {
            while (true) {
                if (arity == maxInt(u8)) {
                    return Error.TooManyParams;
                }
                arity += 1;
                try self.consume(.identifier, Error.ExpectedParamName);
                try self.addLocal(self.scope_depth);
                if (!try self.match(.comma)) {
                    break;
                }
            }
        }
        try self.consume(.right_paren, Error.ExpectedFunRightParen);
        try self.consume(.left_brace, Error.ExpectedBlockLeftBrace);
        try self.block();
        try self.emitReturn();
        const node = self.node;
        self.node = node.parent.?;
        self.scope_depth = 0;
        var raw_chunk = RawChunk.init(node.chunk, self.config.debug.writer);
        const upvalue_count = node.upvalues.items.len;
        try self.emitPair(.closure, try self.makeConstant(.{ .function = .{
            .arity = arity,
            .name = name,
            .upvalue_count = @intCast(upvalue_count),
            .chunk = raw_chunk,
        } }));
        for (node.upvalues.items) |u| {
            try self.emit(if (u.is_local) 1 else 0);
            try self.emit(u.index);
        }
        if (self.config.debug.print_code) {
            try raw_chunk.debugAll(name);
        }
    }

    fn funLocal(self: *Self) !void {
        try self.addLocal(self.scope_depth);
        try self.function();
    }

    fn funGlobal(self: *Self) !void {
        const i = try self.makeGlobal();
        try self.function();
        try self.emitPair(.define_global, i);
    }

    fn funDeclaration(self: *Self) !void {
        try self.consume(.identifier, Error.ExpectedFunName);
        if (self.scope_depth == 0) {
            try self.funGlobal();
        } else {
            try self.funLocal();
        }
    }

    fn declaration(self: *Self) anyerror!void {
        if (try self.match(.@"var")) {
            try self.varDeclaration();
        } else if (try self.match(.fun)) {
            try self.funDeclaration();
        } else {
            try self.statement();
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

    fn number(self: *Self) !void {
        const value = try std.fmt.parseFloat(f64, self.previous.lexeme);
        try self.emitConstant(.{ .number = value });
    }

    fn string(self: *Self) !void {
        try self.emitConstant(.{
            .string = try self.copy(
                self.previous.lexeme[1 .. self.previous.lexeme.len - 1],
            ),
        });
    }

    fn variable(self: *Self) !void {
        var i: u8 = undefined;
        var get: Operation = undefined;
        var set: Operation = undefined;
        if (try self.resolveLocal(self.node)) |j| {
            i = j;
            get = .get_local;
            set = .set_local;
        } else if (try self.resolveUpvalue(self.node)) |j| {
            i = j;
            get = .get_upvalue;
            set = .set_upvalue;
        } else {
            i = try self.makeGlobal();
            get = .get_global;
            set = .set_global;
        }
        if (self.can_assign and try self.match(.equal)) {
            try self.expression();
            try self.emitPair(set, i);
        } else {
            try self.emitPair(get, i);
        }
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
        try self.parsePrecedence(rule.precedence.increment());
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

    fn grouping(self: *Self) !void {
        try self.expression();
        try self.consume(.right_paren, Error.ExpectedGroupingRightParen);
    }

    fn arguments(self: *Self) !u8 {
        var count: u8 = 0;
        if (!self.check(.right_paren)) {
            while (true) {
                if (count == maxInt(u8)) {
                    return Error.TooManyArgs;
                }
                count += 1;
                try self.expression();
                if (!try self.match(.comma)) {
                    break;
                }
            }
        }
        return count;
    }

    fn call(self: *Self) !void {
        const arg_count = try self.arguments();
        try self.consume(.right_paren, Error.ExpectedCallRightParen);
        try self.emitPair(.call, arg_count);
    }

    pub fn run(self: *Self) !Function {
        try self.advance();
        while (!try self.match(.eof)) {
            try self.declaration();
        }
        try self.emitReturn();
        var raw_chunk = RawChunk.init(self.node.chunk, self.config.debug.writer);
        const script = Function{
            .arity = 0,
            .name = null,
            .chunk = raw_chunk,
            .upvalue_count = 0,
        };
        if (self.config.debug.print_code) {
            try raw_chunk.debugAll("__script__");
        }
        return script;
    }
};
