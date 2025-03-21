const std = @import("std");
const Writer = std.fs.File.Writer;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Config = @import("lox.zig").Config;
const Operation = @import("chunk.zig").Operation;
const Value = @import("value.zig").Value;
const Compiler = @import("compiler.zig").Compiler;
const Function = @import("value.zig").Function;
const Native = @import("value.zig").Native;

pub const VM = struct {
    const Self = @This();
    const Stack = ArrayList(Value);
    const CallFrame = struct {
        function: Function,
        ptr: [*]const u8,
        slots: [*]Value,
    };
    const CallFrames = ArrayList(CallFrame);
    const Globals = std.StringHashMap(Value);
    pub const Error = error{
        ExpectedNumberOperand,
        ExpectedNumberOperands,
        ExpectedAddOperands,
        UndefinedVariable,
        NonCallable,
        BadArgCount,
    };

    allocator: Allocator,
    config: Config,
    writer: Writer,
    globals: Globals,
    stack: Stack,
    frames: CallFrames,
    current: *CallFrame,

    pub fn init(allocator: Allocator, config: Config) !Self {
        return Self{
            .allocator = allocator,
            .config = config,
            .writer = std.io.getStdOut().writer(),
            .globals = Globals.init(allocator),
            .stack = try Stack.initCapacity(allocator, 256),
            .frames = CallFrames.init(allocator),
            .current = undefined,
        };
    }

    fn defineNative(self: *Self, native: Native) !void {
        try self.globals.put(native.name, .{ .native = native });
    }

    fn nowNative(_: []const Value) Value {
        return .{ .number = @floatFromInt(std.time.timestamp()) };
    }

    fn peek(self: *Self, delta: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - delta];
    }

    fn push(self: *Self, value: Value) !void {
        try self.stack.append(value);
    }

    fn pop(self: *Self) Value {
        return self.stack.pop();
    }

    fn read(self: *Self) u8 {
        defer self.current.ptr += 1;
        return @as(*u8, @ptrCast(@constCast(self.current.ptr))).*;
    }

    fn readCast(self: *Self, comptime T: type) T {
        return @as(T, @intCast(self.read()));
    }

    fn readOperation(self: *Self) Operation {
        return @enumFromInt(self.read());
    }

    fn readConstant(self: *Self) Value {
        return self.current.function.chunk.constants[self.read()];
    }

    fn readOffset(self: *Self) u16 {
        return (self.readCast(u16) << 8) | self.readCast(u16);
    }

    fn add(a: Value, b: Value) Value {
        return .{ .number = a.number + b.number };
    }

    fn subtract(a: Value, b: Value) Value {
        return .{ .number = a.number - b.number };
    }

    fn multiply(a: Value, b: Value) Value {
        return .{ .number = a.number * b.number };
    }

    fn divide(a: Value, b: Value) Value {
        return .{ .number = a.number / b.number };
    }

    fn equal(a: Value, b: Value) Value {
        return .{ .bool = a.eql(b) };
    }

    fn greater(a: Value, b: Value) Value {
        return .{ .bool = a.number > b.number };
    }

    fn less(a: Value, b: Value) Value {
        return .{ .bool = a.number < b.number };
    }

    fn binary(self: *Self, f: *const fn (a: Value, b: Value) Value) !void {
        const b = self.pop();
        const a = self.pop();
        try self.push(f(a, b));
    }

    fn numeric(self: *Self, f: *const fn (a: Value, b: Value) Value) !void {
        if (!self.peek(0).is(.number) or !self.peek(1).is(.number)) {
            return Error.ExpectedNumberOperands;
        }
        return self.binary(f);
    }

    fn concat(self: *Self) !void {
        const b = self.pop();
        const a = self.pop();
        try self.push(.{ .string = try std.mem.join(
            self.allocator,
            "",
            &.{ a.string, b.string },
        ) });
    }

    fn callReturn(self: *Self, result: Value, arg_count: u8) !void {
        var i: u8 = 0;
        while (i <= arg_count) : (i += 1) {
            _ = self.pop();
        }
        try self.push(result);
    }

    fn callNative(self: *Self, native: Native, arg_count: u8) !void {
        if (arg_count != native.arity) {
            return Error.BadArgCount;
        }
        const stack = self.stack.items;
        const result = native.function(stack[stack.len - arg_count ..]);
        try self.callReturn(result, arg_count);
    }

    fn callFunction(self: *Self, function: Function, arg_count: u8) !void {
        if (arg_count != function.arity) {
            return Error.BadArgCount;
        }
        const stack = self.stack.items;
        const frame = try self.frames.addOne();
        frame.* = .{
            .function = function,
            .ptr = function.chunk.code.ptr,
            .slots = stack.ptr + stack.len - arg_count - 1,
        };
        self.current = frame;
    }

    fn call(self: *Self, callee: Value, arg_count: u8) !void {
        return switch (callee) {
            .native => |n| self.callNative(n, arg_count),
            .function => |f| self.callFunction(f, arg_count),
            else => Error.NonCallable,
        };
    }

    fn debug(self: *Self) !void {
        const frame = self.current;
        const chunk = frame.function.chunk;
        const writer = self.config.debug.writer;
        try writer.writeAll("        |");
        for (self.stack.items) |value| {
            try writer.writeAll(" [");
            try value.print(writer);
            try writer.writeAll("]");
        }
        try writer.writeAll("\n");
        const offset = @intFromPtr(frame.ptr) - @intFromPtr(chunk.code.ptr);
        _ = try chunk.debugAt(writer, offset);
    }

    pub fn run(self: *Self, script: Function) !void {
        try self.defineNative(.{ .arity = 0, .name = "now", .function = nowNative });
        try self.push(Value{ .function = script });
        try self.callFunction(script, 0);
        while (true) {
            if (self.config.debug.trace_execution) {
                try self.debug();
            }
            switch (self.readOperation()) {
                .constant => {
                    try self.push(self.readConstant());
                },
                .nil => {
                    try self.push(.nil);
                },
                .true => {
                    try self.push(.{ .bool = true });
                },
                .false => {
                    try self.push(.{ .bool = false });
                },
                .pop => {
                    _ = self.pop();
                },
                .get_local => {
                    try self.push(self.current.slots[self.read()]);
                },
                .set_local => {
                    self.current.slots[self.read()] = self.peek(0);
                },
                .get_global => {
                    if (self.globals.get(self.readConstant().string)) |value| {
                        try self.push(value);
                    } else {
                        return Error.UndefinedVariable;
                    }
                },
                .define_global => {
                    try self.globals.put(
                        self.readConstant().string,
                        self.pop(),
                    );
                },
                .set_global => {
                    const name = self.readConstant().string;
                    if (!self.globals.contains(name)) {
                        return Error.UndefinedVariable;
                    }
                    try self.globals.put(name, self.peek(0));
                },
                .equal => {
                    try self.binary(equal);
                },
                .greater => {
                    try self.numeric(greater);
                },
                .less => {
                    try self.numeric(less);
                },
                .add => {
                    const b = self.peek(0);
                    const a = self.peek(1);
                    if (a.is(.string) and b.is(.string)) {
                        try self.concat();
                    } else if (a.is(.number) and b.is(.number)) {
                        try self.binary(add);
                    } else {
                        return Error.ExpectedAddOperands;
                    }
                },
                .subtract => {
                    try self.numeric(subtract);
                },
                .multiply => {
                    try self.numeric(multiply);
                },
                .divide => {
                    try self.numeric(divide);
                },
                .negate => {
                    if (self.peek(0).is(.number)) {
                        try self.push(.{ .number = -self.pop().number });
                    } else {
                        return Error.ExpectedNumberOperand;
                    }
                },
                .not => {
                    try self.push(.{ .bool = self.pop().isFalsy() });
                },
                .print => {
                    try self.pop().print(self.writer);
                    try self.writer.writeAll("\n");
                },
                .jump => {
                    const offset = self.readOffset();
                    self.current.ptr += offset;
                },
                .jump_if_false => {
                    const jump = self.readOffset();
                    if (self.peek(0).isFalsy()) {
                        self.current.ptr += jump;
                    }
                },
                .loop => {
                    const offset = self.readOffset();
                    self.current.ptr -= offset;
                },
                .call => {
                    const arg_count = self.read();
                    try self.call(self.peek(arg_count), arg_count);
                },
                .@"return" => {
                    const result = self.pop();
                    const frame = self.frames.pop();
                    const len = self.frames.items.len;
                    if (len == 0) {
                        _ = self.pop();
                        return;
                    }
                    try self.callReturn(result, frame.function.arity);
                    self.current = &self.frames.items[len - 1];
                },
            }
        }
    }
};
