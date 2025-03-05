const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const Operation = @import("compiler.zig").Operation;
const Value = @import("compiler.zig").Value;
const Compiler = @import("compiler.zig").Compiler;
const Chunk = @import("compiler.zig").Chunk;

pub const VM = struct {
    const Self = @This();
    const Stack = std.ArrayList(Value);
    pub const Error = error{
        ExpectedNumberOperand,
        ExpectedNumberOperands,
    };

    debug: bool,
    stack: Stack,
    chunk: Chunk,
    ptr: [*]u8,

    pub fn init(debug: bool, chunk: Chunk, allocator: Allocator) Self {
        return Self{
            .debug = debug,
            .stack = Stack.init(allocator),
            .chunk = chunk,
            .ptr = chunk.code.items.ptr,
        };
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

    fn readByte(self: *Self) u8 {
        defer self.ptr += 1;
        return @as(*u8, @ptrCast(self.ptr)).*;
    }

    fn readOperation(self: *Self) Operation {
        return @enumFromInt(self.readByte());
    }

    fn readConstant(self: *Self) Value {
        return self.chunk.constants.items[self.readByte()];
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

    pub fn run(self: *Self) !void {
        while (true) {
            if (self.debug) {
                print("          ", .{});
                for (self.stack.items) |value| {
                    print("[ ", .{});
                    value.debug();
                    print(" ]", .{});
                }
                print("\n", .{});
                _ = self.chunk.disassembleInstruction(@intFromPtr(
                    self.ptr,
                ) - @intFromPtr(
                    self.chunk.code.items.ptr,
                ));
            }
            const operation = self.readOperation();
            switch (operation) {
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
                    try self.numeric(add);
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
                .@"return" => {
                    self.pop().debug();
                    print("\n", .{});
                    return;
                },
            }
        }
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
    }
};
