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

    fn add(a: f64, b: f64) f64 {
        return a + b;
    }

    fn subtract(a: f64, b: f64) f64 {
        return a - b;
    }

    fn multiply(a: f64, b: f64) f64 {
        return a * b;
    }

    fn divide(a: f64, b: f64) f64 {
        return a / b;
    }

    fn binary(self: *Self, f: *const fn (a: f64, b: f64) f64) !void {
        const b = self.pop();
        const a = self.pop();
        try self.push(f(a, b));
    }

    pub fn run(self: *Self) !void {
        while (true) {
            if (self.debug) {
                print("          ", .{});
                for (self.stack.items) |value| {
                    print("[ {d} ]", .{value});
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
                .add => {
                    try self.binary(add);
                },
                .subtract => {
                    try self.binary(subtract);
                },
                .multiply => {
                    try self.binary(multiply);
                },
                .divide => {
                    try self.binary(divide);
                },
                .negate => {
                    try self.push(-self.pop());
                },
                .@"return" => {
                    print("{d}\n", .{self.pop()});
                    return;
                },
            }
        }
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
    }
};
