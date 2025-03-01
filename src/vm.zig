const std = @import("std");
const print = std.debug.print;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Operation = @import("compiler.zig").Operation;
const Value = @import("compiler.zig").Value;
const Compiler = @import("compiler.zig").Compiler;
const Chunk = @import("compiler.zig").Chunk;

pub const VM = struct {
    const Self = @This();

    debug: bool,
    stack: ArrayList(Value),
    allocator: Allocator,
    ptr: [*]u8,
    chunk: Chunk,

    pub fn init(debug: bool, allocator: Allocator) Self {
        return Self{
            .debug = debug,
            .stack = ArrayList(Value).init(allocator),
            .allocator = allocator,
            .chunk = undefined,
            .ptr = undefined,
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

    fn run(self: *Self) !void {
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
                    try self.push(self.pop() + self.pop());
                },
                .subtract => {
                    try self.push(self.pop() - self.pop());
                },
                .multiply => {
                    try self.push(self.pop() * self.pop());
                },
                .divide => {
                    try self.push(self.pop() / self.pop());
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

    pub fn repl(self: *Self) !void {
        const reader = std.io.getStdIn().reader();
        while (true) {
            print("> ", .{});
            const source = try reader.readUntilDelimiterAlloc(self.allocator, '\n', 1 << 20);
            defer self.allocator.free(source);
            // try self.run(source);
        }
    }

    pub fn runFile(self: *Self, path: [*:0]u8) !void {
        const realpath = try std.fs.realpathAlloc(self.allocator, std.mem.span(path));
        defer self.allocator.free(realpath);
        const file = try std.fs.openFileAbsolute(realpath, .{});
        defer file.close();
        _ = try file.readToEndAlloc(self.allocator, 1 << 20);
        // try self.run(source);
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
    }
};
