const std = @import("std");
const print = std.debug.print;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
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

    code: ArrayList(u8),
    constants: ArrayList(Value),
    lines: ArrayList(u32),

    fn init(allocator: Allocator) Self {
        return Chunk{
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Value).init(allocator),
            .lines = ArrayList(u32).init(allocator),
        };
    }

    fn write(self: *Self, byte: u8, line: u32) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    fn writeOperation(self: *Chunk, operation: Operation, line: u32) !void {
        try self.write(@intFromEnum(operation), line);
    }

    fn writeConstant(self: *Chunk, value: Value, line: u32) !void {
        try self.constants.append(value);
        try self.write(@intCast(self.constants.items.len - 1), line);
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

    fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }
};

pub const Compiler = struct {
    const Self = @This();

    scanner: Scanner,
    allocator: Allocator,

    pub fn init(source: []const u8, allocator: Allocator) !Self {
        return Self{
            .scanner = try Scanner.init(source, allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.scanner.deinit();
    }
};
