const std = @import("std");
const print = std.debug.print;
const heap = std.heap;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const OpCode = enum(u8) {
    constant,
    ret,
};

const Value = f64;

const Chunk = struct {
    code: ArrayList(u8),
    constants: ArrayList(Value),
    lines: ArrayList(u32),

    fn init(allocator: Allocator) Chunk {
        return Chunk{
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Value).init(allocator),
            .lines = ArrayList(u32).init(allocator),
        };
    }

    fn write(self: *Chunk, byte: u8, line: u32) Allocator.Error!void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    fn addConstant(self: *Chunk, value: Value) Allocator.Error!usize {
        try self.constants.append(value);
        return self.constants.items.len - 1;
    }

    fn disassemble(self: Chunk, name: []const u8) void {
        print("== {s} ==\n", .{name});
        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = self.disassembleInstruction(offset);
        }
    }

    fn disassembleInstruction(self: Chunk, offset: usize) usize {
        print("{d:0>4} ", .{offset});
        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            print("   | ", .{});
        } else {
            print("{d: >4} ", .{self.lines.items[offset]});
        }
        const instruction: OpCode = @enumFromInt(self.code.items[offset]);
        switch (instruction) {
            .constant => {
                return self.constantInstruction("constant", offset);
            },
            .ret => {
                return simpleInstruction("ret", offset);
            },
        }
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

pub fn main() !void {
    var arena = heap.ArenaAllocator.init(heap.page_allocator);
    defer arena.deinit();
    var chunk = Chunk.init(arena.allocator());
    defer chunk.deinit();
    const constant = try chunk.addConstant(1.2);
    try chunk.write(@intFromEnum(OpCode.constant), 123);
    try chunk.write(@intCast(constant), 123);
    try chunk.write(@intFromEnum(OpCode.ret), 123);
    chunk.disassemble("test chunk");
}
