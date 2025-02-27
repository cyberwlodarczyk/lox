const std = @import("std");
const print = std.debug.print;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const OpCode = enum(u8) {
    constant,
    add,
    subtract,
    multiply,
    divide,
    negate,
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

    fn write(self: *Chunk, byte: u8, line: u32) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    fn writeOpCode(self: *Chunk, op: OpCode, line: u32) !void {
        try self.write(@intFromEnum(op), line);
    }

    fn writeConstant(self: *Chunk, constant: usize, line: u32) !void {
        try self.write(@intCast(constant), line);
    }

    fn addConstant(self: *Chunk, value: Value) !usize {
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
        return switch (instruction) {
            .constant => self.constantInstruction("constant", offset),
            .add => simpleInstruction("add", offset),
            .subtract => simpleInstruction("subtract", offset),
            .multiply => simpleInstruction("multiply", offset),
            .divide => simpleInstruction("divide", offset),
            .negate => simpleInstruction("negate", offset),
            .ret => simpleInstruction("ret", offset),
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

const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

const VM = struct {
    debug: bool,
    chunk: *Chunk,
    ip: [*]u8,
    stack: ArrayList(Value),

    fn init(debug: bool, chunk: *Chunk, allocator: Allocator) VM {
        return VM{
            .debug = debug,
            .chunk = chunk,
            .ip = chunk.code.items.ptr,
            .stack = ArrayList(Value).init(allocator),
        };
    }

    fn push(self: *VM, value: Value) !void {
        try self.stack.append(value);
    }

    fn pop(self: *VM) Value {
        return self.stack.pop();
    }

    fn readByte(self: *VM) u8 {
        defer self.ip += 1;
        return @as(*u8, @ptrCast(self.ip)).*;
    }

    fn readOpCode(self: *VM) OpCode {
        return @enumFromInt(self.readByte());
    }

    fn readConstant(self: *VM) Value {
        return self.chunk.constants.items[self.readByte()];
    }

    fn run(self: *VM) !InterpretResult {
        while (true) {
            if (self.debug) {
                print("          ", .{});
                for (self.stack.items) |value| {
                    print("[ {d} ]", .{value});
                }
                print("\n", .{});
                _ = self.chunk.disassembleInstruction(@intFromPtr(
                    self.ip,
                ) - @intFromPtr(
                    self.chunk.code.items.ptr,
                ));
            }
            const instruction = self.readOpCode();
            switch (instruction) {
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
                .ret => {
                    print("{d}\n", .{self.pop()});
                    return .ok;
                },
            }
        }
    }

    fn deinit(self: *VM) void {
        self.stack.deinit();
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();
    var constant = try chunk.addConstant(1.2);
    try chunk.writeOpCode(.constant, 123);
    try chunk.writeConstant(constant, 123);
    constant = try chunk.addConstant(3.4);
    try chunk.writeOpCode(.constant, 123);
    try chunk.writeConstant(constant, 123);
    try chunk.writeOpCode(.add, 123);
    constant = try chunk.addConstant(5.6);
    try chunk.writeOpCode(.constant, 123);
    try chunk.writeConstant(constant, 123);
    try chunk.writeOpCode(.divide, 123);
    try chunk.writeOpCode(.negate, 123);
    try chunk.writeOpCode(.ret, 123);
    const debug_env = std.posix.getenv("DEBUG") orelse "";
    const debug = std.mem.eql(u8, debug_env, "true");
    var vm = VM.init(debug, &chunk, allocator);
    defer vm.deinit();
    _ = try vm.run();
}
