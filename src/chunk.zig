const std = @import("std");
const print = std.debug.print;
const ArrayList = std.ArrayList;
const Writer = std.fs.File.Writer;
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").Value;

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
    call,
    @"return",
};

pub const RawChunk = struct {
    const Self = @This();

    code: []const u8,
    constants: []const Value,
    lines: []const u32,

    pub fn init(chunk: Chunk) Self {
        return Self{
            .code = chunk.code.items,
            .constants = chunk.constants.items,
            .lines = chunk.lines.items,
        };
    }

    pub fn debugAt(self: Self, writer: Writer, offset: usize) !usize {
        try writer.print("{d:0>4} ", .{offset});
        if (offset > 0 and self.lines[offset] == self.lines[offset - 1]) {
            try writer.writeAll("   | ");
        } else {
            try writer.print("{d: >4} ", .{self.lines[offset]});
        }
        const operation: Operation = @enumFromInt(self.code[offset]);
        const name = @tagName(operation);
        return switch (operation) {
            .constant, .get_global, .define_global, .set_global => o: {
                const constant = self.code[offset + 1];
                const value = self.constants[constant];
                try writer.print("{s: <16} {d:>4} '", .{ name, constant });
                try value.print(writer);
                try writer.writeAll("'\n");
                break :o offset + 2;
            },
            .get_local, .set_local, .call => o: {
                try writer.print("{s: <16} {d:>4}\n", .{ name, self.code[offset + 1] });
                break :o offset + 2;
            },
            .jump, .jump_if_false, .loop => o: {
                var jump = @as(u16, @intCast(self.code[offset + 1])) << 8;
                jump |= @as(u16, @intCast(self.code[offset + 2]));
                var dest = offset + 3;
                if (operation == .loop) {
                    dest -= jump;
                } else {
                    dest += jump;
                }
                try writer.print("{s: <16} {d:>4} -> {d}\n", .{ name, offset, dest });
                break :o offset + 3;
            },
            else => o: {
                try writer.print("{s}\n", .{name});
                break :o offset + 1;
            },
        };
    }

    pub fn debug(self: Self, writer: Writer, name: []const u8) !void {
        try writer.print("== {s} ==\n", .{name});
        var offset: usize = 0;
        while (offset < self.code.len) {
            offset = try self.debugAt(writer, offset);
        }
    }
};

pub const Chunk = struct {
    const Self = @This();

    code: ArrayList(u8),
    constants: ArrayList(Value),
    lines: ArrayList(u32),

    pub fn init(allocator: Allocator) Self {
        return Chunk{
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Value).init(allocator),
            .lines = ArrayList(u32).init(allocator),
        };
    }

    pub fn write(self: *Self, byte: u8, line: u32) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }
};
