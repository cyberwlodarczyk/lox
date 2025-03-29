const std = @import("std");
const print = std.debug.print;
const maxInt = std.math.maxInt;
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
    get_upvalue,
    set_upvalue,
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
    closure,
    close_upvalue,
    @"return",
};

pub const RawChunk = struct {
    const Self = @This();

    start_line: u16,
    code: []const u8,
    constants: []const Value,
    lines: []const u8,
    debug_writer: Writer,
    debug_state: struct {
        offset: usize,
        line: u16,
        delta: u8,
        is_next_line: bool,
        is_end: bool,
    },

    pub fn init(chunk: Chunk, debug_writer: Writer) Self {
        return Self{
            .start_line = chunk.start_line,
            .lines = chunk.lines.items,
            .code = chunk.code.items,
            .constants = chunk.constants.items,
            .debug_writer = debug_writer,
            .debug_state = undefined,
        };
    }

    pub fn debugStart(self: *Self) void {
        self.debug_state = .{
            .offset = 0,
            .line = 1,
            .delta = 0,
            .is_next_line = true,
            .is_end = false,
        };
    }

    pub fn debug(self: *Self) !void {
        const state = &self.debug_state;
        while (state.delta >= self.lines[state.line - 1]) {
            state.delta -= self.lines[state.line - 1];
            state.line += 1;
            state.is_next_line = true;
        }
        const writer = self.debug_writer;
        const offset = state.offset;
        try writer.print("{d:0>4} ", .{offset});
        if (state.is_next_line) {
            state.is_next_line = false;
            try writer.print("{d: >4} ", .{self.start_line + state.line - 1});
        } else {
            try writer.writeAll("   | ");
        }
        const operation: Operation = @enumFromInt(self.code[offset]);
        const name = @tagName(operation);
        const delta: u8 = switch (operation) {
            .constant, .closure, .get_global, .define_global, .set_global => o: {
                const constant = self.code[offset + 1];
                const value = self.constants[constant];
                try writer.print("{s: <16} {d:>4} '", .{ name, constant });
                try value.print(writer);
                try writer.writeAll("'\n");
                if (operation == .closure) {
                    var x = offset + 2;
                    for (0..value.function.upvalue_count) |_| {
                        const is_local = self.code[x];
                        x += 1;
                        const index = self.code[x];
                        x += 1;
                        try writer.print(
                            "{d:0>4}      |                     {s} {d}\n",
                            .{ x - 2, if (is_local == 1) "local" else "upvalue", index },
                        );
                    }
                    break :o 2 + value.function.upvalue_count * 2;
                } else {
                    break :o 2;
                }
            },
            .get_local, .set_local, .get_upvalue, .set_upvalue, .call => o: {
                try writer.print("{s: <16} {d:>4}\n", .{ name, self.code[offset + 1] });
                break :o 2;
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
                break :o 3;
            },
            else => o: {
                try writer.print("{s}\n", .{name});
                break :o 1;
            },
        };
        state.offset += delta;
        if (state.offset == self.code.len) {
            state.is_end = true;
            return;
        }
        state.delta += delta;
    }

    pub fn debugAll(self: *Self, label: []const u8) !void {
        try self.debug_writer.print("== {s} ==\n", .{label});
        self.debugStart();
        while (!self.debug_state.is_end) {
            try self.debug();
        }
    }
};

pub const Chunk = struct {
    const Self = @This();

    start_line: u16,
    lines: ArrayList(u8),
    code: ArrayList(u8),
    constants: ArrayList(Value),

    pub fn init(allocator: Allocator, start_line: u16) !Self {
        var self = Self{
            .start_line = start_line,
            .lines = ArrayList(u8).init(allocator),
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Value).init(allocator),
        };
        try self.lines.append(0);
        return self;
    }

    pub fn write(self: *Self, byte: u8, line: u16) !void {
        const current_line = self.start_line + @as(u16, @intCast(self.lines.items.len)) - 1;
        for (0..line - current_line) |_| {
            try self.lines.append(0);
        }
        self.lines.items[self.lines.items.len - 1] += 1;
        try self.code.append(byte);
    }
};
