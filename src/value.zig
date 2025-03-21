const std = @import("std");
const Writer = std.fs.File.Writer;
const RawChunk = @import("chunk.zig").RawChunk;

pub const Native = struct {
    arity: u8,
    name: []const u8,
    function: *const fn (args: []const Value) Value,
};

pub const Function = struct {
    arity: u8,
    name: ?[]const u8,
    chunk: RawChunk,
};

pub const Value = union(enum) {
    const Self = @This();
    const Tag = std.meta.Tag(Self);

    bool: bool,
    nil,
    number: f64,
    string: []const u8,
    function: Function,
    native: Native,

    pub fn print(self: Self, writer: Writer) !void {
        switch (self) {
            .bool => |b| {
                if (b) {
                    try writer.writeAll("true");
                } else {
                    try writer.writeAll("false");
                }
            },
            .nil => {
                try writer.writeAll("nil");
            },
            .number => |n| {
                try writer.print("{d}", .{n});
            },
            .string => |s| {
                try writer.print("{s}", .{s});
            },
            .function => |f| {
                if (f.name) |n| {
                    try writer.print("<fn {s}>", .{n});
                } else {
                    try writer.writeAll("<script>");
                }
            },
            .native => |n| {
                try writer.print("<native fn {s}>", .{n.name});
            },
        }
    }

    pub fn is(self: Self, tag: Tag) bool {
        return @as(Tag, self) == tag;
    }

    pub fn eql(self: Self, other: Self) bool {
        if (!self.is(@as(Tag, other))) {
            return false;
        }
        return switch (self) {
            .bool => |x| x == other.bool,
            .nil => true,
            .number => |x| x == other.number,
            .string => |s| std.mem.eql(u8, s, other.string),
            .native => |n| std.mem.eql(u8, n.name, other.native.name) and n.function == other.native.function,
            .function => false,
        };
    }

    pub fn isFalsy(self: Self) bool {
        return switch (self) {
            .bool => |x| !x,
            .nil => true,
            else => false,
        };
    }
};
