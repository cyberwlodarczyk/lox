const std = @import("std");
const Allocator = std.mem.Allocator;
const Chunk = @import("compiler.zig").Chunk;
const Compiler = @import("compiler.zig").Compiler;
const VM = @import("vm.zig").VM;

pub const Lox = struct {
    const Self = @This();

    allocator: Allocator,
    debug: bool,

    pub fn init(allocator: Allocator, debug: bool) Self {
        return Self{
            .allocator = allocator,
            .debug = debug,
        };
    }

    fn run(self: Self, source: []const u8) !void {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();
        var compiler = Compiler.init(self.allocator, source, &chunk);
        try compiler.run();
        var vm = VM.init(self.allocator, self.debug, chunk);
        defer vm.deinit();
        try vm.run();
    }

    pub fn runFile(self: Self, path: [*:0]u8) !void {
        const realpath = try std.fs.realpathAlloc(self.allocator, std.mem.span(path));
        defer self.allocator.free(realpath);
        const file = try std.fs.openFileAbsolute(realpath, .{});
        defer file.close();
        const source = try file.readToEndAlloc(self.allocator, 1 << 20);
        defer self.allocator.free(source);
        try self.run(source);
    }

    pub fn repl(self: Self) !void {
        const stdin = std.io.getStdIn().reader();
        while (true) {
            std.debug.print("> ", .{});
            const source = try stdin.readUntilDelimiterAlloc(self.allocator, '\n', 1 << 20);
            defer self.allocator.free(source);
            try self.run(source);
        }
    }
};
