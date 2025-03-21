const std = @import("std");
const Allocator = std.mem.Allocator;
const Writer = std.fs.File.Writer;
const Chunk = @import("compiler.zig").Chunk;
const Compiler = @import("compiler.zig").Compiler;
const VM = @import("vm.zig").VM;

pub const Config = struct {
    debug: struct {
        writer: Writer,
        print_code: bool,
        trace_execution: bool,
    },
};

pub const Lox = struct {
    const Self = @This();

    allocator: Allocator,
    config: Config,

    pub fn init(allocator: Allocator, config: Config) Self {
        return Self{
            .allocator = allocator,
            .config = config,
        };
    }

    fn run(self: Self, source: []const u8) !void {
        var compiler = try Compiler.init(self.allocator, self.config, source);
        const script = try compiler.run();
        var vm = try VM.init(self.allocator, self.config);
        try vm.run(script);
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
