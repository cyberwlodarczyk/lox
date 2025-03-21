const std = @import("std");
const heap = std.heap;
const print = std.debug.print;
const Lox = @import("lox.zig").Lox;
const Config = @import("lox.zig").Config;

fn getEnvFlag(name: []const u8) bool {
    if (std.posix.getenv(name)) |value| {
        return std.mem.eql(u8, value, "true");
    } else {
        return false;
    }
}

pub fn main() !void {
    var arena = heap.ArenaAllocator.init(heap.page_allocator);
    defer arena.deinit();
    const lox = Lox.init(
        arena.allocator(),
        Config{ .debug = .{
            .writer = std.io.getStdErr().writer(),
            .print_code = getEnvFlag("DEBUG_PRINT_CODE"),
            .trace_execution = getEnvFlag("DEBUG_TRACE_EXECUTION"),
        } },
    );
    const argv = std.os.argv;
    if (argv.len == 1) {
        try lox.repl();
    } else if (argv.len == 2) {
        try lox.runFile(argv[1]);
    } else {
        std.debug.print("Usage: {s} [path]\n", .{argv[0]});
    }
}
