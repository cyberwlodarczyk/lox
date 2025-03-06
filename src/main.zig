const std = @import("std");
const heap = std.heap;
const print = std.debug.print;
const Lox = @import("lox.zig").Lox;

pub fn main() !void {
    const debug_env = std.posix.getenv("DEBUG") orelse "";
    const debug = std.mem.eql(u8, debug_env, "true");
    var arena = heap.ArenaAllocator.init(heap.page_allocator);
    defer arena.deinit();
    const lox = Lox.init(arena.allocator(), debug);
    const argv = std.os.argv;
    if (argv.len == 1) {
        try lox.repl();
    } else if (argv.len == 2) {
        try lox.runFile(argv[1]);
    } else {
        std.debug.print("Usage: {s} [path]\n", .{argv[0]});
    }
}
