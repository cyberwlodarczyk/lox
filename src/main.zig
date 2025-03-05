const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator(.{});
const Lox = @import("lox.zig").Lox;

pub fn main() !void {
    const debug_env = std.posix.getenv("DEBUG") orelse "";
    const debug = std.mem.eql(u8, debug_env, "true");
    var gpa = GeneralPurposeAllocator{};
    defer _ = gpa.deinit();
    const lox = Lox.init(debug, gpa.allocator());
    const argv = std.os.argv;
    if (argv.len == 1) {
        try lox.repl();
    } else if (argv.len == 2) {
        try lox.runFile(argv[1]);
    } else {
        std.debug.print("Usage: {s} [path]\n", .{argv[0]});
    }
}
