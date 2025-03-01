const std = @import("std");
const VM = @import("vm.zig").VM;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();
    const debug_env = std.posix.getenv("DEBUG") orelse "";
    const debug = std.mem.eql(u8, debug_env, "true");
    var vm = VM.init(debug, allocator);
    defer vm.deinit();
    const argv = std.os.argv;
    if (argv.len == 1) {
        try vm.repl();
    } else if (argv.len == 2) {
        try vm.runFile(argv[1]);
    } else {
        std.debug.print("Usage: {s} [path]\n", .{argv[0]});
    }
}
