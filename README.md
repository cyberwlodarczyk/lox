# Lox

## Introduction

Lox is a very basic programming language designed for the [Crafting Interpreters](https://www.craftinginterpreters.com) book by Robert Nystrom. This repository contains an implementation written in Zig based on the official guide that uses C (clox).

## Overview

```
print "hello, world!";
```

### Variables

```
var a = "global";
{
    var a = "outer";
    {
        var a = "inner";
        print a;
    }
    print a;
}
print a;
```

### Control Flow

```
if (2 + 2 < 5 and 6 * 8 != 50) {
    print "yes";
} else {
    print "no";
}
```

```
var a = 1;
while (a <= 10) {
    print a;
    a = a + 1;
}
```

```
for (var a = 1; a <= 10; a = a + 1) {
    print a;
}
```

### Functions

```
fun add(a, b) {
    return a + b;
}

print add(1, 2);
```

### Closures

```
fun outer() {
    var outside = "outside";
    fun inner() {
      print outside;
    }
    return inner;
}

var inner = outer();
inner();
```
