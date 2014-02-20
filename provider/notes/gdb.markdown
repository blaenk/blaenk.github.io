---
title: GDB
published: February 18, 2014
excerpt: Because printf-debugging is tedious
comments: off
toc: left
---

It seems that using a debugger like the [GNU Debugger] (GDB) has become something of a lost art for the generation of developers whom are more involved with higher-level languages. Instead, it seems many people resort to so-called `printf`-debugging, where one places print statements all over the place to sort of trace the state of the program's execution at any given moment. Of course, this is very tedious, and can be done more naturally and efficiently with a dedicated debugger.

[GNU Debugger]: http://en.wikipedia.org/wiki/GNU_Debugger
*[GDB]: GNU Debugger

This notes page covers GDB's native interface in particular, although there are a couple of interfaces such as [cgdb], a lightweight interface with vim binds, as well as [DDD], which is a more general-purpose debugging interface.

[cgdb]: http://cgdb.github.io/
[DDD]: https://www.gnu.org/software/ddd/

Aside from GDB there's also [LLDB] to keep an eye on, which is the LLVM project's very own debugger. A [tutorial] is available, as well as a table listing the [LLDB equivalents] for various GDB commands.

[LLDB]: http://lldb.llvm.org/
[tutorial]: http://lldb.llvm.org/tutorial.html
[LLDB equivalents]: http://lldb.llvm.org/lldb-gdb.html

Remember that, when debugging, it's best if a program is built with a symbol table --- the list of memory addresses corresponding to the program's variables and lines of code --- by passing the `-g` parameter to gcc or clang.

For an additional resource, RMS' own [tutorial][rms_tut] on GDB is available.

*[RMS]: Richard M. Stallman
[rms_tut]: http://www.unknownroad.com/rtfm/gdbtut/

* toc

# Workflow

GDB is usually started by passing it the name of the program we want to debug:

``` bash
$ gdb program_to_debug
```

Processes with curses-like interfaces can be debugged by running them in a separate terminal and then attaching to them with the `-p` switch (or using the `attach` command):

``` bash
$ gdb -p PID
# or
$ gdb programname PID
```

Another thing that can be done is to run GDB and tell it that the program should be run on a different terminal. This is accomplished with the `tty` command. For example, assuming the program should be on `/dev/pts/8` --- perhaps because we have another window paired with that pseudoterminal --- we would run the following:

```
(gdb) tty /dev/pts/8
```

Then the following should be entered so that the keyboard input to the window will go to the program rather than to the shell, by making the shell go to sleep:

``` bash
$ sleep 100000
```

Entering nothing at this prompt re-runs the previous command, which is very useful when stepping with `next`.

It's common to then place a temporary breakpoint on `main` as an entrypoint into the debugging session with `tbreak main`.

The program is then run with the `run` command, passing it the appropriate program arguments. The program's execution will be paused at each breakpoint:

```
(gdb) run arg1 arg2
Starting program: /path/to/program_to_debug arg1 arg2

Breakpoint 1, function_containing_breakpoint(ac=4, av=0xbffff094) at src.c:16
```

If the program was previously run, the arguments passed to the previous `run` can be reused by simply entering `run` on its own. If the program is already running, GDB will ask if it should restart the program.

## Session

It's normal to keep a GDB session to open during the debugging session. As bugs are fixed and the program is recompiled, GDB will detect the changes and load them in. Breakpoints and other information specified in ways not dependent on line numbers should continue to work.

However, if exiting is necessary, the commands entered can be saved to a GDB startup file of name `.gdbinit`. This can then be loaded when GDB loads with:

``` bash
$ gdb -command=.gdbinit program
```

Breakpoints can be saved to a file with:

```
(gdb) save breakpoints filename
(gdb) source filename
```

History can be saved by putting this in `~/.gdbinit`:

```
set history filename ~/.gdb_history
set history save
```

GDB is focused on one file at any given moment, which is the file that will be looked for when given parameters without specifying the file name, such as breaking by line number or function name. The first focused file is the one with the `main` entry point, but the file focus is changed whenever the `list` command is run, when one steps into code residing on a different file, or when hitting a breakpoint from a separate source file.

# Breakpoints

Breakpoints, set with the `break` command, pause execution of the program just before that line of code executes.

```
(gdb) break function
(gdb) break line
(gdb) break file:line
(gdb) break file:function
```

It's also possible to set a breakpoint relative to the currently executing line of course code in the currently selected stack frame:

```
(gdb) break +offset
(gdb) break -offset
```

It's also possible to set a breakpoint at a specific virtual memory address, which is useful when there are no debugging symbols:

```
(gdb) break *address
```

If breaking based on the function name, a breakpoint is applied for every function with that name, which can be troublesome in C++ which allows function overloading. It'd be necessary to disambiguate by line number or file path.

A temporary breakpoint, set with the `tbreak` command, is one that operates just like a regular breakpoint but it clears itself after the first time it is reached.

Catchpoints are similar to breakpoints and are triggered by things such as thrown and caught exceptions, signals, calls to `fork()`, loading and unloading of libraries, and other events.

## Deleting

Breakpoints can be either deleted in two ways, using the `delete` or `clear` commands. The `delete` command accepts breakpoint identifiers or if none are given, deletes all breakpoints:

```
(gdb) delete breakpoint_list
(gdb) delete # deletes all breakpoints
```

The `clear` command accepts arguments similar to the `break` command. If no argument is given, it deletes the breakpoint at the next instruction that GDB will execute, which is useful for deleting the breakpoint that GDB just reached:

```
(gdb) clear # delete breakpoint that was just reached
```

## Disabling

The `disable` command is like the `delete` command in its arguments, but it simply disables breakpoints instead of removing them. Like the `delete` command, if no argument is provided, it disables all breakpoints.

The `enable` command is the opposite of the `disable` command. The `enable` command can provide `tbreak`-like functionality with `enable once`, which enables a list of breakpoints and then disables them once they're reached.

## Conditionals

A condition can be associated with a breakpoint, using the `contidion` command, so that execution only pauses at it if it satisfies the condition. The condition can refer to variables local to the scope of the breakpoint. For example, the following places a condition on breakpoint #1 so that it only breaks if `num_y` is `1`:

```
(gdb) condition 3 i == 10
```

This condition also could have been provided to the `break` command itself as follows:

```
(gdb) break 3 if i == 10
```

A condition can be removed from an existing breakpoint as well:

```
(gdb) cond 3
```

Pretty much any valid C conditional statement can be used, where true is non-zero and false is zero. This includes equality, logical, and inequality operators, bitwise and shift operators, arithmetic operators, and functions --- either from the program itself or from libraries, provided they're linked into the program.

```
(gdb) break 180 if string == NULL && i < 0
(gdb) break test.c:34 if (x & y) == 1
(gdb) break myfunc if i % (j + 3) != 0
(gdb) break test.c:myfunc if !check_variable_sanity(i)
(gdb) break 44 if strlen(mystring) == 0
```

**Note** that when using a library function or other externally-linked function that provides no debugging symbols, the result of the function will be interpreted as an integer. This can lead to misinterpretations, such as:

```
(gdb) print cos(0.0)
$1 = 14368
```

This can be alleviated if necessary by defining a GDB convenience variable with the necessary type information for the function in question:

```
(gdb) set $p = (double (*) (double))cos
(gdb) ptype $p
type = double (*)()
(gdb) p cos(3.14159265)
$2 = 14368
(gdb) p $p(3.14159265)
$4 = -1
```

## Command Lists

It's possible to define a list of commands to be run as soon as a particular breakpoint is triggered using the `commands` command which takes the following form. If the first command in the list is the `silent` command, it'll suppress information about the breakpoint whenever it's triggered.

```
(gdb) commands breakpoint-identifier
> ...commands...
> end
```

Commands can be removed from a breakpoint by redefining an empty command list.

It's possible to define macros --- sets of commands --- with the `define` command. Its form is similar to `commands`' but its first argument is the name of the macro. The arguments are accessible as `$argn` where `n` is anywhere from `0` to `9`. Macro arguments aren't comma separated. Useful macros can be placed in the startup file and referenced within GDB. The command `show user` can be used to list all macros.

```
(gdb) define print_and_go
> printf $arg0, $arg1
> continue
> end
```

Commands can also include if conditions.

## Watchpoints

A _watchpoint_, set with `watch`, can be thought of as being attached to an expression, such that it gets triggered when the result of that expression changes. Setting a watchpoint on a variable causes the debugger to pause execution at the point when the memory location of that variable changes. For example, to watch the `z` variable:

```
(gdb) watch z
```

More powerful watchpoints can be established that are based on conditions related to the variable in question. For example, to watch for when `z` becomes greater than `28`:

```
(gdb) watch (z > 28)
```

Watchpoints are deleted as soon as any variable in the expression goes out of scope, requiring one to re-establish them the next time that function is called, if necessary. This can be automated by a command on a breakpoint in the relevant scope that establishes the watchpoint.

## Signals

It's possible to avoid the pausing of execution or printing a warning when signals are received by having GDB handle them with the `handle` command. For example, to avoid stopping and printing a warning when `SIGUSR` is received:

```
handle SIGUSR1 nostop noprint
```

## Information

The `info breakpoints` command can provide a summary of the various breakpoints that have been established and their properties. The breakpoint's _disposition_ refers to what will happen to it the next time it's reached.

# Executing

A key thing to remember is that whatever line GDB is currently at has _not_ been executed yet.

## Stepping

Single-stepping is possible with the `next` command, where a execution proceeds one original-source-code-line --- i.e. 1 C++ line, not assembly --- at a time. The `step` command is similar, but if the line is on a function call, it will enter that function's body if there are debugging symbols available for it, whereas `next` would step over it.

Both `step` and `next` accept optional arguments specifying how far to step.

## Resuming

Execution can be resumed until the next breakpoint with the `continue` command. It accepts an optional argument `n` telling it to ignore the next `n` breakpoints.

The `finish` command instructs GDB to resume execution until just after the current stack frame finishes, i.e. until just after the current function call finishes. However, if there are any intervening breakpoints, GDB _will_ pause at them.

The `until` command resumes execution until GDB reaches a machine instruction that has a higher memory address than the current one. Like `finish`, it pauses at intervening breakpoints. The `until` command is usually useful when used on loops. It also accepts arguments similar to `break`, which causes execution to continue until that location is reached.

# Inspecting

Inspecting the program's state at any point in its execution is usually the primary motivation for debugging and setting breakpoints, so I consider it one of the most important parts to learn.

## Printing

Basic variables such as integers can be printed with the `print` command. More complicated types will be covered later on. It's also possible to print out individual array elements with regular index syntax.

```
(gdb) print y[0]
$1 = 12
```

The `print` command has support for printing in a variety of different formats, such as `p/x` which would print the variable in hexadecimal. Similarly, `c` would print as a character, `s` a string, and `f` floating-point.

There is also a `printf` function which behaves the same way as the function of the same name in C, but like `print` in GDB, the parentheses are optional.

This makes GDB much more preferable to actual `printf`-debugging which modifies the source code and is itself susceptible to bugs, or can even have unforeseen consequences.

Structures can be printed outright and their constituent data members will be displayed.

```
(gdb) p *structptr
$1 = {val = 12, left = 0x8049698, right = 0x0}
```

The `display` command registers a variable for printing every time execution pauses. If no argument is passed, it lists all auto-displayed variables. Display directives can be disabled or enabled with the corresponding commands. To remove a display directive altogether, use the `undisplay` command:

```
(gdb) disable display 1
(gdb) enable display 1
(gdb) undisplay 1
```

The `call` command can also be used to call a particular function when a breakpoint is triggered, which can be used for printing some state out using a function defined in the program, specialized for that task.

```
(gdb) commands 2
> printf "current tree is"
> call printtree(root)
> end
```

The `x` command, for "examine," can be used to examing memory at a given address.

A summary of information about the current frame can be obtained with the `info frame` command. More specifically, the `info args` command can show the values of the arguments passed to the current function, and the `info locals` command can show all of the variables local to the current stack frame.

The `ptype` command can provide a quick look at the layout of a class or structure.

A static array can be printed regularly with the `print` command:

``` c
int x[25];
```

```
(gdb) p x
```

However, dynamically allocated arrays can't be printed out in such a straightforward manner. Instead we would create an _artificial array_ that provides GDB with the necessary information about the array, specifically how large it is. It takes the form `*ptr@size`.

``` cpp
int x = (int *)malloc(6 * sizeof(int));
x[3] = 12;
```

```
(gdb) p *x@6
$1 = {0, 0, 0, 12, 0, 0}
```

```
(gdb) p (int [6]) *x
$1 = {0, 0, 0, 12, 0, 0}
```

## Call Stack Navigation

The runtime information immediately available to GDB is whatever is located on the current stack frame, determined by the current function call. Consider functions `A`, `B`, and `C`, where each one calls the next. If execution is paused within `C`, you can print values local to `C`'s stack frame with typical commands like `print`. That is, if function `C` has local variable `j`, it can be printed with `print j`.

However, it's also possible to traverse the call stack as it is at any given moment, in order to bring a different frame into GDB's focus and therefore inspect that frame's state. The `frame` command allows traversal relative to the current function call's frame, which is referred to as frame `0`. For example, to go to the grand-parent's frame:

```
(gdb) frame 2
```

GDB also provides simpler commands `up` and `down` for traversing relative to the currently focused stack frame.

The `backtrace` command shows the entire stack.

## Setting

It's possible to set the value of variables mid-execution using the `set` command.

```
(gdb) set x = 12
```

There are also variables specific to GDB that can be created and used. For example, GDB has a _value history_ which simply assigns the result of an entered command to an increasing variable name. This is similar to Scala's interpreter which assigns the expression result values to `res#`. These variables can be referenced in future commands. The `$` variable always refers to the previous result.

```
(gdb) p tmp->left
$1 = (struct node *) 0x80496a8
(gdb) p *$
$2 = {val = 5, left = 0x0, right = 0x0}
```

It's also possible to set environment variables within GDB:

```
(gdb) set environment ENVVAR = 3
```

Convenience variables can change values according to C rules. For example, given an array `w`, its elements can be iterated in GDB by creating a convenience variable that represents the iterations' current index.

``` c
int w[3] = {12, 5, 88};
```

```
(gdb) set $i = 0
(gdb) p w[$i++]
$1 = 12
(gdb)
$2 = 5
(gdb)
$3 = 88
```

# Core Files

A core file contains a detailed description of the program's state when it crashed, such as the contents of the stack(s), CPU's registers, and values of statically allocated variables.

Usually when a core dump file is generated, it's placed in the current directory named `core` or some variation of it. It may be necessary to increase the shell's core dump resource limit with the command:

``` bash
$ ulimit -c unlimited
```

On systems that use [systemd], such as mine, these files are instead managed by systemd's journal --- they don't show up in the current directory regardless of the resource limit. The helper program `systemd-coredumpctl` can be used to extract the core dumps from the journal into individual files:

[systemd]: http://en.wikipedia.org/wiki/Systemd

``` bash
# based on PID
$ systemd-coredumpctl dump PID -o outname

# based on binary name
$ systemd-coredumpctl dump urxvt -o outname
```

Core files are useful from a debugging perspective because they provide a starting point for dissecting the program to determine the cause of the crash. They are more useful than simply re-running the program within GDB because it would be that the bug that caused the crash occurs rarely and randomly.

Core files can be loaded into GDB by supplying them as a second argument, after the name of the program that generated the core file:

``` bash
$ gdb crasher corefile
```

Once loaded, we can run a `backtrace` to determine the point at which the program crashed. The `frame` command can be used to navigate the call stack, and typical inspecting commands can be used to analyze the program's state as it was when it crashed. Breakpoints and similar tools can be established, then the program can be run normally.

# Threads

GDB announces whenever new threads are created. The `info threads` command can provide summary information about what each of the threads are doing. An asterisk denotes the currently focused thread. Note that some threads may be internal to the threading implementation, such as a Pthreads manager thread.

It's possible to switch GDB's focus to a different thread with the `thread` command, which takes as argument the thread identifier denoted in `info threads`.

It's also possible to provide thread disambiguators to the `break` command, to pause execution when a specific thread reaches a breakpoint:

```
(gdb) break linenr thread threadid if condition
```

# Abbreviations

Many commands have abbreviations which are simpler to enter:

Command    Abbreviation
--------  --------------
backtrace bt
break     b
condition cond
continue  c
disable   dis
display   disp
enable    ena
finish    fin
info      i
next      n
print     p
quit      q
run       r
step      s
tbreak    tb
undisplay undisp
until     u

