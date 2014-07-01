---
title: Erlang and the OTP
published: February 26, 2014
excerpt: A very stable and robust fault-tolerant, concurrent programming language
comments: off
toc: left
---

Erlang was the first functional programming language I took a shot at learning. I started with the Manning book _Erlang in Action_ which also explained the OTP. The problem was that I was combining learning functional programming with the pretty fringe environment that is Erlang. So instead I took a break from that and decided to learn Haskell, and loved it.

Now I'm back and would like to pick up Erlang and its OTP library. Even if I don't end up using it, I think it'll teach me many interesting concepts that lend themselves to this new age of massively concurrent programs. [Scala], for example, uses actor-based concurrency much like Erlang's, as well as supervision trees.

[Scala]: /notes/scala/

* toc

# Modules

Modules generally take the following form. The first line contains the module directive, known as the _export declaration_, as in Haskell. This lists the functions and their arity.

``` erlang
-module(hello).
export([start/0]).

start() ->
  io:format("Hello world~n").
```

This can be compiled from the shell with the `c(modulename)` command. However, it's more likely to be compiled with the Erlang compiler `erlc`, which produces a `beam` file. It's possible to then run the program can then be run on the Erlang virtual machine with the `erl` command, which uses `-s` to specify the functions to evaluate, in order.

``` bash
$ erlc hello.erl
$ erl -noshell -s hello start -s init stop
```

## Example {#module-example}

The following describes a file server. It defines a convenience function `start` which spawns a process that forever performs the `loop` function inside a particular directory, which simply waits for a message to be received and processes it. Notice that `loop` performs a tail-recursive call; Erlang has tail-call optimization, so there's no need to worry about overflowing the stack.

This server simply responds to two simple messages: one which requests a file listing and another which request file contents. Notice that the messages are distinguished by pattern matching, which works on atoms as well, such as `list_dir`.

~~~ {.erlang text="server"}
-module(afile_server).
-export([start/1, loop/1]).

start(Dir) -> spawn(afile_server, loop, [Dir]).

loop(Dir) ->
  receive
    {Client, list_dir} ->
      Client ! {self(), file:list_dir(Dir)};
    {Client, {get_file, File}} ->
      Full = filename:join(Dir, File),
      Client ! {self(), file:read_file(Full)}
  end,
  loop(Dir).
~~~

The client simply serves to provide client-facing functions that abstract away the server protocol, i.e. the exact messages sent. These simply send the appropriate message and wait for the response.

~~~ {.erlang text="client"}
-module(afile_client).
-export([ls/1, get_file/2]).

ls(Server) ->
  Server ! {self(), list_dir},
  receive
    {Server, FileList} ->
      FileList
  end.

get_file(Server, File) ->
  Server ! {self(), {get_file, File}},
  receive
    {Server, Content} ->
      Content
  end.
~~~

# Processes

`spawn` is an Erlang primitive that initiates a concurrent process and returns its identifier.

``` erlang
spawn(ModuleName, FuncName, [Arg1, Arg2, ..., ArgN])

% to spawn a process running the `init` function
% from the `person` module, i.e. `person:init`

spawn(person, init, ["Joe"])
```

The way in which processes can interact is through sending messages, which is done using the `!` primitive.

``` erlang
RecipientPid ! Msg

% self() refers to the current process id

OtherProcess ! {self(), "Hey there"}
```

Processes are received in a `receive` block by pattern matching on the messages.

``` erlang
receive
  {From, Message} ->
    ...
end
```

# Values

Values are bound with the `=` operator, like `let` in Haskell. It's also possible to pattern match on the value. Value names start with capital letters. Names beginning with lowercase letters are symbolic constants called _atoms_ [^atoms].

[^atoms]: Like Ruby `:symbols` and Scala `'symbols`.

``` erlang
X = 123.
```

## Lists

Lists can be heterogeneous in Erlang.
