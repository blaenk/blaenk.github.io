---
title: Lua
published: September 1, 2013
excerpt: Embedding Lua JIT into a C++ project
comments: off
toc: off
---

LuaJIT has an FFI which allows code to be JIT compiled, whereas regular C API calls aren't [^jit]:

**Pros**:

  * Speed
  * Easier bindings done from the Lua side, given a simple C ABI of whatever needs binding. No unexpected code being generated that may present edge-case bugs which might be difficult to detect.

**Cons**:

  * Safety. Third-party scripting can easily crash the application
  * Bindings seem to be simpler from a C-language perspective. To this end, C++ programs would presumably have to write C ABI functions to be picked up/wrapped on Lua's end via the FFI [^cpp_wrapping]. This contrasts with automatically generated SWIG bindings from C++ to Lua's C API. However, I think a benefit is that it ends up being cleaner and more precise/predictable than generated code.

# Using FFI

Using the FFI in LuaJIT is pretty straightforward. A shared library has to be created that exposes the C functions:

~~~ {lang="cpp"}
extern "C" {
  DLLEXPORT void some_function();
  DLLEXPORT int get_count();
}
~~~

Where `DLLEXPORT` is `__declspec(dllexport)` on Windows. This can then be loaded by a Lua script:

~~~ {lang="lua"}
local ffi = require('ffi')

ffi.cdef[[
void some_function();
int get_count();
]]

local api = ffi.load("thelibrary")
api.some_function();
local count = api.get_count();
~~~

Official FFI Resources

  * [Tutorial](http://luajit.org/ext_ffi_tutorial.html)
  * [Library](http://luajit.org/ext_ffi.html)
  * [API](http://luajit.org/ext_ffi_api.html)
  * [Semantics](http://luajit.org/ext_ffi_semantics.html)

C wrapper development resources:

  * [Beautiful Native Libraries](http://lucumr.pocoo.org/2013/8/18/beautiful-native-libraries/)

# Callbacks

There is a [heavy overhead](http://luajit.org/ext_ffi_semantics.html#callback_performance) with C-to-Lua calls, generally found in the form of callbacks in which a C FFI function is provided a callback function written in Lua. The overhead is seemingly similar to the C API calls.

> Do not use callbacks for performance-sensitive work: e.g. consider a numerical integration routine which takes a user-defined function to integrate over. It's a bad idea to call a user-defined Lua function from C code millions of times. The callback overhead will be absolutely detrimental for performance.
>
> It's considerably faster to write the numerical integration routine itself in Lua — the JIT compiler will be able to inline the user-defined function and optimize it together with its calling context, with very competitive performance.

Instead of passing a callback to a C function that applies that function to a sequence, a C function should be created that generates/yields the sequence an element at a time, with the processing done in Lua:

> For new designs avoid push-style APIs: a C function repeatedly calling a callback for each result. Instead use pull-style APIs: call a C function repeatedly to get a new result. Calls from Lua to C via the FFI are much faster than the other way round. Most well-designed libraries already use pull-style APIs (read/write, get/put).

Here is a [benchmark](http://stackoverflow.com/a/12435278/101090) of the forms of callbacks.

# Resources

Resources about the language:

* [Introduction to LuaJIT](http://cellux.github.io/articles/introduction-to-luajit-part-1/)
* [Learn Lua in 15 Minutes](http://tylerneylon.com/a/learn-lua/)

Resources on LuaJIT:

* [Scripting with LuaJIT and selectively sandboxing the FFI](http://stackoverflow.com/questions/18376966/scripting-with-luajit-and-selectively-sandboxing-the-ffi)
    * [SandBoxes](http://lua-users.org/wiki/SandBoxes)
    * [FFI Untrusted Code](http://luajit.org/ext_ffi_semantics.html#policy)
* [FFI Embedding Talk #1](https://speakerdeck.com/igdshare/introduction-to-luajit-how-to-bind-cpp-code-base-using-luajit-ffi)
* [FFI Embedding Talk #2](http://vadimg.github.io/luajit-talk/)

Projects using LuaJIT FFI:

* [CuBeat](https://github.com/godfat/cubeat)
* <https://github.com/malkia/ufo>

[^jit]: [When using Luajit, is it better to use FFI or normal lua bindings?](http://stackoverflow.com/questions/16131793/when-using-luajit-is-it-better-to-use-ffi-or-normal-lua-bindings)
[^cpp_wrapping]: In [LuaJIT FFI/C++ binding, best approach?](http://lua-users.org/lists/lua-l/2011-07/msg00492.html), someone decided to use clang to parse each method and generate C wrappers. Others do it manually.
[^speakerdeck]: [LuaJIT C++ binding](https://speakerdeck.com/igdshare/introduction-to-luajit-how-to-bind-cpp-code-base-using-luajit-ffi)
