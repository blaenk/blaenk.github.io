---
title: Lua
published: September 1, 2013
excerpt: Notes about Lua
comments: off
---

LuaJIT has an FFI which allows code to be JIT compiled [^jit]:

* **Pros**:
    * Speed
    * Easier bindings done from the Lua side, given a simple C ABI of whatever needs binding
* **Cons**:
    * Safety. Third-party scripting can easily crash the application
    * Bindings seem to mainly be simpler from a C-language perspective --- after all, they _are_ made with C in mind. To this end, C++ programs would presumably have to write C ABI functions to be picked up/wrapped on Lua's end via the FFI [^cpp_wrapping]. This contrasts with automatically generated SWIG bindings from C++ to Lua's C API.

[^jit]: [When using Luajit, is it better to use FFI or normal lua bindings?](http://stackoverflow.com/questions/16131793/when-using-luajit-is-it-better-to-use-ffi-or-normal-lua-bindings)
[^cpp_wrapping]: In [LuaJIT FFI/C++ binding, best approach?](http://lua-users.org/lists/lua-l/2011-07/msg00492.html), someone decided to use clang to parse each method and generate C wrappers. Others do it manually.
[^speakerdeck]: [LuaJIT C++ binding](https://speakerdeck.com/igdshare/introduction-to-luajit-how-to-bind-cpp-code-base-using-luajit-ffi)
