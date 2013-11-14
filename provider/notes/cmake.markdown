---
title: CMake
published: September 1, 2013
excerpt: Making sense of scattered information
comments: off
toc: off
---

[CMake](http://www.cmake.org/) seems to me like the best available cross-platform build tool. It can generate Visual Studio solutions on Windows and Make files on POSIX systems. While it's widely used and there exists thorough reference documentation for it, I feel like there aren't many resources for getting started. As a result, I've written up some notes from what I've been able to infer from a variety of project's CMake configurations as well as the scant introductory information I was able to find.

# General Usage

CMake directives are stored in files named **CMakeLists.txt**, and there is usually one such file present for each sub-directory in the source tree, each one conventionally containing directives pertinent to the files in that directory. These separate CMake files are then "merged together" using the `add_subdirectory` directive, which immediately makes the CMake interpreter load and evaluate the CMake file in the provided directory.

Variables can be set using the `set` directive in the following format:

``` cmake
set (TEST "something")
```

There are certain built-in CMake variables that can affect the project. A commonly set one is `CMAKE_MODULE_PATH` which defines where CMake modules should be looked for.

Similarly, user-modifiable options can be created with the `option` directive which takes a variable to associate the value with, a description string, and a default value (`OFF` is the default):

``` cmake
option (USE_FFTW "use the fftw library" OFF)
```

Dependencies are generally found using the `find_package` directive. These are backed by CMake modules --- either built-in or found in the `CMAKE_MODULE_PATH` --- and handle the logic of searching for the libraries and headers of the particular package in various common locations.

If the search was successful, these modules typically set a variable of the form `PACKAGENAME_FOUND` which can be tested. Further, they also set variables such as `PACKAGENAME_LIBRARIES` and `PACKAGENAME_INCLUDE_DIRS` --- sometimes singular, sometimes plural.

In pertinent CMake files, these variables set by the `find_package` module are then used to resolve any dependencies in the code. For example, required headers can be added to the set of directories searched by the compiler using the `include_directories` directive:

``` cmake
include_directories (${PACKAGENAME_INCLUDE_DIR})
```

Similarly, a target can be linked with a library with the `target_link_libraries` directive:

``` cmake
target_link_libraries (sometarget ${PACKAGENAME_LIBRARIES})
```

Targets are essentially products of the build process: oftentimes this is either an executable or a library, static or shared. An executable can be created using the `add_executable` directive:

``` cmake
add_executable (sometarget file.cpp)
```

In the above, **file.cpp** refers to the file containing the `main` entry point. Likewise, a library can be created using the `add_library` directive:

``` cmake
add_library (sometarget STATIC ${SRC})
```

In the above, `${SRC}` refers to the set of files whose resultant code would constitute the library. These variables are simply created using the `set` directive:

``` cmake
set (SRC somefile.cpp anotherfile.cpp)
```

Notice that the header files aren't necessary in the set since the C++ files already `#include` them. However, when using the Visual Studio generator on Windows, this means that the header files will be absent from the project. This can be resolved by adding a conditional for Windows and adding them in:

``` cmake
if (WIN32)
  set (SRC somefile.h anotherfile.h)
endif (WIN32)
```

Alternatively, the header files can be added in to the set unconditionally.

Furthermore, on Windows, the `source_group` directive can help organize source files into separate projects in the Visual Studio solution:

``` cmake
source_group (sometarget FILES ${SRC})
```

Libraries built in project are **automatically found**, there's no need for `include_directories` or `find_library`. Try to avoid `link_directories`.

It's possible to wire-up what would happen upon invoking `make install` by using the `install` directive:

``` cmake
install (TARGET sometarget
         RUNTIME DESTINATION bin
         LIBRARY DESTINATION lib)
```

This would install to **/usr/local/bin** and **/usr/local/lib** on POSIX.

A configuration include file can be created using `configure_file`:

``` cmake
configure_file ("Config.h.in" "Config.h")
```

In this configuration file, variables can be interpolated based on their values in the CMake build system:

``` cpp
#cmakedefine VAR // becomes #define VAR if VAR is true
#define @VAR@    // replaced with value of VAR
```

The `add_custom_target` directive creates a custom target that is always rebuilt. Similarly, `add_custom_command` runs a command before or after a build, or before a link.

pkg-config is discouraged.

# SWIG Bindings

It's possible to automatically generate [SWIG](http://www.swig.org/) bindings using built-in CMake modules. The following creates a **swig.(dll|so|dylib)** which a Lua script can subsequently `require`.

``` cmake
find_package (SWIG)
include (UseSWIG)

include_directories (${CMAKE_CURRENT_SOURCE_DIR} ${LUAJIT_INCLUDE_DIR})
set_source_files_properties (swig.i PROPERTIES CPLUSPLUS ON)
swig_add_module (script lua swig.i)
swig_link_libraries (script ${LUAJIT_LIBRARIES})
```

If instead it is desired to statically link the bindings to an existing target, one can create a custom target:

``` cmake
add_custom_command (
  OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/swig.cpp
  COMMAND ${SWIG_EXECUTABLE} -lua -c++
          -o ${CMAKE_CURRENT_SOURCE_DIR}/swig.cpp
          ${CMAKE_CURRENT_SOURCE_DIR}/swig.i
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/swig.i)

add_library (script STATIC swig.cpp)
add_dependencies (script swig)

include_directories (${LUAJIT_INCLUDE_DIR})
target_link_libraries (script ${LUAJIT_LIBRARIES})
```

Then the target that wants to link with it just needs to `add_dependencies`. Then the library should be opened using the generated `luaopen_script` function which the program should of course declare as `extern`.

It is common to place generated files in the `${CMAKE_BINARY_DIR}` directory to avoid clobbering the source directory.

# Visual Studio

CMake works fine with Visual Studio but there are a few things to consider. The property for the working directory in the Debugging section should most likely be set to `$(OutDir)`. Likewise, the start-up project must be set manually as it's set to ALL_BUILD by default. ALL_BUILD is a project that builds all projects and correctly triggers any scripts. ZERO_CHECK is a project that runs and, if any CMake files have been changed, asks to reload Visual Studio.

Furthermore, if you're making a Windows application, you should add the WIN32 parameter to `add_executable` to instruct the compiler to use the `WinMain` [entry-point](http://msdn.microsoft.com/en-us/library/f9t8842e.aspx) and WINDOWS [subsystem](http://msdn.microsoft.com/en-us/library/fcc1zstk%28v=vs.110%29.aspx):

``` cmake
add_executable (target WIN32 ${SOURCES})
```

# Resources

* [Learning CMake](http://www.elpauer.org/stuff/learning_cmake.pdf)
* [CMake Tips](http://web.cs.swarthmore.edu/~adanner/tips/cmake.php)
* [CMake Tutorial](http://www.cmake.org/cmake/help/cmake_tutorial.html)
* [Writing Find Modules](http://www.vtk.org/Wiki/CMake:How_To_Find_Libraries)
* [CMake FAQ](http://www.cmake.org/Wiki/CMake_FAQ)
