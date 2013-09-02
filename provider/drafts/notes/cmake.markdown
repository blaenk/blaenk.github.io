---
title: CMake
published: September 1, 2013
excerpt: Notes about CMake
comments: off
---

[CMake](http://www.cmake.org/) seems to me like the best available cross-platform build tool. It can generate Visual Studio solutions on Windows and Make files on POSIX systems.

Here are some of the resources I've found to be helpful:

* [Learning CMake](http://www.elpauer.org/stuff/learning_cmake.pdf)
* [CMake Tips](http://web.cs.swarthmore.edu/~adanner/tips/cmake.php)

# General Usage

CMake directives are stored in files named **CMakeLists.txt**, and there is usually one such file present for each sub-directory in the source tree, each one conventionally containing directives pertinent to the files in that directory. These separate CMake files are then "merged together" using the `add_subdirectory` directive, which immediately makes the CMake interpreter load and evaluate the CMake file in the provided directory.

Variables can be set using the `set` directive in the following format:

~~~ {lang="cmake"}
set (TEST "something")
~~~

There are certain built-in CMake variables that can affect the project. A commonly set one is `CMAKE_MODULE_PATH` which defines where CMake modules should be looked for.

Dependencies are generally found using the `find_package` directive. These are backed by CMake modules --- either built-in or found in the `CMAKE_MODULE_PATH` --- and handle the logic of searching for the libraries and headers of the particular package in various common locations.

If the search was successful, these modules typically set a variable of the form `PACKAGENAME_FOUND` which can be tested. Further, they also set variables such as `PACKAGENAME_LIBRARIES` and `PACKAGENAME_INCLUDE_DIRS` --- sometimes singular, sometimes plural.

In pertinent CMake files, these variables set by the `find_package` module are then used to resolve any dependencies in the code. For example, required headers can be added to the set of directories searched by the compiler using the `include_directories` directive:

~~~ {lang="cmake"}
include_directories (${PACKAGENAME_INCLUDE_DIR})
~~~

Similarly, a target can be linked with a library with the `target_link_libraries` directive:

~~~ {lang="cmake"}
target_link_libraries (sometarget ${PACKAGENAME_LIBRARIES})
~~~

Targets are essentially products of the build process: oftentimes this is either an executable or a library, static or shared. An executable can be created using the `add_executable` directive:

~~~ {lang="cmake"}
add_executable (sometarget file.cpp)
~~~

In the above, **file.cpp** refers to the file containing the `main` entry point. Likewise, a library can be created using the `add_library` directive:

~~~ {lang="cmake"}
add_library (sometarget STATIC ${SRC})
~~~

In the above, `${SRC}` refers to the set of files whose resultant code would constitute the library. These variables are simply created using the `set` directive:

~~~ {lang="cmake"}
set (SRC somefile.cpp anotherfile.cpp)
~~~

Notice that the header files aren't necessary in the set since the C++ files already `#include` them. However, when using the Visual Studio generator on Windows, this means that the header files will be absent from the project. This can be resolved by adding a conditional for Windows and adding them in:

~~~ {lang="cmake"}
if (WIN32)
  set (SRC somefile.h anotherfile.h)
endif (WIN32)
~~~

Alternatively, the header files can be added in to the set unconditionally.

Furthermore, on Windows, the `source_group` directive can help organize source files into separate projects in the Visual Studio solution:

~~~ {lang="cmake"}
source_group (sometarget FILES ${SRC})
~~~

# Other Notes

Libraries built in project are **automatically found**, there's no need for `include_directories` or `find_library`. Try to avoid `link_directories`.

It's possible to wire-up what would happen upon invoking `make install` by using the `install` directive:

~~~ {lang="cmake"}
install (TARGET sometarget
         RUNTIME DESTINATION bin
         LIBRARY DESTINATION lib)
~~~

This would install to **/usr/local/bin** and **/usr/local/lib** on POSIX.

A configuration include file can be created using `configure_file`:

~~~ {lang="cpp"}
#cmakedefine VAR // becomes #define VAR if VAR is true
#define @VAR@    // replaced with value of VAR
~~~

The `add_custom_target` directive creates a custom target that is always rebuilt. Similarly, `add_custom_command` runs a command before or after a build, or before a link.

pkg-config is discouraged.

# SWIG Bindings

It's possible to automatically generate [SWIG](http://www.swig.org/) bindings using built-in CMake modules. The following creates a **swig.(dll|so|dylib)** which a Lua script can subsequently `require`.

~~~ {lang="cmake"}
find_package (SWIG)
include (UseSWIG)

include_directories (${CMAKE_CURRENT_SOURCE_DIR} ${LUAJIT_INCLUDE_DIR})
set_source_files_properties (swig.i PROPERTIES CPLUSPLUS ON)
swig_add_module (script lua swig.i)
swig_link_libraries (script ${LUAJIT_LIBRARIES})
~~~

If instead it is desired to statically link the bindings to an existing target, one can create a custom target:

~~~ {lang="cmake"}
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
~~~

For completeness, the statically-linked approach then requires the program that links with it to open the library using the generated `luaopen_script` function which the program should of course declare as an `extern`.
