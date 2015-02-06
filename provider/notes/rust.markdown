---
title: Rust
published: December 29, 2013
excerpt: An exciting multiparadigm language from Mozilla
comments: off
toc: left
---

I took a shot at [learning Go] recently and I found its simplicity to be refreshing. Rust is a similar language from Mozilla which I've been interested in for a while now.

My main resources are the [tutorial] and [manual], but there are [many more]. As usual, oftentimes some things will be directly from the source, with my commentary surrounding it. There is also a more basic [Rust by Example] which is similar to the go tour.

[learning Go]: /notes/go/
[tutorial]: http://doc.rust-lang.org/tutorial.html
[manual]: http://doc.rust-lang.org/rust.html
[many more]: http://doc.rust-lang.org/
[Rust by Example]: http://rustbyexample.com/

* toc

# Types

There are no implicit type conversions. Instead, the `as` keyword is used to perform explicit type conversions. The `type` keyword can be used to create type aliases.

Category          Types
---------         ------
signed integers   `i8`, `i16`, `i32`, `i64`, `int` (word size)
unsigned integers `u8`, `u16`, `u32`, `u64`, `uint` (word size)
floating point    `f32`, `f64`
Unicode scalars   `char`
booleans          `bool`

The `Box<T>` type represents a boxed value allocated on the heap. The `Vec<T>` type represents a growable vector. The `String` type represents a growable UTF-8 encoded buffer.

Nearly every statement in Rust is an expression that yields a value, but this can be suppressed by adding a trailing semicolon `;`, which makes the result be unit `()`.

The `Result<T, E>` type is similar to Haskell's `Either`, containing either `Ok` or `Err`. The `try!` macro makes it easier to work with `Result`, expanding to a `match` expression that returns the contained error or evaluates to the. This reminds me of the `Either` monad.

Types are categorized into _kinds_ based on various properties of the components of the type.

* `Send` is for types that can be safely sent between tasks. Includes scalars, owning pointers, owned closures, and structural types containing only other owned types. All `Send` types are `'static`
* `Share` for types that are thread-safe
* `Copy` is for types that consist of Plain Old Data. Includes scalars, immutable references, and structural types containing other `Copy` types
* `'static` is for types that don't contain any references except those with `static` lifetime
* `Drop` is for types that need to be cleaned up by a destructor. Only `Send` types can implement `Drop`
* Default is for types with destructors, closure environments, or other non-first-class types which aren't copyable at all and are only accessible through pointers

# Variables

Variables are immutable by default and are defined using the `let` keyword, though one can be made mutable by providing the `mut` keyword after `let`.

``` rust
let x     = 3; // immutable
let mut y = 4; // mutable
```

Constants represent a value, not a memory address, and can be declared using the `const` keyword. Constants can be declared in any scope, can't be shadowed, and are considered rvalues. Constants are essentially inlined wherever they are used, like `#define` constants in C.

``` rust
const age: int = value;
```

Global variables represent a memory address and can be declared using the `static` keyword. Static variables can be created using the `static` keyword and always have a single address. If the static doesn't contain an `UnsafeCell` then it may be placed in read-only memory.

``` rust
static LANGUAGE: &'static str = "Rust";
static THRESHOLD: int = 10;
```

It's also possible to define mutable statics, though using these is considered unsafe as it can introduce race conditions. Because of this, mutable static can only be read or written to within `unsafe` blocks, and can't be placed in read-only memory.

``` rust
static mut LEVELS: uint = 0;

unsafe fn bump_levels_unsafe1() -> uint {
  let ret = LEVELS;
  LEVELS += 1;
  return ret;
}
```

# Modules

The content of all source code that the compiler directly had to compile in order to end up with a particular binary is collectively called a _crate_. A crate is a unit of independent compilation in Rust. Using an already compiled library in code doesn't make it part of a crate.

There exists a hierarchy of modules where the root is referred to as _crate root_. Global paths begin with the root path `::`, all other paths are local paths, similar to the distinction between absolute `/`-prefixed paths and relative paths in a POSIX file system.

Everything in Rust is private by default with a single exception. If an enumeration is declared public, then its variants are public as well by default, though this may be overridden with the `priv` keyword.

Visibility restrictions are only applicable at module boundaries, so that private items are available within the same module and its descendants. Descendants still have to bring the parent items into view using `use`, however---this simply means that they have permission view private items.

Source files and modules are not the same thing. The only file that's relevant when compiling is the one that contains the body of the crate root. Declaring a module without a body prompts the compiler to look for a file named after that module or for a file named `mod.rs` in a folder named after that module, and use its contents as the module's body.

The `path` attribute may be used to manually specify a module's path.

``` rust
#[path = "manual.rs"]
mod aliased;

mod plants;
mod animals {
  mod fish;
  mod mammals {
    mod humans;
  }
}

// looks for:

// src/manual.rs

// src/plants.rs
// src/plants/mod.rs

// src/animals/fish.rs
// src/animals/fish/mod.rs

// src/animals/mammals/humans.rs
// src/animals/mammals/humans/mod.rs
```

The `use` statement can be used to bring in a module's contents into the current block by providing a global path without the `::` prefix. It's possible to prefix with `super::` to start the path in the parent module, and `self::` to start the path in the current module.

``` rust
use super::some_parent_item;
use self::some_child_module::some_item;
```

Imported items are shadowed by local definitions. To make this fact more explicit, `use` statements must go before any declaration, including `mod` declarations. This looks awkward when importing items from a module that follows:

``` rust
use farm::cow;

mod farm {
  pub fn cow() {
    println!("moo")
  }
}

fn main() {
  cow()
}
```

This is even more awkward when using a `mod` statement that looks in a separate file for the contents:

``` rust
use b::foo;
mod b;

fn main() { foo(); }
```

These restrictions have been removed by [RFC #385], so that it's now possible to use any order:

[RFC #385]: https://github.com/rust-lang/rfcs/blob/master/text/0385-module-system-cleanup.md

``` rust
mod b;
use b::foo;
```

It's possible to import by wildcard or selectively, similar to Scala. It's also possible to rename an imported item:

``` rust
use farm::{chicken, cow};

#[feature(globs)]
use farm::*;

use farm::chicken as egg_layer;
```

It's possible to make a module available through another module by using `pub mod`. This allows the exposed module's public items to be accessible using path-notation.

``` rust
// secondmod.rs
pub fn hi() { println!("hi"); };

// firstmod.rs
pub mod secondmod;

// main.rs
firstmod::secondmod::hi();
```

Likewise, it's possible to re-export specific items to make them appear to be part of the module in which they're exposed, using `pub use`.

``` rust
// firstmod.rs
pub use secondmod::hi;
mod secondmod;

// main.rs
firstmod::hi();
```

It's common to use existing libraries, which in rust are simply referred to as crates. The `extern crate` declaration is used to reference other crates, similar to `extern` in C.

Items imported via `extern crate` can be shadowed by local declarations and by items imported by `use`, so they must go before both. Crates declared in an `extern crate` declaration are looked for in the library search path, which can be expanded with the `-L` switch.

``` rust
extern crate num;

use farm::dog;
use num::rational::Ratio;

mod farm {
  pub fn dog() { println!("woof") }
}

fn main() {
  farm::dog();
  let one_half = ::num::rational::Ratio::new(1, 2);
}
```

Crates can contain metadata used for the resultant libraries:

``` rust
#![crate_name = "farm"];

#![desc = "Farm"]
#![license = "BSD"]
#![comment = "Farm library"]

// specify that it's a library
#![crate_type = "lib"];
```

This information can be used to select the crate from the `extern crate` declaration:

``` rust
extern crate farm = "farm";
```

For example, here's an example library, its compilation, and usage:

~~~ {.rust text="world.rs"}
#[crate_name = "world"];
#[crate_type = "lib"];

pub fn explore() -> &'static str { "world" }
~~~

~~~ {.rust text="main.rs"}
extern crate world;
fn main() {
  println!("hello {}", world::explore());
}
~~~

~~~ {.bash text="compilation"}
$ rustc world.rs     # compiles libworld-<HASH>-0.42.so
$ rustc main.rs -L . # include local dir in library search path
$ ./main
"hello world"
~~~

Many predefined items such as `range` and `Option<T>` come from the standard library's prelude, similar to Haskell's. The rust compiler automatically inserts the following into the crate root:

``` rust
extern crate std;

// to prevent: in crate root
#[no_std];
```

As well as the following into every module body:

``` rust
use std::prelude::*;

// to prevent: in any module
#[no_implicit_prelude];
```

Attributes can be used to specify conditional compilation of code, crate names, disabling or enabling compiler features, and so on. The `cfg` attribute can test for configuration values passed by the rust compiler either implicitly or explicitly with the `--cfg` flag.

``` rust
// crate-wide attribute
#![crate_attribute]

// module-wide attribute
#[module_attribute]

#[cfg(target_os = "linux")]
fn on_linux() {
  println!("on linux")
}

#[cfg(not(target_os = "linux"))]
fn not_on_linux() {
  println!("not on linux")
}
```

# Ownership

An object's lifetime is determined by its owner, either a variable or a task-local garbage collector. Ownership is recursive so that mutability is inherited recursively and a destructor destroys the contained tree of owned objects. Variables are owners and destroy the contained object when they go out of scope.

``` rust
struct Foo { x: int, y: box int }

{
  // `a` is owner of struct and its fields
  let a = Foo { x: 5, y: box 10 };
}
// `a` goes out of scope, destructor for y's `box int` is called

// `b` is mutable, so the objects it owns are also mutable
let mut b = Foo { x: 5, y: box 10 };
b.x = 10
```

Notice that a structure may contain a mutable reference, so that even if the structure is immutable, the pointed-to value can be mutated. Naturally, the reference itself can't be changed since the structure is immutable.

``` rust
struct S1 {
  b: int
}

struct S2<'a> {
  a: &'a mut S1
}

let mut s1 = S1 { b: 56 };
let s2 = S2 { a: &mut s1 };

s2.a.b = 45;    // legal
s2.a = &mut s1; // illegal
```

## Boxes

Values are allocated on the stack by default, but they can also be _boxed_, i.e. allocated on the heap. Values are boxed with the `box` keyword and result in a value of type `Box<T>`. Box values can be dereferenced using the `*` operator, or they can be destructured by using the `box` keyword on the LHS.

*[LHS]: Left-Hand Side

``` rust
enum List {
  Cons(u32, Box<List>),
  Nil
}

let list = Cons(1, box Cons(2, box Cons(3, box Nil)));
```

The `list` represents an owned tree of values.

It's not necessary to worry about manually boxing large return values or arguments, they are essentially passed by reference if they are larger than the size of the machine's register. To quote the tutorial:

> A large value is returned via a hidden output parameter, and the decision on where to place the return value should be left to the caller

What this essentially means is that if for example function `foo()` returns `BigStruct`, the compiler will pass `foo()` a pointer to an uninitialized `BigStruct` in the local scope, something like:

``` rust
let x: BigStruct; // uninitialized
foo(&x); // as if type: foo(_ret: &mut BigStruct)
```

This allows the caller to decide where to place the return value, for example:

``` rust
fn foo() -> (u64, u64, u64, u64, u64, u64) {
  (5, 5, 5, 5, 5, 5)
}

// allocates result in a box and writes u64's directly to it
let x = box foo();
```

## Managed Boxes

There are other ownership strategies that can be employed, such as task-local garbage collected and reference counted. Reference counted ownership [^cpp11_shared_ptr] is possible through `std::rc::Rc`:

[^cpp11_shared_ptr]: This of course reminds me of C++11's [`std::shared_ptr`](http://en.cppreference.com/w/cpp/memory/shared_ptr).

``` rust
use std::rc::Rc;

let x = Rc::new([1, 2, 3]);
let y = x.clone(); // a new owner
let z = x; // moves x into z

assert_eq!(*z.borrow(), [1, 2, 3]);

let mut a = Rc::new([1, 2]);
a = z; // variable is mutable, not its contents
```

## Cells

Mutability can't be inherited when shared ownership is involved, since the contained values may be multiply-aliased, so boxes are always immutable and the contained values can only be borrowed as shared references, not mutable references.

However, dynamic mutability is possible via types like `Cell` and `RefCell` where freezing is handled via dynamic checks which can fail at run-time. Specifically, `Cell` is for types that implement `Copy`, and `RefCell` is for all others.

`Rc` and `Gc` types are not sendable, so they can't be used to share memory between tasks. This is instead possible via the `extra::arc` module.

``` rust
let shared_map: Rc<RefCell<_>> = Rc::new(RefCell::new(HashMap::new()));
shared_map.borrow_mut().insert("africa", 92388i);
shared_map.borrow_mut().insert("kyoto", 11837i);
shared_map.borrow_mut().insert("piccadilly", 11826i);
```

A `Cell` may also be used to hide mutability for operations that appear to be immutable. A good example of this is `Rc` whose `clone()` method must increment the reference count, but `clone` is not expected to change the source value, which is [why it takes] `&self` and not `&mut self`. This can be reconciled by storing the reference count in a `Cell`.

[why it takes]: http://doc.rust-lang.org/std/clone/trait.Clone.html

``` rust
struct Rc<T> { ptr: *mut RcBox<T> }
struct RcBox<T> { value: T, refcount: Cell<uint> }

impl<T> Clone for Rc<T> {
  fn clone(&self) -> Rc<T> {
    unsafe {
      (*self.ptr).refcount.set((*self.ptr).refcount.get() + 1);
      Rc { ptr: self.ptr }
    }
  }
}
```

## Moving

Moving in Rust is similar to [C++11 move semantics], where the "move" refers to moving ownership. Rust performs a shallow copy for parameter passing, assignment, and returning from functions. Performing such a shallow copy is treated by Rust as "moving ownership" of the value, so that the original source location can no longer be used unless it is reinitialized. A move can be avoided by cloning:

[C++11 move semantics]: /notes/cpp#move-semantics

``` rust
let mut xs = Nil;
let ys = xs;

// error if attempt to use `xs`

xs = Nil; // xs can be used again

let x = box 5;
let y = x.clone(); // y is a newly allocated box
let z = x; // no new memory alloc'd, x can no longer be used
```

Mutability can be changed by moving it to a new owner:

``` rust
let r = box 13;
let mut s = r; // box becomes mutable
*s += 1;
let t = s; // box becomes immutable
```

## Borrowing

References don't imply ownership, they're "borrowed." Reference parameters are often used to allow functions to work with all manner of different allocated types.

``` rust
fn distance(p1: &Point, p2: &Point) -> f64 {
  let x = p1.x - p2.x;
  let y = p1.y - p2.y;
  sqrt(x * x + y * y)
}
```

This can be used on stack-allocated variables by using the address-of operator `&` to borrow the local variable, to create an alias for it, i.e. another route to the same data. Managed and owned boxes, on the other hand, are automatically converted to references, in which case the "borrowed" metaphor is more like "lent out".

If the contents of a variable are lent out then the variable can't be sent to another task. Also, no actions can be taken that may cause the borrowed value to be freed or to change its type. It's necessary to wait for the borrowed value to be returned, i.e. reference to go out of scope, before making full use of it again.

Furthermore, the act of lending immutable pointers to variables freezes the objects they point to and prevents their mutation until the reference is destroyed (i.e. out of scope):

``` rust
let mut x = 5;
{
  let y = &x; // x is frozen
}
// x is unfrozen
```

Pointers and boxes are uniformly dereferenced with `*`. The pointer `borrowed` can be read as taking the reference of the mutable `value`, that is, the `mut` is part of the name. Note that the precedence levels of `*` and `.` are similar to those in C/C++, making for the awkward parenthesized dereferencing syntax. Unlike C/C++, there is no `->` shortcut. Instead, there is automatic pointer dereferencing through `*` and `[]`, which applies to any number of levels of indirection:

``` rust
let mut owned = box 20;
let mut value = 30;
let borrowed = &mut value;

*owned = *borrowed + 100;
*borrowed = 1000;

let point = box Point { x: 10.0, y: 20.0 };
println!("{:f}", point.x);
```

## References

The `&` symbol has multiple context-dependent meanings in Rust, as in C++ [^cpp_references]. Remembering that expressions are for constructing and patterns for destructuring, a `&` in a pattern _dereferences_ the expression being destructured. This can be useful to avoid having to manually dereference the variable within the body using `*`.

For example, the `fold` function takes a folding operation in the form of a closure that is passed the accumulator and a reference `&T` to the current element, which means that within the body it would have to be manually dereferenced with `*`, and we can avoid that by specifying the `&` in the pattern.

[^cpp_references]: In C++, an `&` on the LHS can be the ref-qualifier, i.e. _bind by reference_, and on the RHS it can be the address-of operator.

*[RHS]: Right-Hand Side

``` rust
let manual  = v.iter().fold(0, |a, b| a + *b);
let pattern = v.iter().fold(0, |a, &b| a + b);
```

It's also necessary when attempting to match on borrowed values. In this case, it reads as _dereference the variable then see if it matches the pattern_.

``` rust
let x = Some(5);
let y = (&x, 2);

match y {
  (&Some(a), b) => a + b,
  (_, a) => a
}
```

A `&` in a function signature means that the parameter accepts a reference. Owned pointers `box` and types that implement the `Deref` trait are automatically converted to references when passed to such functions. Variables on the stack, on the other hand, can yield a reference manually using `&`, which is the address-of operator in that context.

``` rust
fn eq(xs: &int, ys: &int) -> bool { ... }

let xs = box 34;
let ys = 34;
assert!(eq(xs, &ys));
```

A `&` that's applied to an rvalue is a shorthand for creating a temporary and taking its address.

``` rust
let explicit = Point { x: 3.0, y: 4.0 };
reference_taking_func(&explicit);

let shorthand = &Point { x: 3.0, y: 4.0 };
reference_taking_func(shorthand);
```

A mutable reference `&mut` is one through which the pointed-to variable can be modified, provided the pointed-to variable is also mutable. When a mutable reference exists to an object, no other reference can exist to the same object, regardless of whether it's mutable or not. Compare this with a regular, immutable reference of which many can exist to the same variable, since the variable cannot be modified through it.

There is also the `ref` keyword that can be used in a pattern. In this context, it is similar to C++'s ref-qualifier, which means bind by reference. This is required when matching on something that can't or we don't want to be taken by value. It can be used with `box` and `mut` as well.

For example, we want to get a reference to the `Foo` in `&Option<Foo>`. Taking it by value would require moving ownership, but since the `Option` is borrowed we don't have ownership to begin with. Instead we bind the `foo` by reference which does away with the need to take ownership.

``` rust
let some = Some(foo);
let borrowed = &some;

match borrowed {
  &Some(ref foo) => *foo
}

let mut tuple (box 5u, 3u);
{
  let (box ref mut i, _) = tuple;
  *i = 3;
}
```

Another example is the `Option` type's [`as_mut`](http://doc.rust-lang.org/core/option/enum.Option.html#method.as_mut) method which converts an `Option<T>` to `Option<&mut T>`. It essentially provides a mutable reference to the contained value to facilitate its mutation.

In order to avoid moving the reference out of the `Option` and thereby invalidating it later on, it's necessary to bind it by reference with `ref`.

However, this would yield a `& &mut uint`, and since it's not a mutable reference it would prevent us from mutating the value, otherwise it would essentially break the rule of not allowing more than one mutable reference to any one thing.

For this reason, it's necessary to bind it by mutable reference with `ref mut`, yielding a `&mut &mut uint`, allowing us to mutate the value by doing `**v = 1;`. To avoid having to dereference twice, we would include the dereference operator `&` in the pattern position, to finally arrive at `&ref mut v`.

``` rust
let mut x = Some(2u);
match x.as_mut() {
  Some(&ref mut v) => *v = 42,
  None => {},
}

assert_eq!(x, Some(42u));
```

There's also a language inconsistency where `"str"` is of type `&str`, but to get a type of `&&str` it doesn't suffice to simply do `&"str"`, because that _too_ is treated as `&str`, so _two_ are needed: `&&str`.

In the following example, `contains_key` takes a value of type `&K` where `K` is the key type. The key type is `&str`, so `&K` is `&&str`. Because of the language inconsistency noted above, two `&`'s must prefix the string literal in order to yield a `&&str` to pass to `contains_key`. Alternatively, a single `&` can be used if the string literal is surrounded in parentheses.

Issue [#10105] is a proposal to fix this. I've also read that [DST] will fix this as a consequence of its implementation.

[#10105]: https://github.com/rust-lang/rust/issues/10105
[DST]: https://github.com/rust-lang/rust/issues/6308

``` rust
use collections::Hashmap;

// type inferred as HashMap<&str, &str>
let mut items = HashMap::new();

items.insert("key1", "value1");
items.insert("key2", "value2");

if !items.contains_key(& &"key1") {
  println!("not present");
}

// or
if !items.contains_key(&("key1")) {
  println!("not present");
}
```

## Lifetimes

Objects have lifetimes which aid the borrow checker in enforcing valid borrowing. The compiler automatically sets the lifetimes. In the following example, the compiler may give `integer` lifetime `'i` and `ref_to_int` lifetime `'r` with type `&'i int` denoting that it's a reference type that points to an `int` of lifetime `'i`.

``` rust
let integer: int = 5
let ref_to_int: &int = &integer;
```

It's possible to give a lifetime a name in order to explicitly specify the lifetime of a returned reference. In the following case, the named lifetime `'r` is associated with the parameter `p: &Point`, this named lifetime is then also mentioned in the return type. What this means is that the returned reference will have the same lifetime as the passed in pointer parameter `p`. In effect, this means that the returned reference will remain valid as long as `p` itself remains valid.

This is necessary because, generally it's only possible to return references derived from a parameter to the function, and named lifetimes explicitly specify which parameter that is. The lifetime is something that's implicitly passed in from the caller's context, just like the parameter.

``` rust
struct Point { x: f64, y: f64 }

fn get_x<'r>(p: &'r Point) -> &'r f64 {
  &p.x
}
```

Named lifetimes allow grouping of parameters by lifetime. The following means that all of the parameters are assigned the same named lifetime `r`, so that in the caller the lifetime `r` will be the intersection of the lifetime of the three region parameters.

``` rust
fn select<'r, T>(shape: &'r Shape, threshold: f64, a: &'r T, b: &'r T) -> &'r T {
  if compute_area(shape) > threshold {
    a
  } else {
    b
  }
}
```

For example, if `select()` were called by the following function it would produce an error. The reason is that the lifetime of the first parameter to `select()` is the function body of `select_unit()`. The second two parameters to `select()` share a lifetime in `select_unit()`, so there are actually two lifetimes. The intersection between these two lifetimes would be the `Shape`'s lifetime which is the function body, so the return value of the `select()` call will be the function body of `select_unit()`.

This would lead to a _compilation error_, because `select_unit()` is supposed to return a value with a lifetime of `r`, which is the lifetime of `a` and `b`, _not_ the lifetime of the local `Circle`.

``` rust
fn select_unit<'r, T>(threshold: f64, a: &'r T, b: &'r T) -> &'r T {
  let shape = Circle(Point { x: 0., y: 0. }, 1.);
  select(&shape, threshold, a, b);
}
```

This can be resolved by creating another lifetime, `'tmp` in `select()` and assign it to the `shape` parameter, in order to distinguish it from the lifetime of the last two parameters. However, since we wouldn't be using the `'tmp` lifetime and the compiler is guaranteed to generate a unique lifetime for ever parameter, it's simpler to simply omit it altogether:

``` rust
fn select<'r, T>(shape: Shape, threshold: f64, a: &'r T, b: &'r T) -> &'r T {
  if compute_area(shape) > threshold {
    a
  } else {
    b
  }
}
```

It's also necessary to specify explicit lifetimes in structures that contain references.

``` rust
struct Pair<'a, 'b> {
  one: &'a mut int,
  two: &'b mut int,
}
```

It's also possible to use named lifetime notation to label control structures, allowing for breaking and continuing to specific locations.

``` rust
'h: for i in range(0,10) {
  'g: loop {
    if i % 2 == 0 { continue 'h; }
    if i == 9 { break 'h; }
    break 'g;
  }
}
```

### Elision

RFC [#141] proposed expanded lifetime elision rules. The practical effect of this is that a lot of omitted, or _elided_, lifetime annotations can be inferred by the compiler.

*[RFC]: Request for Comments
*[PR]: Pull Request

[#141]: https://github.com/rust-lang/rfcs/blob/master/text/0141-lifetime-elision.md

To facilitate this, the compiler has notions of input or output. For _functions_, inputs are the lifetimes on arguments and outputs are the lifetimes on result types; this _doesn't_ include lifetimes that appear in the method's `impl` or `trait` header. For _implementations_, inputs are the lifetimes on the type receiving the implementation, and the outputs are the lifetimes on the trait.

``` rust
// elided one input and two outputs
fn foo(s: &str) -> (&str, &str)

// input:  'a, 'c
// output: 'b, 'c
impl<'a, 'b, 'c> Trait<'b, 'c> for Type<'a, 'c>
```

Given this distinction between lifetime parameters, the rules are simple:

1. each elided lifetime in input position becomes a distinct lifetime parameter, as was already the case

    ``` rust
    fn print(s: &str);
    fn print<'a>(s: &'a str);

    impl Reader for (&str, &str) { ... }
    impl<'a, 'b> Reader for (&'a str, &'b str) { ... }
    ```

2. if there's only one input lifetime position, its lifetime is assigned to all elided output lifetimes

    ``` rust
    fn substr(s: &str, until: uint) -> &str;
    fn substr<'a>(s: &'a str, until: uint) -> &'a str;

    impl StrSlice for &str { ... }
    impl<'a> StrSlice<'a> for &'a str { ... }
    ```

3. if there are multiple input lifetime positions and one of them is `&self` or `&mut self`, the lifetime of `self` is assigned to all elided output lifetimes

    ``` rust
    fn args<T: ToCStr>(&mut self, args: &[T]) -> &mut Command
    fn args<'a, 'b, T: ToCStr>(&'a mut self, args: &'b [T]) -> &'a mut Command
    ```

4. otherwise, it's an error to elide an output lifetime

    ``` rust
    fn get_str() -> &str;
    fn frob(s: &str, t: &str) -> &str;
    ```

Lifetime elision with implementations is a bit trickier. Elision inference occurs at two levels: the implementation header line and its methods. Expansion can occur in trait methods since they're treated as any other function.

``` rust
trait Bar<'a> {
  fn bound(&'a self) -> &int { ... }
  fn fresh(&self) -> &int { ... }
}

// fresh expands like any other function
trait Bar<'a> {
  fn bound(&'a self) -> &'a int { ... }
  fn fresh<'b>(&'b self) -> &'b int { ... }
}
```

In general, the elided signatures of the `impl` should match those of the `trait` so that the `impl` expansion matches the `trait` and thus will compile. Failing to do this might break the relationship between an `impl` header-level lifetime and a method's lifetime.

Consider the following example where the `impl` header-level was expanded, and then the `bound` method expanded separately with a distinct lifetime (as per rule #1) even though they're supposed to be the same as specified in the `trait`.

``` rust
impl Bar for &str {
  fn bound(&self) -> &int { ... }
}

// bound's lifetime should've been 'a
impl<'a> Bar<'a> for &'a str {
  fn bound<'b>(&'b self) -> &'b int { ... }
}
```

If the signatures are used exactly as in the `trait`, with the same lifetime elisions, the `impl` expansion will most likely expand to match the `trait`'s expansion.

``` rust
impl<'a> Bar<'a> for &'a str {
  fn bound(&'a self) -> &'a int { ... }
  fn fresh(&self) -> &int { ... }
}

// fresh method expands correctly
impl<'a> Bar<'a> for &'a str {
  fn bound(&'a self) -> &'a int { ... }
  fn fresh<'b>(&'b self) -> &'b int { ... }
}
```

## Dropping

Destructors are functions responsible for cleaning up resources used by an object that is no longer accessible. Objects are never accessible after their destructor has been called, so it's not possible to fail from accessing freed resources. Further, when a task fails, destructors of all objects in the task are called.

The `box` keyword represents a unique handle for a memory allocation on the heap:

``` rust
{
  let y = box 10; // allocated on the heap
}
// destructor frees heap memory as soon as `y` goes out of scope
```

# Collections

## Arrays

Fixed-size vectors are unboxed blocks of memory that owns the elements it contains.

``` rust
let fixed_size: [1, 2, 3];
let five_zeroes: [int, ..5] = [0, ..5];
```

## Slices

Slices point into blocks of memory and don't have ownership over the elements. Other vector types coerce to slices. An unadorned string literal is an immutable string slice:

``` rust
let xs = &[1, 2, 3];
let ys: &[int] = xs;

let three = [1, 2, 3];
let zs: &[int] = three;

let string = "foobar";
let view: &str = string.slice(0, 3);
```

Mutable slices exist, but none for strings, since strings are a multi-byte encoding of Unicode code points, meaning they can't be freely mutated without the ability to alter the length, something that can't be done via slices, which simply provide a window.

``` rust
let mut xs = [1, 2, 3];
let view = xs.mut_slice(0, 2);
view[0] = 5;

let ys: &mut [int] = &mut [1, 2, 3];
```

[RFC #198](https://github.com/rust-lang/rfcs/blob/master/text/0198-slice-notation.md) adds overloaded slice notation via the `Slice` and `SliceMut` traits.

``` rust
let xs = &[1, 2, 3, 4, 5];

assert_eq!(xs[], xs.as_slice());
assert_eq!(xs[n..m], xs.slice(n, m));
assert_eq!(xs[n..], xs.slice_from(n));
assert_eq!(xs[..m], xs.slice_to(m));

assert_eq!(xs[mut], xs.as_mut_slice());
assert_eq!(xs[mut n..m], xs.slice_mut(n, m));
assert_eq!(xs[mut n..], xs.slice_from_mut(n));
assert_eq!(xs[mut ..m], xs.slice_to_mut(m));
```

## Vectors

Vectors are dynamically-sized and have a destructor that cleans up the allocated memory on the heap. Vectors own the elements they contain. They are represented with 3 words: a pointer to the data, its length, and its capacity representing how much memory is reserved for the vector.

``` rust
let mut numbers = vec![1, 2, 3];
numbers.push(4);

let more_numbers: Vec<int> = numbers;
```

## Strings

Strings are represented as vectors of `u8` with a guarantee of containing a valid UTF-8 sequence. The `String` type represents a non-null terminated, growable string, while the `&str` type represents fixed-sized one.

`String` is represented as a vector of bytes `Vec<u8>` that is guaranteed to always be a valid UTF-8 sequence. The `&str` type is a slice `&[u8]` that always points to a valid UTF-8 sequence and can be used as a view into String, as `&[T]` is a view into `Vec<T>`.

String literals are stored in the static region of the program's memory, and so Rust treats static strings as slices which serve as views into the region of memory pertaining to the string: `&'static str`.

``` rust
let mut string = String::from_str("fo");
string.push_char('o');

// trimmed strings are slices into original string
// no new allocation required
let trimmed: &str = string.as_slice().trim_chars(&[',', ' ']);
println!("Unused characters: {}", trimmed);

let literal: &'static str = "testing";
```

Raw string literals are possible by surrounding the string with zero or more hashes `#` as needed to disambiguate from the string contents, and prefixed by an `r`.

``` rust
let pattern = r#"\d+"#;
```

Byte string literals are possible by prefixing the string with a `b` and are stored as `&'static [u8]`. A byte literal is a single ASCII character surrounded by single quotes and prefixed by `b`.

Byte literals can have raw bodies by appending the `b` prefix with the `r` for raw string literals.

``` rust
let letter: u8 = b'A'; // 65u8
let name: &'static [u8] = b"john";

let raw_bytes = br#"test"#;
```

# Pattern Matching

Pattern matching takes the place of regular switch statements. Pattern matches don't fall through. The pipe operator `|` can be used to combine multiple patterns into one arm. As in Haskell, matches must be exhaustive. Every case is separated by commas, unless block expressions are used. As in Haskell, pattern matching can be used to bind values. Pattern guards are added using `if` expressions:

``` rust
match number {
  0     => println("zero"),
  1 | 2 => println("one or two"),
  3..10 => println("three to ten"),
  _     => println("something else")
}
```

The underscore `_` is a placeholder and two dots `..` acts as a wildcard pattern for matching the rest of the fields.

``` rust
match x {
  Cons(_, box Nil) => panic!("singleton list"),
  Cons(..)         => return,
  Nil              => panic!("empty list")
}
```

Patterns can also be bound to variables as in Haskell and Scala:

``` rust
match age {
  a @ 0..20 => println!("{} years old", a),
  _         => println!("older than 21")
}
```

Pattern matching on structures can use the field name which creates a local varaible with that name [^haskell_named_field_puns].

[^haskell_named_field_puns]: This is like Haskell's [named-field puns](/notes/haskell#named-field-puns).

``` rust
struct Test {
  age: int,
  apples: int
}

let Test { age, apples } = b;
```

[RFC #160](https://github.com/rust-lang/rfcs/blob/master/text/0160-if-let.md) added support for `if-let` pattern matching as in [Swift](/notes/swift/#optionals), where the condition succeeds if the pattern matches.

``` rust
if let Some(x) = optional {
  doSomething(x);
}
```

Similarly, [RFC #214](https://github.com/rust-lang/rfcs/blob/master/text/0214-while-let.md) added support for `while-let`. It works in a similar manner to the above `if-let`, essentially continuing the loop so long as the pattern matches. This is useful for iterators.

``` rust
while let Some(value) = iter.next() {
  // ...
}
```

With the `advanced_slice_patterns` feature gate, it's possible to use more advanced pattern matching on slices. With these patterns, `..` can be used to match any number of elements, which can also be bound to a name.

``` rust
match slice {
  [first, second, rest..] => blah,
}
```

# Control Structures

The for loop is in the form of `for-in`; there is no explicit increment looping via for loops. The `loop` keyword denotes an infinite loop.

# Structures

Structures are laid out in memory the same as as they are in C. Structures are constructed similar to Go structures. Structures have inherited mutability. Structures can be pattern matched on to destructure their fields.

``` rust
struct Point {
  x: f64,
  y: f64
}

match mypoint {
  Point { x, .. } => { println(x.to_str()) }
}
```

Structures can define methods by providing implementations.

``` rust
impl Point {
  fn static() {
    // static method
  }

  fn something(&self) {
    // method
  }
}
```

Functional updates can be performed by terminating the structure expression syntax with `..` followed by another value of the same type as the structure being defined. This is similar to what is commonly done in Haskell.

``` rust
let base = Point3d { x: 1, y: 2, z: 3 };
Point3d { y: 0, z: 10, ..base }
```

# Enumerations

Enumerations in Rust feel similar to algebraic datatypes in Haskell. Enumerations can be C-like, in which case they can optionally be given specific values. The specific values can be converted to an `int` using the cast operator `as`. Enumerations can define methods with the `impl` keyword as with structures. Enumerations variants can be structs as well:

``` rust
enum Shape {
  Circle(Point, f64)
  Rectangle(Point, Point)
}

enum Direction {
  Left,
  Right
}

enum Color {
  Red = 0xff0000,
  Green = 0x00ff00,
  Blue = 0x0000ff
}

#[feature(struct_variant)]
enum Shape {
  Circle { center: Point, radius: f64 },
  Rectangle { top_left: Point, bottom_right: Point }
}
```

# Tuples

Tuples are available and are most similar to Haskell's. _Tuple structs_ behave like both structs and tuples. They're like tuples with names. Tuple structs with a single field are similar to Haskell newtypes and are often called the same thing in Rust. These provide a distinct type from the type they wrap. Newtypes' wrapped values can be extracted using the dereference operator `*`:

``` rust
let mytup: (int, int, f64) = (10, 20, 30.0)

struct MyTup(int, int, f64);
let mytup: MyTup = MyTup(10, 20, 30.0)

struct GizmoId(int);
let my_gizmo_id: GizmoId = GizmoId(10);
let id_int: int = *my_gizmo_id;
```

[RFC #184](https://github.com/rust-lang/rfcs/blob/master/text/0184-tuple-accessors.md) adds tuple accessor syntax which applies to tuples and tuple structs. Only one level is possible since more than one would be considered a float.

``` rust
let mut tpl = (0, 1);
tpl.1 = 2;

struct Foo(int, int);
let mut foo = Foo(3, -15);
foo.0 = 5;
```

# Functions

Function definitions are similar to Scala's, with the type following the name of the parameter. The return type follows the parameter list as in Scala, but more like C++11's [alternative function syntax]. If the top-level block of the function produces an expression, the `return` statement may be omitted. Functions that return nothing return nil `()`, which can be omitted from the function declaration. However, ending the function with a semicolon is also equivalent to returning `()`. Function arguments can be destructured, but the patterns must be irrefutable:

[alternative function syntax]: http://en.wikipedia.org/wiki/C++11#Alternative_function_syntax

``` rust
fn line(a: int, b: int, x: int) -> int { a * x + b }

fn do_nothing_the_hard_way() -> () { return (); }
fn do_nothing_the_easy_way() { }
fn do_nothing_another_way(a: int) -> () { a; }

fn first((value, _): (int, f64)) -> int { value }
```

In short, blocks such as `{ expr1; expr2 }` are considered a single expression and evaluate to the result of the last expression if it's not followed by a semicolon, otherwise the block evaluates to `()`.

A diverging function is one that never returns a value to the caller. Every control path must end in `panic!()` or another diverging function. This is informs the compiler that execution will never return to the original context, allowing things to type-check where they wouldn't otherwise.

In the following example, `my_err` can be used even though it's not of type `int` because it's a diverging function.

``` rust
fn my_err(s: &str) -> ! {
  println!("{}", s);
  panic!();
}

fn f(i: int) -> int {
  if i == 42 {
    return 42;
  } else {
    my_err("Bad number!");
  }
}
```

Parameters can be marked as to be moved into the function. They can also be borrowed immutably, or mutably which allows the variable to be mutated. Finally, a parameter can also be moved into the function and marked as mutable, so that once moved into the function it can be mutated within the scope of the function.

``` rust
fn m(x: T);      // move
fn m(mut x: T);  // move and mark mutable
fn m(x: &T);     // immutably borrow
fn m(x: &mut T); // mutably borrow
```

# Methods

Methods are functions that take `self` as the first argument, which is of the same type as the method's receiver. Implementations are used to define methods on specific types, such as structs and enums.

``` rust
struct Point {
  x: f64,
  y: f64
}

enum Shape {
  Circle(Point, f64),
  Rectangle(Point, Point)
}

impl Shape {
  fn draw(&self) {
    match *self {
      Circle(p, f)      => draw_circle(p, f),
      Rectangle(p1, p2) => draw_rectangle(p1, p2)
    }
  }
}

let s = Circle(Point { x: 1.0, y: 2.0 }, 3.0);
s.draw();
```

It's also possible to define static methods by omitting the `self` parameter. This is usually how constructors are defined:

``` rust
use std::f64::consts::PI;

struct Circle { radius: f64 }

impl Circle {
  fn new(area: f64) -> Circle {
    Circle { radius: (area / PI).sqrt() }
  }
}

let c = Circle::new(42.5);
```

## Universal Function Call Syntax

[RFC #132] specifies a _universal function call syntax_ (UFCS) which provides a way to completely disambiguate a particular method call, which is particularly useful when a type implements more than one trait that contains the same method.

[RFC #132]: https://github.com/rust-lang/rfcs/blob/master/text/0132-ufcs.md

*[UFCS]: Universal Function Call Syntax

Given a particular call syntax, different behavior occurs:

* `Trait::method()`: the same behavior occurs as it does now.

* `T::method()` where `T` is a path that resolves to a type: it is rewritten internally to `<T>::method()`.

* `<T>::method()`: if type `T` implements only one trait in the scope that provides `method()`, then that method is called. Otherwise, the call must be disambiguated using the syntax `<T as DesiredTrait>::method()`.

A nice consequence of UFCS is that static methods on traits can be called through the implementing type now, instead of having to be called on the trait itself.

``` rust
#[deriving(Default)]
struct S;

S::default();
Default::default();
```

In fact, UFCS completely erases the distinction between static methods and regular methods, so that `t.a(b)` is also equivalent to `T::a(t, b)`. This means that methods can be called [like functions]:

[like functions]: https://github.com/rust-lang/rust/pull/18053

``` rust
struct Foo;

impl Foo {
  fn bar(&self) {
    // ...
  }
}

let x = Foo;

x.bar();
Foo::bar(&x);
```

# Closures

Regular, named functions don't close over their environment, but closures do.

``` rust
fn call_closure_with_ten(b: |int|) { b(10); }

let captured_var = 20;
let closure = |arg| println!("captured_var={}, arg={}", captured_var, arg);

call_closure_with_ten(closure);
```

Stack closures are a specific kind of closure that directly accesses local variables in the enclosing scope, making them very efficient. To ensure that they don't outlive the current scope, they aren't first class, so they can't be assigned to values or returned from functions.

``` rust
let mut max = 0;
[1, 2, 3].map(|x| if *x > max { max = *x })
```

Owned closures are written with `proc`, e.g. `proc(arg: int)`, and they own values that can be sent safely between processes. As a result, they can only be called once. The values they close over are copied, and they become owned by the closure. These are particularly used in concurrent scenarios, particularly for spawning tasks.

Despite there being different types of closures, functions that expect a `||` closure can accept any kind of closure provided they have the same arguments and return types. For this reason, higher-order functions should usually define their closure types as `||` so that callers can pass any kind of closure.

## Unboxed Closures

[RFC #114] proposed unboxed closure traits `Fn`, `FnMut`, and `FnOnce`, along with new closure syntax which was refined by [RFC #231]. These traits are defined in [`std::ops`][std-ops], which is a module that defines traits for overloading operators. In effect, implementing these traits is equivalent to overloading the function call operator `()` in C++.

[RFC #114]: https://github.com/rust-lang/rfcs/blob/master/text/0114-closures.md
[RFC #231]: https://github.com/rust-lang/rfcs/blob/master/text/0231-upvar-capture-inference.md
[std-ops]: http://doc.rust-lang.org/std/ops/index.html

As in C++, unboxed closures essentially expand to an anonymous structure which contains the captured variables---known as _upvars_---as well as an implementation of one of these traits, which has the effect of overloading the function call operator `()`.

In this context, the receiving parameter `self` refers to the instance of the generated structure, so that the type of `self` determines whether the upvars can be moved or mutated. This is exactly the difference between the three unboxed closure traits.

Trait    Self
-----    ----
`Fn`     `&self`
`FnMut`  `&mut self`
`FnOnce` `self`

The RFC defines two types of closures: escaping and non-escaping closures. _Escaping closures_ are closures that will escape the stack frame that created them---such as task bodies---and are created using the `move ||` syntax. _Non-escaping closures_ are the most common and therefore use the default `||` syntax.

Both escaping and non-escaping closures can implement any of the closure traits, depending on how the upvars are used within the closure.

1. `FnOnce` if any upvars are moved out of the closure
2. `FnMut` if any upvars are mutably borrowed within the closure
3. otherwise: `Fn`

Notice that moving an upvar out of the closure essentially means to move it out of the closure's environment, that is, its implementing anonymous structure.

The trait above is inferred, but inference of these traits is not completely implemented yet. As a result, there exists syntax sugar to explicitly declare which trait to choose.

Trait    Sugar
------   ------
`Fn`     `|&:|`
`FnMut`  `|&mut:|`
`FnOnce` `|:|`

A simple way to remember this sugar is to imagine that the `self` keyword precedes the colon `:`, which makes it obvious that this sugar is designating the type of the `self` parameter, which corresponds to the appropriate trait.

_Capture modes_ determine the manner in which upvars are captured. With escaping closures, all upvars are moved into the closure. For non-escaping closures, there are three different capture modes that are applied depending on three different situations which are checked in-order:

1. if an upvar is mutably borrowed within the closure, it is mutably borrowed into the closure
2. if an upvar is moved within the closure and doesn't implement `Copy`, it is moved into the closure
3. otherwise: the upvar is immutably borrowed

Notice that regular non-escaping closures can end up moving all of the upvars _only_ if they're moved within the closure. This is why the separate escaping closure syntax `move ||` is necessary, for cases where the closure escapes the stack frame, in order to ensure that all upvars are moved into the closure regardless of how they're used within.

There exists sugar for denoting unboxed closure traits at the type level.

``` rust
fn foo<F>() where F: FnMut(int, int) -> int {
  // ...
}
```

# Generics

Types can be parameterized similar to C++ class templates:

``` rust
enum List<T> {
  Const(T, Box<List<T>>),
  Nil
}

fn prepend<T>(xs: List<T>, value: T) -> List<T> {
  Cons(value, box xs)
}
```

The compiler can infer the type of a list like this:

``` rust
let mut xs = Nil;
xs = prepend(xs, 10);
xs = prepend(xs, 20);
xs = prepend(xs, 30);
```

However, it's also possible to explicitly annotate the types:

``` rust
let mut xs: List<int> = Nil::<int>;
xs = prepend::<int>(xs, 10);
xs = prepend::<int>(xs, 20);
xs = prepend::<int>(xs, 30);
```

The list equality function can be updated for this generic list by adding a trait bound of `Eq` on the element type and adding `ref` annotations to avoid moving out the element types. In fact, we might as well implement the `Eq` trait for this generic list already:

``` rust
impl<T: Eq> Eq for List<T> {
  fn eq(&self, ys: &List<T>) -> bool {
    match (self, ys) {
      (&Nil, &Nil) => true,
      (&Cons(ref x, box ref next_xs), &Cons(ref y, box ref next_ys))
        if x == y => next_xs == next_ys,
      _ => false
    }
  }
}
```

Generic functions in Rust have similar performance characteristics as templates in C++ because it performs _monomorphization_ which generates a separate copy of each generic function at each call site, which is specialized to the argument types, optimized specifically for them. This is similar to C++ template instantiation.

``` rust
fn map<T, U>(vector: &[T], function: |v: &T| -> U) -> Vec<U> {
  let mut accumulator = Vec::new();
  for element in vector.iter() {
    accumulator.push(function(element));
  }
  return accumulator;
}
```

Type parameters can also be used to define generic types, structs, and enums.

``` rust
use std::hashmap::HashMap;
type Set<T> = HashMap<T, ()>;
// Set<int>

struct Stack<T> {
  elements: Vec<T>
}
// Stack<int>

enum Option<T> {
  Some(T),
  None
}
// Option<int>
```

## Where Clauses

[RFC #135] adds support for `where` clauses similar to [those in Swift] which allow type bounds to be specified for the type parameters in a generic declaration. Where clauses can be added to anything that can be parameterized with type or lifetime parameters, except for trait method definitions. This includes `impl`, `fn`, `trait`, `struct`, as well as [associated types](#associated-types).

Multiple bounds declarations are separated by commas, and multiple bounds for a given type parameter can be written separately or in a compound form using `+`.

[RFC #135]: https://github.com/rust-lang/rfcs/blob/master/text/0135-where.md
[those in Swift]: /notes/swift/#where-clauses

``` rust
impl<K: Hash + Eq, V> HashMap<K, V> {
  // ...
}

impl<K, V> HashMap<K, V>
  where K: Hash + Eq {
  // ...
}
```

With [associated types](#associated-types), a trait like `Iterator` might no longer have type parameters, since it'll be an associated type. As a result, a bound can't be provided for the type parameter using regular syntax. Instead, `where` clauses can be used to express the bound.

``` rust
// Iterator has no type parameter, so this doesn't work
fn sum<I: Iterator<int>>(i: I) -> int {
  // ...
}

// the where clause can express a bound
// on the Iterator's associated type `E`
fn sum<I: Iterator>(i: I) -> int
  where I::E == int {
  // ...
}
```

# Traits

Traits are similar to type classes in Haskell. They allow the expression of _bounded polymorphism_ which limits the possible types a type parameter could refer to. For example, the `clone` method which allows the copying of a type, isn't defined on all types because it can't be safely performed on all types, due to user-defined destructors for example. Traits allow to bound the polymorphism of a generic function by specifying that a type parameter must implement the `Clone` trait in this case to limit the types on which the function can work:

``` rust
fn head<T: Clone>(v: &[T]) -> T {
  v[0].clone()
}
```

Traits contain zero or more method signatures. In the following trait, it is said that `Printable` provides a `print` method with the given signature:

``` rust
trait Printable {
  fn print(&self);
}

impl Printable for int {
  fn print(&self) { println!("{:?}", *self) }
}

impl Printable for String {
  fn print(&self) { println(*self) }
}
```

It's also possible to define default method implementations which can later be overridden on a case-by-case basis:

``` rust
trait Printable {
  fn print(&self) { println!("{:?}", *self) }
}

// these use default implementation
impl Printable for int {}
impl Printable for bool {}
impl Printable for f32 {}

// overrides default implementation
impl Printable for String {
  fn print(&self) { println(*self) }
}
```

Traits may be parameterized by type variables. Traits can be implemented generically or specifically for a particular type.

``` rust
trait Seq<T> {
  fn length(&self) -> uint;
}

impl<T> Seq<T> for Vec<T> {
  fn length(&self) -> uint {
    self.len()
  }
}

impl Seq<bool> for u32 {
  // implement Seq<bool> for u32
}
```

The `Self` type parameter is available within the trait definition and is replaced with the eventual type `T` in the implementation:

``` rust
trait Eq {
  fn equals(&self, other: &Self) -> bool;
}

impl Eq for int {
  fn equals(&self, other: &int) -> bool {
    *other == *self;
  }
}
```

Traits can also define static methods which are ultimately called with a `::` prefix as well:

``` rust
use std::f64::consts::PI;

trait Shape {
  fn new(area: f64) -> Self;
}

struct Circle { radius: f64 }

impl Shape for Circle {
  fn new(area: f64) -> Circle {
    Circle { radius: (area / PI).sqrt() }
  }
}

let c: Circle = Shape::new(area);
```

Type parameters can have multiple bounds by combining them with `+`. Method calls to bounded type parameters are statically dispatched:

``` rust
fn print_all<T: Printable + Clone>(printable_things: Vec<T>) {
  // can clone() then print()
}
```

It's possible to have method calls to trait types that are dynamically dispatched. With this, it's possible to define a function that performs the `draw` method on type that implements `Drawable`, which itself provides the `draw` method. This particular method takes a borrowed pointer to an array of owned values that implement the `Drawable` trait. Such an array must be constructed by casting each element to `Box<Drawable>`:

``` rust
fn draw_all(shapes: &[Box<Drawable>]) {
  for shape in shapes.iter() {
    shape.draw()
  }
}

draw_all([circle as Box<Drawable>, rectangle as Box<Drawable>])
```

Traits can inherit from other traits, so that the traits they inherit from are referred to as _supertraits_. This means that for any type to implement the derived trait, it must also implement the supertrait:

``` rust
trait Shape {
  fn area(&self) -> f64;
}

trait Circle : Shape {
  fn radius(&self) -> f64;
}

struct CircleStruct { center: Point, radius: f64 }

impl Circle for CircleStruct {
  fn radius(&self) -> f64 {
    (self.area() / PI).sqrt()
  }
}

impl Shape for CircleStruct {
  fn area(&self) -> f64 {
    PI * square(self.radius)
  }
}
```

It's also possible to call supertrait methods on subtrait-bound type parameter values. This is also possible from trait objects:

``` rust
fn radius_times_area<T: Circle>(c: T) -> f64 {
  c.radius() * c.area()
}

let concrete = box CircleStruct { center: Point { x: 3f, y: 4f }, radius: 5f }
let mycircle: Box<Circle> = concrete as Box<Circle>;
let nonsense = mycircle.radius() * mycircle.area();
```

Some traits can be automatically derived, similar to Haskell. The full list is `Eq`, `TotalEq`, `Ord`, `TotalOrd`, `Encodable`, `Decodable`, `Clone`, `DeepClone`, `IterBytes`, `Rand`, `Default`, `Zero`, `FromPrimitive`, and `Show`:

``` rust
#[deriving(Eq)]
struct Circle { radius: f64 }

#[deriving(Rand, ToStr)]
enum ABC { A, B, C }
```

## Associated Items

[RFC #195] adds support for associated functions, statics, types, and lifetimes. Both associated types and statics allow defaults, just as associated methods and functions do. However, if an implementation overrides any default associted types, then they must override _all_ default functions and methods.

[RFC #195]: https://github.com/rust-lang/rfcs/blob/master/text/0195-associated-items.md

### Associated Types

Consider a `Graph` type with node and edge type parameters. Without associated types, any function that operates on generic `Graph`s has to parameterize on all type parameters even when they're not relevant.

``` rust
trait Graph<N, E> {
  fn has_edge(&self, &N, &N) -> bool;
}

fn distance<N, E, G: Graph<N, E>>(graph: &G, start: &N, end: &N) -> uint {
  // ...
}
```

With associated types, it's made clear that the node and edge types are defined by specific implementations. This way, code can abstract over the entire `Graph` and refer to associated types through the `Graph` as needed.

``` rust
trait Graph {
  // trait bounds can be specified with the usual + delimited syntax
  type N;
  type E;
  fn has_edge(&self, &N, &N) -> bool;
}

fn distance<G: Graph>(graph: &G, start: &G::N, end: &G::N) -> uint {
  // ...
}
```

As shown, associated types make code more readable and also scalable, since traits can incorporate additional associated types without breaking code that doesn't care about them.

Currently, before associated types, traits' type parameters are considered either inputs or outputs, where the input type parameter determines which implementation to use and the output type parameter is determined by the implementation itself but otherwise plays no role in implementation selection.

Typically the only input type is `Self`, i.e. the type implementing the trait, while all other trait type parameters are outputs. It would be useful to be able to specify other input type parameters so that the implementation could be chosen depending on `Self` _and_ the additional input type parameters.

``` rust
trait Add<Rhs, Sum> {
  fn add(&self, rhs: &Rhs) -> Sum;
}

impl Add<int, int> for int { ... }
impl Add<Complex, Complex> for int { ... }
```

This is impossible without this RFC, which implements the equivalent of Haskell's [multi-parameter type classes], effectively treating all trait type parameters as input types and allowing output types to be specified via associated types. Specifying traits like this essentially defines a family of traits, one for each combination of input types (e.g. of `Rhs` in this example), each for which an implementation can be provided.

[multi-parameter type classes]: http://en.wikibooks.org/wiki/Haskell/Advanced_type_classes#Multi-parameter_type_classes

``` rust
// Self and Rhs are input types
// Self is defined at the impl site,
// it's the type in `for T`
trait Add<Rhs> {
  type Sum; // output type
  fn add(&self, &Rhs) -> Sum;
}

// Sum = Self + Rhs
// int = int  + Complex
impl Add<int> for int {
  type Sum = int;
  fn add(&self, rhs: &int) -> int { ... }
}

// Complex = int + Complex
impl Add<Complex> for int {
  type Sum = Complex;
  // or fn add(&self, rhs: &Complex) -> Sum { ... }
  fn add(&self, rhs: &Complex) -> Complex { ... }
}
```

Equality constraints and trait bounds can be provided for associated types using where clauses.

``` rust
trait Iterator {
  type A;
  fn next(&mut self) -> Option<A>;
}

// associated type A must implement Show
fn print_iter<I>(iter: I) where I: Iterator, I::A: Show { ... }

// associated type must be uint (i.e. specialization)
fn sum_uints<I>(iter: I) where I: Iterator, I::A = uint { ... }

// or sugar
fn sum_uints<I: Iterator<A = uint>>(iter: I) { ... }
```

When using a trait as a type (e.g. trait object) then all input and output types must be provided as part of the type.

``` rust
fn consume_obj(t: Box<Foo<IT1, IT2, OT1 = int, OT2 = String, 'a = 'static>>)
```

All associated items are also allowed in inherent implementations, i.e. implementations for a particular struct. However, output constraints don't make sense for inherent outputs. In inherent implementations, associated types act more like type aliases with the added benefit that the associated types can be referred to using path syntax without having to import the modules that define those types.

### Associated Statics

Currently, without associated statics, numeric traits like `Float` need to expose constants as static functions---which would be useful when writing a function generic over the type of `Float`---but these functions can't be used in static initializers, so modules for the numeric type implementation (e.g. `f64`) also export a separate set of statics outside of traits. Associated statics allow statics to be declared within traits.

``` rust
trait Float {
  static NAN: Self;
  static INFINITY: Self;
  static PI: Self;
  static TWO_PI: Self;
  // ...
}
```

[Where clauses](#where-clauses) can be used within object types using a type-parameterization syntax.

``` rust
// create and get reference to an Iterator for int
&Iterator<where E=int>
```

## Drop

The `Drop` trait can be used to define destructors via its method `drop`. The global `drop` function also exists which essentially takes ownership of the passed value so that the moved variable's destructor---its `drop` method---is called by the end of the function scope. Destructors are run in top-down order, so that the value is completely destroyed before any values it owns run their destructors.

``` rust
struct TimeBomb {
  explosivity: uint
}

impl Drop for TimeBomb {
  fn drop(&mut self) {
    for _ in range(0, self.explosivity) {
      println("boom");
    }
  }
}
```

## Clone

The `Clone` trait can be implemented to support copying of types via the `clone` method.

``` rust
#[deriving(Clone,Show)]
struct Pair(Box<int>, Box<int>);

fn main() {
  let pair = Pair(box 1, box 2);
  let cloned_pair = pair.clone();

  drop(pair);

  println!("clone: {}", cloned_pair);
}
```

## Operator Overloading

Most operators are implemented as functions and can be overloaded by implementing the [appropriate traits]. For example, the `Add` trait can be used to overload the `+` operator.

[appropriate traits]: http://doc.rust-lang.org/core/ops/

``` rust
struct Point {
  x: int,
  y: int
}

impl Add<Point, Point> for Point {
  fn add(&self, other: &Point) -> Point {
    Point { x: self.x + other.x, y: self.y + other.y }
  }
}

fn main() {
  println!("{}", Point { x: 1, y: 0 } + Point { x: 2, y: 3 })
}
```

# Dynamically Sized Types

[Dynamically Sized Types] (DST) [[metabug]] introduces two new types: `[T]` representing some number of instances of `T` laid out sequentially in memory with the exact number unknown to the compiler, and `Trait` representing some type `T` that implements the trait `Trait`.

[Dynamically Sized Types]: http://smallcultfollowing.com/babysteps/blog/2014/01/05/dst-take-5/
[metabug]: https://github.com/rust-lang/rust/issues/12938

*[DST]: Dynamically Sized Types

Smart pointers, for example, can implement `Deref` and automatically dereference down to the base type that is indexable. Concretely, `r` of type `Rc<[T]>` would autoderef to `*r` which is of type `&[T]` which is not indexable either and so would autoderef to `**r` which is of type `[T]` which _is_ indexable, and so Rust indexes into that. The same thing is done for trait objects, where `Rc<Trait>` is derefed to `&Trait`.

# Tasks

Tasks have dynamically sized stacks, starting out with small ones and dynamically growing as required. This means that unlike C/C++, it's not possible to write beyond the end of the stack. Tasks provide failure isolation and recovery. When a problem occurs, the runtime destroys the entire task, and other tasks can monitor each other for failure.

Tasks can't share mutable state with each other. Instead they communicate with each other by transferring owned data through the global _exchange heap_.

## Spawning

Tasks are spawned with the `spawn` function which accepts a closure which is executed in a separate task. Creating tasks isn't defined at the language-level, but rather, inside the standard library.

``` rust
fn print_msg() {
  println!("running in diff task");
}

spawn(print_msg);

// or with lambda expression
spawn(proc() println!("also in diff task"));
```

Since the signature of the `spawn` function is `spawn(f: proc())`, it accepts only owned closures, which by extension can only contain owned data. For this reason, `spawn` can safely move the entire closure and its associated state to an entirely different task for execution. As with any closure, it can capture an environment that it carries across tasks:

``` rust
let child_task_nr = generate_task_nr();

spawn(proc() {
  println!("I am child number {}", child_task_nr);
});
```

## Channels

Pipes are used for communication between tasks [^channels]. Each pipe is defined by a pair of endpoints: one for sending and another for receiving. In Rust, a channel is the _sending_ endpoint of a pipe and the _port_ is the receiving endpoint.

[^channels]: Pipes remind me of Go's channels, or Haskell's.

The following code creates a channel for sending and receiving `int` types. Note that `Sender` and `Receiver` are both sendable types that may be captured into task closures or transferred between them.

``` rust
let (tx, rx): (Sender<int>, Receiver<int>) = comm::channel();

spawn(proc() {
  let result = expensive_computation();
  tx.send(result);
});

other_expensive_computation();
let result = rx.recv();
```

A regular `Sender` and `Receiver` created by `comm::channel` can't be used by more than one task. Attempting to do so leads to an error, since the first task to use it becomes its owner. Instead it's necessary to clone the given endpoint.

``` rust
let (tx, rx) = comm::channel();

for init_val in range(0u, 3) {
  // create a SharedChan
  let child_rx = tx.clone();

  spawn(proc() {
    child_rx.send(expensive_computation(init_val));
  });
}

let result = rx.recv() + rx.recv() + rx.recv();
```

The above example is contrived and could've been done with three separate channels:

``` rust
let rxs = vec::from_fn(3, |init_val| {
  let (tx, rx) = comm::channel();
  spawn(proc() {
    tx.send(some_expensive_computation(init_val));
  });
  rx
});

let result = rxs.iter().fold(0, |accum, rx| accum + rx.recv() );
```

## Futures

Futures can be used for requesting a computation and getting the result later. In the code below, `sync::Future::spawn` immediately returns a `future` object whether or not the computation is complete. The result can be explicitly retrieve with the `get()` function, which blocks until the value becomes available. **Note** that the future is mutable so that it can save the result inside the object once the computation is complete.

``` rust
fn fib(n: u64) -> u64 { 12586269025 }

let mut delayed_fib = sync::Future::spawn(proc() fib(50));
something_else();
println!("fib(50) = {:?}", delayed_fib.get());
```

## Arc

When wanting to share immutable data between tasks, it may be expensive to use a typical pipe as that would create a copy of the data on each transfer. For this it would be more efficient to use an Atomically Reference Counted wrapper, `Arc`, which is implemented in the `sync` library. `Arc` acts as a reference to shared data so that only the reference itself is shared and cloned.

``` rust
use std::sync::Arc;

fn main() {
  let numbers = Vec::from_fn(100, |i| i as f32);
  let shared_numbers = Arc::new(numbers);

  for _ in range(0u, 10) {
    let child_numbers = shared_numbers.clone();

    spawn(proc() {
      let local_numbers = child_numbers.as_slice();
      // Work with the local numbers
    });
  }
}
```

## Failure

Exceptions can be raised in Rust using the `panic!()` macro. Exceptions are unrecoverable within a single task. When an exception is raised, the task unwinds its stack, running destructors and freeing memory along the way, then exits.

However, tasks may notify each other of failure. The `try` function is similar to `spawn` but blocks until the child is finish, yielding a return value of `Result<T, ()>` [^either] which has two variants: `Ok(T)` and `Err`. This `Result` can then be pattern-matched to determine the outcome of the task, with `Err` representing termination with an error. **Note** currently, it's not possible to retrieve a useful error value from the `Err` variant, since `try` always returns `()`:

[^either]: This `Result` type is a lot like Haskell's `Either`.

``` rust
let result: Result<int, ()> = task::try(proc() {
  if some_condition() {
    calculate_result()
  } else {
    panic!("oops!");
  }
});

assert!(result.is_err());
```

# Errors

It can quickly become unwieldly to handle multiple kinds of error types. For example, your application might define its own error type for a particular task, such as a service API, but the lower-level components that facilitate the API have their own error types as well, such as IO or more specifically network errors.

One solution to this is to define an enum with a variant for each kind of error type possible for a particular action (e.g. API requests). The `FromError` trait can be used to define how to convert from one error type to another, therefore allowing the `try!` macro to work with any that can be converted to a common type.

``` rust
enum MyError {
  Io(IoError),
  Map(MapError)
}

// how to convert IoError to MyError
impl FromError<IoError> for MyError {
  fn from_error(err: IoError) -> MyError { Io(err) }
}

// how to convert MapError to MyError
impl FromError<MapError> for MyError {
  fn from_error(err: MapError) -> MyError { Map(err) }
}

fn open_and_map() -> Result<(), MyError> {
  let f = try!(File::open(&Path::new("foo.txt")));
  let m = try!(MemoryMap::new(0, &[]));
  // ...
}
```

The `Error` trait can be used to provide uniform base functionality for errors. The `description` method is the only necessary one and it yields a very short string that identifies the error type, and so is usually a static string. The `detail` method, if implemented, yields a string that is much more descriptive of the actual error, usually including dynamic information, hence the `String` return type.

The `cause` method, if implemented, can yield the lower-level cause for the current error, allowing for the possibility of maintaining a chain of errors which led to the current one. For example, an API service error type might include a network error as its cause if a request failed for that reason.

``` rust
pub trait Error: Send {
  fn description(&self) -> &str;

  fn detail(&self) -> Option<String> { None }
  fn cause(&self) -> Option<&Error> { None }
}
```

# Iterators

Iterators can be created by implementing the `Iterator` trait, particularly its `next` method. Notice that an `Option` is returned to indicate whether or not the iterator has finished, which is signified by yielding `None`.

``` rust
struct ZeroStream { remaining: uint }

impl ZeroStream {
  fn new(n: uint) -> ZeroStream {
    ZeroStream { remaining: n }
  }
}

impl Iterator<int> for ZeroStream {
  fn next(&mut self) -> Option<int> {
    if self.remaining == 0 {
      None
    } else {
      self.remaining -= 1;
      Some(0)
    }
  }
}

fn main() {
  let mut nums = ZeroStream::new(3);

  for i in nums {
    print!("{}", i);
  }
}

// prints "000"
```

Iterators can be transformed by adaptors that themselves return another iterator:

``` rust
let xs = [1, 2, 3, 4, 5];
let ys = ["one", "two", "three", "four", "five"];

let mut it = xs.iter().zip(ys.iter());

for (x, y) in it {
  println!("{} {}", *x, *h);
}
```

Iterators offer a generic conversion to containers with the `collect` adaptor. This method is provided by the `FromIterator` trait:

``` rust
let xs = [1, 2, 3, 4];
let ys = xs.rev_iter().skip(1).map(|&x| x * 2).collect::<Vec<int>>();
```

# Macros

Rust has support for advanced [hygienic macros]. Macros are defined using the `macro_rules!` syntax extension which is followed by the macro name. Its body consists of pattern matching cases, where the LHS is the macro invocation syntax (how macro is called) and the RHS is the macro transcription syntax (what macro expands to).

The macro definition can be surrounded by parentheses, brackets, or braces. Regardless of what was used in the definition, the macro can be invoked using either one as well, such as `vec![]` or `vec!()`. By convention, braces are used for blocks and parentheses for one-liners.

[hygienic macros]: http://en.wikipedia.org/wiki/Hygienic_macro

The macro invocation syntax must be surrounded by parentheses and the parentheses, brackets, or braces contained within must be balanced. Everything contained in the parentheses that isn't prefixed by `$` is taken literally, so that the same thing must be repeated at the call-site in order to match.

``` rust
// the following
macro_rules! my_macro {
  ($x:ident verbatim $e:expr) => {}
}

// means it could be invoked as
my_macro!(i verbatim 2+2);
```

Items prefixed by `$` are ways to refer to different types of syntax such as identifiers, expressions, types, and so on. The `$` is followed by a name to give it within the transcription syntax body followed by the fragment specifier.

Designator  Type
----------- -----
`block`     code block
`expr`      expressions
`ident`     identifiers
`item`      [crate component](http://doc.rust-lang.org/rust.html#items)
`matchers`  LHS of `=>` in macro
`pat`       patterns
`path`      module path
`stmt`      statement
`tt`        RHS of `=>` in macro
`ty`        types

The `tt` specifier seems to refer to token tree, and it seems to refer to code within a block's delimiters `{}`. It's used for implementing macros that are [called with code blocks], reminiscent of the removed do-notation syntax.

[called with code blocks]: https://github.com/rust-lang/rust/pull/12497

``` rust
macro_rules! expr( ($e: expr) => { $e } )

macro_rules! spawn {
  ($($code: tt)*) => {
    expr!(spawn(proc() { $($code)* } ))
  }
}

spawn! {
  info!("stmt");
};
```

The patterns may also contain the syntax `$(...)*` or `$(...)+` which expresses that the pattern contained must appear zero or more times, or one or more times, respectively---as in regular expressions. This special syntax may be also specify a separator token after the closing parenthesis but before the match-type discriminator, i.e. `$(...),*` would be repeatable and comma-separated.

``` rust
// from RFC 163: https://github.com/rust-lang/rfcs/pull/163
// returns true if the expression matches the pattern
macro_rules! matches {
  ($expression: expr, $($pattern:pat)|*) => (
    match $expression {
      $($pattern: pat)|+ => true,
      _ => false
    }
  );
}

let input: &str = ...;
if input.len() >= 2 &&
   matches!(input.char_at(0), '+' | '-') &&
   matches!(input.char_at(1), '0'..'9') {
  // parse signed number
}
```

Macros may expand to expressions, items, or statements depending on the arguments it was invoked with and the location at which it was invoked.

The transcription syntax body must be enclosed by delimiters---parentheses, brackets, or braces---which are ignored. The contents of the delimiters must contain regular Rust syntax or interpolated fragments.

``` rust
macro_rules! min {
  ($x:expr) => {
    $x
  };
  ($x:expr, $($y:expr),+) => {
    std::cmp::min($x, min!($($y),+))
  }
}

println!("{}", min!(5u, 2u * 3, 4u));
```

## Syntax Extensions

There are a variety of built-in syntax extensions which expand to expressions with values.

Extension      Purpose
---------      -------
`format!`      format data into a string
`env!`         lookup env-var at compile-time
`file!`        path to file being compiled
`stringify!`   pretty-print Rust expression
`include!`     include expression in given file
`include_str!` include contents of file as string
`include_bin!` include contents of file as binary
`info!`        print diagnostics. also `error!`, `warn!`, and `debug!`

# Foreign Function Interface

The `extern` block can be used to list function signatures in a foreign library, and is usually paired with a `link` attribute to link against the library in question. Foreign functions are considered unsafe, so they must be wrapped in `unsafe`, which essentially tells the compiler that everything within is truly safe.

``` rust
#[link(name = "snappy")]
extern {
  fn snappy_max_compressed_length(
    source_length: size_t
  ) -> size_t;
  fn snappy_compress(
    input: *const u8,
    input_length: size_t,
    compressed: *mut u8,
    compressed_length: *mut size_t
  ) -> c_int;
}
```

The `link_name` attribute could be used to specify an alternative function name, which is useful when a function collides with the Rust namespace.

``` rust
extern {
  // reference exit(i32) as die(i32)
  #[link_name = "exit"]
  fn die(x: i32) -> !;
}

fn main() {
  unsafe {
    die(2)
  }
}
```

To avoid requiring `unsafe` blocks at the call-site, safe wrappers are usually created with native Rust types.

``` rust
pub fn compress(src: &[u8]) -> Vec<u8> {
  unsafe {
    let srclen = src.len() as size_t;
    let psrc = src.as_ptr();

    let mut dstlen = snappy_max_compressed_length(srclen);
    let mut dst = Vec::with_capacity(dstlen as uint);
    let pdst = dst.as_mut_ptr();

    snappy_compress(psrc, srclen, pdst, &mut dstlen);
    dst.set_len(dstlen as uint);
    dst
  }
}
```

Rust functions can be made available to C code, such as for callbacks, by marking them as `extern` with the correct calling convention.

``` rust
extern fn callback(a: i32) {
  println!("I'm called from C with the value {0}", a);
}

#[link(name = "extlib")]
extern {
  fn register_callback(cb: extern fn(i32)) -> i32;
  fn trigger_callback();
}

unsafe {
  register_callback(callback);
  trigger_callback();
}
```

The `link` attribute can take a `kind` argument that specifies the type of library being linked. If the `kind` argument is not provided, it is assumed to be dynamic. Otherwise, the `kind` may be `static` or, if on OS X, `framework`.

Linked static libraries are included within the output crate. Dynamic dependencies (and OS X frameworks), however, aren't linked until the rlib is included in the final target (e.g. binary).

``` rust
#[link(name = "dependency", kind = "static")]
#[link(name = "CoreFoundation", kind = "framework")]
```

Global variables must also be declared within `extern` blocks before they're accessible. They may also be marked as `mut`.

``` rust
#[link(name = "readline")]
extern {
  static rl_readline_version: libc::c_int;
  static mut rl_prompt: *const libc::c_char;
}

println!("readline version {}", rl_readline_version as int);

">> ".with_c_str(|buf| {
  unsafe { rl_prompt = buf; }
  // get a line
  unsafe { rl_prompt = ptr::null(); }
})
```

The `extern` keyword can also take as argument a calling convention to use with the contained function declarations. The `system` calling convention selects the appropriate calling convention for interoperating with the target's libraries, e.g. `stdcall` on x86 or `C` on x86_64.

``` rust
#[link(name = "kernel32")]
extern "stdcall" {
  fn SetEnvironmentVariableA(n: *const u8, v: *const u8) -> libc::c_int;
}
```

Structures are guaranteed to be compatible with the platform's representation in C. Since strings aren't null terminated, the `c_str::to_c_str` function is used to null-terminate them.

# Unsafe

The `unsafe` blocks are mainly used for dereferencing raw pointers, calling FFI functions, casting types in a bitwise manner using `transmute`, and inline assembly.

*[FFI]: Foreign Function Interface

There are two additional raw pointer types that are an approximation of pointer types in C, namely `*const T` which is an approximation of `const T*` and `*mut T` is an approximation of `T*`.

Rust       C
-----      --
`*const T` `const T*`
`*mut T`   `T*`

Rust pointer types aren't guaranteed to not be null or even to point to valid memory, nor do they also don't implement RAII for automatic resource management, like `Box`. Raw pointers are considered plain-old-data, and don't have a notion of move ownership so that Rust can't protect against bugs like use-after-free. Raw pointers are also considered sendable, allowing them to be access from multiple concurrent threads without synchronization.

*[RAII]: Resource Allocation Is Initialization

Dereferencing raw pointers is only possible within `unsafe` blocks, since it may result in crashes. Pointer arithmetic also needs to be within `unsafe` blocks. Creating a raw pointer or converting one to an integer is not unsafe.

At run-time, raw pointers `*` and references `&` pointing to the same data have identical representations. A `&T` reference implicitly coerces to raw pointer `*const T` in safe code, and can even be made explicit as `value as *const T`.

``` rust
// explicit
let i: u32 = 1;
let p_imm: *const u32 = &i as *const u32;

// implicit
let mut m: u32 = 2;
let p_mut: *mut u32 = &mut m;
```

However, the reverse isn't true, since a reference `&` is always valid, but a `*const T` may not be, since the pointed object has to be a valid instance of the correct type and it must satisfy the aliasing and mutability laws of references. Raw pointers can be converted back to references by using the `&*p` syntax where `p` must be a pointer.

``` rust
unsafe {
  let ref_imm: &u32 = &*p_imm;
  let ref_mut: &mut u32 = &mut *p_mut;
}
```

Inline assembly is possible via the `asm!` macro which requires the `asm` feature and must be within an `unsafe` block. The template parameter is the only one that is required.

``` rust
asm!(template
   : output
   : input
   : clobbers
   : options);
```

Input and output operands consist of `constraint(expr)` [as in GCC].

[as in GCC]: https://gcc.gnu.org/onlinedocs/gcc/Constraints.html

``` rust
asm!("add $2, $0"
   : "=r"(c)
   : "0"(a), "r"(b)
   );
```

Clobbers are used to list registers that may have been modified by the assembly code. If the code changes the condition code register or modifies memory, `cc` and `memory` should be specified in the clobbers list respectively.

Inputs and outputs are omitted from the following code as they're not required by the assembly code, and no whitespace is necessary between the colon delimiters.

``` rust
// put 0x200 in eax
asm!("mov $$0x200, %eax" ::: "eax");
```

The final parameter is a list of options to apply to the assembly code, such as `volatile`, `alignstack` to align the stack, or `intel` to use Intel syntax.

The standard library `std` can be avoided by using the `no_std` attribute. Further, the entry point can be modified by applying the `start` attribute to what will be the entry point.

``` rust
#![no_std]

extern crate libc;

#[start]
fn start(_argc: int, _argv: *const *const *u8) -> int {
  0
}
```

Alternatively, the compiler-inserted `main` shim can be disabled and overridden using the `no_main` attribute. Another one must then be provided with the correct name and bypassing compiler name mangling.

``` rust
#![no_std]
#![no_main]

extern crate libc;

#[no_mangle]
pub extern fn main(_argc: int, _argv: *const *const *u8) -> int {
  0
}
```

There are also two functions that are normally provided by the standard library must de defined. The `stack_exhausted` function is called whenever a stack overflow is detected, and the `eh_personality` function is used by the failure mechanisms of the compiler.

The [`libcore`](http://doc.rust-lang.org/core/) library provides the minimum necessary functionality for writing idiomatic Rust.

Lang items are pluggable operations that aren't hard-coded into the language but instead implemented in libraries. The `lang` attribute is used to inform the compiler of a given implementation.

The [`transmute`] function is essentially like C's [`reinterpret_cast`]. Both types must have the same size and alignment.

[`transmute`]: http://doc.rust-lang.org/core/intrinsics/ffi.transmute.html
[`reinterpret_cast`]: http://en.cppreference.com/w/cpp/language/reinterpret_cast

# Comments

Comments are available in single and multi-line variants as in C. Single line comments with three slashes `///` and multi-line comments with one extra asterisk `/**` are interpreted as documentation

# Testing

Test functions are marked with the `test` attribute and use the `assert!` macro for asserting conditions. Test functions must not have any arguments or return values. A test is considered successful if the function returns, and fails if the test fails through `panic!`, `assert`, or any other means.

``` rust
fn return_two() -> int { 2 }

#[test]
fn return_two_test() {
  let x = return_two();
  assert!(x == 2);
}
```

For tests to be run, the crate has to be compiled with the `--test` flag. The resulting executable will then run all tests.

``` bash
$ rustc --test foo.rs
$ ./foo
running 1 test
test return_two_test ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured
```

The `--nocapture` flag can be provided to the test harness to prevent it from stealing standard output, so that tests can print messages to the terminal.

``` bash
$ ./foo --nocapture
$ cargo test -- --nocapture
```

The `ignore` attribute can be used to specify that a test should not be run. The existence of the test will be noted in the runner's output. The `ignore` attribute can take a configuration value as predicate, to ignore a given test depending on a configuration value.

``` rust
#[ignore(cfg(target_os = "win32"))]
#[test]
fn linux_test() {}
```

Tests that are intended to fail can be annotated with the `should_fail` attribute.

The test runner can take as argument a regular expression used to run the matching tests, and the `--ignored` flag can tell the runner to only run the ignored tests.

``` bash
$ ./foo sometest
$ cargo test -- sometest
```

It's also possible to benchmark functions using the `bench` attribute. Benchmarks are compiled along with tests when compiled with the `--test` flag. Benchmark functions take as parameter a mutable reference to type `test::Bencher`. Setup should happen at the beginning of the function, and the actual code to be benchmarked should occur within a closure passed to `test::Bencher`'s `iter` method.

``` rust
#[bench]
fn bench_sum_1024_ints(b: &mut Bencher) {
  let v = Vec::from_fn(1024, |n| n); // setup
  b.iter(|| v.iter().fold(0, |old, new| old + *new))
}
```

If measuring throughput, the `bytes` field of the `Bencher` can be set to the bytes consumed/produced in each iteration.

``` rust
#[bench]
fn initialize_vector(b: &mut Bencher) {
  b.iter(|| Vec::from_elem(1024, 0u64));
  b.bytes = 1024 * 8;
}
```

The `--bench` flag must be passed to the compiled test-runner to run the benchmarks.

``` bash
$ rustc mytests.rs -O --test
$ mytests --bench

running 2 tests
test bench_sum_1024_ints ... bench: 709 ns/iter (+/- 82)
test initialise_a_vector ... bench: 424 ns/iter (+/- 99) = 19320 MB/s

test result: ok. 0 passed; 0 failed; 0 ignored; 2 measured
```

It's possible that the optimizer might optimize away what is being measured. One way to avoid this is to have the closure return a value or to wrap the contents in `test::black_box`, which forces the optimizer to consider any argument used. Larger values can be passed by reference to `black_box` for efficiency.

The testsuite supports ratcheting against a metrics file, which basically compares benchmark results with previously saved results and considers it a regression if the results are worse by a certain noise value. This is possible through the `--save-metrics=file.json` parameter to save the results, and the `--ratchet-metrics=file.json` parameter to ratchet the results against those saved in the specified file.

# Cargo

[Cargo] is the package manager for Rust, which actually reminds me a lot of Haskell's [Cabal]. The package configuration is defined in a `Cargo.toml`{.path} file which is written in [TOML] format. The TOML file [contains] a `package` section which establishes the project's metadata. A `[[bin]]` section can also be created to specify that the target is to be a binary, as well as information pertaining to it, such as its name.

[Cargo]: http://crates.io/
[TOML]: https://github.com/toml-lang/toml
[Cabal]: http://www.haskell.org/cabal/
[contains]: http://crates.io/manifest.html

``` toml
[package]

name = "hello_world"
version = "0.1.0"
authors = [ "someone@example.com" ]

[[bin]]

name = "hello_world"
```

The project can then be built using the `build` command.

``` bash
$ cargo build
   Compiling hello_world v0.1.0 (file:/home/yourname/projects/hello_world)
$ ./target/hello_world
Hello, world!
```

Dependencies may be expressed by adding sections of the form `[dependency.name]` with a property of type `git` or `path` with the appropriate path.

``` toml
[dependencies.color]

git = "https://github.com/bjz/color-rs"
```

# Printing

Use the `print!`, `println!`, and `write!` macros to print strings in a `printf`-like manner. If just printing an actual string, then just use the direct functions `print` and `println`. The `format!` macro also exists for creating a `String` with a specific format. See the docs for [`std::fmt`](http://static.rust-lang.org/doc/master/std/fmt/index.html) for more information.

# Input

Simple input can be achieved using the `io::stdin()` function to retrieve a [`BufferedReader`] which has functions such as `read_line()`.

[`BufferedReader`]: http://doc.rust-lang.org/std/io/struct.BufferedReader.html

``` rust
let input = io::stdin().read_line().ok().expect("failed to read line");
println!("{}", input);
```

# Documentation

A built-in tool called `rustdoc` can be used to generate documentation, similar to [haddock](/notes/haskell#documentation) with Haskell. Documentation is primarily provided using "doc comments" which are simply line comments of three forward slashes: `///`. If a `//!` comment is used, it applies to the parent of the comment rather than what follows. Doc comments are implicitly converted to `doc` attributes by the compiler, which themselves may be used explicitly as well.

``` rust
/// this documentation applies to S
pub struct S { a: int }

fn test() {
  //! this documentation applies to `test`
}

#[doc = "
Calculates the factorial of a number.
"]
pub fn factorial(n: int) -> int { ... }
```

The `doc` attribute can also be used to control how documentation is emitted, such as to prevent inlining documentation of a `pub use`.

``` rust
#[doc(no_inline)]
pub use std::option::Option;
```

Code examples can be provided within documentation as regular code blocks, and those marked as being Rust code can be tested using the `--test` option with `rustdoc`. The `ignore` directive can be provided instead of a language to not run that code block but still highlight it as rust. The `should_fail` directive specifies that the code block is expected to fail, and thus shouldn't be considered an error when testing. The `no_run` directive specifies that the code block should be compiled but not run. The `test_harness` directive compiles the code as if `--test` were passed to the compiler.

~~~ rust
```rust{.example}
// some code
```

```test_harness
#[test]
fn foo() {
  panic!("runs and registers as failure");
}
```
~~~

Tests are facilitated by implicitly adding `extern crate doc-target` at the top of each code example.

If a line is prefixed with `#`, then the line won't show up in the resulting HTML but it _will_ be used when testing the code block.

~~~ rust
```
# fn fib(n: int) { n + 2 }
spawn(proc() { fib(200); })
```
~~~

API stability can be documented by using stability attributes. There are six stability levels, where levels _stable_, _frozen_, and _locked_ convey a guarantee of backwards-compatibility. The levels are listed below in order of increasing stability.

Level        Meaning
------       --------
deprecated   should no longer be used
experimental may change or be removed
unstable     still under development
stable       will not change significantly
frozen       very stable; unlikely to change
locked       never change unless there are bugs

Individual items such as functions and structs can be annotated with an API stability level of the form `#[level]`.

``` rust
#[deprecated = "was stupid to begin with"]
pub fn learn_visual_basic() {
  // ...
}

#[stable]
pub fn use_rust() {
  // ...
}
```

When importing another crate, lints can be enabled to, for example, warn when a given item is used from it with a stability less than the one specified.

``` rust
#![warn(unstable)]
extern crate other_crate;
```

# Debugging

Existing debugging infrastructure such as [GDB](/notes/gdb) can be used to debug Rust programs. It may be useful to set the optimization level to 0 and enable debugging symbols in Cargo using a development profile:

``` toml
[profile.dev]
opt-level = 0
debug = true
```

There's also the `RUST_BACKTRACE` environment variable which, when set (e.g. to 1), will output a backtrace when a Rust program crashes.
