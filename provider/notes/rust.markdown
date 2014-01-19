---
title: Rust
published: December 29, 2013
excerpt: An exciting multiparadigm language from Mozilla
comments: off
toc: left
---

I took a shot at [learning Go] recently and I found its simplicity to be refreshing. However, after having tasted the simplicity of concurrency in Haskell and being familiar with good software design practice, something rubs me the wrong way about how prevalent the use of globals is and the direct use of classic concurrency primitives like mutexes on shared memory. Go does have channels builtin with syntax specifically for reading and writing them. However, I do feel there was a missed opportunity in innovating in this area.

My main resources are the [tutorial] and [manual]. As usual, oftentimes some things will be directly from the source, with my commentary surrounding it.

[learning Go]: /notes/go/
[tutorial]: http://static.rust-lang.org/doc/master/tutorial.html
[manual]: http://static.rust-lang.org/doc/master/rust.html

* toc

# Pattern Matching

Pattern matching takes the place of regular switch statements. Pattern matches don't fall through. The pipe operator `|` can be used to combine multiple patterns into one arm. The underscore `_` is a wildcard pattern as in Haskell. As in Haskell, matches must be exhaustive. Every case is separated by commas, unless block expressions are used. As in Haskell, pattern matching can be used to bind values. Pattern guards are added using `if` expressions:

``` rust
match number {
  0     => println("zero"),
  1 | 2 => println("one or two"),
  3..10 => println("three to ten"),
  _     => println("something else")
}
```

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

# Enums

Enums in Rust feel similar to algebraic datatypes in Haskell. Enums can be C-like, in which case they can optionally be given specific values. The specific values can be converted to an `int` using the cast operator `as`. Enum variants can be structs as well:

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

Tuples are available and are most similar to Haskell's. **Tuple structs** behave like both structs and tuples. They're like tuples with names. Tuple structs with a single field are similar to Haskell newtypes and are often called the same thing in Rust. These provide a distinct type from the type they wrap. Newtypes' wrapped values can be extracted using the dereference operator `*`:

``` rust
let mytup: (int, int, f64) = (10, 20, 30.0)

struct MyTup(int, int, f64);
let mytup: MyTup = Mytup(10, 20, 30.0)

struct GizmoId(int);
let my_gizmo_id: GizmoId = GizmoId(10);
let id_int: int = *my_gizmo_id;
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

# Destructors

Destructors are functions responsible for cleaning up resources used by an object that is no longer accessible. Objects are never accessible after their destructor has been called, so it's not possible to fail from accessing freed resources. Further, when a task fails, destructors of all objects in the task are called.

The `~` sigil represents a unique handle for a memory allocation on the heap:

``` rust
{
  let y = ~10; // allocated on the heap
}
// destructor frees heap memory as soon as `y` goes out of scope
```

# Ownership

An object's lifetime is determined by its owner, either a variable or a task-local garbage collector. Ownership is recursive so that mutability is inherited recursively and a destructor destroys the contained tree of owned objects. Variables are to-level owners and destroy teh contained object when they go out of scope:

``` rust
struct Foo { x: int, y: ~int }

{
  // `a` is owner of struct and its fields
  let a = Foo { x: 5, y: ~10 };
}
// `a` goes out of scope, destructor for y's `~int` is called

// `b` is mutable, so the objects it owns are also mutable
let mut b = Foo { x: 5, y: ~10 };
b.x = 10
```

# Boxes

Owned boxes are denoted by the `~` sigil and they use a dynamic memory allocation so that it's always the size of a pointer, regardless of the contained type. This can be used to construct a recursive type, like a list:

``` rust
enum List {
  Cons(u32, ~List),
  Nil
}

let list = Cons(1, ~Cons(2, ~Cons(3, ~Nil)));
```

The `list` represents an owned tree of values.

# Move Semantics

Move semantics in Rust is different from [C++11 move semantics]. In Rust, the "move" refers to moving ownership. Rust performs a shallow copy for parameter passing, assignment, and returning from functions. Performing such a shallow copy is treated by Rust as "moving ownership" of the value, so that the original source location can no longer be used unless it is reinitialized. A move can be avoided by cloning:

[C++11 move semantics]: /notes/cpp#move-semantics

``` rust
let mut xs = Nil;
let ys = xs;

// error if attempt to use `xs`

xs = Nil; // xs can be used again

let x = ~5;
let y = x.clone(); // y is a newly allocated box
let z = x; // no new memory alloc'd, x can no longer be used
```

Mutability can be changed by moving it to a new owner:

``` rust
let r = ~13;
let mut s = r; // box becomes mutable
*s += 1;
let t = s; // box becomes immutable
```

# References

References are non-owning views of a value which are obtained using the address-of operator `&` and dereferenced using the `*` operator. In patterns, the `ref` keyword can be used to bind a variable name by reference rather than by value. [^cpp11_ref]

[^cpp11_ref]: This really reminds me of C++11's [`std::ref`](http://en.cppreference.com/w/cpp/utility/functional/ref).

``` rust
fn eq(xs: &List, ys: &List) -> bool {
  match (xs, ys) {
    (&Nil, &Nil) => true,
    (&Cons(x, ~ref next_xs), &Cons(y, ~ref next_ys)) if x == y => eq(next_xs, next_ys),
    _ => false
  }
}

let xs = Cons(5, ~Cons(10, ~Nil));
let ys = Cons(5, ~Cons(10, ~Nil));
assert!(eq(&xs, &ys));
```

# Parameterized Types

Types can be parameterized similar to C++ class templates:

``` rust
enum List<T> {
  Const(T, ~List<T>),
  Nil
}

fn prepend<T>(xs: List<T>, value: T) -> List<T> {
  Cons(value, ~xs)
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
      (&Cons(ref x, ~ref next_xs), &Cons(ref y, ~ref next_ys)) if x == y => next_xs == next_ys,
      _ => false
    }
  }
}
```

# Large Parameters and Return Values

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

let x = ~foo(); // allocates ~ box and writes u64's directly to it
```

# More on References

Referneces don't imply ownership, they're "borrowed". Reference parameters are often used to allow functions to work with all manner of different allocated types.

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
let mut owned = ~20;
let mut value = 30;
let borrowed = &mut value;

*owned = *borrowed + 100;
*borrowed = 1000;

let point &@~Point { x: 10.0, y: 20.0 };
println!("{:f}", point.x); // dereferences all three levels
```


