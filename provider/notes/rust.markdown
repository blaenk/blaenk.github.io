---
title: Rust
published: December 29, 2013
excerpt: An exciting multiparadigm language from Mozilla
comments: off
toc: left
---

I took a shot at [learning Go] recently and I found its simplicity to be refreshing. Rust is a similar language from Mozilla which I've been interested in for a while now.

My main resources are the [tutorial] and [manual]. As usual, oftentimes some things will be directly from the source, with my commentary surrounding it.

[learning Go]: /notes/go/
[tutorial]: http://static.rust-lang.org/doc/master/tutorial.html
[manual]: http://static.rust-lang.org/doc/master/rust.html

* toc

# Printing

Use the `print!`, `println!`, and `write!` macros to print strings in a `printf`-like manner. If just printing an actual string, then just use the direct functions `print` and `println`. The `format!` macro also exists for creating a `~str` with a specific format. See the docs for [`std::fmt`](http://static.rust-lang.org/doc/master/std/fmt/index.html) for more information.

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

Patterns can also be bound to variables as in Haskell and Scala:

``` rust
match age {
  a @ 0..20 => println!("{} years old", a),
  _         => println!("older than 21")
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

In short, blocks such as `{ expr1; expr2 }` are considered a single expression and evaluate to the result of the last expression if it's not followed by a semicolon, otherwise the block evaluates to `()`.

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

An object's lifetime is determined by its owner, either a variable or a task-local garbage collector. Ownership is recursive so that mutability is inherited recursively and a destructor destroys the contained tree of owned objects. Variables are to-level owners and destroy the contained object when they go out of scope:

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

Move semantics in Rust is similar to [C++11 move semantics], where the "move" refers to moving ownership. Rust performs a shallow copy for parameter passing, assignment, and returning from functions. Performing such a shallow copy is treated by Rust as "moving ownership" of the value, so that the original source location can no longer be used unless it is reinitialized. A move can be avoided by cloning:

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

The `&` symbol has multiple context-dependent meanings in Rust, as in C++ [^cpp_references]. Remembering that expressions are for constructing and patterns for destructuring, a `&` in a pattern _dereferences_ the expression being destructured. This can be useful to avoid having to manually dereference the variable within the body using `*`.

For example, the `fold` function takes a folding operation in the form of a closure that is passed the accumulator and a reference `&T` to the current element, which means that within the body it would have to be manually dereferenced with `*`, and we can avoid that by specifying the `&` in the pattern.

[^cpp_references]: In C++, an `&` on the lhs can be the ref-qualifier, i.e. _bind by reference_, and on the rhs it can be the address-of operator.

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

A `&` in a function signature means that the parameter accepts a reference. Owned pointers `~` and types that implement the `Deref` trait are automatically converted to references when passed to such functions. Variables on the stack, on the other hand, can yield a reference manually using `&`, which is the address-of operator in that context.

``` rust
fn eq(xs: &int, ys: &int) -> bool { ... }

let xs = ~34;
let ys = 34;
assert!(eq(xs, &ys));
```

A `&` that's applied to an rvalue is a shorthand for creating a temporary and taking its address.

``` rust
let explicit = Point {x: 3.0, y: 4.0};
reference_taking_func(&explicit);

let shorthand = &Point {x: 3.0, y: 4.0};
reference_taking_func(shorthand);
```

A mutable reference `&mut` is one through which the pointed-to variable can be modified, provided the pointed-to variable is also mutable. When a mutable reference exists, no other kind of reference can exist, regardless of whether it's mutable or not. Compare this with a regular, immutable reference of which many can exist to the same variable, since the variable cannot be modified through it.

There is also the `ref` keyword that can be used in a pattern. In this context, it is similar to C++'s ref-qualifier, which means bind by reference. This is required when matching on something that can't or we don't want to be taken by value.

For example, we want to get a reference to the `Foo` in `&Option<Foo>`. Taking it by value would require moving ownership, but since the `Option` is borrowed we don't have ownership to begin with. Instead we bind the `foo` by reference which does away with the need to take ownership.

``` rust
let some = Some(foo);
let borrowed = &some;

match borrowed {
  &Some(ref foo) => *foo
}
```

There's also a language inconsistency where `"str"` is of type `&str`, but to get a type of `&&str` it doesn't suffice to simply do `&"str"`, because that _too_ is treated as `&str`, so _two_ are needed: `&&str`.

In the following example, `contains_key` takes a value of type `&K` where `K` is the key type. The key type is `&str`, so `&K` is `&&str`. Because of the language inconsistency noted above, two `&`'s must prefix the string literal in order to yield a `&&str` to pass to `contains_key`.

Issue [#10105] is a proposal to fix this. I've also read that [DST] will fix this as a consequence of its implementation.

[#10105]: https://github.com/mozilla/rust/issues/10105
[DST]: https://github.com/mozilla/rust/issues/6308

``` rust
use collections::Hashmap;

// type inferred as HashMap<&str, &str>
let mut items = HashMap::new();

items.insert("key1", "value1");
items.insert("key2", "value2");

if !items.contains_key(& &"key1") {
  println!("not present");
}
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
let mut owned = ~20;
let mut value = 30;
let borrowed = &mut value;

*owned = *borrowed + 100;
*borrowed = 1000;

let point = &@~Point { x: 10.0, y: 20.0 };
println!("{:f}", point.x); // dereferences all three levels
```

# Vectors and Strings

Fixed-size vectors are unboxed blocks of memory that owns the elements it contains.

``` rust
let fixed_size: [1, 2, 3];
let five_zeroes: [int, ..5] = [0, ..5];
```

Unique vectors are dynamically-sized and have a destructor that cleans up the allocated memory on the heap. Unique vectors also own the elements they contain.

``` rust
let mut numbers = ~[1, 2, 3];
numbers.push(4);

let more_numbers: ~[int] = numbers;
```

Strings are represented as vectors of `u8` with a guarantee of containing a valid UTF-8 sequence.

``` rust
let mut string = ~"fo";
string.push_char('o');
```

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

Vectors can be destructured using pattern matching:

``` rust
let numbers: &[int] = &[1, 2, 3];
let score = match numbers {
  [] => 0,
  [a] => a * 10,
  [a, b] => a * 6 + b * 4,
  [a, b, c, ..rest] => a * 5 + b * 3 + c * 2 + rest.len() as int
};
```

# Ownership Escape Hatches

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

There are also garbage collected pointers via `std::gc::Gc` under ownership of a task-local garbage collector. These pointers allow the creation of cycles. Individual `Gc` pointers don't have destructors.

``` rust
use std::gc::Gc;

// fixed-sized array allocated in garbage-collected box
let x = Gc::new([1, 2, 3]);
let y = x; // doesn't perform a move, unlike Rc
let z = x;

assert_eq!(*z.borrow(), [1, 2, 3]);
```

With shared ownership, mutability can't be inherited so the boxes are always immutable. Dynamic mutability is possible via types like `std::cell::Cell`. `Rc` and `Gc` types are not sendable, so they can't be used to share memory between tasks. This is instead possible via the `extra::arc` module.

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

Owned closures are written with `proc`, e.g. `proct(arg: int)`, and they own values that can be sent safely between processes. The values they close over are copied, and they become owned by the closure. These are particularly used in concurrent scenarios, particularly for spawning tasks.

Despite there being different types of closures, functions that expect a `||` closure can accept any kind of closure provided they have the same arguments and return types. For this reason, higher-order functions should usually define their closure types as `||` so that callers can pass any kind of closure.

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

let s = Circle(Point {x: 1.0, y: 2.0}, 3.0);
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

# Generics

Generics are available in Rust which allows for generic functions to be defined, such as a `map` function. Generic functions in Rust have similar performance characteristics as templates in C++ because it performs _monomorphization_ which generates a separate copy of each generic function at each call site, which is specialized to the argument types, optimized specifically for them. This is similar to C++ template instantiation.

``` rust
fn map<T, U>(vector: &[T], function: |v: &T| -> U) -> ~[U] {
  let mut accumulator = ~[];
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
  elements: ~[T]
}
// Stack<int>

enum Option<T> {
  Some(T),
  None
}
// Option<int>
```

# Traits

Traits are similar to type classes in Haskell. They allow the expression of _bounded polymorphism_ which limits the possible types a type parameter could refer to. For example, the `clone` method which allows the copying of a type, isn't defined on all types because it can't be safely performed on all types, due to user-defined destructors for example. Traits allow to bound the polymorphism of a generic function by specifying that a type parameter must implement the `Clone` trait in this case to limit the types on which the function can work:

``` rust
fn head<T: Clone>(v: &[T]) -> T {
  v[0].clone()
}
```

There are three traits that are automatically derived and implemented for applicable types:

* `Send` is for sendable types, which don't contain managed boxes, managed closures, or references.
* `Freeze` is for constant/immutable types, types that don't contain anything intrinsically mutable.
* `'static` is for non-borrowed types, which don't contain any references or any data whose lifetime is bound to a particular stack frame, or they are types where the only contained references have `'static` lifetime.

The `Drop` trait can be used to define destructors via its method `drop`.

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

Traits contain zero or more method signatures. In the following trait, it is said that `Printable` provides a `print` method with the given signature:

``` rust
trait Printable {
  fn print(&self);
}

impl Printable for int {
  fn print(&self) { println!("{:?}", *self) }
}

impl Printable for ~str {
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
impl Printable for ~str {
  fn print(&self) { println(*self) }
}
```

Traits may be parameterized by type variables.

``` rust
trait Seq<T> {
  fn length(&self) -> uint;
}

impl<T> Seq<T> for ~[T] {
  fn length(&self) -> uint {
    self.len()
  }
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
fn print_all<T: Printable + Clone>(printable_things: ~[T]) {
  // can clone() then print()
}
```

It's possible to have method calls to trait types that are dynamically dispatched. With this, it's possible to define a function that performs the `draw` method on type that implements `Drawable`, which itself provides the `draw` method. This particular method takes a borrowed pointer to an array of owned values that implement the `Drawable` trait. Such an array must be constructed by casting each element to `~Drawable`:

``` rust
fn draw_all(shapes: &[~Drawable]) {
  for shape in shapes.iter() {
    shape.draw()
  }
}

draw_all([circle as ~Drawable, rectangle as ~Drawable])
```

The three storage classes enforce a set of traits that their contents must fulfill in order to be packaged up in a trait object for that storage class:

* contents of `~owned` traits must fulfill the `Send` bound
* contents of `@managed` traits must fulfill the `'static` bound
* contents of `&reference` traits are not constrained by any bound

Trait objects automatically fulfill their respective trait bounds. This can be overridden by specifying a list of bounds on the trait type, such as `~Trait:Send+Freeze` which indicates that contents must fulfill `Send` and `~Freeze`.

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

let concrete = @CircleStruct { center: Point {x: 3f, y: 4f}, radius: 5f }
let mycircle: @Circle = concrete as @Circle;
let nonsense = mycircle.radius() * mycircle.area();
```

Some traits can be automatically derived, similar to Haskell. The full list is `Eq`, `TotalEq`, `Ord`, `TotalOrd`, `Encodable`, `Decodable`, `Clone`, `DeepClone`, `IterBytes`, `Rand`, `Default`, `Zero`, `FromPrimitive`, and `Show`:

``` rust
#[deriving(Eq)]
struct Circle { radius: f64 }

#[deriving(Rand, ToStr)]
enum ABC { A, B, C }
```

# Modules

The content of all source code that the compiler directly had to compile in order to end up with a particular binary is collectively called a **crate**. A crate is a unit of independent compilation in Rust. Using an already compiled library in code doesn't make it part of a crate.

There exists a hierarchy of modules where the root is referred to as **crate root**. Global paths begin with the root path `::`, all other paths are local paths, similar to the distinction between absolute `/`-prefixed paths and relative paths.

Module contents are private by default. They can be made explicitly public with the `pub` qualifier. In fact, visibility restrictions are only applicable at module boundaries. On the other hand, `struct` fields are public by default and are made explicitly private with `priv`. Since visibility restrictions only apply at module boundaries, a private field of a `struct` defined within a module is itself accessible within that module:

``` rust
mod farm {
  pub fn chicken() { ... }

  pub struct Farm {
    priv chickens: ~[Chicken],
    farmer: Human
  }

  impl Farm {
    fn feed_chickens(&self) { ... }
    pub fn add_chickens(&self, c: Chicken) { ... }
  }

  pub fn feed_animals(farm: &Farm) {
    farm.feed_chickens();
  }
}

fn main() {
  ::farm::chicken();

  let f = make_farm();
  f.add_chicken(make_chicken());
  farm::feed_animals(&f);
  f.farmer.rest();
}
```

Source files and modules are not the same thing. The only file that's relevant when compiling is the one that contains the body of the crate root. Declaring a module without a body prompts the compiler to look for a file named after that module or for a file named `mod.rs` in a folder named after that module.

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

It's possible to import by wildcard or selectively, similar to Scala. It's also possible to rename an imported item:

``` rust
use farm::{chicken, cow};

#[feature(globs)]
use farm::*;

use egg_layer = farm::chicken;
```

Likewise, it's possible to re-export items from another module:

``` rust
mod farm {
  pub use self::barn::hay;
}
```

It's common to use existing libraries, which in rust are simply referred to as other crates. The `extern mod` declaration is used to be able to reference other crates, similar to `extern` in C. `extern mod`-imported items can be shadowed by local declarations and `use`-imported items, so they must go before both. Crates declared in an `extern mod` declaration are looked for in the library search path, which can be expanded with the `-L` switch.

``` rust
extern mod num;

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

Crates can contain metadata used for the resultant libraries, such as the package ID:

``` rust
#[crate_id = "farm#2.5"];

// specify that it's a library
#[crate_type = "lib"];
```

This information can be used to select the crate from the `extern mod` declaration:

``` rust
extern mod farm = "farm#2.5";
// or
extern mod my_farm = "farm";
```

For example, here's an example library, its compilation, and usage:

~~~ {.rust text="world.rs"}
#[crate_id = "world#0.42"];
#[crate_type = "lib"];

pub fn explore() -> &'static str { "world" }
~~~

~~~ {.rust text="main.rs"}
extern mod world;
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
extern mod std;

// to prevent: in crate root
#[no_std];
```

As well as the following into every module body:

``` rust
use std::prelude::*;

// to prevent: in any module
#[no_implicit_prelude];
```

# Named Lifetimes

It's possible to give a lifetime a name in order to explicitly specify the lifetime of a returned reference. In the following case, the named lifetime `'r` is associated with the parameter `p: &Point`, this named lifetime is then also mentioned in the return type. What this means is that the returned reference will have the same lifetime as the passed in pointer parameter `p`. In effect, this means that the returned reference will remain valid as long as `p` itself remains valid.

This is necessary because, generally it's only possible to return references derived from a parameter to the function, and named parameters explicitly specify which parameter that is.

``` rust
struct Point {x: f64, y: f64}

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

This would lead to a **compilation error**, because `select_unit()` is supposed to return a value with a lifetime of `r`, which is the lifetime of `a` and `b`, _not_ the lifetime of the local `Circle`.

``` rust
fn select_unit<'r, T>(threshold: f64, a: &'r T, b: &'r T) -> &'r T {
  let shape = Circle(Point {x: 0., y: 0.}, 1.);
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

# Tasks

Tasks are _green threads_ similar to those in Haskell and perhaps Erlang. Tasks have dynamically sized stacks, starting out with small ones and dynamically growing as required. This means that unlike C/C++, it's not possible to write beyond the end of the stack. Tasks provide failure isolation and recovery. When a problem occurs, the runtime destroys the entire task, and other tasks can monitor each other for failure.

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

Pipes are used for communication between tasks [^channels]. Each pipe is defined by a pair of endpoints: one for sending and another for receiving. In Rust, a channel is the **sending** endpoint of a pipe and the **port** is the receiving endpoint.

[^channels]: Pipes remind me of Go's channels, or Haskell's.

The following code creates a channel for sending and receiving `int` types. Note that `Chan` and `Port` are both sendable types that may be captured into task closures or transferred between them.

``` rust
let (port, chan): (Port<int>, Chan<int>) = Chan::new();

spawn(proc() {
  let result = expensive_computation();
  chan.send(result);
});

other_expensive_computation();
let result = port.recv();
```

A regular `Chan` and `Port` created by `Chan::new` can't be used by more than one task. Attempting to do so leads to an error, since the first task to use it becomes its owner. Instead it's possible to use a `SharedChan` by calling `clone` on the endpoint.

``` rust
let (port, chan) = Chan::new();

for init_val in range(0u, 3) {
  // create a SharedChan
  let child_chan = chan.clone();

  spawn(proc() {
    child_chan.send(expensive_computation(init_val));
  });
}

let result = port.recv() + port.recv() + port.recv();
```

The above example is contrived and could've been done with three separate channels:

``` rust
let ports = vec::from_fn(3, |init_val| {
  let (port, chan) = Chan::new();
  spawn(proc() {
    chan.send(some_expensive_computation(init_val));
  });
  port
});

let result = ports.iter().fold(0, |accum, port| accum + port.recv() );
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
use sync::Arc;

fn main() {
  let big_data = ~[1, 2, 3, 4, 5];
  let data_arc = Arc::new(big_data);

  for num in range(1u, 10) {
    let (port, chan) = Chan::new();

    // send arc
    chan.send(data_arc.clone());

    spawn(proc() {
      // receive arc
      let local_arc : Arc<~[f64]> = port.recv();

      // data pointed to by arc
      let task_data = local_arc.get();

      expensive_computation(task_data);
    });
  }
}
```

## Failure

Exceptions can be raised in Rust using the `fail!()` macro. Exceptions are unrecoverable within a single task. When an exception is raised, the task unwinds its stack --- running destructors and freeing memory along the way --- then exits.

However, tasks may notify each other of failure. The `try` function is similar to `spawn` but blocks until the child is finish, yielding a return value of `Result<T, ()>` [^either] which has two variants: `Ok(T)` and `Err`. This `Result` can then be pattern-matched to determine the outcome of the task, with `Err` representing termination with an error. **Note** currently, it's not possible to retrieve a useful error value from the `Err` variant, since `try` always returns `()`:

[^either]: This `Result` type is a lot like Haskell's `Either`.

``` rust
let result: Result<int, ()> = task::try(proc() {
  if some_condition() {
    calculate_result()
  } else {
    fail!("oops!");
  }
});

assert!(result.is_err());
```

## Duplex Streams

A `DuplexStream` can be used to both send and receive from one task to another. The following code creates a task that continually receives a `uint` and sends it back converted to a string.

``` rust
fn stringifier(channel: &DuplexStream<~str, uint>) {
  let mut value: uint;
  loop {
    value = channel.recv();
    channel.send(value.to_str());
    if value == 0 { break; }
  }
}

let (from_child, to_child) = DuplexStream::new();

spawn(proc() {
  stringifier(&to_child);
});

from_child.send(22);
assert!(from_child.recv() == ~"22");
```

# Iterators

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
let ys = xs.rev_iter().skip(1).map(|&x| x * 2).collect::<~[int]>();
```
