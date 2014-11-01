---
title: Go
published: December 20, 2013
excerpt: A simple C-like language from Google
comments: off
toc: left
---

I've been meaning to learn Go for a while now. I've recently come to renew an interest in simple, no non-sense languages like Python, and what I imagine Go to be. The resources I'll be using are the [go specification], [go tour], and [effective go]. A lot of this is straight from these sources, with my commentary of how I come to understand it.

[go specification]: http://golang.org/ref/spec
[go tour]: http://tour.golang.org
[effective go]: http://golang.org/doc/effective_go.html

* toc

# Packages

Packages can be imported with the `import` statement. They can also be grouped inside parentheses. The containing package can be specified with the `package` statement. Names that begin with capitalized letters are exported by the package:

``` go
package somepkg

import (
  "fmt"
  "math/rand"
)
```

# Functions

When there are consecutive named function parameters of the same type, we can omit the type of all but the last:

``` go
func add(x, y int) int {
  return x + y
}
```

Functions can return multiple values:

``` go
func swap(x, y string) (string, string) {
  return y, x
}
```

Result values can be named and set within the function, in which case `return` returns them as they are:

``` go
func split(sum int) (x, y int) {
  x = sum * 4 / 9;
  y = sum - x
  return
}
```

Functions are also values, and can be used to create closures:

``` go
hypot := func(x, y float64) float64 {
  return math.Sqrt(x * x + y * y)
}
```

# Variables

Variables are declared with `var`. Multiple variables can be initialized in a tuple-assignment fashion, in which case the type can be omitted as it will be inferred from the initializers. Inside functions, `:=` can be used in place of a var declaration with inferred type. Variables can be declared constant with `const` but not using the `:=` syntax:

``` go
var i, j = 1, 2
k := 3
const World = "hello"
```

Numeric constants have arbitrary precision and don't overflow.

The following are the available types in Go. Type conversions are possible through `T(v)`.

```
bool

string

int  int8  int16  int32  int64
uint uint8 uint16 uint32 uint64 uintptr

byte // alias for uint8

rune // alias for int32, represents a Unicode code point

float32 float64

complex64 complex128
```

# Control Structures

There's only one looping construct and that's the `for` loop. The `for` loop in Go doesn't use parentheses around the parameters, but the braces around the code to loop are mandatory.

``` go
for i := 0; i < 10; i++ {
  sum += i
}
```

As in C, pre and/or post conditions can be omitted. If both are omitted, it's just like a while loop:

``` go
for sum < 1000 {
  sum += sum
}
```

Further still, the condition can be omitted to achieve a forever-loop.

`if` conditions can take a statement to run before the condition, scoped until the end of the `if` block (i.e. including `else` blocks):

``` go
if v:= math.Pow(x, n); v < lim {
  return v
} else {
  fm.Printf("%g >= %g\n", v, lim)
}
```

The `switch` statement case bodies break by default, unless they end in a `fallthrough` statement:

``` go
switch os := runtime.GOOS; os {
case "darwin":
  fmt.Println("OS X")
case "linux":
  fmt.Println("Linux")
default:
  fmt.Println("%s", os)
}
```

The condition on the switch statement can be omitted, in which case it acts like a long if-then-else chain.

# Structures

Structures can be defined with the `struct` keyword and given a name with `type`. Fields are accessed using a dot as in other languages. When creating a struct, any non-initialized field takes on that type's default value:

``` go
type Vertex struct {
  X int
  Y int
}

func main() {
  // type Vertex
  v := Vertex{1, 2}
  v.X = 4

  // Y:0 implicit
  var r = Vertex{X: 1}

  // constructs pointer to newly allocated struct
  // new(T) == &T{}
  var q = &Vertex{1, 2} // type *Vertex
}
```

# Pointers

There are pointers but no pointer arithmetic. Indirection through pointers is transparent:

``` go
p := Vertex{1, 2}
q := &p
q.X = 1e9
```

A value of type `T` can be allocated with the `new` function:

``` go
var t *T = new(T)
t := new(T)
```

# Arrays and Slices

An array of `n` type `T` elements is created with the `[n]T` syntax. A slice points to an array and includes a length and capacity. Slices can be re-sliced with the syntax `s[lo:hi]` as in Python slices, which essentially points to a region of the same array. Slices can be created with `make` which takes the type of array, the length and optional capacity. The `range` function can be used to iterate over a slice or map, yielding an `index, value` per iteration, or if the `value` isn't needed simply omit it:

``` go
var a [2]int
a[0] = 1

s := make([]int, 0, 5)
var z []int

if z == nil {
  fmt.Println("nil")
}

p := []int{2, 3, 5}
fmt.Println("p[1:4] == ", p[1:4])

for i, v := range p {
  fmt.Printf("%d: %d\n", i, v)
}

for i := range p {
  // something
}

for _, v := range p {
  // something
}
```

# Maps

Maps take the form `map[key]value`. A `nil` map is empty and can't be assigned to. Map literals accept a trailing comma. When reading a map element, if the key doesn't exist in the map, the value returned is the default value for the map value type:

``` go
type Vertex struct {
  Lat, Long float64
}

var m map[string]Vertex

func main() {
  m = make(map[string]Vertex)
  m["Bell Labs"] = Vertex{40.68433, -74.39967}

  var l = map[string]Vertex{
    "Google": Vertex{
      37.42202, -122.08408
    },
  }
  
  // top-level type is just a type name, can be omitted
  var s = map[string]Vertex{
    "Bell Labs": {40.68433, -74.39967},
  }

  delete(m, "Bell Labs")

  // ok is true if key exists
  // else val is default value for map val type
  val, ok := m["Bell Labs"]
}
```

# Methods

While Go doesn't have classes, it has a syntax for defining methods on structures. In the function declaration, the method receiver appears before the function name. This can also be done on any type we create, such as with `type`, but _not_ on types from other packages or basic types. A pointer receiver can be modified and doesn't have to copy the entire structure on the method call, just like in C/C++:

``` go
func (v *Vertex) Abs() float64 {
  return math.Sqrt(v.X * v.X + v.Y * v.Y)
}

v := &Vertex{3, 4}
v.Abs()

type MyFloat float64

func (f MyFloat) Abs() float64 {
  if f < 0 {
    return float64(-f)
  }
  return float64(f)
}

f := MyFloat(-math.Sqrt2)
f.Abs()
```

# Interfaces

Interfaces types are defined by a set of methods. A value of interface type can hold any value that implements those methods. It's important to note that types implement interfaces implicitly, there's no explicit declaration of intent as with Haskell typeclasses (`class T where`) or traditional Java interfaces (`class A implements B`):

``` go
type Abser interface {
  Abs() float64
}

var a Abser
a = MyFloat(-math.Sqrt2)
a = &v
a = v // error: Vertex doesn't implement Abser, *Vertex does
```

Interfaces can also be composed of other interfaces:

``` go
type Reader interface {
  Read(b []byte) (n int, err error)
}

type Writer interface {
  Write(b []byte) (n int, err error)
}

type ReadWriter interface {
  Reader
  Writer
}
```

# Errors

In Go, an error is anything that can be described as a string. Interface type `error` defines a single method `Error` that returns a string:

``` go
type error interface {
  Error() string
}

type MyError struct {
  When time.Time
  What string
}

func (e *MyError) Error() string {
  return fmt.Sprintf("at %v, %s", e.When, e.What)
}

func run() error {
  return &MyError{
    time.Now(),
    "it didn't work"
  }
}

func main() {
  if err := run(); err != nil {
    fmt.Println(err)
  }
}
```

# Concurrency

A goroutine is a lightweight thread managed by the Go runtime. The evaluation of the function and its arguments occurs in the current goroutine but execution of the function occurs in a new goroutine:

``` go
func say(s string) {
  for i := 0; i < 5; i++ {
    time.Sleep(100 * time.Millisecond)
    fmt.Println(s)
  }
}

func main() {
  go say("world")
  say("hello")

  // javascript-like idiom of defining
  // and calling a function
  go func() {
    // something
  }()
}

```

Channels are typed conduits through which data of that type can be sent, a lot like Haskell channels. The `<-` operator is used to send and receive values, where the data travels in the direction of the arrow. Sending and receiving blocks until the other side is ready, allowing simple synchronization of the different goroutines:

``` go
ch := make(chan T) // create channel of type T
ch <- v            // send v to channel ch
v := <-ch          // receive from channel ch into v
```

There are also bounded, buffered channels, in which case the buffer doesn't block on send as long as the buffer isn't full, nor on receive as long as the buffer isn't empty:

``` go
buffered := make(chan T, 100)
```

Channels can be closed with the `close` function. Receivers can test if the channel is closed by accepting the second return value. Alternatively, looping with a `range` on the channel will read values until the channel is closed:

``` go
v, ok := <-ch

for i := range ch {
  fmt.Println(i)
}
```

The `select` statement lets a goroutine wait on multiple communication operations, a lot like the POSIX `select()` system call. It'll block until one of its cases can run, randomly choosing if multiple are ready. A `default` case can be specified for when no other case is ready, particularly useful when the `select` is used within a forever loop, as is usually the case. The `default` case is usually used to try a send or receive without blocking:

``` go
for {
  select {
  case c <- x:
    x, y = y, x + y
  case <-quit:
    fmt.Println("quit")
    return
  default:
    // ...
  }
}
```

