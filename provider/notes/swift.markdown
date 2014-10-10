---
title: Swift
published: July 5, 2014
excerpt: A surprising new language from Apple
comments: off
toc: left
---

People have been wishing for a new language to work with in the Apple environment. There were rumors that Ruby would be chosen, but that didn't seem to pan-out. Instead, a variety of different companies have created products that allow different languages to compile to iOS, such as [Xamarin], [RubyMotion], and many others, particularly game engines like [Unity] and the [Unreal Engine].

[Xamarin]: http://xamarin.com/platform
[RubyMotion]: http://www.rubymotion.com/
[Unity]: http://unity3d.com
[Unreal Engine]: https://www.unrealengine.com/

Then, in WWDC 2014, Apple announced [Swift]. It appears to be influenced by Objective-C [^objectivec_interop], Rust, Haskell, Ruby, Python, and C#. It has completely surprised me how well designed the language appears to be. It seems like a mixture of Go and Rust.

*[WWDC]: World Wide Developer Conference

[Swift]: https://developer.apple.com/swift/

[^objectivec_interop]: With which it must interoperate, after all.

An interesting characteristic of Swift is that code written at the global scope is considered the entry point of the program.

* toc

# Basics

A distinction is made between mutable and immutable variables. The `let` keyword defines an immutable variable---a constant. The `var` keyword defines a mutable variable. Types can be inferred in general situations. Names can be surrounded in backticks to treat them literally, i.e. to use a Swift keyword as a name.

## Implicit Conversions

There are no implicit conversions. Instead, instances must be created of the desired type.

``` swift
let width = 94
let widthLabel = label + String(label)
```

## Interpolation

Values can be interpolated into strings by surrounding them in parentheses and escaping that group with a backslash.

``` swift
let apples = 3
let summary = "I have \(apples) apples."
```

## Collections

Arrays and dictionaries have literal support with brackets. Empty ones can be created using initializer syntax. If the type of the collection can be inferred, then `[]` or `[:]` can be used as an empty array and dictionary respectively.

``` swift
var shoppingList = ["one", "two", "three"]
shoppingList[1] = "changed"

var occupations = [
  "key1": "val1",
  "key2": "val2"
]

occupations["key3"] = "val3"

let emptyArray = String[]()
let emptyDict  = Dictionary<String, Float>()
```

## Optionals

Optional values have special syntax with a `?` suffix on the type to denote that they are optional. Optional values are either `nil` or contain the value stored for that type. Declaring an optional without providing a value automatically sets it to `nil`.

``` swift
var optionalString: String? = "hi"
var surveyAnswer: String? // set to nil
```

Optionals can be compared explicitly against `nil` to see if they have a value. A `!` suffix on an optional can then _force unwrap_ the contained value, though note that force unwrapping an optional with no contained value produces a run-time error.

An alternative to this is to perform an _optional binding_ to bind the contained value within the body in the event that there is a value present. Note that either `let` or `var` can be used for the binding, depending on the situation.

``` swift
var optionalString: String? = "hi"

if optionalString == nil {
  println("force unwrapped: \(optionalString!)")
}

var possibleNumber = "123"

if let actualNumber = possibleNumber.toInt() {
  println("let binding: \(actualNumber)")
}
```

When working with optional values, the `?` can be written before operations such as methods, properties, and subscripting. If the value before the `?` is `nil`, the result of the whole expression is `nil`. Otherwise, the operation continues. In either case, the result is an optional value.

Tuples can also be marked optional by following the type with a question mark:

``` swift
var someTuple: (Int, Int)? = nil
```

There is a nil coalescing operator as in C# that unwraps the optional's value if it exists or returns a default value if it's `nil`.

## Implicit Optionals

If a given optional can be safely assumed to always contain a value once it is set for the first time, it can get tedious to have to explicitly unwrap it or test it each time. Implicit optionals can be used for these situations and are written with a `!` suffix in the type declaration instead of a `?`. Implicit optionals are more or less like specifying in the type declaration that forced unwrapping should occur whenever it's used.

Implicit optionals can be used to mitigate a scenario of two-phase initialization and reference cycles. The `self` reference can't be used elsewhere---such as in other initializers---until all fields have been initialized. This poses a problem with a field that is of a type that should contain a reference to the object being constructed: a circular reference.

Normally a field of this type would be instantiated by passing `self` to that field's initializer so that it can set its reference to the parent object, but again the object wouldn't be finished initializing by then because that field remains to be initialized, but that field can't be initialized since it requires a reference to the initializing object `self` which again can't be used until the object is finished initializing, and so on.

A work-around for this is to make that value be an implicit optional type. This way---like all optionals that aren't given an initial value---it will receive an initial value of `nil`, meaning that the object will essentially be finished initializing after all other fields have been initialized, and so `self` can be passed to the initializer of the problem field. The fact that it is an implicit optional allows it to continue to be used transparently as any other field without needing to explicitly unwrap the optional.

``` swift
class Country {
  let name: String
  let capitalCity: City! // implicit optional
  init(name: String, capitalName: String) {
    self.name = name
    self.capitalCity = City(name: capitalName, country: self)
  }
}

class City {
  let name: String
  unowned let country: Country // weak reference
  init(name: String, country: Country) {
    self.name = name
    self.country = country
  }
}
```

## Numbers

Integers exist in 8, 16, 32, and 64 bit forms in both signed and unsigned forms. Their names are like the C equivalents but without underscores and capitalized names: `UInt8` or `Int8` for example. Bare `Int` and `UInt` types correspond to the word-size of the machine, i.e. on a 64-bit platform it will be an `Int64`.

The bounds of integers can be found using the `min` and `max` properties, e.g. `UInt8.max`.

Binary literals are possible with a `0b` prefix, octal with a `0o` prefix, and hexadecimal with a `0x` prefix.

Assigning literal numbers to types that cannot hold them, e.g. negative number in unsigned integer or value that exceeds bounds, emits a compile-time error.

Numeric type conversion is opt-in and explicit, and required in order to mix numbers of different types in the same expression. This is done by instantiating a new value of the target type out of the source value. Failure to do this leads to a compile-time error.

Note that this does not apply to literals, as they can be inferred to the correct type.

``` swift
let twoThousand: UInt16 = 2_000
let one: UInt8 = 1
let twoThousandAndOne = twoThousand + UInt16(one)

let three = 3
let pointOneFourOneFiveNine = 0.14159
let pi = Double(three) + pointOneFourOneFiveNine
```

## Type Aliases

Type aliases are possible through the `typealias` keyword.

``` swift
typealias AudioSample = UInt16
var maxAmplitudeFound = AudioSample.min
```

## Tuples

Tuples are available as in Haskell and Rust, and can be destructured with pattern matching. Tuple components can be ignored in pattern matching with `_` as in Haskell. Tuples can be accessed by 0-based indices. Individual tuple elements can be named at time of definition, and individual components can be accessed by those names.

``` swift
let tpl = (404, "not found")
println("status message is \(tpl.1)")

let http200Status = (statusCode: 200, description: "OK")
println("status message is \(http200Status.description)")
```

## Assertions

Assertions are possible with the `assert` function which takes a condition and an optional message which _cannot_ use string interpolation.

# Operators

The assignment `=` operator doesn't return a value to avoid errors related to the equality `==` operator.

Arithmetic operators detect and disallow value overflow, though this can be opted-out of by prefixing the operator with `&`.

There are range operators `..<` (exclusive) and `...` (inclusive) to specify a range of values.

The remainder operator `%` also works on floating-point numbers.

The unary `+` prefix operator doesn't do anything, but can be used for symmetry where other variables or literals have the unary `-` prefix operator.

Compound assignment operators like `+=` **do not** return a value.

There are also identity operators `===` and `!==` which test whether two object references refer to the same instance.

# Strings and Characters

The `String` type in Swift is interchangeable with Foundation's `NSString` and they are value types, so that strings are copied when they're passed to functions or assigned to other variables. Value semantics clarify ownership ambiguities. The compiler optimizes string use so that the copying only takes place if necessary.

Unicode scalars can be inserted into strings using the following notation, where the `n` sequences represent hexadecimal digits:

Notation    Type
---------   -----
Single-byte `\xnn`
Arbitrary   `\u{nnnn}`

The `isEmpty` property can be used to determine if the string is empty.

String mutability is determined by the way the variable is defined.

Strings can be iterated over with a `for-in` loop to access individual `Character` values.

Single characters can be initialized using a single character in a string literal.

``` swift
let yenSign: Character = "¥"
```

The number of characters in a string can be calculated using the global `countElements` function, which iterates through the length of the string to count the number characters, taking into account the fact that different Unicode characters may require more than one 16-bit unit.

The original `length` function of `NSString` doesn't do this, and instead returns the number of 16-bit units. Swift's `utf16Count` function on `String` types is its equivalent.

``` swift
let test = "Koala 🐨, Snail 🐌, Penguin 🐧, Dromedary 🐪"
println("test has \(countElements(test)) characters")
```

Characters can be added to strings using the `append` method.

*[LHS]: Left-Hand Side

Strings can be compared using the `==` operator, or using the `hasPrefix` and `hasSuffix` methods.

The `uppercaseString` and `lowercaseString` properties can yield their respective equivalents.

## Unicode

Characters are represented in Unicode by one or more Unicode scalars. A unicode scalar is a 21-bit number (and a name) for a character or modifier, e.g. `U+0061`{.path} is LOWERCASE LATIN LETTER A, `a`.

Unicode scalars are encoded as small chunks called code units, which in UTF-8 would be 8-bit code units, and so on.

Unicode strings can be represented in Swift as a collection of characters, UTF-8 or UTF-16 code units, or 21-bit Unicode scalar values.

Collection Of                Accessor
---------                    ---------
characters                   `for-in`
UTF-8 code units             `utf8`
UTF-16 code units            `utf16`
21-bit Unicode scalar values `unicodeScalars`

For example `utf8` is a property of type `UTF8View` which is a collection of `UInt8` values. In this representation, the last four code units are the four-byte UTF-8 representation of the DOG FACE character.

``` swift
let dogString = "Dog!🐶"

for codeUnit in dogString.utf8 {
  print("\(codeunit) ")
}

// 68 111 103 33 240 159 144 182
```

Unicode scalars can be interpolated into a string as their string value.

# Collections

Declaring collections as immutable with `let`, when the collection doesn't need to be changed, allows the compiler to optimize the collections.

Array types are declared as `Array<SomeType>` or in the more idiomatic shorthand `[SomeType]`.

``` swift
var shoppingList: [String] = [
  "Eggs",
  "Milk",
  "Flour",
  "Baking Powder",
  "Chocolate Spread",
  "Cheese",
  "Butter"
  ]
```

Arrays support `count` and `isEmpty` properties, as well as an `append` method, which supports appending either a single item or an array of items.

Arrays can be accessed and modified via subscript. It's also possible to modify multiple values at once using range syntax within the subscript. A run-time error is triggered if the range is exceeds the array's bounds. If less items are provided than the specified range allows, the remaining elements are removed.

``` swift
shoppingList[4...6] = ["Bananas", "Apples"]
```

The `insert(atIndex:)` method can be used to insert an item before the item at the provided index. The converse is also possible via `removeAtIndex`, which removes the specified item and also returns it. The `removeLast` method is a shorter way of using `removeAtIndex` on the last element.

A for-in loop can be used to iterate over every item in the list. If an index is wanted, the global `enumerate` function can yield an index-value pair for every item in the array [^python_enumerate].

[^python_enumerate]: Much like the [Python function](https://docs.python.org/2/library/functions.html#enumerate) of the same name.

``` swift
for (index, value) in enumerate(shoppingList) {
  println("Item \(index + 1): \(value)")
}
```

Arrays can be initialized using initializer syntax. The default initializer initializes an empty array. There's also an initializer that takes parameters `count` and `repeatedValue` to initialize an array of specified count each with the specified value.

``` swift
var threeDoubles = [Double](count: 3, repeatedValue: 0.0)
// threeDoubles = [0.0, 0.0, 0.0]

// or leveraging type inference
var threeDoubles = Array(count: 3, repeatedValue: 0.0)
```

Dictionaries are written as key-value pairs separated by commas in a list. Shorthand for dictionary types is `[KeyType: ValueType]`. Empty dictionaries can be written using the shorthand `[:]`, similar to `[]` with arrays.

``` swift
var airports: Dictionary<String, String> = [
  "TYO": "Tokyo",
  "DUB": "Dublin"
  ]

// or using the shorthand
var airports: [String: String] = [ /* ... */ ]

// or leveraging type inference
var airpots = ["TYO": "Tokyo", "DUB": "Dublin"]
```

The `updateValue(forKey:)` can be used to update a value for a given key as with subscript notation, except that it returns an optional of the old value after performing the update or `nil` if no value existed, allowing one to know if an update took place or not.

Accessing a value via subscript notation yields an optional of the contained type, equaling `nil` if there is no value associated with the provided key.

As a result, assigning `nil` to a given key using subscript notation removes that key-value pair from the dictionary. The `removeValueForKey` function returns the value after removing it of type optional, yielding `nil` if no value existed for that key.

``` swift
airports["LHR"] = "London"
airports.updateValue(forKey: "LHR", "London Heathrow")
```

Iterating over a dictionary with a for-in loop yields a key-value pair for every association.

``` swift
for (airportCode, airportName) in airports {
  println("\(airportCode): \(airportName)")
}
```

The `keys` and `values` properties yield a collection of the keys and values, respectively.

# Control Flow

It's possible to use an underscore `_` in for loops when a certain value isn't needed within the body.

``` swift
let power = 10
var answer = 1

for _ in 1...power {
  answer *= base
}
```

If conditionals _must_ be booleans or optionals.

Switch statements must be exhaustive, and automatically break out at the end of each case body unless the `fallthrough` keyword is used to explicitly express that a fallthrough is desired. The `default` keyword may be used to specify a catch-all case.

Switch statements can use ranges to test the value for inclusion.

``` swift
switch 5 {
  case 0: // ...
  case 1...3: // ...
  case 4...7: // ...
  default: // ...
}
```

Tuples' component values can also be tested using pattern matching, and the underscore `_` can be used to ignore certain components. Components can be bound to temporary names using the `let` or `var` keywords, where it can prefix the entire tuple as a simple way of binding every component.

``` swift
switch somePair {
  case (let x, 0): // uses x
  case (0, let y): // uses y
  case let (x, y): // uses both x and y
}
```

Switch statement cases can specify `where` clauses to check for additional conditions.

``` swift
switch somePair {
  case let (x, y) where x == y: // ...
  default: // ...
}
```

Loops and switch statements can be labeled for the purpose of breaking to or continuing specific control structures, oftentimes useful for breaking out of outter-loops given inner-control structure conditions. This is possible by prefixing the control structure with a label, followed by a colon `:`.

``` swift
labelName: while condition {
  while otherCondition {
    if someCondition {
      break labelName
    }
  } 
}
```

# Functions

Functions that don't have an explicit return type gain an implicit return type of `Void`, which can be written simply as `()`, and don't need to specify the return arrow or return type. Function return values can also be ignored if they are not needed.

If returning a tuple that has named components, they don't need to be named at return-time if they are already named in the function return type.

By default, function parameter names are referred to as _local parameter names_ because they're only available within the function's body. It's also possible to provide _external_ parameter names which can make functions be self-documenting by _requiring_ each parameter to be named each time the function is called. External parameter names are specified before the local parameter names, separated by a space.

Alternatively, the parameter name can be prefixed by the hash symbol `#` to make the local name be the same as the external name.

``` swift
func someFunc(externalName localName: Int) {
  // ...
}

func otherFunc(#sameName: Int) {
  // ...
}
```

Default parameter values can be specified by using the assignment operator. Swift automatically creates an external name the same as the local name for default parameters, as if it had been prefixed by `#`. It's possible to opt-out of this by providing an underscore `_` as the explicit external name.

``` swift
func join(#string: String, toString: String, withJoiner: String = "") {
  // ...
}
```

Variadic parameters are also possible by appending three dots `...` after the variadic parameter's type. The values passed to the parameter are available within the function body as an array.

``` swift
func mean(numbers: Double...) {
  var total = 0.0
  for number in numbers {
    total += number
  }
  return total / Double(numbers.count)
}
```

It's possible to define a parameter as variable, which allows the parameter to be modified _within_ the function, avoiding the need to explicitly create a temporary copy of the parameter.

``` swift
func someFunc(var param: Int) -> Int {
  ++param 
  return param
}
```

In-out parameters, denoted by the `inout` keyword, have the ability to actually modify the variable passed into the function, much like pointers and references. The variable that's passed to this parameter needs to be prefixed by an ampersand `&` to denote that the function may modify the variable [^explicit_modification].

[^explicit_modification]: I have to say, I like the fact that this needs to be explicitly declared in the function declaration and at the call site. This completely side-steps ambiguities found in C++ with reference parameters. Since this has to be expressed explicitly at the call-site, the reader of the source can know that the passed variable is being modified by the function without having to consult the documentation or definition.

Constants or literals can't be passed as in-out parameters. Variadic parameters can't be in-out parameters. In-out parameters can't be marked as `var` or `let`.

``` swift
func addOne(inout a: Int) {
  ++a
}

let anInt = 3

addOne(&anInt)

// anInt = 4
```

Function types can be used like any other types, such as for annotating parameters, return types, or variables. Note that functions can be nested within each other.

``` swift
func callFunc(func: (Int, Int) -> Int) -> String {
  return "result is \(func(4, 5))"
}

func retFunc() -> (Int, Int) -> Int {
  func add(a: Int, b: Int) -> Int {
    return a + b
  }

  return add
}
```

# Closures

Closures are available in Swift, and all memory handling of captured entities is handled automatically. Global functions and nested functions are special cases of closures. Specifically, there are three kinds of closures:

* **global functions** have a name and don't capture any values
* **nested functions** have a name and capture values from the enclosing function.
* **closure expressions** have no name and can capture values from the surrounding context

Closure expressions are written in the form of braces which contain the closure body, where the first line in the body is the parameter list and return type, followed by the `in` keyword and the closure's body of statements.

``` swift
{ (parameters) -> returntype in
  statement1
  statement2
}
```

Consider the global `sorted` function which takes an array of values and a function used to sort them, specifically, it should return `true` if the first value should appear before the second, and `false` otherwise. Once sorted, it returns a new, sorted array.

A named, global function may be provided as the sorting function. Alternatively, a closure expression may be provided. The types of the parameters may be inferred from the context, however, specifically the type of the parameter it's being passed to. Further, single statement closures may omit the `return` keyword, as it's implied.

Within closures, shorthand argument names are available in the form of `$n` where `n` starts with 0 corresponding to the first parameter.

If the final parameter in a function is of function type and a closure is going to be passed, it can be passed after the parameter list, which is known as trailing closure syntax [^rust_do]. Further, if the closure expression is the _only_ argument provided to the function, then the pair of parentheses is can be omitted.

[^rust_do]: This is very much like a feature in early Rust---which has since been removed---called do notation. Scala supports this via multiple parameter lists.

Finally, operators are generally implemented as functions, and so they alone may be passed in many cases [^haskell_operators].

[^haskell_operators]: This is very much like Haskell, except for the lack of [sections](http://www.haskell.org/haskellwiki/Section_of_an_infix_operator).

``` swift
func backwards(s1: String, s2: String) -> Bool {
  return s1 > s2
}

var reversed = sorted(names, backwards)
var reversed = sorted(names, { (s1: String, s2: String) -> Bool in
  return s1 > s2
})
var reversed = sorted(names, { s1, s2 in return s1 > s2 }) // inferred types
var reversed = sorted(names, { s1, s2 in s1 > s2 }) // implicit return
var reversed = sorted(names, { $0 > $1 }) // shorthand arguments
var reversed = sorted(names) { $0 > $1 } // trailing closure
var reversed = sorted(names, >) // operator functions
```

The `@auto_closure` attribute can be used to specify that a closure argument should be taken as a closure even if it's not within braces.

``` swift
func myassert(predicate: @auto_closure () -> Bool) {
  #if !NDEBUG
    if predicate() {
      abort()
    }
  #endif
}

myassert(someExpensiveComputation() != 42)
```

# Enumerations

Enumerations may provide computed properties, instance methods, initializers, be extended by extensions, and adopt protocols.

It's not necessary to provide a raw value for members of an enumeration. If no raw value is provided, then it must be of type string, character, integer, or floating-point. If no raw value is provided, then each member is its own kind of value. Enumeration members may also specify associated values of any type to be stored [^rust_enumerations].

[^rust_enumerations]: This is very much like Rust enumerations or Scala case classes.

When the enumeration's type can be inferred, a shorter dot syntax can be used which omits the enumeration type.

``` swift
enum CompassPoint {
  case North, South, East, West
}

var someValue = CompassPoint.North

// or
var someValue: CompassPoint = .North

switch someValue {
  case .North: // ...
  case .South: // ...
  case .East:  // ...
  case .West:  // ...
}
```

The components of enumeration members' associated values can be bound to names within a switch statement in the same manner as tuples.

``` swift
// associated values
enum Barcode {
  case UPCA(Int, Int, Int)
  case QRCode(String)
}

let code = BarCode.QRCode("TEST")

switch code {
  case let .UPCA(numSystem, ident, check): // ...
  case .QRCode(let code): // ...
}
```

Raw values must be unique within the enumeration. If an integer is used, it auto-increments for remaining members if they don't specify a raw value. Raw values may be accessed using the `toRaw` function, and an enumeration member can be produced given a raw value using the `fromValue` function, which yields an optional of the enumeration's type in case member associated with that raw value isn't defined.

``` swift
enum Planet {
  case Mercury = 1, Venus, Earth, Mars, Jupeter, Saturn, Uranus, Neptune
}

let earthsOrder = Planet.Earth.toRaw() // = 3
let possiblePlanet: Planet? = Planet.fromRaw(7) // = Planet.Uranus
```

# Classes and Structures

The only difference between classes and structures is that classes can inherit from other classes, type cast, specify deinitializers, and allow for more than one reference to a particular instance.

``` swift
class SomeClass {
  // ...
}

struct SomeStruct {
  // ...
}
```

Structures and enumerations are value types that are always copied when passed around (subject to compiler optimizations). Classes are reference types so that assigning it to another variable or constant simply creates another reference to the instance.

Structures have automatically generated memberwise initializers.

Testing whether two references point to the same instance is possible via the identity operators `===` and `!==`.

# Properties

Stored properties store constant or variable values that are part of an instance. Naturally, if a structure instance is assigned to a constant, properties of that instance may not be modified even if they are defined as variable. On the other hand, instances of a class assigned to a constant can still have their properties modified.

``` swift
struct FixedLengthRange {
  var firstValue: Int
  let length: Int
}
```

Lazy stored properties, denoted by the `lazy` declaration modifier, are properties whose initial values are not calculated until the first time they are accessed. Since the initial value may not be retrieved until after initialization completes, lazy properties must be defined as variable and not constants, since constant properties must be initialized before initialization completes.

``` swift
class SomeClass {
  lazy var expensiveValue = ExpensiveObject()
}
```

Computed properties can define getters and setters to compute a property on the fly. The name to use for the value provided to the setter can be placed within parentheses after the `set` keyword, and if omitted, defaults to `newValue`.

``` swift
struct Rect {
  var origin = Point() // .x and .y
  var size = Size()    // .height and .width

  var center: Point {
    get {
      // calculate x and y given origin and width/height
    }

    set(newCenter) {
      // set new origin given new center and width/height
    }
  }
}
```

Read-only computed properties are ones that only define a getter. They must still be defined as variables because their value is not fixed. Since only the getter is defined, the `get` and `set` keywords may be omitted.

``` swift
struct Cuboid {
  var width = 0.0, height = 0.0, depth = 0.0 
  var volume: Double {
    return width * height * depth
  }
}
```

Property observers can monitor changes to a property's value, specifically before and after a value has been set, even if the new value is the same. Property observers can be added for any stored properties, even inherited ones, except lazy stored properties.

The `willSet` and `didSet` keywords are used to establish property observers for before and after a value is stored, respectively. Both kinds of property observers can define a name for a value to be passed as with computed property setters, and if omitted, take on the name of `newValue` and `oldValue` for `willSet` and `didSet` respectively.

``` swift
class StepCounter {
  var totalSteps: Int = 0
  willSet(newTotalSteps) {
    println("About to set to \(newTotalSteps)")
  }
  didSet {
    if totalSteps > oldValue {
      println("added \(totalSteps - oldValue) steps")
    }
  }
}
```

Global and local variables can also make use of computed and observing properties. Global variables are always computed

Type properties are properties that belong to the type itself, such that there is only one copy of the property regardless of the number of instances created. Specifically, value types may define stored and computed type properties, while reference types can only define computed type properties, read-only or read-write.

Stored type properties must _always_ be given a default value, since types can't define initializers.

Type properties may be defined for value types with the `static` keyword placed before the introducer keyword and for reference types with the `class` keyword.

``` swift
struct SomeStructure {
  static var storedProperty = "Some Value"
}

class SomeClass {
  class var computedTypeProperty: Int {
    return 3
  }
}

SomeStructure.storedProperty // = "Some Value"
SomeClass.computedTypeProperty // = 3
```



# Methods

By default, Swift treats the first parameter of a method as a local name only, but subsequent parameters as both local and external names. This is to facilitate the convention of ending a method name in a preposition like `with`, `for`, or `by`, thereby preventing the first parameter from causing redundancy.

``` swift
class Counter {
  var count: Int = 0
  func incrementBy(amount: Int, numberOfTimes: Int) {
    count += amount * numberOfTimes
  }
}

let counter = Counter()
counter.incrementBy(5, numberOfTimes: 3)
```

This can be changed by specifying a separate external name or prefixing the name with a `#` to use the same local name as the external name.

It's also possible to opt-out of this behavior for subsequent names by specifying an explicit external name as an underscore `_`.

By default, instance methods of value types like structures and enumerations cannot modify properties of that instance, unless marked with the `mutating` keyword. Note that mutating methods can't be called on immutable values.

Mutating methods can replace the instance with another one by assigning to `self`.

``` swift
struct Point {
  var x = 0.0, y = 0.0
  mutating func moveByX(deltaX: Double, y deltaY: Double) {
    self = Point(x: x + deltaX, y: y + deltaY)
  }
}

enum TriStateSwitch {
  case Off, Low, High
  mutating func next() {
    switch self {
      case Off:
        self = Low
      case Low:
        self = High
      case High:
        self = Off
    }
  }
}
```

Type methods are methods that are written on the type itself, more or less like static methods in other languages. They're written by prefixing the `func` keyword by `class` in a class and `static` in structures and enumerations.

In type methods, the `self` keyword refers to the type itself.

``` swift
class SomeClass {
  class func someTypeMethod() {
    // ...
  }
}

SomeClass.someTypeMethod()
```

# Subscripts

Overloaded subscript accessors/setters can be defined for classes, structures, and enumerations. The syntax is facilitated by the `subscript` keyword and looks similar to computed properties, so that if the subscript is read-only, the `get` and `set` keywords can be omitted.

``` swift
subscript(index: Int) -> Int {
  get {
    // return value
  }

  set(newValue) {
    // set value
  }
}
```

It's also possible to define a subscript with multiple parameters.

``` swift
struct Matrix {
  var grid: [Double]
  // ...
  subscript(row: Int, column: Int) -> Double {
    get {
      return grid[(row * columns) + column]
    }

    set {
      grid[(row * columns) + column] = newvalue
    }
  }
}

var matrix = Matrix(rows: 2, columns: 2)
matrix[0, 1] = 1.5
```

# Inheritance

Inheritance is what differentiates classes from structures and enumerations. Classes can call, access, and override methods, properties, and subscripts of the superclass. Overrides are ensured by the compiler to actually be overriding an item in the superclass. It's also possible to add property observers to inherited properties.

Unlike other languages, classes in Swift don't automatically inherit from a universal base class like `Object`, so an inheritance relationship must be defined explicitly by following the class name with a colon `:` and the name of the superclass from which to inherit.

``` swift
class SomeClass: SomeSuperclass {
  // ...
}
```

The superclass's initializer can be called within the initializer of the subclass, usually at the very beginning.

``` swift
init() {
  super.init()

  self.otherProperty = 3
}
```

Overriding is explicitly denoted by the `override` keyboard. The `super` keyword can be used to refer to the superclass version of methods, properties, or subscripts.

``` swift
class Car: Vehicle {
  override func description() -> String {
    return super.description() + "; traveling at \(speed) mph"
  }
}
```

A read-only property can be overridden as a read-write property by providing a getter and setter. The reverse is not possible, however, a read-write property overridden as a read-only property.

An overriding setter and overriding property observer are mutually exclusive, since the changes can already be observed in the setter.

It's possible to outright prevent overriding of a method, property, or subscript in subsequent subclasses by marking the item as final with the `final` declaration modifier by placing this marker before the introducer keywords `var`, `func`, `class func`, `subscript`, or even `class` to mark the entire class as final.

# Initialization

Swift requires that all classes and structures set _all_ of their stored properties to an appropriate initial value by the time the instance is created. This can be achieved either within an initializer or, more idiomatically, by assigning a default value within the property's definition. Note that this bypasses property observers.

Since there is no differentiating initializer name for each initializer in a class, the parameters are used as a means of differentiating, and so Swift provides automatic external names for every parameter if one isn't provided. Further, if an external name is defined---if an underscore `_` isn't specified as the external name---then the external names must _always_ be used when calling the initializer.

Optionals are given a `nil` value if no initial value is provided.

Constant properties can be modified at any point during initialization. With respect to classes, a constant property can only be modified by the class that defined it.

Structures automatically receive a memberwise initializer that has parameters named after the corresponding properties. However, defining a custom initializer removes this default memberwise initializer. One method to circumvent this is to define the custom initializers within an extension.

``` swift
struct Size {
  var width = 0.0, height = 0.0
}

let twoByTwo = Size(width: 2.0, height: 2.0)
```

It's also possible to use a closure or function to initialize a property, though due to two-phase initialization, no other property, instance method, or `self` be used unless the property is marked as `lazy`, which would denote that it wouldn't accessed until after initialization.

``` swift
class SomeClass {
  let someProperty: Int = {
    return 4
  }()
}
```

## Initializer Delegation

Designated initializers are initializers that can _fully_ initialize an instance by initializing all properties in its class and its superclass. Convenience initializers are usually simpler initializers which delegate to designated initializers. There are three rules that are enforced for initializer delegation:

1. designated initializers must call a designated initializer in the immediate superclass
2. convenience initializers must call another initializer in the same class
3. convenience initializers must ultimately end up calling a designated initializer

In other words, designated initializers must delegate up and convenience initializers must delegate across.

Designated initializers use the simple syntax denoted by the `init` keyword, while convenience initializers use the same syntax prepended by the `convenience` keyword.

``` swift
init(params) {
  // ...
}

convenience init(params) {
  // ...
}
```

The `required` modifier can be used on an initializer to specify that every subclass of the class must implement that initializer. This modifier must also be included in every subclass' implementation of the required initializer.

## Two-Phase Initialization

Classes are initialized in two phases. In the first phase, each property is assigned an initial value by the class that introduced it. In the second phase, each class is given the opportunity to customize its stored properties.

Two-phase initialization prevents property values from being accessed before they're initialized or changed by another initializer unexpectedly.

Two-phase initialization is validated by four safety checks:

1. **designated initializers** must ensure that _all_ properties introduced by its class are initialized before delegating to a superclass initializer
2. **designated initializers** must delegate to a superclass initializer _before_ assigning a value to an inherited property, otherwise it would be overwritten by the superclass initializer.
3. like #2, **convenience initializers** must delegate to another initializer before assigning a value to _any_ property, otherwise it would be overwritten by the designated initializer
4. **initializers** can't call instance methods, read values of instance properties, or refer to `self` _until_ the first phase of initialization is complete

A run-down of two-phase initialization follows:

1. first phase: initializer called on a class
    2. memory is allocated but not initialized
    3. designated initializer confirms all stored properties introduced by the class have a value and their memory are initialized
    3. designated initializer delegates to superclass initializer to do the same
    4. continue until top of inheritance chain is reached
    5. instance's memory is considered to be fully initialized
2. second phase: working back from top to bottom of inheritance chain
    1. designated initializers have the option to customize the instance further, as the are able to access `self` and modify its properties, call instance methods, etc.
    2. convenience initializers can customize the instance and to work with `self`

## Automatic Initializer Inheritance

Initializers are automatically inherited in two situations, assuming that any newly introduced properties have default values provided.

1. the class doesn't define any designated initializers, it automatically inherits all of the superclass' _designated initializers_
2. the class provides implementations for _all_ of the superclass' designated initializers---either implicitly via #1 or explicitly---then it automatically inherits all of the superclass _convenience initializers_

# Deinitialization

Deinitializers are called right before a class instance is deallocated, and are written using the `deinit` keyword. Superclass deinitializers are inherited automatically and are automatically called at the end of a class' deinitializer implementation, even if one isn't explicitly provided.

``` swift
class Player {
  var coinsInPurse: Int
  // ...
  deinit {
    Bank.receiveCoins(coinsInPurse)
  }
}
```

# Automatic Reference Counting

Automatic Reference Counting (ARC) tracks memory usage as in modern Objective-C. Whenever a class instance is assigned to a property, constant, or variable, that target makes a strong reference to the instance, preventing it from being deallocated so long as the strong reference remains.

*[ARC]: Automatic Reference Counting

Circular strong references can prevent involved instances from ever being deallocated, causing a memory leak in the application.

## Weak References

A weak reference is one that doesn't establish a strong reference to the instance it refers to, thereby not preventing ARC from disposing of the referenced instance when necessary. Weak references are denoted by the `weak` keyword before the property or variable declaration, and are used when it's possible for that reference to have no referred instance during its lifetime.

Weak references must be declared as optionals since they are allowed to not refer to any instance, and must be declared as variables since they're allowed to change the instance to which they refer.

ARC is able to deallocate instances referred to by weak references---since they don't establish strong references---and ARC automatically sets the weak reference to `nil` if this occurs.

## Unowned References

Unowned references are similar to weak references, except they are assumed to _always_ hold a value. They are denoted by the `unowned` keyword on non-optional types.

Since unowned references are assumed to always hold a value, and as a result aren't optional types, ARC can't set them to `nil` when the instances to which they refer have been deallocated. Consequently, attempting to access the instance through that reference in such circumstance would yield a run-time error.

[Implicit optionals](#implicit-optionals) can be used to simplify situations in which both properties in a reference cycle must always have a value and should never be `nil` once initialized. In such a scenario, one class defines the property as an unowned reference while the other defines it as an implicit optional.

## Closure Reference Cycles

Strong reference cycles may also occur if a closure is assigned to a property of an instance that captures that instance by accessing an instance property or method. Such reference cycles occur because closures are reference types, such that assigning a closure to a property assigns a reference to that closure and not the closure itself.

Capture lists define the rules to use when capturing reference types within the closure's body, declaring them as either weak or unowned references. Capture lists are written within brackets `[]` and consist of lists of pairs of reference types, unowned or weak, and the parameter being captured.

Unowned references should be used when the closure and instance always refer to each other, and will be deallocated at the same time. Weak references should be used when the captured reference may become `nil` at any time and so can be checked within the closure's body.

``` swift
lazy var someClosure: (Int, String) -> String = {
  [unowned self] (index: Int, stringToProcess: String) -> String in

  // or with parameter list inferred
  [unowned self] in

  // ...
}
```

# Optional Chaining

Optional chaining can be used on properties, methods, and subscripts in an elaborate expression such that if any point in its evaluation yields `nil`, the entire expression evaluates as `nil` [^maybe_monad]. Otherwise, the result of the expression is an optional containing the resulting value.

[^maybe_monad]: This is an elegant way to mimic Haskell's `Maybe` monad behavior.

When using optional chaining, the resulting value will _always_ be **one** level of optional regardless of the number of levels of chaining used or whether the resulting value was already optional.

It's possible to set a property's value, assign to a subscript, or calla  mutating method or operator through optional chaining.

``` swift
let obj = SomeType()

if let test = obj.optionalProperty?.property {
  // ...
}

if let test = obj.someSubscript?[0].name {
  // ...
}

if let test = obj.methodReturningOptional()?.uppercaseString {
  // ...
}

var testScores = ["A": [1, 2, 3], "B": [4, 5, 6]]
testScores["A"]?[0] = 91
testScores["A"]?[0]++
```

# Type Casting

If an array is defined containing instances of related classes, their most common base class is inferred as the type of the array. When iterating through the array, each instance will be treated as the base class, therefore losing the functionality provided by their actual class. To get that functionality back, it's necessary to check their type and downcast them to their appropriate type.

The type of an instance can be checked with the type check operator `is`, which returns `true` if the instance is a _subclass_ of the provided type.

``` swift
for item in items {
  if item is SomeType {
    // ...
  }
}
```

An instance can be downcast with the type cast operator `as`. Since downcasting may fail, the `as?` variant returns an optional, whereas the `as` performs a forced unwrapping on that optional, which would yield a run-time error if the downcast fails.

``` swift
for item in items {
  if let someType = item as? SomeType {
    // ...
  }
}
```

There are two type aliases for working with non-specific types. The `AnyObject` alias can represent an instance of any class type, while the `Any` alias can represent an instance of any type except function types.

For example, Cocoa APIs might return arrays of `[AnyObject]`, even though it may be obvious what the type of the elements may be. For this, the forced downcast operator `as` can be used to downcast each item to the appropriate type. A temporary for the downcast instance can be avoided by providing the downcasting directive in the for loop itself.

``` swift
let someObjects: [AnyObject] = [ /* all are SomeType */ ]

for item in someObjects as [SomeType] {
  // ...
}
```

The `is` and `as` operators can be used within switch statement cases. Note that the forced cast operator `as` can be used safely within a switch statement, such that if the downcast fails then that case statement simply does not apply.

``` swift
var items = [Any]()
items.append(0)
items.append(0.0)
items.append("hello")
items.append((3.0, 5.0))

for item in items {
  switch item {
  case 0 as Int:
  case 0 as Double:
  case let someInt as Int:
  case let someDouble as Double where someDouble > 0:
  case is Double:
  case let (x, y) as (Double, Double):
  }
}
```

# Nested Types

Swift allows nesting types within other types, which is particularly useful when the nested type need only be available within the outter type. Nested types can be referred to using dot notation, traversing the types that it's nested under to get to it.

# Extensions

Extensions can add new functionality to existing classes, structures, or enumerations---even to types for which the original source is not available, which is known as retroactive modeling.

Extensions are declared using the `extension` keyword. If an extension is extending a type to adopt one or more protocols, they're written as in a class declaration.

``` swift
extension Sometype {
  // ...
}

// or adopting a protocol
extension SomeType: SomeProtocol, AnotherProtocol {
  // ...
}
```

Extensions can add new computed properties---but _not_ stored properties---to an existing type.

``` swift
extension Double {
  var km: Double { return self * 1_000.0 }
  var m: Double  { return self }
  var cm: Double { return self / 100.0 }
}

let oneInch = 25.4.mm
```

Extensions can add new convenience initializers---but _not_ designated initializers---to an existing type. When a custom initializer is defined for a class, the default and memberwise initializers no longer become available. These initializers remain available if the custom initializer is defined via an extension.

``` swift
extension Rect {
  init(center: Point, size: Size) {
   // delegate to the memberwise initializer
   self.init(/* ... /*)
  }
}
```

Extensions can add new instance and type methods to existing types. These methods can also modify the instance, and such methods must be marked as `mutating` for structures and enumerations.

``` swift
extension Int {
  func repetitions(task: () -> ()) {
    for i in 0..<self { task() }
  }

  mutating func square() {
    self = self * self
  }
}

3.repetitions {
  println("hello")
)

var someInt = 3
someInt.square() // someInt = 9
```

Extensions can define new subscripts to existing types.

``` swift
extension Int {
  subscript(digitIndex: Int) -> Int {
    var decimalBase = 1
    for _ in 1...digitIndex {
      decimalBase *= 10
    }
    return (self / decimalBase) % 10
  }
}

746381295[8] // 8th digit is 7
```

Extensions can add nested types to existing types.

``` swift
extension Character {
  // nested type
  enum Kind {
    case Vowel, Consonant, Other
  }

  // computed property
  var kind: Kind {
    switch String(self).lowercaseString {
    case "a", "e", "i", "o", "u": return .Vowel
    case /* consonant */: return .Consonant
    default: return .Other
    }
  }
}

var char: Character = "c"
char.kind // Character.Kind.Consonant
```

# Protocols

Protocols are like interfaces in object-oriented languages, typeclasses in Haskell, and traits in Rust. Protocols are declared with the `protocol` keyword. Types can express the fact that they adopt a particular set of protocols by listing them after the type name and a colon. If the type is a class and it has a superclass, the superclass comes before any protocol in the list.

``` swift
protocol SomeProtocol {
  // ... 
}

struct SomeStructure: SomeProtocol, AnotherProtocol {
  // ...
}

class SomeClass: SuperClass, Protocol1, Protocol2 {
  // ...
}
```

Protocols can require that adopting types provide an instance or type property with a given name, type and access policy (i.e. read-only or read-write), regardless of whether it is a stored or computed property.

Property requirements are always written as variables with the `var` keyword. Type properties should always be prefixed with the `class` keyword regardless of whether the adopting type is a class or not.

``` swift
protocol SomeProtocol {
  var readWrite: Int { get set }
  var readOnly: Int { get }

  class var typeProperty: Int { get set }
}
```

It's possible to restrict protocols so that they can only be adopted by class types by beginning the inheritance list with the `class` keyword.

``` swift
protocol ClassOnlyProtocol: class, InheritedProtocol {
  // ...
}
```

Protocols can require adopting types to provide specified instance or type methods. As with type properties, the `class` keyword must prefix type methods regardless of whether or not the adopting type is a class. Methods that are intended to mutate instances of any type that adopts the protocol should be marked as `mutating`, which allows structures and enumerations to adopt the protocol.

``` swift
protocol SomeProtocol {
  class func typeMethod()
  func random() -> Double
  mutating func toggle()
}
```

Protocols can be treated as fully-fledged types, allowing them to be used as parameter or return types, variable types, or the type of contained elements in collections---while providing uniform functionality as defined by the specified protocol.

Protocols can be adopted by existing types via an extension.

``` swift
protocol Display {
  func display()
}

extension Int: Display {
  func display() {
    println("\(self)")
  }
}
```

It's also possible that a type already conforms to a protocol, but hasn't explicitly expressed that it does by simply providing an empty extension declaration stating this [^go_interfaces].

[^go_interfaces]: This is unlike Go, in which types automatically conform to interfaces they fulfill. I believe this is known as [structural typing](http://en.wikipedia.org/wiki/Structural_type_system).

``` swift
struct SomeType {
  var name = "test"

  func display() {
    println("displaying as \(name)")
  }
}

extension SomeType: Display {}
```

Protocols can inherit from one or more other protocols to add further requirements. The syntax is similar to class inheritance.

Protocol composition allows one to specify that a type must conform to multiple protocols at once. Protocol compositions essentially define a temporary local protocol that has the combined requirements of all protocols in the composition.

``` swift
func someFunc(item: protocol<First, Second>) {
  // ...
}
```

Protocol conformance can be checked with the `is` operator, and instances can be cast to a specific protocol with the `as` operator. However, conformance can only be checked if the protocol is marked with the `@objc` attribute---which indicates that the protocol should be exposed to Objective-C code---even if in reality it _won't_ be used by Objective-C code. A consequence of this is that `@objc` protocols can only be adopted by classes and not structures or enumerations.

It's also possible to define optional requirements within protocols by prefixing the requirement with `optional`. Optional property or method requirements that return a value always return it as an optional, and so optional chaining may be used. Again, optional requirements are only possible if the protocol is marked as `@objc`.

# Generics

Generics are available and they seem most similar to Rust's. They can be used to define generic functions and generic types.

``` swift
func swapTwoValues<T>(inout a: T, inout b: T) {
  let tempA = a
  a = b
  b = tempA
}

struct Stack<T> {
  var items = [T]()
  mutating func push(item: T) {
    items.append(item)
  }
  mutating func pop() -> T {
    return items.removeList()
  }
}
```

When extending a generic type, a type parameter list doesn't need to be provided. Instead, the type parameter list from the original definition is available within the body of the extension.

``` swift
extension Stack {
  var topItem: T? {
    return items.isEmpty ? nil : items[items.count - 1]
  }
}
```

## Type Constraints

Type constraints can specify that a type parameter must inherit from a specific class or conform to a particular protocol or protocol composition. The syntax for this is pretty straightforward.

``` swift
func someFunc<T: SomeClass, U: SomeProtocol>(someT: T, someU: U) {
  // ...
}
```

## Associated Types

Associated types can be expressed within protocols as types that are related to that protocol's requirements, but won't be specified until the protocol is adopted, and are expressed using the `typealias` keyword.

At time of protocol adoption, the associated type is usually inferred by the implementations of the requirements using that associated type. If it can't successfully be inferred for whatever reason, the associated time may be provided explicitly using a `typealias` directive.

``` swift
protocol Container {
  typealias ItemType
  mutating func append(item: ItemType)
  var count: Int { get }
  subscript(i: Int) -> ItemType { get }
}
```

When existing types are explicitly declared as conforming to a particular protocol via an empty extension declaration, the associated type is picked up as well.

## Where Clauses

Constraints and equality relationships can be expressed between types and associated types in parameter lists using where clauses, denoted by the keyword `where`.

The following example uses a where clause to enforce that the contained items' types are the same in both containers, even if the type of the containers differs. It also enforces that the contained items' types conform to the `Equatable` protocol.

``` swift
func allItemsMatch<
  C1: Container, C2: Container
  where C1.ItemType == C2.ItemType,
        C1.ItemType: Equatable>
  (someContainer: C1, anotherContainer: C2) -> Bool {
  if someContainer.count != anotherContainer.count {
    return false
  }

  for i in 0..<someContainer.count {
    if someContainer[i] != anotherContainer[i] {
      return false
    }
  }

  return true
}
```

# Advanced Operators

As mentioned in the [operators](#operators) section, mathematical operators don't overflow by default, but this can be opted-into by prefixing the operator with the ampersand `&`. If the overflow division `&/` or remainder operators `&%` are used and the right operand is zero, a value of zero is returned.

``` swift
let y = x &/ 0 // y = 0
```

Operators can be overloaded within classes or structures. In the following operator overloading, the `infix` attribute specifies that the operator is to be an infix operator. There are also `prefix` and `postfix` attributes.

``` swift
struct Vector2D {
  var x = 0.0, y = 0.0
}

func + (left: Vector2D, right: Vector2D) -> Vector2D {
  return Vector2D(x: left.x + right.x, y: left.y + right.y)
}

prefix func - (vector: Vector2D) -> Vector2D {
  return Vector2D(x: -vector.x, y: -vector.y)
}

func += (inout left: Vector2D, right: Vector2D) {
  left = left + right
}

prefix func ++ (inout vector: Vector2D) -> Vector2D {
  vector += Vector2D(x: 1.0, y: 1.0)
  return vector
}

func == (left: Vector2D, right: Vector2D) -> Bool {
  return (left.x == right.x) && left.y == right.y)
}

func != (left: Vector2D, right: Vector2D) -> Bool {
  return !(left == right)
}
```

## Custom Operators

Custom operators can be defined with the characters `/ = - + * % < > ! & | ^ . ~`. New operators are declared at the global level with the `operator` keyword, and can be declared as `prefix`, `infix`, or `postfix`. After being declared, it can be implemented by any type.

``` swift
prefix operator +++ {}

prefix func +++ (inout vector: Vector2D) -> Vector2D {
  vector += vector
  return vector
}
```

Custom infix operators can specify their precedence and associativity. Associativity can be `left`, `right`, or `none`. Associativity defaults to `none` if none is specified, and the precedence defaults to `100` if none is specified.

``` swift
infix operator +- {
  associativity left
  precedence 140
}

func +- (left: Vector2D, right: Vector2D) -> Vector2D {
  return Vector2D(x: left.x + right.x, y: left.y - right.y)
}
```

# Access Control

Xcode 6 beta 4 [added support][access-control] for [access control][access-control-docs] [^rust_access_control]. By default most entities have `internal` access. Swift also allows the `get` property to be more accessible than the `set`.

[access-control]: https://developer.apple.com/swift/blog/?id=5
[access-control-docs]: https://developer.apple.com/library/prerelease/ios/documentation/swift/conceptual/swift_programming_language/AccessControl.html#//apple_ref/doc/uid/TP40014097-CH41-XID_29

[^rust_access_control]: This reminds me of Rust, where access control isn't specific to classes but rather to modules, so that an access control modifier on a structure member is enforced at the module level.

Level      Accessible From
------     -------------
`private`  within source file where defined
`internal` entire module (app or framework) that includes definition
`public`   any file that imports the module (for APIs)

``` swift
public class ListItem {
  public var text: String
  public var isComplete: Bool

  // readable throughout module
  // writeable within this file
  private(set) var UUID: NSUUID

  // usable within framework target
  func refreshIdentity() {
    self.UUID = NSUUID()
  }
}
```

