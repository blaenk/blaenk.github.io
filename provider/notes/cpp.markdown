---
title: C++
published: September 10, 2013
excerpt: Keeping track of C++
comments: off
toc: left
---

A lot of people really dislike C++ because it's a very complex language that often catches one by surprise. Despite this, C++ is undisputed when it comes to striking a balance between abstraction and speed. Those that need to use it for these reasons generally take one of two approaches, while the rest completely dismiss it as an option to begin with.

The first consists of restricting its usage to a specific subset of the language specification; for example, exceptions are generally avoided.

The other approach, perhaps an extreme, is for people to become "language lawyers," poring over the language specification for every minute detail.

I try to take a pragmatic approach. I do appreciate C++'s advantage in striking a balance between speed and abstraction, I do limit my use of it to a certain subset of the language, and I do try to learn as much about the language short of actually reading the specification to lower the probability that the language may catch me off guard.

To that end, these are non-exhaustive notes about C++---particularly the trickier bits---including C++11 and C++14 changes.

For C++11 in particular, some compilers are faster than others at adopting the new feature set. [Visual Studio](http://msdn.microsoft.com/en-us/library/vstudio/hh567368%28v=vs.120%29.aspx) is particularly behind the rest, while [Clang](http://clang.llvm.org/cxx_status.html) and [GCC](http://gcc.gnu.org/projects/cxx0x.html) seem to be very quick on adopting the new features.

* toc

# Type Conversions

## Signed-to-Unsigned

When a signed value is assigned to an unsigned variable, the underlying bit representation **is not altered**. Instead, the signed value is simply treated literally as if it were an unsigned value.

If the signed value is negative, then it is likely represented at the bit-level in [Two's Complement](http://en.wikipedia.org/wiki/Two%27s_complement). For example, given:

``` cpp
uint8_t var = -1;
```

The value `-1` is encoded by first representing it as a positive number:

$$ 0000\ 0001 $$

The digits are then flipped, so that 1s become 0s and vice versa:

$$ 1111\ 1110 $$

Finally, the value is incremented by 1 to arrive at the Two's Complement representation of `-1`:

$$ 1111\ 1111 $$

When this value is assigned to an unsigned integer, the value is simply interpreted as if it were unsigned to begin with. Therefore, this value is interpreted as being `255`.

## Implicit Conversions

The compiler automatically converts operands in the following circumstances:

* usually integral types smaller than `int` are first promoted to an appropriate larger integral type (presumably to the smallest working unit, i.e. the size of a register on a computer?)
* in **conditions**, non-`bool` expressions are converted to `bool`
* in **initializations**, initializer is converted to the type of the variable
* in **arithmetic and relational expressions** with operands of mixed types, the types are converted to a common type
* during certain **function calls**

### Integer Promotion

In general, operands are converted to the same type of the widest operand type in the expression. Loss of precision is avoided, so this also means that when integral and floating-point values are mixed, they're all converted to floating-point values.

**Integer promotion** concerns converting small integral types to larger integral types.

`bool`, `char`, `signed char`, `unsigned char`, `short`, `unsigned short` are promoted to `int` if all possible values fit within an `int`. Otherwise, they are promoted to `unsigned int`.

Larger types are promoted to the smallest type of `int`, `unsigned int`, `long`, `unsigned long`, `long long`, or `unsigned long long` which fits the value.

### Mixing Unsigned Types

If the types still don't match but the signs match, then the type of the smaller value is promoted to the type of the larger value.

If the signs don't match and the type of the unsigned operand is the same as or larger than that of the signed operand, then the signed operand is converted to unsigned as described in [Signed-to-Unsigned](#signed-to-unsigned), which most likely yields unexpected behavior.

If the signs don't match and the type of the unsigned operand is smaller than that of the signed operand, the **result is machine-dependent**. If all values in the unsigned type fit in the larger signed type, it's converted to the larger signed type. Otherwise, the signed operand is converted to the unsigned type as described in [Signed-to-Unsigned](#signed-to-unsigned), which most likely yields unexpected behavior.

## Negative Modulus

The modulus operation `%` simply calculates the remainder of the left expression divided by the right expression. There is confusion when it comes to modulus operations with negative operands, which as far as I know isn't clearly defined mathematically. For example, the operation `-1 % 256`.

The equation generally used to calculate the modulus is:

$$ \text{mod}(a, n) = a - \lfloor a / n \rfloor * n $$

The operation `-1 % 256` yields the result `255` with this implementation. This is the result yielded in languages such as Python and Ruby.

C and C++ uses the same equation as the above, **but** the division operation has an additional restriction when used with negative operands:

$$ \text{div}(-a, n) = \text{div}(a, -n) = -(a/n) $$

With these definitions, the division of `-1 / 256` in the above equation becomes `-(1 / 256)`. The result of `1 / 256` is zero due to truncation. The negation of this result is still zero, so the result of the modulus operation is simply `-1`, which is **very different** from the result of `256` yielded above without these restrictions.

Given the above restriction on the division operation with negative operands, the definition of the modulus operation with negative operands can be simplified to:

$$
\begin{align}
  \text{mod}(\phantom {-} a, -n) &= \phantom {-} \text{mod}(a, n) \\
  \text{mod}(-a, \phantom {-} n) &= -\text{mod}(a, n)
\end{align}
$$

# Classes

It's a good thing to remember that the _only_ distinction between a `class` type and a `struct` type is that `struct` has by default public visibility and `class` has default private visibility. That's all!

## Rule of Five

The copy constructor, move constructor, copy-assignment operator, move-assignment operator, and destructor should be thought of as a unit: if one needs to be defined, then the rest should be defined as well.

* if a class needs a destructor, it likely also needs a copy-assignment operator and copy constructor
* if a class needs a copy constructor, it likely so needs a copy-assignment operator, **and vice versa**

## Rule of Zero

This [recent rule] is unlike the [other two] in that it instead says that classes that contain custom destructors, copy/move constructors, or copy/move assignment operators should deal _exclusively_ with ownership, i.e. encapsulating a so called _ownership policy_ which handles the allocation and deallocation of a particular resource (via RAII). All other classes should **not have** custom destructors, copy/move constructors, or copy/move assignment operators.

[recent rule]: http://flamingdangerzone.com/cxx11/2012/08/15/rule-of-zero.html
[other two]: http://en.cppreference.com/w/cpp/language/rule_of_three

*[RAII]: Resource Allocation Is Initialization

This rule is enforceable out-of-the-box in C++11 through the use of smart pointers such as `shared_ptr` and `unique_ptr` along with custom deleters when necessary.

## Member Initialization

The order of initializing member variables is:

1. in-class initializers
2. constructor initializer lists
3. constructor body initialization

Constructor initializer lists initialize member variables. If a member variable is missing from the initializer list it is default initialized. Members that are `const` or references must be initialized in the constructor initializer lists. Members in a constructor initializer list are initialized in the order in which they are defined in the class definition.

It is considered best practice to use in-class initializers for member variables, opting for constructor initializer lists for edge cases, and for constructor initialization in the worst case.

Value initialization occurs when:

* in an array initialization, fewer declarations appear than the size of the array
* defining a local static object without an initializer
* explicitly requesting value initialization by writing expressions of the form `T()` where `T` is the name of the type

Member functions defined inside the class definition are inlined.

## Default Constructors

The best practice is to always define a default constructor if any other constructors are defined.

Default constructors are synthesized only if all of the following criteria are met:

1. no other constructors are defined
2. all of the members of built-in or compound type have in-class initializers
3. all members of class type have default constructors

If other constructors are defined but otherwise all other criteria is met for synthesizing a default constructor, the default constructor can be constructed using the `= default` directive:

~~~ {.cpp}
class A {
  A() = default;
  A(int a, int b);
};
~~~

Class members can be initialized inside the class definition. These initializers are known as _in-class initializers_. In-class initializers must be defined either using the `=` assignment operator or list initialization syntax `{}`.

Constructors can _delegate_ their constructing to other constructors inside the constructor initializer list.

Virtual functions can be explicitly overridden in derived classes using the `override` trailing keyword.

Class methods or entire classes can be defined `final` which prevents their overriding or deriving, respectively.

## Destructors

Destructors do whatever work must be done to free resources used by an object, e.g. file handles. While in constructors the members are initialized before the constructor body runs, a destructor body's body executes first and then the members are destroyed afterward, in the reverse order of declaration in the class definition.

## Copy Constructors

A copy constructor is one consisting of a single parameter that is a reference to the same type of the constructor:

``` cpp
struct A {
  A(const A&);
};
```

Copy constructors are _synthesized_ if none are defined. Synthesized copy constructors perform member-wise copies of the argument. Members of class type are copied using their respective copy constructors and members of built-in type---including arrays---are copied directly.

``` cpp
A::A(const A& toCopy) :
  firstMember(toCopy.firstMember),
  secondMember(toCopy.secondMember) {}
```

_Copy initialization_ occurs when:

* assigning variables with the `=` assignment operator
* pass object as argument to parameter of non-reference type. **note** that this is why the parameter to the copy constructor has to be a reference type, or infinite recursion would occur
* brace initialize elements in array or aggregate class

The compiler can perform [copy elision](http://en.wikipedia.org/wiki/Copy_elision) to avoid unnecessary copies, short of using actual move semantics.

## Copy-Assignment Operators

Assignment operators control how objects of its class are assigned. They generally should return a reference to the left-hand object.

``` cpp
A& A::operator=(const A& rhs) {
  if (this != &rhs) {
    firstMember = rhs.firstMember;
    secondMember = rhs.secondMember;
  }

  return *this;
}
```

Copy-assignment operators are _synthesized_ if none are define. Synthesized copy-assignment operators perform member-wise assignment before returning a reference to the left-hand object.

## Conversion Constructors

Conversion constructors allow for the implicit conversion **from** other types to the class type. Only one such implicit conversion is possible; it isn't possible to chain multiple such conversions.

Such conversion constructors can be suppressed using the `explicit` keyword, which effectively only allows the direct form of initialization:

``` cpp
explicit A(std::string &str) : internal(str) {};
```

However, the `explicit` keyword still allows one to use an explicit conversion using a `static_cast`:

**VERIFY**: When defining a copy constructor in the above manner, it forces the compiler to always copy the string instead of being able to use move semantics. Instead, prefer to pass by value and then moving ([source](https://news.ycombinator.com/item?id=6398924)):

``` cpp
explicit A::A(std::string str) : internal(std::move(str)) {}
```

## Conversion Operators

Where as [conversion constructors](#conversion-constructors) provide a way of converting another type to the class type, conversion operators provide a way of converting the class type to another type. They are defined using the `operator` keyword followed by the type it converts to.

``` cpp
struct A {
  operator bool () const { return B; }
};
```

However, creating a `bool` conversion operator can cause unexpected results such as in the following:

``` cpp
int i = 42;
cin << i;
```

The above code is legal even though `<<` isn't defined for `cin` which is of type `istream`. The reason it's legal is that `cin` gets converted to `bool`, which then gets promoted to an `int`, after which the operation becomes a simple left-shift operation.

For this reason, conversion operators can be defined as explicit. A conversion operator that is defined as explicit won't be performed implicitly and instead it must be performed explicitly through the use of `static_cast`. The only exception to this is when the expression would be used for boolean logic.

``` cpp
struct A {
  explicit operator bool () const { return B; }
};
```

## Conversion Ambiguity

It's pretty easy to get into a situation where it becomes ambiguous as to how a type is being converted.

In general:

* don't define mutually converting classes
* avoid conversions to built-in arithmetic types. If this is necessary, then:
    * don't define overloaded versions of operators that take arithmetic types since the conversion will handle it
    * don't define a conversion for more than one arithmetic type

However, it's probably best to try to completely avoid conversion functions with the exception of explicit conversions to `bool` and others that are very obvious.

### Mutual Conversions

One way is to create a conversion constructor to a type that itself defines a conversion operator to the original type.

For example, given:

``` cpp
struct B;

struct A {
  A() = default;
  A(const B&);
};

struct B {
  operator A() const;
};
```

Both `A` and `B` define mutual conversions. `A` defines a conversion constructor that converts `B` to `A`, and `B` itself defines a conversion operator that converts from `B` to `A`. Therefore, the last line in the following code is ambiguous:

``` cpp
A f(const A&);
B b;
A a = f(b);
```

Because the conversion operation is ambiguous to the compiler, an error is emitted. Instead, it would have to be explicitly qualified:

``` cpp
A a1 = f(b.operator A()); // use B's conversion operator
A a2 = f(A(b));           // use A's conversion constructor
```

To avoid ambiguity, one should not define classes with mutual conversions.

### Redundant Built-In Conversions

Another way is to define multiple conversions to or from types that themselves are related by conversions.

For example, given:

``` cpp
struct A {
  A(int = 0);
  A(double);
  operator int () const;
  operator double () const;
};
```

Due to implicit integer promotion, the two conversions to and from `int` and `double` become ambiguous to the compiler:

``` cpp
void f2(long double);
A a;
f2(a);    // operator int () or operator double ()

long lg;
A a2(lg); // A(int) or A(double)
```

The calls above are ambiguous because `long -> double` and `long -> int` both have the same rank in terms of integral promotion. If instead the parameter had been of type `short` then the promotion of `short -> int` would have had a higher rank than `short -> double` and so that conversion would have been chosen by the compiler.

For this reason, one should not define more than one conversion to or from an arithmetic type.

## Delete

Functions can be specified as **deleted** which prevents the compiler from generating code for them. This can be helpful for preventing copying of a specific type:

``` cpp
struct NoCopy {
  NoCopy(const NoCopy&) = delete;
  NoCopy &operator=(const NoCopy&) = delete;
};
```

The compiler sometimes defines copy-control members, which it would have otherwise synthesized, as **deleted** for the following reasons:

* **destructor**: if a member has a deleted or inaccessible destructor, e.g. `private`
* **copy constructor**: if a member has a deleted or inaccessible copy constructor _or_ if a member has a deleted or inaccessible destructor
* **copy-assignment operator**: if a member has a deleted or inaccessible copy-assignment operator _or_ if the class has a `const` or reference member
* **default constructor**: if a member has a deleted or inaccessible destructor _or_ has a reference member without an in-class initializer _or_ has a `const` member whose type has no explicit default constructor and the member has no in-class initializer

## Swapping

Classes that allocate resources might want to define a `swap` inline friend function that simply swaps pointers around. This is useful for classes that allocate resources, and can be re-used in copy and move operations.

``` cpp
struct A {
  friend void swap(A&, A&);
private:
  SomeType *B;
};

inline void swap(A &lhs, A &rhs) {
  using std::swap;
  swap(lhs.B, rhs.B);
}
```

It's _very_ important to recognize that the `swap` function used isn't explicitly qualified to be from the `std` namespace. Instead, the `swap` function from the `std` namespace is brought into the scope for purposes of name resolution.

**Not** explicitly qualifying the function allows a type-specific `swap` function to be used in the event that one is defined, which would be much more efficient than using the `std` function which simply creates a temporary swap value.

One use of the `swap` function is to implement the assignment operator:

``` cpp
A& A::operator=(A rhs) {
  swap(*this, rhs);
  return *this;
}
```

It's important to note that this implementation passes the right-hand side by value and not by reference. This is done so that after the type internals are swapped, the right-hand side's copy's destructor is run and the resources are freed. This handles self-assignment gracefully.

## Inheritance

Constructors, copy and move operations, and assignment operations all have to handle initializing not only their members but also those of the base class. This is usually accomplished by delegating that work to the equivalent operation from the base class.

_However_, a destructor is always only in charge of destroying only its own members. The base class destructor is implicitly invoked after the completion of the derived class destructor.

_Name lookup_ is affected by inheritance and virtual functions. Given a call `p->mem()` or `p.mem()`:

1. determine the static type of `p`
2. look for `mem` in the class that corresponds to the static type of `p`. If it's not found, continue the lookup up the inheritance hierarchy. Error if not found.
3. perform normal type checking (&sect; 6.1 p. 203) to see if the call is legal
4. if it's legal, generate code depending on whether the call is virtual:
    1. **virtual**: if the call is made through a reference or pointer, then generate code to determine at run-time which version to run based on the dynamic type of `p`
    2. **otherwise**: if the call isn't virtual or made through a reference or pointer, then generate a normal function call

Inheritance can be prevented by a class using the `final` directive:

``` cpp
class A final {};
```

This directive can also be used on specific member functions:

``` cpp
struct A {
  void Perform() final;
};
```

### Constructors

Constructors of derived classes can't directly initialize base-class members. Instead, initialization is delegated to the base-class constructor:

``` cpp
B(const std::string& str, int num, char ltr) :
  A(str, num), ltr_(ltr) {}
```

If the base-class is not initialized in this manner, then the base-class is default initialized.

### Inherited Constructors

It's possible to "inherit" constructors from the base class:

``` cpp
struct B : public A {
  using A::A;
};
```

The `using` directive causes `B` to "inherit" _each_ of the constructors from `A` except:

1. the default, copy, and move constructors
2. those which have the same parameter lists as one of the constructors already defined in the derived class

Despite the first exception above, the inherited constructors aren't considered to be "user defined" and so the compiler can still synthesize the default, copy, and move constructors if allowed.

The inherited constructors have the exact same properties as defined in the base class, including accessibility, `explicit`, and `constexpr`.

### Copy and Move Operations

If a derived class defines a copy or move operation, then it is responsible for copy or moving the entire object including base-class members. This is accomplished similar to what a regular does by delegating the work to the equivalent constructor in the base class.

### Copy-Assignment Operator

As with the constructor and copy/move operations, the copy-assignment operator can delegate its work to the copy-assignment operator of the base class:

``` cpp
B& B::operator=(const B& rhs) {
  A::operator=(rhs);
  // assign members of derived class
  return *this;
}
```

### Destructors

Base classes that intend to be derived from should define their constructors as `virtual`, so that correct destructor is run through dynamic dispatch based on the dynamic type of the object being destroyed, instead of the static type.

This has an implication with move semantics. If a destructor is defined, even as `default`, then no move operations are synthesized for that class. This issue percolates throughout the inheritance hierarchy, since classes don't synthesize operations that the base class doesn't define.

For this reason, the base class usually explicitly defines---even if as `default`---all of the operations it requires. First the virtual destructor for the aforementioned reasons, then move operations for the aforementioned reasons, and then the copy operations since they would otherwise not be synthesized since the move operations are explicitly defined.

# Move Semantics

C++11 introduced _move semantics_ which simply refers to recognizing the notion of moving objects instead of only being able to copy them. With this introduction came _rvalue-references_ which designate an object as being "moveable," usually because it's about to be destroyed anyways.

A simple explanation for the act of "moving" is that of a string class with an underlying `char` array. If there is an instance **A** that needs to be replicated into instance **B**, it can be done by copying **A** into **B** using a copy constructor which would make a copy of the underlying array. However, if **A** was going to be destroyed shortly after, then the copy would have been unnecessary. Instead of copying the array from **A**, it could simply _steal_ its pointer.

## rvalue-references

_rvalue-references_ are simply references that can _only_ be bound to rvalues. rvalues are either temporary objects or literals, both of which are ephemeral over the course of evaluating an expression. It then follows naturally that an object bound to an rvalue-reference has no "owner", and more importantly that the object is _about to be destroyed_, **so code is free to steal its contents**. rvalue-references are simply a way of "tagging" such objects, to be able to write functions that apply specifically to objects that are about to be destroyed, i.e. a move constructor.

Aside from binding rvalue-references to rvalues, it is possible to derive an rvalue-reference from an lvalue through the use of `static_cast`. Such a cast has been implemented as the function `std::move` in order to be more semantic:

``` cpp
Object &&ref = std::move(instance);
```

However, deriving an rvalue-reference from an lvalue is seen as a promise that the lvalue will no longer be used other than to assign or destroy it, as the actual value of the lvalue is not well defined or guaranteed.

### Reference Collapsing

rvalue-references to template parameters have special rules. For example, given the definition:

``` cpp
template <typename T> void func(T&&);
```

If an lvalue `int` is passed to the function, a language rule states that the template parameter `T` will be deduced as being an lvalue-refernece, `int&`. This poses a problem, since the function parameter's type ends up being an lvalue-reference to an rvalue-reference, `int& &&`. A reference to a reference, of any type, can't usually be created but an **exception** is made for template parameters.

Template parameters that are deduced as being references to references undergo a process that is referred to as _reference collapsing_, the rules of which are as follows:

* `X& &`, `X& &&`, `X&& &` &rarr; `X&`
* `X&& &&` &rarr; `X&&`

Basically, all reference-to-reference instances collapse to lvalue-references, unless an actual rvalue-reference was what the template parameter `T` was deduced to.

The consequence of this is that function parameters that are an rvalue-reference to a template parameter type can match _any_ type.

This is the mechanism behind the `std::move` function, which is defined by the standard as:

``` cpp
template <typename T>
typename remove_reference<T>::type&& move(T&& t) {
  return static_cast<typename remove_reference<T>::type&&>(t);
}
```

This has the effect that rvalues are passed through as-is. Instead, when an lvalue is passed to `std::move`, the templated function is instantiated as follows:

1. `T` type deduces to `string&`
2. `remove_reference` is instantiated with `string&`
3. `remove_reference<string&>::type` is `string`
4. return type of `move` is therefore `string&&`
5. function parameter instantiates as `string& &&` which collapses to `string&`

The above instantiation procedure yields the following function signature:

``` cpp
string&& move(string &str);
```

The actual `static_cast` is what yields and returns an rvalue-reference.

### Type-Matching {#rvalue-ref-type-matching}

An rvalue-reference can be converted to a `const` reference. This means that if a class defines copy constructor but not a move constructor and as a result the compiler [defines the move constructor as deleted](#move-operation-synthesis), rvalue-references will type match with `const` references and as a result, rvalue-reference arguments will use the copy constructor seamlessly.

### Reference Qualifiers

It's usually the case that member functions can be called on objects regardless of whether they're lvalues or rvalues. However, this can lead to unexpected usage of objects such as the following:

``` cpp
s1 + s2 = "wow!";
```

C++11 allows for the explicit restriction on the usage of a member function based on the lvalue/rvalue property of the calling object using a _reference qualifier_, which is similar to a `const` qualifier in that it appears at the end of the parameter list but *_after_* the `const` qualifier, and must appear in both the declaration and definition of the function.

Two possible reference qualifiers exist:

1. `&` can only be called from an lvalue
2. `&&` can only be called from an rvalue

**Note**: If a function has a reference qualifier, than _all_ of the same functions require a reference qualifier.

``` cpp
struct A {
  A& operator=(const A&) &;
};

A& A::operator=(const A &rhs) & {
  return *this;
}
```

## Move Constructors

Because rvalue-references serve as a sort of "tag" on an object that's about to be destroyed, functions can overload implementations specifically for such objects. An example of this would be a move constructor:

``` cpp
A::A(A &&moveFrom) noexcept :
  firstMember(moveFrom.firstMember),
  secondMember(moveFrom.secondMember) {
  moveFrom.firstMember = moveFrom.secondMember = nullptr;
  }
```

It's important to leave the moved-from object in a destructible state.

## Move-Assignment Operator

This is similar to the move constructor:

``` cpp
A& A::operator=(A&& rhs) noexcept {
  if (this != &rhs) {
    delete firstMember;
    firstMember = rhs.firstMember;
    rhs.firstMember = nullptr;
  }

  return *this;
}
```

An interesting thing to note is that the move-assignment operator can be defined in terms of the copy-assignment operator if a move constructor is defined:

``` cpp
struct A {
  A(A &&other) noexcept : B(other.B) { other.B = nullptr; }
  A& operator=(A rhs) {
    swap (*this, rhs);
    return *this;
  }
};
```

In this case, if an rvalue-reference is used with the assignment operator, then the `rhs` variable is created using the move-constructor which simply allows `rhs` to steal the `B` pointer from the rvalue. Once inside the assignment operator function body, the current instance steals the `B` pointer from the `rhs` copy. The `rhs` copy is automatically destroyed when it goes out of scope.

## Synthesis {#move-operation-synthesis}

Unlike the copy operations that are _always_ synthesized if they're not otherwise defined or deleted, the compiler _only_ synthesizes move operations if the class doesn't define any copy operations and if every non-static data member is moveable. Moveable members include built-in types and those that define a move operation.

If a class defines move operations, the respective copy operation will be defined as deleted and must be defined explicitly.

If a default implementation is explicitly requested with the `default` directive, but the compiler can't define one due to the following reasons, then it will be defined as `deleted`:

* the class has a member that defines its own copy constructor but not a move constructor _or_ if the class has a member that doesn't define its own copy operations _and_ for which the compiler is unable to synthesize a move constructor. The same applies for move-assignment.
* the class has a member whose respective move operation is deleted or inaccessible
* the destructor is deleted or inaccessible
* the class has a `const` or reference member

## Exception Guarantees

Some classes make guarantees about what occurs in the event that exceptions are thrown. For example, `std::vector` guarantees that if an exception occurs during `push_back`, the original `vector` would be left unchanged. In the event that the `push_back` would have had to reallocate space, if the `vector` decided to use the move constructor to move the objects to the new space and an exception were thrown at some point, the original `vector` would be left in an inconsistent state, with some of its elements having been moved to the new allocation of memory.

For this reason, such classes use copy constructors unless they are guaranteed that a type's move constructor doesn't throw exceptions. This guarantee is specified using the `noexcept` declaration on a function definition as shown above.

# Miscellaneous

`static_assert` is a compile-time assertion.

The **type_traits** header defines a variety of type trait queries.

The `auto` keyword allows for type-deduction and should be preferred in the following circumstances:

* when an expression would otherwise be repeated on both sides
* lambdas, though can also use `std::function`
* iterators and other long type names

The `decltype` operator can deduce and "return" the type of the argument to be used to declare something else such as a variable of a function. The rules for what gets returned depends on the expression:

* **identifier** (name of object or function) or **class member access**, _yieds_ type of identifier or class member access
* **parenthesized identifier** becomes an lvalue expresion, _yields_ lvalue reference to type of expression
* **function call**, _yields_ return type of the function
* **rvalue**, _yields_ rvalue reference to type of expression
* **lvalue**, _yields_ lvalue reference to type of expression

The suffix-return syntax is useful when the return type is deduced from information---such as the function arguments---and has to appear after the function argument list so that the arguments are "in scope":

``` cpp
template <class T, class U>
auto add(T x, U y) -> decltype(x + y) {
  return x + y;
}
```

Suffix-return syntax can also be useful in class methods in classes with nested types. Given the following class:

``` cpp
struct LL {
  struct Link {};
  Link *erase(Link *p);
};
```

Given the following declaration:

``` cpp
LL::Link *LL::erase(Link *p) {};
```

Using suffix-return syntax, after the compiler reads `LL::erase` it enters the class scope of `LL`, making it unnecessary to fully qualify the `Link` type that's nested within `LL`:

``` cpp
auto LL::erase(Link *p) -> Link * {};
```

The `std::function` type is a generalized type that "matches" any kind of function-like type, such as an actual function pointer, lambdas, function objects, etc.

## User-Defined Literals

User-defined literals can easily be created:

``` cpp
Out operator "" _intlit(int literal);
Out operator "" _strlit(const char * literal);
Out someVar = 1234_intlit;
Out otherVar = "testing"_strlit;
```

## Enumerations

_Scoped enumerations_ can be created to avoid symbol clashing and enumerations' underlying type can be specified explicitly:

``` cpp
enum class EventType : uint8_t { STATUS, LOG, ERROR };

EventType type = EventType::STATUS;
```

## Tuples

The `std::tuple` type is similar to tuples in other languages. `get<index>(tuple)` retrieves the value at a given index. Tuples can easily be created with the `make_tuple` function. As in other languages, tuples can be "unpacked" into multiple values using the `tie` function:

``` cpp
tie(num, ignore, letter) = make_tuple(10, 4.23, 'a');
```

Uniform initialization allows for constructing any type using the `{}` syntax that was previously used for arrays for example. If a class defines a constructor that takes an `std::initializer_list` then that constructor takes precedence when using initializer list construction. Initializer lists cause an error if a construction would narrow a type.

# Resources

* [Three Optimization Tips for C++](https://www.facebook.com/notes/facebook-engineering/three-optimization-tips-for-c/10151361643253920)

