---
title: Java
published: July 6, 2014
excerpt: A recap of a traditional language
comments: off
toc: left
---

Java is considered by many to be a very boring language, relegated to insurance applications---or the enterprise in general. Compared to many other languages, it's dry and monotone. It has a reputation of being over-engineered, being home to the classic `FactoryFactory` joke [^over_engineered]. In the past, programs written in Java were generally regarded as being slow and bloated resource hogs.

[^over_engineered]: For example, [`AbstractSingletonProxyFactoryBean`](http://docs.spring.io/spring/docs/2.5.x/api/org/springframework/aop/framework/AbstractSingletonProxyFactoryBean.html) from the Spring framework.

When Java was introduced in the 90s, it was hailed as being very portable, giving rise to the famous slogan "Write Once, Run Anywhere." Java runtimes became available on various operating systems, cell phones, and even gained traction on websites as Java applets.

Java applets quickly became a very popular attack vector for malicious entities, leading public perception of Java as being insecure, ultimately leading Mozilla to adopt a stance of having Java disabled by default in Firefox---though this was eventually reverted due to public outcry.

Despite this negative perception of Java by many, I've always held a certain respect for it. Throughout all this, real Java applications have and continue to be created, seemingly impervious to this image. Where many are creating virtual machines from scratch, Java has a very robust JVM which is the added experience of the many years that Java has been in existence.

*[JVM]: Java Virtual Machine

These notes are a recap of Java and also cover Java 8.

* toc

# JVM

Java is an interpreted language. Java code is compiled to highly optimized bytecode which is run by the JVM. HotSpot provides a Just-in-Time (JIT) compiler for Java bytecode. However, this doesn't mean that the entire Java program is compiled into executable code, which would be too complicated since it requires many run-time checks that can only be performed at run-time. Instead, the JIT compiler compiles parts of the program as it sees fit.

*[JIT]: Just-in-Time

JDK 8 adds the concept of compact profiles which contain a subset of the Java API. Profiles go from 1 to 3, where `compact1` is the smallest profile. When compiling a program, the profile can be specified to determine if the program conforms to the subset specified by the profile.

``` bash
# ensure Program only uses compact1 subset
$ javac -profile compact1 Program
```

# Primitives

Java doesn't support unsigned integers. The `double` floating-point type is more commonly used than `float` because all of the standard library's math functions use doubles. The `char` can hold unsigned 16-bit values and uses UTF-16.

Underscores can be written within integer or floating-point literals to make them more readable.

Automatic type conversions only take place if the two types are compatible and the destination is larger than the source type, a widening conversion. Manual conversions can be performed using casts, the same as C-style casts.

When different types are present in the same expression, Java enforces type promotion rules. The `char`, `byte`, and `short` values are promoted to `int`. If `long`, `float`, or `double` values are present in the expression, then the entire expression becomes of that type.

Integer types are always signed in Java. Bitwise right-shift operations therefore shift the sign bit into the high-order bit. This is not always preferable, and so the unsigned right shift operator `>>>` exists to shift a zero in the high-order bit regardless of sign.

``` java
// 11111111 11111111 11111111 11111111
int a = -1;

// 11111111 11111111 11111111 11111111
a >> 24;

// 00000000 00000000 00000000 11111111
a >>> 24;
```

The `strictfp` modifier can be applied to a class, method, or interface to ensure that floating-point calculations perform truncations of certain intermediate values during a computation, as in previous JVM versions.

Array type syntax can place the `[]` in one of two locations. The latter is better for methods returning arrays and for declaring multiple arrays in one line.

``` java
int a[] = new int[3];
int[] a = new int[3];

// three arrays
int[] nums, nums2, nums3;
```

Type wrappers are classes that wrap primitive types, such as `Character` which wraps `char`. All numeric type wrappers such as `Integer` and `Float` inherit from abstract class `Number` [^NSNumber] which provides conversion methods for all numeric types.

[^NSNumber]: Reminds me of Objective-C's `NSNumber`.

Encapsulating a primitive in an object is referred to as _boxing_, and the reverse is called _unboxing_. _Autoboxing_ and _auto-unboxing_ refers to the automatic wrapping and unwrapping of primitive values. JDK 5 added support for autoboxing and auto-unboxing, which works whenever a primitive type must be converted to an object, such as when passed as parameters to methods or when used in expressions.

``` java
Integer i = 100; // autoboxed

Integer ib = 1;
++ib; // auto-unboxed, incremented, re-boxed
```

# Control Structures

Switch statements in Java can operate on expressions of type `byte`, `short`, `int`, `char`, enumerations, or `String`. Case statements don't break automatically, and so the `break` keyword must be used.

Labeled break statements can specify exactly which block to break to, causing execution to jump to the _end_ of the specified _enclosing_ block. Blocks can be given names by prefixing the `{` character with a label in the form of `thelabel:`.

The `continue` keyword also supports this functionality, in effect specifying which outter-loop to `continue`.

``` java
redundant: for (int i = 0; i < 4; i++) {
  break redundant;
}

outter: {
  for (int i = 1; i < 4; i++) {
    for (int j = 1; j < 4; j++) {
      for (int k = 1; k < 4; k++) {
        // break out of _all_ loops
        if (somecondition)
          break outter;
      }
    }
  }
} // execution jumps here
```

Assertions aren't exactly control structures but they're in this section anyway. The `assert` keyword takes a condition which is optionally followed by a colon and an expression that is converted to a string and displayed if the assertion fails.

``` java
assert divisor != 0: "attempted to divide by zero";
```

Assertions aren't run by default due to the performance impact, but can be enabled by supplying the `-ea` option to the `java` interpreter, or disabled with the `-da` option. Assertions can be enabled or disabled at the package level by specifying the package and following it by three periods.

``` bash
# enable all assertions
$ java -ea Program

# only enable assertions from Core package
$ java -ea:Core... Program
```

# Classes

Something to remember is that variables of class type hold references to `new` instantiated objects of that type, not the objects themselves.

Classes can have finalizer methods which can be used to free resources and are run just prior to being garbage collected.

Static methods can only directly call other static methods and access static data. Static variables that require computation for initialization can use static blocks which are executed exactly once.

``` java
static int a = 3;
static int b;

static {
  System.out.println("static block initialized");
  b = a * 4;
}
```

Fields can be `final`, which makes them immutable. Final fields can be initialized via a value given at declaration or within a constructor.

Fields can be `transient`, which means that they should not be persisted when the object is stored.

Variable-length arguments are specified by threep periods and makes the arguments available as an array.

``` java
void printArgs(int ... v) {
  for (int a : v) {
    System.out.println(a);
  }
}

printArgs(1, 2, 3);
```

The `instanceof` operator can test to see if an instance is of a given type or _can be_ cast into a given type, yielding a boolean value.

the `native` keyword can be used to mark a method as native via the Java Native Interface (JNI), in which case a method shoudldn't be provided. The library that contains the definition of the function should be linked using the `System.loadLibrary` method, particularly within a `static` block to ensure that this only occurs once.

*[JNI]: Java Native Interface

``` java
class Test {
  public native void test();

  static {
    System.loadLibrary("NativeDemo");
  }
}
```

The code should be compiled normally, then the `javah` program should be run on the result to produce a header file that must be included in the implementation of the native method.

``` bash
# produces Test.h
$ javah -jni Test
```

This generated header specifies the expected prototype of the native method which should be used for its implementation.

``` c
#include <jni.h>
#include "Test.h"
#include <stdio.h>

JNIEXPORT void JNICALL Java_Test_test(JNIEnv *env, jobject obj) {
  printf("successfully called\n");
}
```

# Inheritance

Inheritance is expressed with the `extends` keyword. Private members in the superclass can't be accessed by the child class. The superclass can be accessed via the `super` keyword. A superclass initializer can be called with `super` as well, which must be the first statement in a subclass constructor.

``` java
class A {
  int a;
}

class B extends A {
  int b;
}
```

The `abstract` keyword can be used to denote that subclasses must override a method, and this property bubbles up to the class, so that a class with an abstract method must itself be declared abstract. Even if a class is abstract, it may contain concrete method implementations. Abstract classes cannot be instantiated, though they may be used to create references in order to leverage run-time polymorphism.

``` java
abstract class A {
  abstract void callme();
}

class B extends A {
  void callme() {
    System.out.println("called");
  }
}
```

The `final` keyword can be used to prevent a method from being overridden in subclasses. Such methods can be inlined by the compiler because it knows that they will not be overridden and thus doesn't need to resolve the call dynamically at run-time.

Further, the `final` keyword can be used to prevent inheriting from a particular class at all.

All classes are subclasses of `Object`, so that a reference of type `Object` can refer to any other class.

Overloaded constructors can call other constructors by using the `this` keyword as a method, but if this is done then it must be the first statement within the constructor. Calling overloaded constructors in this manner imposes a performance impact due to the calla nd return mechanism used when the second constructor is invoked, so this mechanism shouldn't be used simply for the sake of cleaner code.

There are two restrictions with calling other constructors. The first is that an instance variable of the constructor's class can't be used in a call to another constructor (i.e. passing it as an argument). The second is that superclass constructor delegation and same-class constructor delegation can't be used in the same constructor, since each has the requirement of being the first statement in the constructor.

# Generics

Java doesn't actually create different versions of parameterized classes or methods, unlike C++ template instantiations. Instead it performs _type erasure_ so all generic type information is substituted by necessary type casts.

With type erasure, all generic type information is erased at compile-time, replacing type parameters with their bound type---`Object` if no explicit bound is specified. Appropriate casts are then inserted to maintain compatibility with the types specified by the type arguments, a compatibility which the compiler also enforces.

It's possible for methods overridden in subclasses to mismatch the type erasure of the superclass method definition. In this case, the compiler inserts a bridge method that has the same type erasure as the superclass which then calls the method that has the type erasure specified by the override.

In the following example, `getOb` in `T1<String>` results in a return type of `Object` due to type erasure, so the override isn't actually an override since the return types don't match. For this reason, the compiler would insert a method of the same name with the same return type as `T1` which itself would call the `T2` "override".

In this example the only difference between the two methods with the same name is the return type, which is not a valid overload and would normally yield a compiler error, but it's handled automatically and correctly by the JVM.

``` java
class T1<T> {
  T ob;

  T getOb() {
    return ob;
  }
}

class T2 extends T1<String> {
  String getOb() {
    System.out.println("called String override");
    return ob;
  }
}
```

It's also possible for type erasure to lead to ambiguity errors, where two distinct declarations resolve to the same erased type. This can be fixed by placing strict type bounds or making the method names distinct.

``` java
class T<X, Y> {
  X obj1;
  Y obj2;

  // both resolve to void set(Object o)
  void set(X o) { ... }
  void set(Y o) { ... }
}
```

Ambiguity errors can also be caused when there are more than one type parameters with no restriction on them being distinct, incompatible types.

Generics only work with reference types, so that a primitive type such as `int` can't be a type argument to a type parameter, instead necessitating a boxed type such as `Integer`.

It's not possible to create an instance of a type parameter, since the compiler won't know what type to actually create. By extension, it's not possible to instantiate arrays whose element type is a type parameter or a specific generic type as well. However, it is possible to create arrays of references to a generic type via a wildcard. It's also not possible to create generic exception classes.

``` java
class T<A> {
  A obj;

  T() {
    obj = new A(); // error
    T vals[] = new T[10]; // error
    T<Integer> vals[] = new T<Integer>[10]; // error
    T<?> vals[] = new T<?>[10]; // fine
  }
}
```

It's also not possible to define static members that use type parameters declared by the enclosing class. There can, however, be generic static methods that define their own type parameters.

Generic classes are defined in the following form.

``` java
class Name<TypeParameter> {}
```

Bounded types allow the specification of an upper or lower bound on the expected type. An upper bound specifies the superclass from which the type argument must derive and is accomplished using the `extends` keyword. It's possible to use an interface as a bound, in which case the passed type must implement the given interface. A combination of type and interface(s) may be provided separated by ampersands `&`, but the type must come first.

``` java
// upper bound: type argument must be or extend SuperClass
<T extends SuperClass>

// type argument must be or extend SuperClass
//   and extend Interface1 and Interface2
<T extends SuperClass & Interface1 & Interface2>
```

It's possible to specify a wildcard type parameter with the question mark `?`, which represents an unknown type. This would match any generic type regardless of its type argument, so that `Test<A>` and `Test<B>` would match `Test<?>`.

``` java
// this can take in a Stats<Integer> and Stats<Double>
boolean sameAvg(Stats<?> obj) {
  return average() == obj.average();
}
```

Wildcards can also be bounded with an upper or lower bound with the `extends` and `super` keywords respectively. In both cases, the provided bound type is eligible for satisfying the bound.

``` java
// upper bound
<? extends SuperClass>

// lower bound: type must be subclass of SubClass
//   or SubClass itself
<? super SubClass>
```

Generic methods can be defined within non-generic classes. In this case, the type parameter list precedes the return type. If type inference fails in inferring the types, they may be explicitly provided before the method name.

``` java
class Generic {
  static <T, V> boolean method(T a, V b) { ... }
}

Generic.<Integer, Double>method(2, 3.0);
```

Constructors can also be generic even if their classes aren't, in which case the type parameter list also precedes the constructor name.

``` java
class DoubleContainer {
  private double val;

  <T extends Number> DoubleContainer(T arg) {
    val = arg.doubleValue();
  }
}
```

Interfaces can also be made generic, in which case their declaration syntax is identical to that of a generic class. In most cases, if a class implements a generic interface then the class itself must be generic in order to pass the type parameter to the interface's type parameter list.

In order to preserve backwards compatibility, Java allows a generic class to be used without any type arguments, in which case it's referred to as a _raw type_. Type casts, which would normally be substituted automatically during type erasure, must be explicitly included to type check. However, if the type cast fails at run-time, it yields a run-time error.

Due to the danger imposed by raw types, the Java compiler displays unchecked warnings when raw types are used in ways that may break type safety.

``` java
class Type<T> {
  T ob;
}

Type raw = new Gen(new Double(3.0));
double d = (Double)raw.ob;
int i = (Integer)raw.ob; // run-time error
```

Generic classes may inherit from generic and non-generic classes. It's also possible to inherit from a specific generic type (e.g. `T<String>`).

The `instanceof` operator can be used on generic classes, but since generic type information is not available at run-time, a wildcard must be used to check.

``` java
Type<Integer> t = new Type<Integer>(3);

assert t instanceof Type<?>;
```

It's possible to cast an instance of a generic class into another if the type arguments are the same and the classes are compatible (related).

the diamond operator `<>` can be used to instantiate a generic class and infer the type arguments from the types passed to the constructor.

``` java
Type<Integer, String> ob = new Type<>(3, "string");
```

# Packages

Packages serve as containers for classes and serve a similar purpose to namespaces in C++, in particular they help avoid name collisions.

Packages are created by specifying a `package` declaration at the beginning of a source file, which has the effect of putting all classes declared within that file to belong to the package.

``` java
package MyPackage;
package Some.Hierarchy.Here;
```

Multiple source files may contain the same `package` declaration, allowing packages to be spread across many source files. Packages map to directories on the file system.

The default access specification is that, if a class member doesn't have an explicit access specification, it is visible to subclasses and other classes in the same package. Specifying a member as `protected` makes it accessible outside of the package but only to subclasses of the class to which they are a member.

The following table specifies whether a class member with a particular access modifier is accessible by other package components.

Class Member Accessible By      Public   Protected   No Modifier   Private
-------------                  -------- ----------- ------------- ---------
Same Class                     Yes      Yes         Yes           Yes
Same Package SubClass          Yes      Yes         Yes           No
Same Package Non-SubClass      Yes      Yes         Yes           No
Different Package SubClass     Yes      Yes         No            No
Different Package Non-SubClass Yes      No          No            No

Packages can be imported using the `imported` keyword in order to avoid having to fully qualify package contents. The import statement may import either a classname or the `*` to import all classes.

``` java
import java.util.Date;
import java.io.*;
```

If the `import` keyword is followed by the `static` keyword then only static members are imported, avoiding the need to fully qualify them. A wildcard is also possible with static imports.

``` java
import static java.lang.Math.sqrt;
import static java.lang.Math.pow;

// or
import static java.lang.Math.*;
```

# Interfaces

Classes must implement the complete set of methods specified in an interface in order to fully implement that interface. Interfaces must be declared as either `public` or use the default access level, while nested interfaces may be declared as `public`, `private`, or `protected`.

``` java
interface Callback {
  void callback(int param);
}
```

Classes specify that they implement a particular interface by using the `implements` keyword followed by a list of interfaces that it implements. Methods that implement an interface _must_ be declared `public`.

``` java
class Client implements Callback {
  public void callback(int p) {
    System.out.println("callback called with " + p);
  }
}
```

As with subclasses, it's possible to create references of interface types that point to objects that implement the interface, such that method calls resolve to those implemented by the object.

``` java
Callback c = new Client();
c.callback(42);
// callback called with 42
```

If a class doesn't fully implement the methods required by the interface it claims to implement, then that class must be declared as `abstract`.

``` java
abstract class Incomplete implements Callback {
  int a, b;
}
```

Variables may also be declared within interface declarations, but they are implicitly `final` and `static` such that they cannot be changed by the implementing class.

Interfaces may inherit from each other, such that the derived interface requires all methods in its parent interfaces to be implemented as well as its own.

``` java
interface A {
  void meth();
}

interface B extends A {
  void meth2();
}

class SomeClass implements B {
  public void meth() { /* ... */ }
  public void meth2() { /* ... */ }
}
```

JDK 8 makes it possible to provide default implementations of methods. Such implementations are referred to as _default methods_ or _extension methods_. Default methods are specified by prefixing the method implementation with the `default` keyword.

*[JDK]: Java Development Kit

Class implementations take priority over interface default implementations. If a class implements two interface with the same default method, the method must be overridden to disambiguate the call.

If an interface inherits from another and both define a common default method, the sub-interface's version takes precedence. However, the sub-interface can refer to the super-interface's default implementation by using the `super` keyword, as in `Interface.super.method()`.

``` java
public interface SomeInterface {
  int getNumber();

  default String getString() {
    return "default";
  }
}
```

JDK 8 also added the ability to define static methods in interfaces which can only be called off of the interface name, since static interface methods aren't inherited by an implementing class or a subinterface.

``` java
interface SomeInterface {
  static int getDefaultNumber() {
    return 0;
  }
}

int defNum = SomeInterface.getDefaultNumber();
```

# Exceptions

The `try` block is used to enclose code that may potentially throw an exception. These can be nested so that an exception thrown within an inner one bubbles outwards until it is caught.

The `catch` statement essentially works like a pattern matching in functional languages, where the match succeeds if the actual exception type is a subclass of or _is_ the type specified within the parentheses. This is the manner in which the type of error is determined, in order to appropriately handle it.

``` java
try {}

catch (ExceptionType e) {}

finally {}
```

Exception types are subclasses of the built-in class `Throwable`. Under `Throwable` there are two subclasses: `Exception` which is for exceptional conditions that programs should catch, and `Error` which is for exceptions that aren't expected to be caught under normal circumstances. In particular, exceptions of type `Error` are used for errors pertaining to the Java run-time environment and are usually created in response to serious failures that usually can't be handled by the program.

The `throw` statement is used to throw instances of exception types, particularly of type `Throwable` or a subclass of it. Execution immediately stops after the `throw` statement and jumps to wherever the exception is caught, bubbling out of enclosing `try` blocks until a handler is found or the run-time catches it.

``` java
try {
  throw new NullPointerException("demo");
} catch (NullPointerException e) {
  System.out.println("caught " + e);
}
```

If a method is capable of throwing an exception that it doesn't handle, then it must be marked with the `throws` keyword to inform callers that they should put it within a `try` block. The `throws` keyword is placed after the parameter list and includes a list of exception types that may be thrown. Failing to do this prevents the program from compiling.

``` java
class Throws {
  static void throwOne() throws IllegalAccessException {
    throw new IllegalAccessException("demo");
  }
}

try {
  throwOne();
} catch (IllegalAccessException e) {
  // ...
}
```

The `finally` block is used to define code that must be run regardless of whether or not an exception was thrown, even in the event that an exception is thrown but not handled.

Chained exceptions allow associating one exception with another. This is facilitated by two constructors on `Throwable`, one which takes the other exception instance and another that takes a message as well as the instance. The `getCause` method can then yield the exception instance that was the cause of the current exception. The `initCause` method allows associating another exception with the current exception after it has been created.

Multi-catch allows two or more exceptions to be caught by the same `catch` clause. This is useful if two ore more exception handlers use the same exact code despite responding to different exceptions. To facilitate this, the exception types are separated by `|` and the exception parameter is implicitly `final`.

``` java
catch (ArithmeticException | ArrayIndexOutOfBoundsException e) {}
```

More precise rethrow refers to the restriction of the type of exceptions that can be rethrown to only those checked exceptions that the associated `try` block throws that aren't handled by a preceding `catch` clause and are a subtype or supertype of the parameter. For this restriction to be enforced, the `catch` parameter must be treated as or be explicitly declared as `final`.

When working with resources in pre-JDK 7 environments, it's necessary to leverage exception handling to make sure that resources don't leak if exceptions are thrown.

``` java
try {
  FileInputStream fin = new FileInputStream("test");
  // ...
} catch (Exception e) {
  // ...
} finally {
  // dispose of the resource if it was created
  try {
    if (fin != null) fin.close();
  }
  catch (IOException e) { /* error closing file */ }
}
```

JDK 7 introduced _try-with-resources_ which allows initializing a resource within a `try` statement that should be automatically closed if the body ends, whether it threw or not. This can only be used on those resources that implement the `AutoCloseable` interface which defines a `close` method. This allows `catch` clauses to be used for more meaningful reasons.

Multiple resources can be defined within the same `try` statement by separating their declarations with semicolons.

Something to note is that the resource declared in the `try` statement is implicitly `final`, so that the resource can't be assigned to after it has been created.

``` java
try (FileInputStream fin = new FileInputStream("test")) {
  // work with fin
} catch (FileNotFoundException e) {
  // handle meaningful exception
}
```

Normally when an exception occurs after another exception leads to the `finally` block, the original exception is lost in favor of the new exception. With try-with-resources, the new exception is supressed and can be accessed using the `getSuppressed` method of the original exception.

# Multithreading

The `Runnable` interface represents a unit of executable code and consists of a `run` method. The `Runnable` object's `run` method can be executed in a separate thread by instantiating a `Thread` and passing it a reference to the `Runnable`. This can be accomplished by using a particular `Thread` constructor which takes the reference, then calling the `Thread`'s `start` method.

It's also possible extend the `Thread` class and override its `run` method to more directly specify code that should be run in a separate thread.

The `join` method from `Thread` can be used to join one thread to another, i.e. wait for another thread to finish.

A monitor is an object that's used as a mutually exclusive lock, and every object has its own implicit monitor that's automatically "entered," i.e. acquired, when a `synchronized` method is called, so that only one thread may enter a `synchronized` method at a time on a given object.

``` java
class Synchronized {
  synchronized void raceCall(int arg) {
    // ...
  }
}
```

There is also a `synchronized` statement which can be used to synchronize sections of code, which can be useful when one doesn't have control over the methods of a class. The statement takes a reference to the object to use as the monitor and contains code that should be synchronized for it.

``` java
synchronized(obj) {
  // ...
}
```

Java threads can communicate with each other via the `Object` methods `wait`, `notify`, and `notifyAll`. The `wait` method causes the calling thread to relinquish the monitor and sleep until another thread enters the monitor and calls `notify` or `notifyAll`. The `notify` method wakes up a thread that called `wait` on the same object, and the `notifyAll` method wakes up all threads that did so with one gaining access at random.

Despite calling `wait`, a thread may be woken up for no apparent reason (e.g. interrupted by a signal), which is why it's advised to put the `wait` call within a loop that checks the overall condition that is being waited upon.

``` java
while (condition) {
  try {
    wait();
  } catch (InterruptedException e) {
    // ...
  }
}
```

The functionality for suspending, resuming, and stopping threads must be implemented manually, usually in the form of a loop that checks a flag which represents the user's request. A suspend method can't be provided by the standard library because it could end up suspending a thread before it relinquishes its locks, leading to deadlocks. A stop method can't be provided either because it could leave data in an inconsistent state if it's stopped abruptly.

Normally in multithreaded programs when two or more threads share the same variable they store thread-local copies and update the "master copy" at certain points in execution, such as when `synchronized` methods are entered. Specifying the variable as `volatile` tells the compiler that it must always use the master copy of the varaible, or to always keep the local copies synchronized with the master copy.

# Enumerations

In Java, enumerations define class types that implicitly inherit from the `Enum` class, meaning that they may define constructors, methods, and instance variables. Despite this, they may not explicitly inherit or be inherited from.

Enumeration constants are implicitly `static` and `final`. Each enumeration constant is an object of its enumeration type, and each enumeration constant has its own copy of instance variables.

When defining a constructor, it may be called once for each enumeration constant that is specified by providing the parameters in parentheses after each constant.

``` java
enum Colors {
  Red(3), Green(2), Blue(1)

  private int number;

  Colors(int n) { number = n; }
  int getNumber() { return number; }
}
```

An enumeration constant's position, i.e. its _ordinal value_, can be retrieved by calling the `ordinal` method, and it can be compared against another enumeration constant's ordinal using the `compareTo` method. The `equals` method can be used to test if two enumeration constants are the same. Since enumeration constants are objects of their enumeration type, they can also be compared using the reference equality operator `==`.

# Annotations

Annotations provide metadata about code that can be used by development tools. Annotations are created through a special kind of interface that consists solely of method declarations for which Java provides implementations. All annotations implicitly extend the `Annotation` interface, so that `Annotation` is a super-interface of all annotations.

``` java
@interface Annot {
  String str();
  int val();
}

@Annot(str = "Example", val = 100)
public static void method() {}
```

Annotations could be used on declarations of any type, including classes, methods, fields, parameters, enumeration constants, and even other annotations. Annotations are applied by giving values to the annotation members.

Annotations members can be given default values by following the member line with the `default` keyword and the value to give it, such as:

``` java
int val() default 3;
```

Annotation retention policies refer to how long the annotation is retained. Regardless of the policy, annotations on local variable declarations are not retained in `.class` files.

Policy    Lifetime
-------   ---------
`SOURCE`  source code
`CLASS`   `.class` files
`RUNTIME` `.class` files; available at runtime

Annotation retention policies are specified using the `@Retention` annotation.

``` java
@Retention(Retention.Policy.RUNTIME)
@interface Annot {
  String str();
  int val();
}
```

Annotations with `RUNTIME` retention policies can be obtained at run-time via reflection. First, a `Class` object must be obtained that represents the class whose annotations we want to obtain, which is usually done with `getClass` or the `class` member. Next, it's necessary to obtain an object that represents the item for which we want to obtain annotations, e.g. `getMethod`. Once one of these objects is obtained, the actual annotation may be obtained with `getAnnotation` which can then be queried for the values of its members.

``` java
@Retention(RetentionPolicy.RUNTIME)
@interface MyAnno {
  String str();
  int val();
}

class Meta {
  @MyAnno(str = "test", val = 3)
  public static void myMeth(String str, int i) {
    Class<?> c = Meta.class;
    Method m = c.getMethod("myMeth", String.class, int.class);
    MyAnno anno = m.getAnnotation(MyAnno.class);

    System.out.println("str: " + anno.str() + ", val: " + anno.val());
  }
}
```

Alternatively, the `getAnnotations` method on a given item, such as `Method`, yields all annotations associated with the item with a `RUNTIME` retention. This method is defined by the `AnnotatedElement` interface, which defines many other annotation introspection methods.

Marker annotations don't have any members, so that their only purpose is to _mark_ the items to which they're applied, which can then be checked using the method `isAnnotationPresent`. Parentheses are optional with marker annotations.

Single-member annotations are those that only contain one member. These annotations can leverage a short-hand syntax if the member's name is `value`, in which case the value of the single member is the only thing within the parentheses.

It's also possible to use this short-hand if there are other members but they have default values.

``` java
@interface MySingle {
  int value();
}

@MySingle(100)
class Single {}
```

There are a variety of built-in annotations but some are used more than others.

The `@Target` annotation specifies the types of items to which the annotation may be applied by supplying possible targets as defined by the `ElementType` enumeration. If more than one target is specified, it must be specified in a comma-separated manner within braces, as in array initialization syntax.

``` java
@Target({ElementType.FIELD, ElementType.LOCAL_VARIABLE})
@interface Whatever {}
```

Constant          Applicable To
---------         --------------
`ANNOTATION_TYPE` another annotation
`CONSTRUCTOR`     constructor
`FIELD`           field
`LOCAL_VARIABLE`  local variable
`METHOD`          method
`PACKAGE`         package
`PARAMETER`       parameter
`TYPE`            class, interface, enumeration
`TYPE_PARAMETER`  type parameter (JDK 8)
`TYPE_USE`        type use (JDK 8)

The `@Inherited` annotation can only be applied to annotations being applied to class declarations, causing the annotation of a superclass to be inherited by a subclass. That is, if a subclass is searched for a given annotation and it's not found, its superclass is searched.

The `@Override` annotation can only be used on methods in order to declare that the method to which it's applied must be overriding a method from a superclass, yielding a compile-time error if this isn't the case.

The `@Deprecated` annotation is used to mark a declaration obsolete.

The `@FunctionalInterface` is a marker annotation added by JDK 8 that indicates that the annotated interface is a functional interface. However, any interface with exactly one abstract method is by definition a functional interface, so that this annotation is purely informational, aside from yielding compile-time errors if the constraint isn't satisfied.

Beginning with JDK 8, annotations can also be placed in most cases in which a type is used---such as return types, the type of `this`, a type cast, and so on---in which case they're referred to as type annotations. These annotations are mainly used for external tools to enforce stricter checks than the Java compiler may perform.

To annotate the type of `this`, known as the _receiver_, JDK 8 allows explicitly declaring `this` as the first parameter of a method in which case it should take on the type of the class the method belongs to.

``` java
int myMeth(@TypeAnno SomeClass this, int i, int j) {}
```

When annotating return types, it's not possible to annotate a return type of `void`.

JDK 8 added support for so called _repeating annotations_ which are annotations that can be repeated on the same element. The annotation that is intended to be repeatable must be annotated with the `@Repeatable` annotation which specifies the annotation's container type, that is, another annotation for which its `value` field is an array of the repeatable annotation type.

These repeated annotations can then be retrieved using `getAnnotation` to retrieve the container type.

Alternatively, it's more straightforward to use the `getAnnotationsByType` method.

``` java
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(MyRepeatedAnnos.class)
@interface MyAnno {
  String str() default "test";
  int val() default 3;
}

@Retention(RetentionPolicy.RUNTIME)
@interface RepeatedAnnos {
  MyAnno[] value();
}

@MyAnno(str = "first", val = 1)
@MyAnno(str = "second", val = 2)
class Annotated {
  Annotation container = Annotated.class.getAnnotation(RepeatedAnnos.class);
  MyAnno[] annos = container.value();

  // or
  MyAnno[] annos = Annotated.class.getAnnotationsByType(MyAnno.class);

  for (Annotation a : annos)
    System.out.println(a);
}
```

# Lambdas

A functional interface is an interface that contains only one abstract method. This means that the interface can contain other methods so long as they have default implementations. The functional interface's method specifies the target type, and lambda expressions can only be specified in a context in which a target type is defined.

When a lambda expression does occur in a target type context, an instance of a class is automatically created that implements the functional interface. The parameters and return type of the lambda expression must match those of the abstract method's, and any exceptions thrown by the lambda must be acceptable to the method.

``` java
interface Test {
  double getValue();
}

class Demo {
  Test t = () -> 2.0;
  System.out.println("value: " + t.getValue());
}
```

If a lambda expression has only one parameter, it's not necesary to surround the parameters with parentheses. If it's necessary to explicitly declare the type of a parameter, all of them must be specified---all or nothing. If multiple statements are required within a lambda, they simply need to be surrounded with braces as with a method body and a return statement must be given.

Lambdas may only use local variables from their enclosing scope if they're effectively final, that is, their value doesn't change after they're first assigned. As a result, lambdas can't modify local variables from their enclosing scope. However, it _may_ use and modify instance varaibles from its invoking class.

Method references can refer to methods without executing them. Static method references can be obtained using the `::` separator introduced in JDK 8. A method reference can then be used anywhere in which it is compatible with the target type.

``` java
ClassName::staticMethod;
```

It's also possible to obtain references to instance methods of a specific object with the same syntax.

``` java
objRef::methodName;
```

It's also possible to obtain a reference to an instance method that can be used on any object. In this case, the first parameter of the functional interface should be of the type of the invoking object and the second should be the parameter(s) specified by the method.

``` java
interface Func {
  boolean func(ClassName a, int b);
}

ClassName::instanceMethod;
```

If the class is generic, then the type parameter is specified after the `::` separator.

``` java
ClassName::<Type, OtherType>instanceMethod;
```

It's also possible to reference constructors. If the class is generic, then the type parameters are provided as mentioned above. Constructor references for arrays can also be created. A functional interface for a constructor references to arrays should contain a method that takes an integer parameter to refer to an array constructor.

``` java
ClassName::new;

ClassName[]::new; // arrays
```

A superclass version of a method may be referred to with the `super` keyword.

``` java
super::methodName;
```

JDK 8 contains predefined functional interfaces in `java.util.function`{.path}.

# IO

IO is performed in Java through the stream abstraction which is split into two types: byte streams reserved for binary data and character streams reserved for internationalizable Unicode text (and are sometimes more efficient than byte streams).

Byte streams consist of two hierarchies with the following abstract classes at the top: `InputStream` and `OutputStream`. Character streams are similar, with `Reader` and `Writer` being at the top. Each of these sets of classes define `read` and `write` methods respectively.

The `System` class defines three static, predefined stream variables `in`, `out`, and `err` where `in` is an `InputStream` while `out` and `err` are `PrintStream` types.

For example, to read input from the keyboard, the `InputStream` can be wrapped by `InputStreamReader` to convert bytes to characters, then wrapped in `BufferedReader` to support a buffered input stream.

``` java
BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
```

Likewise, to print characters to the terminal it's preferred to use a `PrintWriter` which can be created by wrapping the `PrintStream`.

``` java
PrintWriter pw = new PrintWriter(System.out, true);
```

# Strings

String objects are automatically created from string literals, which means that string literals may be used as if they were String objects themselves. When working with regions, the end index is one-past the last affected index, as with C++ iterators.

Java automatically converts data to strings using the `String`'s static method `valueOf`, which is overloaded for all primitive types and `Object`. For other objects, `valueOf` calls the object's `toString` method.

The `equals` and `equalsIgnoreCase` methods can be used to determine if a string is equal to another. The `regionMatches` method can be used to determine if separate regions of two different strings match. The `startsWith` and `endsWith` methods can be used to determine if a string ends or begins with another string. The `Comparable` interface's `compareTo` and `compareToIgnoreCase` methods can be used to get a less, equal, or greater than result with respect to another string.

The `indexOf` and `lastIndexOf` methods can be used to obtain the index where the first occurrence of a substring begins. There are overloads which take a starting point as well, which can simplify getting all the positions of all of the occurrences.

Strings are immutable, so operations that appear to modify them simply return new copies of the resulting strings. The `substring` method can be used to extract a copy of a region of a string given a starting index and optionally en ending index. The `replace` method can replace all occurrences of a character with another. An overload exists which replaces character sequences. The `replaceAll` method can replace any substring that matches the given regex with the specified string.

JDK 8 adds a static `join` method that can join a number of strings with a given delimiter. Conversely, the `split` method can split a string based on a regex.

## StringBuffer

The `StringBuffer` class can represent a growable, mutable string. JDK 5 added `StringBuilder` which is similar but not thread-safe, making it inadvertently faster.

It's possible to ensure a certain capacity is available with the `ensureCapacity` method which is given the minimum size that the buffer should have. The `setLength` method can be used to either extend the string by adding null characters or to truncate the string.

`StringBuffer` provides a `setCharAt` method that can modify a character at the provided position. The `append` method can concatenate strings to the buffer while returning the updated buffer, allowing calls to this method to be chained. The `insert` method can insert a given string at the specified index. The `reverse` method can reverse the string. The `delete` and `deleteCharAt` methods can remove a region of the string or a single character, respectively. The `replace` method is similar to `String`'s except it's in-place.

