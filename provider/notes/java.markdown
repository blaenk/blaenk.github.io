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

# Arrays

Array type syntax can place the `[]` in one of two locations. The latter is better for methods returning arrays and for declaring multiple arrays in one line.

``` java
int a[] = new int[3];
int[] a = new int[3];

// three arrays
int[] nums, nums2, nums3;
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

Fields can be final, which makes them immutable. Final fields can be initialized via a value given at declaration or within a constructor.

Variable-length arguments are specified by threep periods and makes the arguments available as an array.

``` java
void printArgs(int ... v) {
  for (int a : v) {
    System.out.println(a);
  }
}

printArgs(1, 2, 3);
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

# Interfaces

Classes must implement the complete set of methods specified in an interface in order to fully implement that interface. They must be initialized within the interface. Interfaces must be declared as either `public` or use the default access level, while nested interfaces may be declared as `public`, `private`, or `protected`.

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

# Multithreaded Programming

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

Despite calling `wait`, a thread may be woken up for no apparent reason (e.g. `sleep` system call interrupted by a signal), which is why it's advised to put the `wait` call within a loop that checks the overall condition that is being waited upon.

``` java
while (condition) {
  try {
    wait();
  } catch (InterruptedException e) {
    // ...
  }
}
```

The functionality for suspending, resuming, and stopping threads must be implemented manually, usually in the form of a loop that checks a flag which represents the user's request. A suspend method can't be provided by the standard library because it could end up suspending a thread before it relinquishes its locks, leading to deadlocks. A stop method can't be provided as well because it could leave data in an inconsistent state if it's stopped abruptly.

# Enumerations

In Java, enumerations define class types that implicitly inherit from the `java.lang.Enum` class, so that they may have constructors, methods, and instance variables. Despite this, they may not explicitly inherit or be inherited from.

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

# Type Wrappers

Type wrappers are classes that wrap primitive types, such as `Character` which wraps `char`. All numeric type wrappers such as `Integer` and `Float` inherit from abstract class `Number` [^NSNumber] which provides conversion methods for all numeric types.

[^NSNumber]: Reminds me of Objective-C's `NSNumber`.

Encapsulating a primitive in an object is referred to as _boxing_, and the reverse is called _unboxing_. _Autoboxing_ and _auto-unboxing_ refers to the automatic wrapping and unwrapping of primitive values. JDK 5 added support for autoboxing and auto-unboxing, which works whenever a primitive type must be converted to an object, such as when passed as parameters to methods or when used in expressions.

``` java
Integer i = 100; // autoboxed

Integer ib = 1;
++ib; // auto-unboxed, incremented, re-boxed
```

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

Annotations could be used on declarations of any type, such as classes, methods, fields, parameters, and enumeration constants. Even an annotation can be annotated. Annotations are applied by giving values to the annotation members.

Annotation retention policies refer to at what point an annotation is discarded. Regardless of the policy, annotations on local variable declarations are not retained in `.class` files.

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

