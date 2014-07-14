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

# Primitives

Java doesn't support unsigned integers. The `double` floating-point type is more commonly used than `float` because all of the standard library's math functions use doubles. The `char` can hold unsigned 16-bit values and uses UTF-16.

Underscores can be written within integer or floating-point literals to make them more readable.

Automatic type conversions only take place if the two types are compatible and the destination is larger than the source type, a widening conversion. Manual conversions can be performed using casts, the same as C-style casts.

When different types are present in the same expression, Java enforces type promotion rules. The `char`, `byte`, and `short` values are promoted to `int`. If `long`, `float`, or `double` values are present in the expression, then the entire expression becomes of that type.

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

