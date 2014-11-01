---
title: Clojure
published: April 6, 2014
excerpt: A lisp-inspired, dynamic JVM language
comments: off
toc: left
---

Recently I've been thinking about my opinions on the various languages I know, particularly with regards to which I should focus on, and I decided that knowing a JVM language would be very beneficial because of how robust and time-proven the JVM is, especially compared to other language VMs. For this reason I considered Scala and Clojure, and Scala seemed more like Haskell to me so I decided to [go with that one first].

[go with that one first]: /notes/scala

*[JVM]: Java Virtual Machine
*[VM]: Virtual Machine

I didn't have an overwhelming reaction to Scala, so I decided to give Clojure a shot as well to better compare them. My main resource is the book [Clojure Programming]. There's also [Clojure for the Brave and True].

[Clojure Programming]: http://amzn.com/1449394701
[Clojure for the Brave and True]: http://www.braveclojure.com/

* toc

# Homoiconicity

Clojure and other lisp languages are _homoiconic_, often referred to as "code as data," which means that the way the code is written is itself the abstract syntax tree (AST) of the program. This makes creating embedded domain specific languages (EDSLs) very straightforward, as well as simply making the code easier to reason about. Questions of precedence, for example, are directly encoded into the code.

*[AST]: Abstract Syntax Tree
*[EDSL]: Embedded Domain Specific Language

Because Clojure code is itself an AST in a Clojure data structure, metaprogramming is also more powerful because it simply involves manipulating that data structure; this is the basis of macros.

# Reader

Clojure AST structures can be deserialized into Clojure structures using the `read`-like functions. In the following examples, the structures are printed back out by the REPL using the `pr-str` function. The fact that serialization of Clojure structures is this straightforward is what drives most Clojure developers to use it as the primary serialization mechanism.

*[REPL]: Read-Eval-Print-Loop

``` clojure
(read-string "42")
;= 42

(read-string "(+ 1 2)")
;= (+ 1 2)

(pr-str (read-string "[1 2 3]"))
;= [1 2 3]
```

**Note**: the Clojure REPL always starts in the default `user` namespace.

The reader allows for different syntax to make code more concise. For example, evaluation of a form can be suppressed by prefixing it with a quote `'`. Anonymous function literals can be defined with `#()`.

## Scalar Literals

Characters are denoted by a blackslash, as in `\c`, and they natively support Unicode. It's also possible to use special characters such as `\n` would be used in strings, but individually:

* `\space`
* `\newline`
* `\formfeed`
* `\return`
* `\backspace`
* `\tab`

## Keywords

Keywords are similar to Ruby/Scala symbols and Erlang atoms. They are prefixed by a colon `:` and consist of any non-whitespace character, where a slash `/` denotes a _namespaced keyword_, and a double colon `::` is expanded by the reader to a namespaced keyword in the current namespace, or another namespace if the keyword started by a namespace alias as in `::alias/keyword`.

``` clojure
(def pizza {:name "Ramunto's"
            :location "Claremont, NH"
            ::location "123,-456"})
;= #'user/pizza

pizza
;= {:name "Ramunto's", :location "Claremont, NH", :user/location "123,-456"}

(:user/location pizza)
;= "123,-456
```

Keywords are "named" values which are values that have intrinsic names that can be accessed using the `name` function, and the namespace can be accessed with the `namespace` function:

``` clojure
(name :user/location)
;= "location"

(namespace :user/location)
;= "user"
```

As in Ruby, keywords are often used for indexing hashes. The following defines a hashmap with a `:name` and `:city` key and then accesses the value for the `:city` key. Keywords can be used in the function position because they _are_ functions that look themselves up in collections passed to them.

``` clojure
(def person {:name "Sandra Cruz"
             :city "Portland, ME"})
;= #'user/person

(:city person)
;= "Portland, ME"
```

## Symbols

Symbols are identifiers that evaluate to the values they name. For example, in the following code, `average` is a symbol referring to the function held in the var named `average`. Symbols containing a slash `/` denote a _namespaced symbol_ which evaluates to the named value in the specified namespace.

``` clojure
(average [10 20 30])
;= 20
```

The variables can be referred to directly by prefixing a symbol with `#'`.

## Numbers

Numeric literals exist for a variety of number types. Custom numerical bases can be used with the `#r` prefix where `#` would be the desired number base.

Syntax               Type
-------              -----
42, 0xff, 2r101, 040 long
3.14, 6.02e23        double
42N                  clojure.lang.BigInt
0.01M                java.math.BigDecimal
22/7                 clojure.lang.Ratio

## Regular Expressions

Strings prefixed with a hash `#` are regex literals which yield `java.util.regex.Pattern`{.path} instances.

``` clojure
(re-seq #"(\d+)-(\d+)" "1-3")
;= (["1-3" "1" "3"])
```

## Comments

Single-line comments are started with a semicolon `;`. There are also _form-level_ comments prefixed by the `#_` reader macro which cue the reader to ignore the next Clojure _form_ following the macro. This is particularly useful when wanting to comment out blocks of code. The `comment` macro can also be used to comment out code but they always evaluate to `nil`, which may be unexpected.

``` clojure
(read-string "(+ 1 2 #_(* 2 2) 8)")
;= (+ 1 2 8)

(defn some-func
  [args]
  code
  #_(if debug-level
    (println "debugging")
    (println "more debugging")))

(+ 1 2 (comment (* 2 2)) 8)
;= NullPointerException
```

## Whitespace

Commas are considered whitespace by the reader. Whether to use them or not is a question of style, but they're generally used when multiple pairs of values appear on the same line.

``` clojure
(= [1 2 3] [1, 2, 3])
;= true

(create-user {:name user, :email email})
```

## Collections {#collection-literals}

There are literals for lists, vectors, maps, and sets. Note that since lists denote calls in Clojure, it's necessary to quote them to prevent their evaluation as a call.

``` clojure
'(a b :name 12.5)     ;; list
['a 'b 12.5]          ;; vector
{:name "Bob" :age 31} ;; map
#{1 2 3}              ;; set
```

## Record Literals

Record literals consist of the pound sign `#` followed by the record name and a map literal containing the field names as keyword keys with their associated values. See [records](#records).

## Tagged Literals

Tagged literals are custom data readers. On startup, Clojure looks for files named `data_readers.clj`{.path} at the root of the classpath which must contain a map of symbols mapping tags---to be recognized by the Clojure reader---to the name of fully-qualified Vars that are invoked by the reader to parse the form following the tag.

``` clojure
{foo/bar my.project.foo/bar
 foo/baz my.project/baz}

#foo/bar [1 2 3]
```

The Var `#'my.project.foo/bar` is invoked by the reader on vector `[1 2 3]` _after_ it has been read as a normal Clojure data structure by the reader. That is, the reader will parse the form after the tag as a data structure, then invoke the tagged literal function on the data structure itself. The tagged literal function should then return a value that _replaces_ the tagged data structure in the final result [^clojurescript_js].

[^clojurescript_js]: This is used in Om in the form of `#js` to parse a map and output JSON in its place, or parse a vector and output an array in its place.

# Namespaces

Vars are defined using the `def` special form which takes the symbol used to refer to the var and the value to store in that var. When the symbol is used on its own to access the var's value, the symbol is said to be _unqualified_ and so it is resolved within the current namespace. Vars can also be redefined by supplying the same symbol with a different value to the `def` function.

``` clojure
(def x 1)
;= #'user/x

x
;= 1

(def x "hello")
;= #'user/x

x
;= "hello"
```

Contrary to unqualified symbols, symbols can be _namespace-qualified_ so that they are resolved within the specified namespace. For example, if we create a new namespace `foo`, we can continue to refer to the symbol `x` by qualifying the namespace. However, if we attempt to access the unqualified symbol, it will try to find `x` within the `foo` namespace, which doesn't exist:

``` clojure
*ns*
;= #<Namespace user>

(ns foo) ; create and switch to new 'foo' namespace
;= nil

*ns*
;= #<Namespace foo>

user/x
;= "hello"

x
;= CompilerException: Unable to resolve symbol: x
```

The `in-ns` function can be used to switch to another namespace, creating it if it doesn't already exist. Symbols from other namespace will have to be fully qualified in order to access. The `refer` function can add mappings to all of another namespace's vars into the current namespace.

``` clojure
(def x 42)

(in-ns 'asdf)
x
;= #<CompilerException: Unable to resolve symbol: x>

(clojure.core/refer 'user)
x
;= 42
```

The `refer` function also takes optional keyword arguments `:exclude` which can be used to exclude specific vars from the namespace, `:only` which specifies which mappings should be available unqualified, and `:rename` which can alias vars from the namespace to appear another way in the current namespace.

``` clojure
(refer 'clojure.core
  :exclude '(range)
  :rename '{+ add
            - sub
            / div
            * mul})
```

The `require` function can ensure that a namespace is loaded and can optionally establish aliases for the name of the namespace by passing a _libspec_, which is simply a vector containing the namespace, the `:as` keyword, and the alias to use.

If multiple namespaces are to be required and they share a common prefix, a list can be provided where the first element is the common prefix and the remaining elements are the remaining segments for the specific namespaces to load.

``` clojure
(require 'clojure.set)
(clojure.set/union #{1 2} #{3 4})
;= #{1 2 3 4}

; or
(require '[clojure.set :as set])
(set/union #{1 2} #{3 4})

; or
(require '(clojure string [set :as set])
```

The `use` function is similar to `require`, except that it `refer`s the specified namespace after it's loaded.

``` clojure
(use 'clojure.xml)

; equivalent to

(require 'clojure.xml)
(refer 'clojure.xml)
```

The aforementioned functions create mappings from symbols to vars, but namespaces can also contain Java classes and interfaces. Mappings to these can be created with the `import` function, which essentially makes the class' short name available for use. This function also supports the common-prefixed collection that `require` accepts.

There is no equivalent of the Java wildcard import. Inner classes such as `java.util.Map.Entry`{.path} can be referred to by the Java-internal notation e.g. `java.util.Map$Entry`{.path}.

``` clojure
(import 'java.util.Date 'java.text.SimpleDateFormat)
(.format (SimpleDateFormat. "MM/dd/yyyy") (Date.))
;= "06/25/2014"

(import '(java.util Arrays Collections))
```

The aforementioned namespace functions are mainly useful in a REPL. In actual source files, the `ns` macro should be used, which combines each of the aforementioned functionality by allowing one to specify what needs to be required, refered, used, and imported for the namespace to load and work properly.

The `:refer` keyword can be used in a `:require` form similar to `:only`, to allow the use of the provided symbols in an unqualified manner. In fact, the `:use` form can be obviated entirely by passing `:all` to `:refer` instead of a vector of symbols.

The `:refer-clojure` form is a synonym for `refer`ing to the `clojure.core`{.path} namespace, which is particularly useful for the purpose of `:exclude`ing symbols to avoid collisions.

The following are equivalent.

``` clojure
(in-ns 'examples.ns)
(clojure.core/refer 'clojure.core :exclude '[next replace remove])
(require '(clojure [string :as string]
                   [set :as set])
         '[clojure.java.shell :as sh])
(use '(clojure zip xml))
(import 'java.util.Date
        'java.text.SimpleDateFormat
        '(java.util.concurrent Executors
                               LinkedBlockingQueue))

(ns examples.ns
  (:refer-clojure :exclude [next replace remove])
  (:require (clojure [string :as string]
                     [set :as set]
                     ; thanks to :refer
                     [zip :refer :all]
                     [xml :refer :all])
            [clojure.java.shell :as sh])
  ; before :require x :refer :all
  ; (:use (clojure zip xml))
  (:import java.util.Date
           java.text.SimpleDateFormat
           (java.util.concurrent Executors
                                 LinkedBlockingQueue)))
```

Note that if a namespace contains dashes, the filename should contain underscores in their place.

There are functions `ns-map`, `ns-imports`, `ns-refers`, `ns-publics`, `ns-aliases`, and `ns-interns` which can yield a mapping of symbols pertaining to the respective category.

The `ns-unmap` and `ns-unalias` functions can be used to remove mappings to certain symbols and remove aliases for certain namespaces. The `remove-ns` function can be used to drop a namespace from the namespace map, making the code under that namespace eligible for garbage collection if it's not referred from anyplace else.

# Special Forms

Special forms are Clojure's primitives of computation upon which the rest of Clojure is built.

## Suppressing Evaluation

The special form `quote` suppresses evaluation of a Clojure expression. For example symbols evaluate to the value of the var they represent, but with `quote` that evaluation is suppressed, so they evaluate to themselves like strings and numbers do. The quote character `'` is reader syntax for `quote`. In fact, `quote` can be used on reader sugars to determine how they're actually represented.

``` clojure
(quote x)
;= x

(symbol? (quote x))
;= true

'x
;= x

(symbol? 'x)
;= true

(= '(+ x x) (list '+ 'x 'x))
;= true
```

## Code Blocks

The special form `do` evaluates all of the expressions provided to it in order and yields the last expression's value as its value. Many other forms such as `fn`, `let`, `loop`, `try` and `defn` wrap their body in an implicit `do` expression so that multiple inner expressions are evaluated.

``` clojure
(do
  (println "hi")
  (apply * [4 5 6]))
; hi
;= 120

(let [a (inc (rand-int 6))
      b (inc (rand-int 6))]
  (println (format "You rolled a %s and a %s" a b))
  (+ a b))
```

## Vars

The special form `def` defines or redefines a var with an optional value within the current namespace. Other forms implicitly create or redefine vars and are usually prefixed with `def` such as `defn` and `defn-`.

It's possible to refer to vars instead of the values that they hold by using the special form `var`. There's also a shorthand for this with `#'`.

``` clojure
(def x 5)
;= #'user/x

(var x)
;= #'user/x

#'x
;= #'user/x
```

## Local Bindings

The special form `let` allows lexically scoped named references to be defined.

``` clojure
(defn hypot
  [x y]
  (let [x2 (* x x)
        y2 (* y y)]
    (Math/sqrt (+ x2 y2))))
```

The `let` form also allows _destructuring_ similar to pattern-matching in languages like Haskell, Rust, Scala, and Erlang. For example, to destructure a sequence, specifically a vector, we simply pass it a list of symbols that will take on the appropriate values. Destructuring can also be nested, as in other languages. The ampersand `&` can be used to specify that the following symbol should take on the remaining _sequence_ of values. The `:as` keyword can be used to bind the collection to a value, similar to what `@` does in Haskell, Scala, and Rust.

``` clojure
(def v [42 "foo" 99.2 [5 12]])
;= #'user/v

(let [[x y z] v]
  (+ x z))
;= 141.2

(let [[x _ _ [y z]] v]
  (+ x y z))
;= 59

(let [[x & rest] v]
  rest)
;= ("foo" 99.2 [5 12])

(let [[x _ z :as original-vector] v]
  (conj original-vector (+ x z)))
;= [42 "foo" 99.2 [5 12] 141.2]
```

Maps can also be destructured in a similar manner. This works with Clojure's `hash-map`, `array-map`, records, collections implementing `java.util.Map`{.path}, and values supported by the `get` function such as Clojure vectors, strings, and array can be keyed by their indices.

``` clojure
(def m {:a 5 :b 6
        :c [7 8 9]
        :d {:e 10 :f 11}
        "foo" 88
        42 false})
;= #'user/m

(let [{a :a b :b} m]
  (+ a b))
;= 11

(let [{x 3 y 8} [12 0 0 -18 44 6 0 0 1]]
  (+ x y))
;= -17
```

The `:as` keyword can be used to bind the collection. The `:or` keyword can be used to provide a defaults map which will be consulted if the destructured keys aren't present.

``` clojure
(let [{k :unknown x :a
       :or {k 50}} m]
  (+ k x))
;= 55
```

Often times it may be desirable to destructure a map such that the symbols are named after the keys of the map, but doing this explicitly can get repetitive, which is why the options `:keys`, `:strs`, and `:syms` can be used.

``` clojure
(def chas {:name "Chas" :age 31 :location "Massachusetts"})
;= #'user/chas

(let [{name :name age :age location :location} chas]
  (format "%s is %s years old and lives in %s." name age location))

(let [{:keys [name age location]} chas]
  (format "%s is %s years old and lives in %s." name age location))
```

It's also possible to destructure vectors which themselves contain key-value pairs. This can be done explicitly by binding the key-value pairs with `&`, converting that to a `hash-map`, and then destructuring that, however, it's also possible with regular destructure syntax. This is specifically made possible by `let` by allowing the destructuring of rest sequences if they have an even number of values, i.e. key-value pairs.

``` clojure
(def user-info ["robert8990" 2011 :name "Bob" :city "Boston"])
;= #'user/user-info

(let [[username account-year & extra-info] user-info
      {:keys [name city]} (apply hash-map extra-info)]
  (format "%s is in %s" name city))
;= "Bob is in Boston"

(let [[username account-year & {:keys [name city]}] user-info]
  (format "%s is in %s" name city))
;= "Bob is in Boston"
```

## Functions

The special form `fn` is used to create functions. A function defined this way has no name, and so cannot be referred to later on. It can be place inside a var using the `def` form. The `fn` form also takes an optional name by which the function can reference itself. Furthermore, a function can have _multiple arities_, that is, define different bodies depending on the number of arguments passed.

``` clojure
((fn [x] (+ 10 x)) 8)

(def add-ten (fn [x] (+ 10 x)))

(add-ten 20)
;= 30

(def strange-adder (fn adder-self-reference
                     ([x] (adder-self-reference x 1))
                     ([x y] (+ x y))))

(strange-adder 10)
;= 11

(strange-adder 10 50)
;= 60
```

The `defn` form encapsulates the functionality of `def` and `fn`.

``` clojure
(defn strange-adder
  ([x] (strange-adder x 1))
  ([x y] (+ x y)))
```

The special form `letfn` can be used to define multiple functions at once that are aware of each other. This is useful for definining mutually recursive functions.

``` clojure
(letfn [(odd? [n]
          (even? (dec n)))
        (even? [n]
          (or (zero? n)
            (odd? (dec n))))])

(odd? 11)
;= true
```

Variadic functions are possible using the rest arguments syntax from destructuring.

``` clojure
(defn concat-rest
  [x & rest]
  (apply str (butlast rest)))

(defn make-user
  [& [user-id]]
  {:user-id (or user-id (str (java.util.UUID/randomUUID)))})
```

It's also possible to use keyword arguments in functions, which is facilitated through map destructuring of rest sequences.

``` clojure
(defn make-user
  [username & {:keys [email join-date]
               :or {join-date (java.util.Date.)}}]
  {:username username
   :join-date join-date
   :email email
   :exp-date (java.util.Date. (long (+2.592e9 (.getTime join-date))))})

(make-user "Bobby")
(make-user "Bobby"
           :join-date (java.util.Date. 111 0 1)
           :email "bobby@example.com")
```

Function literals have specific, concise syntax by being prepended with `#`. Placeholder arguments are prepended with `%`, though the first argument can be referred to by a single `%`. Function literals don't contain an implicit `do` form, so multiple statements require an explicit `do` form. It's also possible to specify variadic functions by assigning the rest of the arguments to `%&`.

``` clojure
(fn [x y] (Math/pow x y))
#(Math/pow %1 %2)

(fn [x & rest] (- x (apply + rest)))
#(- % (apply + %&))
```

**Note**: Function literals cannot be nested.

## Conditionals

The special form `if` is the single primitive conditional operator in Clojure. If no else-expression is provided it is assumed to be `nil`. There are other conditionals based on this form that are more convenient in specific situations.

``` clojure
(if condition? true false)

(if-let [nums (seq (filter even? [1 2 3 4]))]
  (reduce + nums)
  "No even numbers found.") ; else

(when (= x y) true) ; else nil

(cond
  (< n 0) "negative"
  (> n 0) "positive"
  :else   "zero")

(condp = 2
  1 "one"
  2 "two"
  "neither")
;= 2

(condp some [1 2 3 4]
  #{0 6 7} :>> inc
  #{4 5 9} :>> dec
  #{1 2 3} :>> #(+ % 3))
;= 3
```

## Looping

The special form `recur` transfers control to the local-most `loop` or function, allowing recursion without consuming stack space and thereby overflowing the stack. The `loop` special form takes a vector of binding names and initial values. The final expression is taken as the value of the form itself. The `recur` special form is considered very low-level that is usually unnecessary, instead opting for `doseq`, `dotimes`, `map`, `reduce`, `for`, and so on.

``` clojure
(loop [x 5]
  (if (neg? x)
    x
    (recur (dec x))))

(defn countdown
  [x]
  (if (zero? x)
    :blastoff!
    (do (println x)
        (recur (dec x)))))
```

## Java Interop

The special forms `.` and `new` exist for Java interoperability. Their use is somewhat unnatural in Clojure, however, and so there are sugared forms which are idiomatic.

There are also special forms for exception handling and throwing. There are also lock primitives to synchronize on the monitor associated with every Java object, but this is usually unnecessary as there's macro `locking` that is better suited.

``` clojure
; instantiation
(new java.util.ArrayList 100)
(java.util.ArrayList. 100)

; static method
(. Math pow 2 10)
(Math/pow 2 10)

; instance method
(. "hello" substring 1 3)
(.substring "hello" 1 3)

; static fields
(. Integer MAX_VALUE)
Integer/MAX_VALUE

; instance field
(. some-object some-field)
(.someField some-object)
```

## Specialized Mutation

The `set!` special form can be used to perform in-place mutation of state, which is useful for setting thread-local values, Java fields, or mutable fields.

## Eval

The `eval` form evaluates its single argument form, which is useful when used with `quote` or `'` to suppress evaluation of the argument until it's evaluated by `eval`. With this final form, it's possible to reimplement a simple REPL.

``` clojure
(defn simple-repl
  "Simple REPL. :quit to exit."
  []
  (print (str (ns-name *ns*) ">>> "))
  (flush)
  (let [expr (read)
        value (eval expr)]
    (when (not= :quit value)
      (println value)
      (recur))))

(simple-repl)
```

# Function Application

The `apply` function can be used to apply a function to arguments, where the last argument can be a collection of arguments to apply, but the complete set of arguments must be passed. The `partial` function allows _partial application_ by providing only a subset of the arguments, yielding a function that can be used further. The `comp` function can be used to compose other functions [^haskell_compose].

[^haskell_compose]: Similar to Haskell's compose function `.`

``` clojure
(def args [2 -2 10])
(apply * 0.5 3 args)
;= -60.0

(def only-strings (partial filter string?))
(only-strings ["a" 5 "b" 6])
;= ("a" "b")

(def negated-sum-str (comp str - +))
(negated-sum-str 10 12 3.4)
;= "-25.4"
```

The `->` and `->>` macros can be used in place of explicitly using `comp`. The `->` macro inserts its first argument as the first argument to the first form, which is very useful when the item is, for example, a Java object on which we're calling methods. The `->>` macro on the other hand inserts its first argument as the last argument in the first form, useful for non-object items. This makes it unnecessary to create partial functions, instead simply requiring function calls without the last argument applied:

``` clojure
(defn composed
  (comp keyword
        str/join
        (partial interpose \-)
        (partial map str/lower-case))

(defn composed
  [s]
  (->> (map str/lower-case)
       (interpose \-)
       str/join
       keyword))
```

Functions can be memoized using the `memoize` function.

# Collections

There are a set of core collection functions which allow data structures to participate in the common collection abstraction. These consist of the following functions:

Function  Purpose
--------- --------
`conj`    add item
`seq`     yield sequence
`count`   count items
`empty`   yield empty instance
`=`       check equality

The `conj` function has the guarantee that it adds values efficiently, so that it _prepends_ items to lists instead of appending them [^haskell_cons].

[^haskell_cons]: Similar to Haskell's cons function `:`

## Sequences

Sequences are an abstraction to obtain and traverse sequential views over arbitrary values, such as in a collection. Sequences provide a base set of operations:

Function   Purpose
---------  --------
`seq`      produces sequence
`first`    yields first item
`rest`     yields items excluding first
`next`     consumes current item
`lazy-seq` yields lazy sequence

The important distinction between `next` and `rest` is that `next` yields `nil` when the sequence is empty, whereas `rest` continues to yield an empty sequence.

Sequences can be constructed using either `cons` or `list*`, where `cons` is similar to the Haskell `cons`. Unlike `conj`, which _may_ prepend items in order to satisfy the efficiency guarantee, `cons` _always_ prepends items. The `list*` function is a helper which can take any number of head values, followed by a sequence, so that the following two are equivalent:

``` clojure
(cons 0 (cons 1 (range 2 5)))
;= (0 1 2 3 4)

(list* 0 1 (range 2 5))
;= (0 1 2 3 4)
```

### Lazy Sequences

Sequence contents can be evaluated lazily, where the process of accessing a lazy sequence is referred to as _realization_ [^haskell_forcing], such that when all values of a lazy sequence have been computed, it is considered to be _fully realized_. An important characteristic of the `cons` and `list*` functions is that they _don't_ force the evaluation of lazy sequence arguments.

[^haskell_forcing]: Compared to Haskell's _forcing_.

``` clojure
(defn random-ints
  [limit]
  (lazy-seq
    (cons (rand-int limit)
          (random-ints limit))))

(take 3 (random-ints 50))
;= (53 23 62)
```

The `repeatedly` function constructs infinite lazy sequences by invoking a given function, so that the above could be simply expressed as:

``` clojure
(repeatedly 3 (partial rand-int 50))
;= (13 64 36)
```

All of the core sequence-processing functions return lazy sequences, such as `map`, `for`, `filter`, `take`, and `drop`, so that lazy sequences can be layered without forcing underlying lazy sequences.

The `next` function is able to return `nil` instead of an empty sequence because it checks to see if it's empty, and that emptiness check _forces_ the head of the tail sequence [^haskell_thunks]. Because sequential destructuring uses `next` and _not_ `rest`, it's important to note that destructuring a lazy sequence _always_ realizes the tail's head value.

[^haskell_thunks]: It wouldn't force the value in Haskell because of the concept of [thunks].

[thunks]: http://www.haskell.org/haskellwiki/Thunk

``` clojure
(let [[x & rest] (random-ints 50)])
; realized random number
; realized random number
;= nil
```

Laziness in Clojure is mostly used to transparently process big datasets that otherwise wouldn't fit entirely in memory, in order to express algorithms in a more straightforward manner. In this sense, sequences should be viewed as an ephemeral medium of computation, _not_ as collections. In fact, evaluation of some lazy sequences is batched as a performance optimization, which means that some items' side effects can be run ahead of consumption by some amount.

It's possible to force the realization of an entire list with functions `doall` and `dorun`, where `doall` retains the contents of the sequence and `dorun` disposes of them [^haskell_mapm].

[^haskell_mapm]: This strikes me as _very_ similar to Haskell's [`sequence`](http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Monad.html#v:sequence) and [`sequence_`](http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Monad.html#v:sequence_), respectively.

### Head Retention

When a reference is held to a sequence, which is done by keeping a reference to the _head_ of a sequence, it implies that the program is interested in that sequence's items. As a result, this prevents garbage collection from occurring on the items of the sequence, which is known as _head retention_.

Consider the `split-with` function [^haskell_break] in the following scenario where a very long sequence is split into a very short prefix and a very long suffix and then both are counted. Lazy processing (counting) of the much longer suffix is negated (i.e. _not_ lazy) due to the fact that a reference to the head of the larger sequence (of which the suffix is a part) is held as `t`, causing any potentially lazily processed items in the sequence to be retained anyways.

[^haskell_break]: This function is essentially Haskell's [`break`](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-List.html#v:break)

``` clojure
(let [[t d] (split-with #(< % 12) (range 1e8))]
  [(count d) (count t)])
;= #<OutOfMemoryError ...>
```

If instead the short prefix were counted first so that the reference to the head of the full sequence were no longer necessary, there would be no reason to retain the elements of the entire sequence (including the longer suffix) and so it could process (count) the suffix lazily.

Inserting into a map or set, or using the `=` or `count` functions on a sequence are common causes of potentially premature realization.

## Associative

Like sequences, associative is an abstraction shared by data structures that associate keys with values, the most common one being a map. These consist of the following functions:

Function    Purpose
---------   --------
`assoc`     establish new association
`dissoc`    drop association
`get`       get value for given key
`contains?` check if association exists

The `get` function returns `nil` if the key doesn't exist in the association. This poses a problem because it may be intentional to associate `nil` with a key, making it ambiguous as to whether or not the association exists. The solution is to use `find`, which always returns the entire association as a tuple, which in Clojure is expressed as a vector.

``` clojure
(get {:a 1} :b)
;= nil

(find {:a 1} :b)
;= nil

(find {:a 1} :a)
;= [:a 1]
```

The `get-in` function can yield a value in a nested associative structure given a vector path to the value. Like `get`, it can accept a default value to return if the path is not found.

``` clojure
(def m {:profile {:name "Test"}})

(get-in m [:profile :name])
;= "Test"
```

The `assoc` and `dissoc` functions can be used on multiple sets of values and list of keys, respectively.

``` clojure
(assoc {:a 1 :b 2}
  :c 3
  :d 4)
;= {:a 1, :b 2, :c 3, :d 4}

(dissoc {:a 1 :b 2}
  :a
  :b)
;= {}
```

## Indexed

The indexed abstraction works on things that can be numerically indexed, such as vectors, lists, sequences, Java arrays and lists, strings, and regular expression matchers.

The `get` and `assoc` functions can also be used on vectors, where the indices would serve as the keys. There also exists the function `nth` which works the same way except that `nth` returns an exception when an index is out of bounds. However, both can be provided a default return value, in which case their semantics are identical. The `get` function is more resilient than `nth` since it returns `nil` when the subject of the lookup is unsupported.

``` clojure
(get [1 2 3] 1)
;= 2

(nth [1 2 3] 5)
;= exception

(nth [1 2 3] 5 :not-found)
(get [1 2 3] 5 :not-found)
;= :not-found

(get 42 0)
;= nil

(nth 42 0)
;= exception
```

## Stacks

Clojure doesn't have a distinct stack type, but it supports stack operations on lists and vectors through the following functions:

Function  Purpose
--------- --------
`conj`    push value
`pop`     pop value
`peek`    peek top value

## Sets

Sets can be added to with `conj` and can be tested for membership with `get`, acting like associations of items with themselves. There's also a `disj` which can be used to remove items from a set.

## Sorted

The sorted abstraction guarantees that items in the collection will be maintained in a stable ordering. This abstraction supports maps and sets with following functions:

Function  Purpose
--------- --------
`rseq`    reverse sequence in constant time
`subseq`  return sequence of values within a certain range
`rsubseq` reversed `subseq`

Sorted maps and sets can be created with `sorted-map`, `sorted-set`, or with the `sorted-map-by` and `sorted-set-by` functions which accept a predicate or comparator which defines the sort order.

Comparators are functions that return an integer of value `-1`, `0`, or `1` depending on if the first argument is less than, equal to, or greater than the second argument. Any two-argument predicate can automatically be converted to a comparator using simple logic by treating the predicate as if it were `<`.

1. if the predicate returns true, return `-1`
2. otherwise, flip the arguments
    1. if true, return `1`
    2. else, return `0`

This may be a possible implementation:

``` clojure
(defn comparator
  [f]
  (fn [a b]
    (if (f a b)
      -1
      (if (f b a) 1 0))))
```

It's important to note that sort order, i.e. the comparator, defines the equality operation to use within a sorted collection. This can be fixed by delegating equality to a more general comparison function such as `compare` when the comparator results in `0`.

## Accessing

Collections can also be used as functions that can be used to access values within a collection. For maps, an optional default return value can be provided, as with `get`.

``` clojure
([:a :b :c] 2)
;= :c

({:a 5 :b 2} :b)
;= 2

({:a 1 :b 3} :z :not-found)
;= :not-found
```

Similarly, keywords and symbols are also functions that look themselves up in the provided collection. This is the recommended, idiomatic form of accessing values in a collection, as it helps avoid null pointer exceptions. This also makes them amenable for use as higher-order functions.

``` clojure
(:b {:a 5 :b 3})
;= 3

(:c {:a 5 :b 3} :not-found)
;= :not-found

(map :name [{:age 1 :name "Tom"}
            {:age 2 :name "Dick"}
            {:age 3 :name "Harry"}])
;= ("Tom" "Dick" "Harry")
```

For example, the `some` function returns the first item in a sequence that satisfies a given predicate. This can be used with a set to find the first item in a sequence that is contained in the set.

``` clojure
(some #{1 3 7} [0 1 4])
;= 1
```

However, this can pose a problem when the set in question contains either `false` or `nil`, since sets return their own value if they're present in the set, and those two values are logically false. In these situations, it would be more appropriate to use `contains?` [^set_membership].

[^set_membership]: It seems to me like it would always be preferable to use `contains?` due to this edge case, but it's good to know that sets can be used in this way.

``` clojure
(remove #{5 7} (cons false (range 3)))
;= (false 0 1 2)

(remove #{5 7 false} (cons false (range 3)))
;= (false 0 1 2)

(remove (partial contains? #{5 7 false}) (cons false (range 3)))
;= (0 1 2)
```

## Constructing

It's important to remember that list literals are the form in which code is specified in Clojure. To define an actual list without evaluating it as a function call, it's necessary to quote it so that it's not evaluated. This has the consequence of not evaluating any of the members, which may not be the intention. For this reason, the `list` function exists which evaluates each argument and uses the result as an element in the list.

``` clojure
'(1 2 (+ 1 2))
;= (1 2 (+ 1 2))

(list 1 2 (+ 1 2))
;= (1 2 3)
```

Vectors can be constructed using the `vector` function, which is similar to `list`, or the `vec` function which accepts a sequence. Vectors are the type used to represent tuples.

Sets can be created from any sequence using the `set` function, similar to `vec`.

## Transients

Transient collections are mutable and, as a result, any reference to an old version of a transient collection may no longer be valid. Only vectors, unsorted maps, and sets can be transients. Transient collections are usually used within functions, such as those in the core, for purposes of efficiency, and are typically publicly exposed as immutable, persistent collections.

Transients have their own collection manipulation functions that end in a `!` to signify that they are destructive. Using any of these functions on a transient renders that reference to the transient invalid. Instead, the _result_ of these calls should be used going forward, just as with the persistent equivalents.

* `conj!`
* `assoc!`
* `dissoc!`
* `disj!`
* `pop!`

The `transient` function can turn any persistent collection into a transient one, and the `persistent!` function can turn a transient back into a persistent. Converting a transient into a persistent makes the transient unusable.

Consider this possible implementation of `into`:

``` clojure
(defn into
  [coll source]
  (persistent! (reduce conj! (transient coll) source)))
```

Transients have a concurrency safeguard to ensure that only the thread that created it can use or modify it. Transients don't have value semantics due to being mutable.

# Metadata

Metadata is expressed as a map and can be attached to data structures, sequences, records, symbols, or reference types. There is reader syntax for attaching metadata to a value literal. Metadata that contains only keyword keys and whose value is `true` can be provided in shorter form, and can stack onto the next metadata values.

``` clojure
(def a ^{:created (System/currentTimeMillis)}
  [1 2 3])

(meta a)
;= {:created 134134141234}

(meta ^:private ^:dynamic [1 2 3])
;= {:dynamic true, :private true}
```

The `with-meta` function yields an object of the same type and object as a given object, but with the provided metadata. The `vary-meta` function does the same thing but instead takes a function that modifies the original object's metadata to use its result as its metadata.

``` clojure
(def a ^{:key 1} [1 2 3])
a
;= [1 2 3]

(meta a)
;= {:key 1}

(def b (with-meta a {:other 4}))
b
;= [1 2 3]

(meta b)
;= {:other 4}

(def c (vary-meta b assoc :newkey 2))
c
;= [1 2 3]

(meta c)
;= {:other 4, :newkey 2}
```

# Zippers

Zippers are a stack of all nodes traversed, thereby serving as a cursor into a data structure [^xmonad]. The `clojure.zip`{.path} namespace provides a `zipper` factory and specialized zipper instances `seq-zip` for nested sequences, `vector-zip` for nested vectors, and `xml-zip` for `clojure.xml`{.path}.

Zippers can be moved with operations `up` and `down`, `left` and `right` to move along siblings, `prev` and `next` which are depth-first traversals, and `leftmost` and `rightmost` to move to the first or last sibling.

[^xmonad]: Zippers are used in [Xmonad](http://en.wikipedia.org/wiki/Xmonad) to track window placement and focus.

The `node`, `branch?`, `children`, `lefts`, `rights`, and `root` function can be used to respectively yield the current node, whether it's a branch, the child nodes, all left or right siblings of the current nodes. When operating on trees, the `root` function is used to yield an updated tree which reflects all of the modifications made by the zipper since its creation.

Zippers can manipulate the structure they're representing with the `remove` function to remove the current node, `replace` it with another node, `insert` a child node at the front, or `append` it to the back. The `edit` function takes a function and extra arguments which replaces the current node with the result of applying the current node and the extra arguments to that function. The `make-node` function can also be used to create a new node, but it won't be added unless one of the above functions are used.

``` clojure
(require '[clojure.zip :as z])

(def v [[1 2 [3 4]] [5 6]])

(-> v z/vector-zip z/down z/right (z/replace 56) z/node)
;= 56

(-> v z/vector-zip z/down z/right (z/replace 56) z/root)
;= [[1 2 [3 4]] 56]

(-> v z/vector-zip z/down z/right z/remove z/node)
;= 4

(-> v z/vector-zip z/down z/right z/remove z/root)
;= [[1 2 [3 4]]]

(-> v z/vector-zip z/down z/right (z/edit * 42) z/root)
;= [[1 84 [3 4]] [5 6]]
```

Custom zippers can be created with the `zipper` function which takes three functions:

1. predicate that determines if a node can have children
2. function that yields a sequence of a given branch's children
3. function that returns a new branch node given an existing node and a sequence of children

Consider creating a zipper for HTML elements represented as vectors and maps.

``` clojure
(def el [:body [:h1 "Clojure"]
               [:p "Is very flexible"]])

(defn html-zip [root]
  (z/zipper
    ; predicate for if node can have children
    vector?
    ; yield sequence of branch's children
    (fn [[tagname & xs]]
      (if (map? (first xs)) (next xs) xs))
    ; return new branch node given existing node
    ; and sequence of children
    (fn [[tagname & xs] children]
      (into (if (map? (first xs)) [tagname (first xs)] [tagname])
        children))
    root))
```

A helper function can be created that operates on a zipper, such as one to wrap a DOM node in another tag.


``` clojure
(defn wrap
  [loc tag]
  (z/edit loc #(vector tag %)))

(-> el html-zip z/down z/right z/down (wrap :b) z/root)
;= [:body [:h1 "Clojure"] [:p [:b "Is very flexible"]]]
```

# Concurrency and Parallelism

It's possible to use delays, futures, and promises to control when and how computations are performed.

## Delays

The `delay` function suspends some body of code such that it's evaluation is delayed until it's explicitly _dereferenced_ with `deref`. There exists convenient reader syntax for dereferencing as `@`, which is in fact almost always preferred, except for when `deref` is used as a higher-order function or when wanting to set a timeout. Many Clojure types are dereferenceable including delays, futures, promises, atoms, refs, agents, and vars.

``` clojure
(def d (delay (println "Running...")
              :done!))

(deref d) ; or @d
; Running...
;= :done!
```

The difference between delays and regular functions is that delays evaluate their body only once and from then on cache the return value so that subsequent dereferences are instant. The relation to this and concurrency is that multiple threads can safely attempt to dereference a delay and all of them will block until the delay is evaluated.

The `realized?` function can check if a delay has been materialized yet. This function can also be used with futures, promises, and lazy sequences.

## Futures

A future evaluates a body of code in another thread. A future returns immediately, allowing the current thread to continue execution. The future can be dereferenced to access the return value of the future, which will naturally block if the separate thread isn't finished computing. An upper-bound can be placed on the amount of time that is spent blocking by providing a timeout and a timeout value.

``` clojure
(def long-calculation (future (apply + (range 1e8))))
@long-calculation
;= 4999999950000000

@(future (Thread/sleep 5000) :done!)
;= <blocked for 5 seconds>
;= :done!

(deref (future (Thread/sleep 5000) :done!)
               1000
               :impatient!)
;= :impatient!
```

## Promises

Promises are like delays and futures, except that they are not created with any code that will eventually define their value. Instead, they're initially an empty container which can be filled at a later point in time using the `deliver` function. Like delays and futures, dereferencing a promise will block until there's a value to provide; timeouts can be specified [^haskell_mvar].

[^haskell_mvar]: This reminds me of Haskell's [`MVar`](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-MVar.html)

``` clojure
(def p (promise))
(realized? p)
;= false

(deliver p 42)
(realized? p)
;= true

@p
;= 42
```

## Watches

Watches are functions that are called whenever the state of a reference is changed, though not necessarily to a different value. For that reason, it's common for watch functions to check that the new value is different before proceeding. Watches take four arguments: a key, the reference, its old state, and its new state. The watch function can be registered on a reference with the `add-watch` function which takes the reference, the key to associate with the watch, and the function. The key associated with the watch can also be used to remove the watch later on.


``` clojure
(defn echo-watch
  [key identity old new]
  (println key old "=>" new))

(def sarah (atom {:name "Sarah" :age 25}))
(add-watch sarah :echo echo-watch)

(swap! sarah update-in [:age] inc)
; :echo {:name Sarah, age 25} => {:name Sarah, :age 26}

(remove-watch sarah :echo)
```

It's important to note that the watch functions are called synchronously on the same thread that caused the state change, such that by the time the watch function is called, the value could have been changed again by another thread. For this reason it's important to rely only on the `old` and `new` values passed to the watch function instead of dereferencing the reference.

## Validators

Validators are essentially constraints on reference state. It consists of a single argument function which takes the proposed new state, and if the result is logically false or throws an exception, the change is aborted with an exception.

A validator can be attached at the time of creation by passing a key-value pair to `atom`, `ref`, or `agent` consisting of the key `:validator` and the function to use. Alternatively, it's possible to add a validator to a var, or change a validator associated with an atom, ref, or agent using the `set-validator!` function.

``` clojure
;= constrains state to positive values
(def n (atom 1 :validator pos?))
(swap! n + 500)
;= 501

(swap! n - 1000)
;= #<IllegalStateException ...>

(def sarah (atom {:name "Sarah" :age 25}))
(set-validator! sarah :age)
```

The default thrown exception can be made more descriptive by assuring that the validator we use throws its own exception with a more descriptive string.

``` clojure
(set validator! sarah
     #(or (:age %)
          (throw (IllegalStateException. "People must have `:age`s!"))))

(swap! sarah dissoc :age)

;= #<IllegalStateException ... People must have `:age`s!>
```

## Parallelization Strategies

The `pmap` function can be used to perform a `map` in parallel, evenly spread across available computational cores. However, as usual, the parallelization overhead must be taken into account with respect to the work being done, so that the overhead doesn't outweigh the computation itself. In the event that it does, however, a common workaround is to chunk the dataset so that each unit of work is larger.

Two other constructs are built on top of `pmap`: the `pcalls` function evaluates a variable number of zero-arity functions, yielding a lazy sequence of their return values, while the `pvalues` function does the same but for a variable number of expressions.

## Reference Types

References are essentially boxes that hold a value which can be changed by certain functions, depending on the reference type. The value "within" can be accessed using the `deref` function, which will _never_ block for reference types. The four reference types are `var`, `ref`, `agent`, and `atom`. Each of these reference types are intended for use in different concurrent operations.

<table>
  <thead>
    <tr>
      <td style="background-color: #D0E9FF"></td>
      <th>coordinated</th>
      <th>uncoordinated</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>synchronous</th>
      <td style="text-align: center">refs</td>
      <td style="text-align: center">atoms</td>
    </tr>
    <tr>
      <th>asynchronous</th>
      <td style="text-align: center">N/A</td>
      <td style="text-align: center">agents</td>
    </tr>
  </tbody>
</table>

Reference types can be decorated with metadata, although the metadata can only be changed using the `after-meta!` function which modifies it in-place. The metadata for `atom`, `ref`, and `agent` references can be specified using an optional `:meta` keyword argument. Reference types can notify specified functions when their state changes, where the functions are known as _watches_. Changes to the state that reference types hold can be validated with _validator_ functions, potentially aborting non-conforming change operations.

### Atoms

Atoms are the most basic reference type which implement synchronous, uncoordinated, atomic compare-and-set modifications. Operations that modify the state of atoms block until the modification is complete, i.e. the modifications are atomic.

The `atom` function is used to create an atom, and the `swap!` function is the most common modification operation used on them, which replaces the value of the atom with the result of applying some function and additional arguments to its present value, returning the new value.

``` clojure
(def sarah (atom {:name "Sarah" :age 25 :wears-glasses? false}))

; this sets the atom's value to
; (update-in @sarah [:age] + 3)
(swap! sarah update-in [:age] + 3)
;= {:age 28, :wears-glasses? false, :name "Sarah"}
```

Since atoms use compare-and-set semantics, if the value of the atom changes (i.e. by another thread) before the new value is finished computing, it will abort the update and re-attempt based off of the new value, doing so until the original value remains the same when the new value is computed.

Since atoms are synchronous reference types, functions that change their values don't return until they have completed.

``` clojure
(def x (atom 2000))
(swap! x #(Thread/sleep %)) ;= blocks for 2 seconds
;= nil
```

A more direct function `compare-and-set!` can be used if we already know the value that the atom currently holds. The function returns a boolean indicating whether the value was indeed changed, which happens only if the value provided is indeed what's currently in the atom.

``` clojure
(def a (atom 0))
(compare-and-set! a 10 20)
;= false

(compare-and-set! a 0 20)
;= true

@a
10
```

Note that this function doesn't use value semantics when comparing the provided value to the value inside the atom. Instead, the value must be `identical?`.

``` clojure
(def xs (atom #{1 2}))
(compare-and-set! xs #{1 2} "new value")
;= false
```

We can also completely overwrite the contained value using the `reset!` function, regardless of what its current value may be.

``` clojure
(reset! xs :y)
;= :y

@xs
;= :y
```

### Refs

Refs are the coordinate reference type which ensure that multiple identities can be used with concurrent operations with:

* no possibility of being in an observable inconsistent state
* no possibility of race conditions or deadlocks
* no manual use of locks, monitors, or other synchronization primitives

See the section on [Software Transactional Memory](#software-transactional-memory) for more information.

### Vars {#reference-types-vars}

Vars are generally used implicitly in Clojure. Top-level functions and values are stored in vars, which are defined in the current namespace using the special form `def` or its derivatives, which also copies the metadata provided. [Recall](#symbols) that vars can be created with reader syntax `#'`, but they can also be explicitly created using the `var` function.

> Words are pale shadows of forgotten names. As names have power, words have power. Words can light fires in the minds of men. Words can wring tears from the hardest hearts. There are seven words that will break a strong man's will. But a word is nothing but a painting of a fire. A name is the fire itself.
>
> <cite>**The Name of the Wind** by Patrick Rothfuss</cite>

Evaluating a symbol involves looking for a var with the name of the symbol in the current namespace and dereferencing it to obtain its value. The above passage reminds me of this relationship, where words are to names as symbols are to vars.

``` clojure
map
;= #<core$map ...>

#'map
;= #'clojure.core/map

@#'map
;= #<core$map ...>
```

#### Private vars

Private vars can only be referred to using the fully qualified name when in another namespace and their value can only be manually dereferenced. They're created by providing the `:private` metadata key as true. The `defn-` form creates private functions.

``` clojure
; in user namespace
(def ^:private everything 42)

; switch to other namespace
(ns other-namespace)

(refer 'user)
;= nil

everything
;= #<CompilerException ...>

@#'user/everything
;= 42
```

#### Docstrings

Docstrings are string literals that serve as documentation for top-level vars, immediately following the symbol name. The `def` form actually takes the docstring as an optional argument and sets the `:doc` metadata key to its value.

``` clojure
(def a
  "A sample value."
  5)

(def b
  "Some function"
  [c]
  (+ a c))

(doc a)
; -------
; user/a
;   A sample value.
```

#### Constants

Constant values can be declared with the `:const` metadata keyword. Constants aren't resolved at runtime, instead the value held by the var is retained permanently at compile time [^static]. Later redefinitions of the var have no effect.

[^static]: Reminds me of static variables in C/C++.

``` clojure
(def ^:const max-value 255)
(defn valid-value?
  [v]
    (<= v max-value))

(def max-value 500)
(valid-value? 299)
;= false
```

#### Dynamic Scope

Although for the most part Clojure is lexically scoped, vars have a feature known as dynamic scope. Vars have a root binding, which is the value bound to the var when it's defined with `def` or some derivative.

However, if the var is defined to be dynamic, with the `:dynamic` metadata keyword, then the root binding can be overridden and shadowed on a per-thread basis with the `binding` form. The naming convention for dynamic vars is to surround the names with asterisks `*`.

``` clojure
(def ^:dynamic *max-value* 255)
(defn valid-value?
  [v]
  (<= v *max-value*))

(binding [*max-value* 500]
  (valid-value? 299))
;= true
```

Dynamic bindings can stack by nesting `binding` calls, each inner scope shadowing the outer one. As a result, `get-*var*` will evaluate to `:c` and not the outer scope bindings.

``` clojure
(def ^:dynamic *var* :root)
(defn get-*var* [] *var*)

(binding [*var* :a]
  (binding [*var* :b]
    (binding [*var* :c]
      (get-*var*))))
;= c
```

Dynamic scope is idiomatically used to specify optional return values. This is possible using the `thread-bound?` function to check if the current thread has established a thread-local binding on the var. The `set!` function can then be used to change the value of the current thread-local binding.

``` clojure
(def ^:dynamic *ret-val* nil)

(defn some-func
  (when (thread-bound? #'*ret-val*)
    (set! *ret-val* 42)))

(some-func)
*ret-val*
;= nil

(binding [*ret-val* nil]
  (some-func)
  (println "ret-val: " *ret-val*))
;= ret-val: 42
```

Binding conveyance is the propagation of dynamic var bindings across threads, and it's possible with agents, futures, and `pmap` and its derivatives. In the following code, `future` propagates the dynamic scope across to the other thread for the duration of its operation.

``` clojure
(binding [*max-value* 500]
  (println (valid-value? 299))
  @(future (valid-value? 299)))
; true
;= true
```

However, lazy sequences in general do not support binding conveyance. To mitigate this, it's necessary to push the dynamic scope required for each step in the lazy sequence into the code that's values are realized.

``` clojure
(binding [*max-value* 500]
  (map valid-value? [299]))
;= (false)

(map #(binding [*max-value* 500]
        (valid-value? %))
     [299])
;= (true)
```

Although generally discouraged, the root binding of vars can be mutated as a function of its current value using the `alter-var-root` function. Other functions like `with-redefs` can change the root binding within their scope, which can be useful for testing, such as mocking out functions or values.

``` clojure
(def x 0)
(alter-var-root #'x inc)
;= 1
```

Forward declarations are possible by not providing a value when defining a var. Although the `def` form can be used for this purpose, the `declare` macro is more idiomatic since it makes the intention more explicit, and allows a variable number of unbound vars to be declared.

``` clojure
(declare a-fn b-fn)

(defn public-api-function []
  (b-fn (a-fn)))

(defn- a-fn []
  ...)

(defn- b-fn []
  ...)
```

### Agents

Changes to an agent's state are independent of changes to other agents' states, and all changes are made away from the thread of execution that scheduled them. I/O and other side-effecting functions may be safely used with agents. Agents are STM-aware, such that they can safely be used in the context of retrying transactions.

The modification operations for agents are `send` and `send-off`. The function and optional arguments passed to these functions are called an agent action.  Each agent maintains a queue of actions. Invoking a modification function returns immediately after queueing the specified action, and each action is evaluated serially, in order, on one of many threads dedicated to the evaluation of agent actions.

The `send` function sends actions to be evaluated within a fixed-size thread pool, and due to being fixed in size, it's important to not give it actions that may perform I/O or other blocking operations, as it would prevent other non-blocking, CPU-bound actions from fully utilizing the resource.

``` clojure
(def a (agent 500))
(send a range 1000)
@a
;= (500 501 ... 999)
```

On the other hand, actions queued with `send-off` are evaluated in an unbounded thread pool (same one used by futures), allowing any number of potentially blocking actions to be evaluated concurrently.

It's possible to block on the completion of all actions from a given set of actors sent from the current thread using `await`. It's important to realize that by the time an agent is dereferenced, it could've been modified by another action. The `await-for` function allows one to specify a timeout.

``` clojure
(def a (agent 5000))
(def b (agent 10000))

(send-off a #(Thread/sleep %))
(send-off b #(Thread/sleep %))

@a
;= 5000

(await a b)
;= nil

; a has been set to nil because
; it's the return value of Thread/sleep
@a
;= nil
```

Because actions are evaluated in a separate thread, exceptions thrown by them can't be dealt with by the same thread that dispatched the action. As a result, an error causes the agent to fail silently.

``` clojure
(def a (agent nil))
(send a (fn [_] (throw (Exception. "something is wrong"))))
;= #<Agent: nil>

a
;= #<Agent FAILED: nil>

@a
;= nil

(send a identity)
;= #<Exception ... something is wrong>
```

The agent can be salvaged by using `restart-agent` to reset the agent's state to the provided value, enabling it to receive actions again.

``` clojure
(restart-agent a 42)
;= 42

(send a inc)
;= #<Agent: 43>

; queue three actions which throw exceptions
(reduce send a
             (for [x (range 3)]
               (fn [_] (throw (Exception. (str "error #" x))))))

(agent-error a)
;= #<Exception ... error #0>
```

The `restart-agent` function also accepts `:clear-actions` which would cause it to clear up any pending actions, otherwise they immediately begin evaluating once again.

``` clojure
(restart-agent a 42)
;= 42

; didn't pass :clear-actions
; so next action is valuated
(agent-error a)
;= #<Exception ... error #1>

(restart-agent a 42 :clear-actions true)
;= 42

(agent-error a)
;= nil
```

The default behavior for agents is for them to require intervention when an action fails. An alternative is to have the agent ignore a failed action and continue with the evaluation of the next action in the queue. This can be specified at creation time with the `agent` function and the `:error-mode` option with a value of `:continue`, where the default is `:fail`.

``` clojure
(def a (agent nil :error-mode :continue))
(send a (fn [_] (throw (Exception. "something is wrong"))))
(send a identity)
;= #<Agent: nil>
```

It's also possible to specify an error handler which takes as argument the agent in question and the error using the `:error-handler` option with the `agent` steam.

``` clojure
(def a (agent nil
              :error-mode :continue
              :error-handler (fn [the-agent exception]
                               (.println System/out (.getMessage exception)))))

(send a (fn [_] (throw (Exception. "something is wrong"))))
;= #<Agent: nil>
; something is wrong

(send a identity)
;= #<Agent: nil>
```

The error mode and handler can be changed dynamically with the `set-error-mode!` and `set-error-handler!` functions.

Agent actions queued within [Software Transactional Memory](#software-transactional-memory) transactions aren't actually added to the queue until the transaction is actually committed. This allows agents to be used within transactions regardless of the number of times the transaction is retried. The same thing applies to actions queued within the scope of other agent actions.

If a validator or something else causes the transaction to fail, then the held off queued actions will be discarded.

The `*agent*` var is one that's usually unbound except for within the scope of an evaluating action, where it's bound to the current agent, the owner of that action.

## Software Transactional Memory

Software Transactional Memory (STM) transactions ensure that changes to refs are made atomically, consistently (transaction fails if changes don't satisfy ref constraints), and in isolation (transaction changes don't affect states from within transactions in other threads).

*[STM]: Software Transactional Memory

The `dosync` function establishes the scope of a transaction. Nested transaction scopes are joined into a single logical transaction. All modifications of refs must occur within a transaction. Transactions are synchronous, blocking the current thread until the transaction is complete.

The `alter` function can be used to modify the value of a ref, much like `swap!`. When `alter` returns, the in-transaction value of the ref is changed to what it was set to, allowing further modifications within the same transaction to work off of the previous modifications.

``` clojure
(def names (ref []))

(defn add-name
  [name]
  (dosync
    (alter names conj name)))

(add-name "zack")
(add-name "shelley")

(println @names)
;= ["zack" "shelley]
```

When the transaction is about to be committed (i.e. completed), the value of the ref outside of the transaction _must_ be the same as it was prior to the first in-transaction use of `alter` on that ref. If this is not the case, then the transaction is restarted from the beginning with the new value of the involved refs.

There are instances in which the order in which a function is applied to a ref is not important, however. In these situations, the behavior in which transactions are retried would waste a lot of time and unnecessary work trying to keep the values up to date. The `commute` function can be used for when the order of application doesn't matter.

``` clojure

; in transaction, (commute ref ...) sets ref to
(apply fun in-transaction-value-of-ref args)

; when committed sets ref to
(apply fun most-recently-committed-value-of-ref args)
```

A good example of this is incrementing a counter. Using `alter` in this situation would lead to many retried transactions, even though what ultimately matters is the end result: the final count.

``` clojure
(def counter (ref 0))

(defn alter-inc! [counter]
  (dosync (Thread/sleep 100) (alter counter inc)))

(defn commute-inc! [counter]
  (dosync (Thread/sleep 100) (commute counter inc)))

(defn bombard-counter! [n f counter]
  (apply pcalls (repeat n #(f counter))))

(dosync ref-set counter 0)
(time (doall (bombard-counter! 20 alter-inc! counter)))
"Elapsed time: 2007.049224 msecs"
(3 1 2 4 7 10 5 8 6 9 13 14 15 12 11 16 17 20 18 19)

(dosync ref-set counter 0)
(time (doall (bombard-counter! 20 commute-inc! counter)))
"Elapsed time: 401.748181 msecs"
(1 2 3 4 5 9 10 6 7 8 11 15 13 12 14 16 19 17 18 20)
```

It's important to realize that the in-transaction value of the ref will be the one altered by the function application passed to `commute`, but the actual value of the ref once the transaction is committed will be based off of the latest value of that ref. For this reason, care needs to be taken to ensure that the transaction doesn't rely on the value of the ref.

The `ref-state` function can set the in-transaction value of the ref to the provided value, usually used to set ref states to initial values. Like `alter`, it will cause a retry of the transaction if the ref's state changes prior to commit-time. It's essentially `alter` with a function that returns a constant value.

``` clojure
; equivalent
(dosync (ref-set bilbo {:name "Bilbo"}))
(dosync (alter bilbo (constantly {:name "Bilbo"})))
```

Validators can be used in transactions, since they throw exceptions when the validation fails, and any exception thrown within a transaction causes that transaction to fail.

The `io!` function makes explicit side-effecting code. If it's evaluated within a transaction, it throws an exception, aborting the transaction. This is useful because the retry-behavior of transactions makes it possible that side-effecting code is run multiple times, which is often not the intention. Operations on atoms should also be considered side-effecting, as they don't participate in STM semantics.

``` clojure
(defn unsafe []
  (io! (println "writing to database")))

(dosync (unsafe))
;= #<IllegalStateException I/O in transaction>
```

It's also very important to ensure that values held in refs are immutable, or unintended effects may occur.

### Live Locks

A live lock is the equivalent of a deadlock in STM, which occurs when a transaction never gets the chance to commit due to ref contention. One fallback that Clojure uses is called _barging_, where an older transaction is allowed to proceed in certain circumstances, forcing newer transactions to retry. If barging fails to push through the older transaction within a certain time frame, Clojure's STM will cause the transaction to fail, yielding a stack trace via a thrown exception. There is also a limit to the amount of times a transaction can be retried, which if exceeded, causes the transaction to fail.

``` clojure
(def x (ref 0))
(dosync
  @(future (dosync (ref-set x 0))))
  (ref-set x 1)
;= <RuntimeException ... Transaction Failed after reaching retry limit>

@x
;= 0
```

### Reader Retry

In general, dereferencing ref types is guaranteed to never block, but inside of a transaction, dereferencing ref types may trigger a transaction retry. This may seem confusing, since a single read would always be a consistent snapshot of a ref.

However, consider the scenario where two refs are being dereferenced for the purpose of adding them and returning their sum [^ref_history]. If during the course of this transaction, one of the ref's state is modified, the computation would be _inconsistent_ because the two refs are from different points in time. This is why it's logical to retry the entire transaction.

[^ref_history]: [what is the role of ref state history?](http://stackoverflow.com/questions/21966319/deref-inside-a-transaction-may-trigger-a-retry-what-is-the-role-of-ref-state-h)

This situation can be mitigated by maintaining a history of the ref states. If this were done, then the transaction could simply look for the state of the ref from the point in time that the transaction began.

With this scheme, each ref has a minimum and maximum history size, which can be specified as `:min-history` and `:max-history` key-value pairs to `ref`. With the default minimum of 0, a transaction retry would increase the history size by one so that two states would be tracked throughout the duration of the transaction, and again increment it on the next retry, and so on.

In the worst case, if the max history size isn't large enough to track the target state of the ref throughout the duration of the transaction, then the transaction will continually retry until it can complete or until it reaches the retry limit, in which case it fails.

Consider the following code where transactions continuously increment a ref every 20 milliseconds for a total of 500 times within a future, followed by a transaction on the same thread that tries to read the ref after one second.

``` clojure
(defn stm-experiment
  [min-hist max-hist]
    (let [a (ref 0 :min-history min-hist :max-history max-hist)]
      (future (dotimes [_ 500] (dosync (Thread/sleep 20) (alter a inc))))
      (dosync (Thread/sleep 1000) @a)))
```

In the default case, the minimum history size is 0 and the maximum size is 10. This maximum history size is never enough to fully track the value that `a` was at at the time the transaction was started, since by the time that it wants to read `a`, it would have changed 50 times. As a result, it has to keep retrying, effectively waiting until the writer transactions finish, by which time it'll then re-read the value of `a` as 500, which is the end result.

``` clojure
(stm-experiment 0 10)
;= 500
```

If the minimum history size is set to 50 from the beginning, then Clojure won't have to wait to incrementally grow the history by one on each retry. As a result, the transaction will be able to keep track of the state of the ref as it was at the beginning of the transaction, which was 0. That state will be at the very end of the history list.

``` clojure
(stm-experiment 50 100)
;= 0
```

If we set the maximum size just under the required size to 48, then it'll retry twice at which time the size (50) will be sufficient to keep track of the state. By this time, twice 50 increments will have occurred, so the value at the beginning of the third transaction run will be around 100.

``` clojure
(stm-experiment 48 100)
;= ~100
```

If we cap the maximum size under what is required, then it is as in the first case, where the history list will not be able to grow to the required capacity, and as a result the transaction will continue to retry until it can successfully complete, which should be once all writer transactions are complete.

``` clojure
(stm-experiment 48 48)
;= 500
```

### Write Skew

Write skew refers to the situation in which a transaction doesn't modify a ref, but the consistency of the transaction's changes depend on the state of the ref that was read, and this state changes mid-transaction, causing the transaction's effects on other refs to end up being inconsistent with the read ref.

The `ensure` function can be used to mitigate this, as it dereferences a ref such that the read will conflict with any modifications prompted by other transactions until the transaction is complete, causing them to retry as necessary.

The `ensure` function is equivalent to dummy writes, which end up requiring that the read value persists until the transaction is committed, with the advantage that `ensure` generally minimizes the number of transaction retries involving read-only refs.

``` clojure
; all semantically equivalent
(ensure a)
(alter a identity)
(ref-set a @a)
```

## Channels

The [`core.async`{.path}](https://clojure.github.io/core.async/) library provides an implementation of channels similar to Go's. `go` is a macro that examines its body for any channel operations and turns it into a state machine, immediately returning a channel on which it eventually places the value of its body's last expression, if non-nil, and closes it. Upon a blocking operation, the state machine is parked and the actual thread of control is released. The body is resumed on a thread-pool thread (or the sole JavaScript VM thread) when the blocking operation completes,

The primary channel operations within go blocks are `>!` for putting and `<!` for taking. There are also analogous operations for ordinary threads which block the thread on which they're called until complete `->!!` for putting and `<!!` for taking. These can be used on threads created with, for example, `future`, but there's also a macro called `thread` that's similar to `go` which launches a first-class thread and returns a channel, and should be preferred over `future` for channel work.

The `alts!` function can be used in `go` blocks to wait for any one of a set of operations to complete, much like `select`. Timeouts are channels that close after a period of time, and can be created with the `timeout` function and included in the set given to an `alt` variant to place an upper bound on the amount of time spent waiting for an operation to complete.

Given the persistent, immutable nature of data structures in Clojure, it's safe and efficient to place them in channels.

Broadcast channels can be created by creating a _mult_ channel from the source channel with the `mult` function. Other channels can _tap into_ the mult channel with the `tap` function, which copies the mult channel into an existing channel (i.e. mutates the channel). Data is sent through the source channel as usual. To stop listening into the mult channel, the `untap` channel is used with the mult channel and the end channel as arguments.

# Macros

Code in Clojure is represented as data structures, as was described in [homoiconicity](#homoiconicity). These structures are then evaluated depending on the data type's rules, such as most literals evaluating to themselves (integers, strings, keywords, etc.), symbols evaluating to the value in the var in some namespace, and lists to calls of functions, special forms, or macros.

Whereas function calls are provided their arguments already evaluated, macros are called by the compiler _at compile-time_ with their arguments _unevaluated_ and must return some Clojure data structure that itself _can_ be evaluated. This data structure itself could contain other macro calls, which are recursively expanded until there are no macro calls. This is known as _macroexpansion_.

Macros are expanded at compile time so the compiler can catch compile-time errors.

## Debugging {#debugging-macros}

It's easy to refer to vars within macros that aren't yet defined, and this would produce an error when the macro is actually used. This is because macros are expanded at compile time, at which time Clojure can't know if the symbol will refer to a var that has a defined value at runtime.

## Macroexpansion

The `macroexpand-1` function can be used to debug macros as it allows one to see what code the compiler would expand the macro to. The 1 at the end of the function name refers to the fact that it only expands the _top-level_ macro once.

Using the `macroexpand-1` function can expand to yet another top-level macro, for this the `macroexpand` function exists, since it expands the top-level macro until it's no longer a macro.

Neither of the above functions expand nested macros. For this, the `clojure.walk/macroexpand-all` function can be used.

## Syntax

It's common to want to return lists to represent further calls to functions, special forms, or macros. The `list` function can be used with each symbol quoted to avoid its evaluation, but that's too cumbersome.

The first useful syntax is the backquote `` ` ``, also known as syntax-quote. The backquote is similar to the regular `quote` function or syntax `'`, except that it fully qualifies unqualified symbols with the current namespace. The syntax-quote also handles qualification of the corresponding namespace in the event of namespace-aliasing. This qualification behavior is critical to ensure that the macro doesn't expand to code that inadvertently refers to or redefines already-named values, a safety practice known as macro hygiene.

Another advantage of syntax-quoting is that specific elements of the list can be unquoted, causing them to be evaluated within the scope of the syntax-quoted form. This can be done with the tilde `~` symbol prefix.

``` clojure
; both are equivalent
(list `map `println [foo])
`(map println ~[foo])
```

The unquote-splicing operator `~@` can be used to unpack the contents of one list into another.

``` clojure
; equivalent
(let [defs '((def x 123)
             (def y 456))]
  (concat (list 'do) defs))
;= (do (def x 123) (def y 456))

(let [defs '((def x 123)
             (def y 456))]
  `(do ~@defs))
;= (do (def x 123) (def y 456))
```

Using the unquote-splicing operator is idiomatic for macros that accept code bodies, such as the following.

``` clojure
(defmacro foo
  [& body]
  `(do-something ~@body))
```

The `gensym` function returns a symbol that is guaranteed to be unique, and thus won't collide with the environment surrounding the macro. It can take a string to prefix the symbol name with. Since creating gensym macros is very common, there is shorthand syntax for them with the pound sign `#` suffix, which will automatically expand into a gensym, each time into the same one _as long as_ it's within the same syntax-quoted form. This is known as an auto-gensym.

``` clojure
(gensym)
;= G__2386

(gensym "sym")
;= sym2391

(defmacro hygienic [& body]
  (let [sym (gensym)]
    `(let [~sym :macro-value]
      ~@body)))

; or
(defmacro hygienic [& body]
  `(let [x# :macro-value]
    ~@body))

(let [x 1]
  (hygienic (println "x:" x)))
; x: 1
```

It's important to emphasize that the auto-gensym only evaluates to the same symbol as long as it's within the same syntax-quoted form. If auto-gensyms need to span across syntax-quoted forms, then an explicit gensym is required.

``` clojure
`(x# x#)
;= (x__asdf x__asdf)

[`x# `x#]
;= [x__asdf x__wxyz]

(let [x (gensym)]
  [x x])
;= [G__asdf G__asdf]
```

A macro that deliberately exposes a name to the caller of the macro is _anaphoric_. Anaphoric macros are generally discouraged as it requires the user of the macro to remember the exposed names within their scope. Instead, it's idiomatic to let users choose the names to use.

``` clojure
(defmacro with
  [name & body]
  `(let [~name 5]
    ~@body))

(with bar (+ 10 bar))
;= 15
```

Double expansion is a common problem where a macro argument appears more than once in the expansion, causing multiple evaluations which can lead to unexpected results if they contain side-effects or decreased performance if the evaluation is computationally expensive.

The idiomatic solution to this problem is to introduce a local binding to which the evaluation of the expression is centralized and by which it is referred to as henceforth.

``` clojure
(defmacro spy [x]
  `(do
    (println "spied" '~x ~x)
    ~x))

(spy (rand-int 10))
; spied (rand-int 10) 9
;= 7

(defmacro spy [x]
  `(let [x# ~x]
    (println "spied" '~x x#)
    x#))

(spy (rand-int 10))
; spied (rand-int 10) 9
;= 9
```

The `defmacro` macro introduces the two implicit local bindings `&env` and `&form`. The `&env` binding contains a map where the keys are the names of all of the current locals, though the values are unspecified. Since the keys don't have a namespace, their values can't be accessed outside of the macro. However, within the macro, the keys and their values can be accessed using typical expansion syntax.

``` clojure
(defmacro spy-env []
  (let [ks (keys &env)]
    ; zip the symbols with what they evaluate to
    `(prn (zipmap '~ks [~@ks]))))

(let [x 1 y 2] (spy-env))
; {x 1, y 2}

; equivalent to
(let [x 1 y 2] (prn (zipmap '(x y) [x y])))
```

Since macros are _currently_ implemented as functions that take two extra arguments at the front, `&form` and `&env`, it's possible to access the macro's var and dereference it to use its implementing function directly. This is possible with the `#'` syntax to refer to a var specifically (instead of the value the symbol represents) and `@` to then dereference that var.

``` clojure
; equivalent
(@#'spy-env nil {'x 2})
((deref (var simplify)) nil {'x 2})
```

The `&form` binding holds the whole form that is being macro-expanded, which consists of a list containing the name of the macro as a symbol and the arguments to the macro. As a result, `&form` contains all of the metadata specified by the user or added by the reader.

One instance where having access to this metadata could be useful is to produce more accurate errors, with "correct" line numbers, specifically the line number of the macro's _usage_ and not its location within the macro's expanded code.

``` clojure
(defmacro func
  [& xs]
  (if (condition)
    (throw (IllegalArgumentException.
            (format "some error on line %s"
              ; get :line from &form's metadata
              ; which contains the line number
              ; on which the user used the macro
              (-> &form meta :line))))
    1))
```

It's important to note that by default, defining a macro does not preserve the its arguments' metadata. To preserve it, the arguments should be re-bound by creating a symbol with `gensym` and using the metadata of `&form` and setting it to the gensym with `with-meta`, then binding the argument to that gensym.

``` clojure
(defn preserve-metadata
  [&form expr]
  (let [res (with-meta (gensym "res") (meta &form))]
    `(let [~res ~expr]
      ~res)))
```

# Protocols

Protocols are the corollary to interfaces in other languages [^objectivec_protocols] and they consist of a variable number of methods each with a variable number of arities, where each has an arity of at least 1 which corresponds to `this` in Java, which is implicit and so can be given any name. By convention, protocol names are written in camel-case to make them easy to distinguish from other Clojure entities.

[^objectivec_protocols]: This seems similar to Objective-C protocols.

Note that the "rest" arguments destructuring syntax can't be used with Clojure protocols since they generate JVM interfaces which can't support all of the argument structure variations of Clojure functions.

``` clojure
(defprotocol ProtocolName
  "documentation"
  (a-method [this arg1 arg2] "method docstring")
  (another-method [x] [x arg] "docstring"))
```

Protocols can then be extended to existing types using `extend-protocol` with the name of the protocol as the first argument and the type that will be extending it as the second. A variable number of symbols (type names) and corresponding lists (method implementations for the previous symbol) can be provided to `extend-protocol`.

It's _not_ necessary to implement every method in a protocol. Instead, calling an unimplemented method will simply throw an exception.

``` clojure
(extend-protocol SomeProtocol
  Type1
  (method1-from-SomeProtocol [this x])
  Type2
  (method1-from-SomeProtocol [this x]))
```

The converse to `extend-protocol` is `extend-type` which can be used to extend several protocols to one type.

``` clojure
(extend-type SomeType
  Protocol1
  (method-from-protocol1 [this x] (impl))
  Protocol2
  (method-from-protocol2 [this y] (impl)))
```

It's possible extend a protocol to `nil`, potentially doing away with many null pointer exceptions if a sane default exists for an operation.

# Custom Types

Clojure types are Java classes that can be defined with `defrecord` and `deftype`. Records are meant for application-level data, while types are meant for low-level infrastructure types such as for new data structures. The differences between the two functions lie in the defaults that records provide for interoperating with the rest of Clojure and Java, and the capabilities of types to optimize low-level operations.

Creating an instance of a type is done by putting the type name in the function position with a period `.` as suffix. Conversely, fields can be accessed with the field name in the function position with a period `.` prefix and the instance as the second argument.

``` clojure
(defrecord Point [x y])

; or
(deftype Point [x y])

(.x (Point. 3 4))
;= 3
```

It's important to realize that types are not defined in namespaces, so they're not implicitly imported when using the namespace from another one, and so they must be explicitly qualified when importing.

``` clojure
(defrecord Point [x y])

(ns user2)
(refer 'user)
Point
;= CompilerException ...

(import 'user.Point)
Point
;= user.Point
```

It's also possible to provide type metadata for primitive and non-primitive fields, otherwise they are assumed to be `java.lang.Object`{.path}.

``` clojure
(defrecord NamedPoint [^String name ^long x ^long y])
```

The enumeration of defined fields for a class defined by `deftype` or `defrecord` is known as its _basis_ and is available via a static method `/getBasis`, and each symbol in this basis retains the metadata provided in its definition.

``` clojure
(NamedPoint/getBasis)
;= [name x y]

(map meta (NamedPoint/getBasis))
;= ({:tag String} {:tag long} {:tag long})
```

## Records

Record types are defined by `defrecord` and are a specialization of types defined by `deftype`, specifically, they support:

* value semantics
* associative collection abstraction
* metadata support
* reader support (serialization)
* convenience constructors for instances with metadata and auxiliary fields

Value semantics refers to being immutable and basing equality on the constituent fields, so that two records are equal if their fields have the same values.

Since they implement the associative collection abstraction, fields can be accessed by keywords named after the fields. A consequence of this is that new fields can be added with `assoc`, which is implemented as map associated with the record. However, if a _declared_ field is removed with `dissoc`, the result is a map of the remaining fields and not the original record. Note, however, that unlike maps, records are not functions.

Record literals consist of the pound sign `#` followed by the record name and a map literal containing the field names as keyword keys with their associated values. Thanks to this syntax, records can very easily be serialized and deserialized.

``` clojure
(pr-str (Point. 3 4))
;= "#user.Point{:x 3, :y 4}"

(= (read-string *1)
   (Point. 3 4))
;= true
```

Records also have auxiliary constructors that take a penultimate argument consisting of the metadata to attach to the instance and a final argument consisting of extra fields to create.

``` clojure
(Point. 3 4 {:foo :bar} {:z 5})

; equivalent
(-> (Point. 3 4)
    (with-meta {:foo :bar})
    (assoc :z 5))
```

Both `deftype` and `defrecord` implicitly create a factory function of the form `->MyType` which takes fields values positionally. Records also create a `map->MyType` factory function which takes a single map used for populating the new instance. These are both useful when used as higher-order functions.

``` clojure
(->Point 3 4)
;= #user.Point{:x 3, :y 4}

(map->Point {:x 3, :y 4, :z 5})
;= #user.Point{:x 3, :y 4, :z 5}

(map (partial apply ->Point) [[5 6] [7 8]])
;= (#user.Point{:x 5, :y 6}
;=  #user.Point{:x 7, :y 8})

(map map->Point [{:x y :y 2} {:x 5 :y 6 :z 44}])
;= (#user.Point{:x 1, :y 2}
;=  #user.Point{:x 5, :y 6, :z 44})
```

## Types

The `deftype` form is the lowest-level method of defining types in Clojure, which is why `defrecord` is built on top of it. Types defined by `deftype` allow for mutable fields. By contrast to `defrecord`, _immutable_ fields are accessible _only_ via interop forms, where the field name is prefixed by a period `.`, and _not_ via keyword as in the associative abstraction.

Fields can be specified via metadata to be volatile or unsynchronized, denoted respectively by `^:volatile-mutable` and `^:unsynchronized-mutable`. Volatile refers to `volatile` in Java, where reads and writes of the field are atomic and cannot be reordered by the JIT compiler or CPU. Unsynchronized fields are regular Java mutable fields.

Unlike immutable fields, mutable fields are always private and as such can only be accessed from within method bodies provided inline with the type definition. Mutable fields can be set using the `set!` function.

## Implementing Protocols

Protocols can be extended to existing types using the aforementioned `extend*` family of functions or within the `deftype` or `defrecord` functions, which would be known as an inline implementation.

Inline implementations correspond to Java interface implementations, which means that inline implementation methods have direct access to fields, whereas external implementations must use Java interop or keyword accessors, and method calls will be as fast as Java interface method calls.

However, this has a variety of consequences. For example, clashes can occur by implementing two protocols each containing methods with the same name and signature, which can occur with those interfaces for which `defrecord` automatically provides implementations, or even by implementing protocols whose methods conflict with `java.lang.Object`{.path} methods.

Since inline implementations are baked into the type, those implementations can't be changed at runtime without redefining the entire type, necessitating the re-evaluation of all code that depends on the type. Worse, existing instances will not be updated after this redefinition and re-evaluation.

It's possible to create anonymous types with the `reify` function, which evaluates to an instance of an unnamed type, which is useful for creating objects that satisfy a given protocol on-the-fly, much like anonymous inner classes in Java.

``` clojure
(.listFiles (java.io.File. ".")
  (reify
    java.io.FileFilter
    (accept [this f]
      (.isDirectory f))))
```

Both `extend-type` and `extend-protocol` are based on `extend`, which takes the type to extend as the first argument and then a variable number of protocols and implementation maps, which are maps consisting of method names as keywords to functions implementing those methods.

``` clojure
(defrecord Point [x y])

(extend Point
  Protocol1
  {:method1 (fn [pt arg1 arg2] (impl))
   :method2 (fn [pt arg1 arg2] (impl))})
```

Since the implementation maps are just maps, they can be passed around and expanded upon.

``` clojure
(def some-core-impl
  {:core-method1 (fn [pt] (impl))
   :core-method2 (fn [pt] (impl))})

(extend Point
  Matrix
  (assoc some-core-impl
    :mixed-in-method1 (fn [pt] (impl))
    :mixed-in-method2 (fn [pt] (impl))))
```

Similarly, it's possible to simulate mixin behaviors. Consider a `Measurable` protocol which defines methods for retrieving dimensions of things, and a type `Button` that extends it. Given another implementation map `bordered`, we want to create a `BorderedButton`.

``` clojure
(defprotocol Measurable
  "Protocol for retrieving the dimensions of widgets"
  (width [measurable] "Returns the width")
  (height [measureable] "Returns the height"))

(defrecord Button [text])

(extend-type Button
  Measurable
  (width [btn]
    (* 8 (-> btn :text count)))
  (height [btn] 8))

(def bordered
  {:width #(* 2 (:border-width %))
   :height #(* 2 (:border-height %))})
```

To create a `BorderedButton`, we would need the implementation map of `Button`. Protocols name vars that contain a map with relevant information, including an implementation map. We can then use the `merge-with` function to merge the two implementation maps, combining the same methods by adding their results.

``` clojure
(defrecord BorderedButton [text border-width border-height])

(extend BorderedButton
  Measurable
  (merge-with
    #(+ (apply %1 %&) (apply %2 %&))
    (get-in Measurable [:impls Button])
    bordered))

(let [btn (BorderedButton. "Hello World" 6 4)]
  [(width btn) (height btn)])
;= [100 16]
```

There are a variety of functions that can be used for protocol introspection. The `extenders` function returns the classes that have been extended by the given protocol at the point in time in which the function is called, since classes can extend protocols at run-time. Similarly, the `extends?` function tests if a type extends a given protocol. The `satisfies?` function is like the `instance?` corollary to `extends?`, since it works on instances.

``` clojure
(extenders Measurable)
;= (user.BorderedButton user.Button)

(extends? Measurable Button)
;= true

(satisfies? Measurable (Button. "hello"))
;= true
```

# Multimethods

Protocols provided polymorphic dispatch by way of type-based single dispatch. Multimethods provide multiple dispatch, and even dispatch based on things other than argument type. Multimethods are created using `defmulti`, which defines the multiple dispatch, and implementations are provided with `defmethod`, which defines the methods to which the calls are dispatched.

Multimethods work by passing the arguments passed to the multimethod are passed to a _dispatch function_ defined by `defmulti`, which yields a _dispatch value_ which is used to select the method to invoke for those arguments.

In the example below, the `:tag` is is the dispatch value.

``` clojure
(defmulti fill
  "Fill xml/html node with the provided value"
  (fn [node value] (:tag node)))

(defmethod fill :div
  [node value]
  (assoc node :content [(str value)]))

(defmethod fill :input
  [node value]
  (assoc-in node [:attrs :value] (str value)))

(fill {:tag :div} "hello")
;= {:content ["hello"], :tag :div}

(fill {:tag :input} "hello")
;= {:attrs {:value "hello"}, :tag :input}
```

It's possible to define a base case implementation using the special dispatch value `:default`.

``` clojure
(fill {:span :input} "hello")
;= #<IllegalArgumentException ...>

(defmethod fill :default
  [node value]
  (assoc node :content [(str value)]))

(fill {:span :input} "hello")
;= {:content ["hello"], :span :input}
```

It's also possible to specify what the default dispatch value should be by passing it as an option to `defmulti`.

``` clojure
(defmulti fill
  (fn [node value] (:tag node))
  :default nil)
```

## Hierarchies

Hierarchies express relationships between dispatch values which can multimethods can use to refine the selection of implementation method. Relationships are defined with the `derive` function, which accepts an optional hierarchy reference, and the remaining arguments express that the first derives from the second.

If the first optional argument---the hierarchy reference---is not provided to `derive`, then it is assumed to encode that relationship in the default, global hierarchy. Otherwise, a separate hierarchy can be created with the `make-hierarchy` function.

The relationships are defined to be between named objects (e.g. keywords or symbols) and classes, but classes (and interfaces) can only serve as leaves in a relationship. Since the global hierarchy is global, and thus shared, collisions are avoided by prohibiting non-namespaced keywords or symbols.

``` clojure
; ::keyword expands to :currentnamespace/keyword
; `symbol would expand to 'originnamespaceorcurrent/keyword
(derive ::checkbox ::checkable)
(derive ::radio ::checkable)
;= true
```

The `isa?` function can be used to test a relationship. It also takes an optional first argument for the hierarchy reference, in case it's not the global one.

``` clojure
(isa? ::checkbox ::checkable)
```

Multimethods can use independent hierarchies via a `:hierarchy` option, and then use the relationships in method implementations to refer to categories of dispatch values.

``` clojure
(def fill-hierarchy
  (-> (make-hierarchy)
    (derive :input.radio ::checkable)
    (derive :input.checkbox ::checkable)
    (derive ::checkable :input)))

(defmulti fill
  #'fill-dispatch
  :default nil
  :hierarchy #'fill-hierarchy)

; default case
(defmethod fill nil [node value] ...)

; all ::checkable's match
; - :input.radio
; - :input.checkbox
(defmethod fill ::checkable [node value] ...)
```

The hierarchy can be modified dynamically with `alter-var-root`, which can be used in a pattern where the default method implementation can extend the hierarchy to include new dispatch values which triggered that method.

The `isa?` function treats vectors, specifically pairs, as special cases. As a result, dispatch values can be vectors consisting of a relationship. Since the Java class hierarchy is in every hierarchy, a pattern is to piggyback a Java class as added information. The implementation method below, for example, applies when the dispatch value is a `::checkable` and the value in question is a Clojure set. This is possible because the dispatch function returns a pair of the dispatch value and the value's class, both encoded as a hierarchical relationship.

``` clojure
; equivalent
(isa? [::checkbox ::checkable])
(isa? ::checkbox ::checkable)

(defmethod fill [::checkable clojure.lang.IPersistentSet]
  [node value]
  ...)
```

When a type implements multiple interfaces, multimethods won't know which implementation to pick unless one is defined as being preferred using the `prefer-method` function, which takes as argument the multimethod in question and the two dispatch values, where the first is the one that is preferred over the second.

``` clojure
(defmulti run class)
(defmethod run Runnable [x] (.run x))
(defmethod run java.util.concurrent.Callable [x] (.call x))

(run #(println "hello!"))
;= multiple methods match dispatch value, neither preferred

(prefer-method run java.util.concurrent.Callable Runnable)
(run #(println "hello!"))
;= hello!
```

Multimethods support a variety of introspection methods including `remove-method`, `remove-all-methods`, `prefers`, `methods`, and `get-method`, which are pretty self-explanatory.

The `type` function is similar to the `class` function, except that if a `:type` metadata is present, it returns its value instead of what `class` would return.

# Build Tools

The most popular build tool in the Java world is Maven. Maven's dependency management model consists of _artifacts_, which are files that are the product of the build process. Artifacts are identified by _coordinates_, which are a group of attributes that uniquely identify a particular version of an artifact, in the form `groupId:artifactId:packaging:version`{.path}.

Attribute  Meaning
---------- --------
groupId    organizational/project identifier
artifactId project identifier
packaging  type of artifact (jar)
version    version string

A project defines its own coordinates in a Maven `pom.xml`{.path} or Leiningen `project.cjl`{.path} file. The `pom.xml`{.path} file can be uploaded to a Maven repository in order to make it available to others. The most popular Maven repositories consist of Maven central and, for Clojure, [clojars.org](http://clojars.org).

Dependencies can be expressed by specifying the versions that are required. There are snapshot and release version types, where snapshot versions are tracked "bleeding edge" releases suffixed with `-SNAPSHOT` and release versions are frozen releases that may follow [semantic versioning practices].

[semantic versioning practices]: http://semver.org/

Type     Example
-----    --------
Release  0.2.1-beta5
Snapshot 1.0.0-SNAPSHOT

Version ranges can be used to restrict or relax certain dependencies. Version range format uses [mathematical range format] to express inclusive or exclusive ends, where an omitted end represents "infinity." A version without range delimiters refers to a "soft" version requirement so that the version chosen is deferred to the one required by any other transitive dependency, if any. Conversely, version number within brackets specifies a "hard" version dependency.

[mathematical range format]: http://en.wikipedia.org/wiki/Interval_%28mathematics%29#Excluding_the_endpoints

Although Maven is the most popular build tool in Java, its dependency management and project configuration is overly verbose for what's needed with Clojure. Instead, it's more common to use [Leiningen] when working with Clojure.

[Leiningen]: http://leiningen.org/

A `project.cjl`{.path} file is used for project configuration and dependency management. The `defproject` macro is used takes as first two arguments the projects coordinates, with the second of the two being the version string on its own, so that Maven's coordinate `com.abc:site:1.0.0`{.path} corresponds to `com.abc/site "1.0.0"` in Leiningen. If the first argument is an unnamespaced symbol, then it's assumed that the artifact ID and the group ID are the same.

``` clojure
(defproject leiningen.org "1.0.0"
  :description "Generate static HTML for http://leiningen.org"
  :dependencies [[enlive "1.0.1"]
                 [cheshire "4.0.0"]
                 [org.markdownj/markdownj "0.3.0-1.0.2b4"]]
  :main leiningen.web)
```

Leiningen has a variety of sub-commands to the main `lein` command.

Command   Use
--------  --------
`repl`    start REPL with dependencies in classpath
`test`    run all tests
`jar`     package project into a jar file
`uberjar` like `jar` command but unpacks all dependencies into it as well
`compile` perform ahead-of-time compilation
`pom`     generate Maven-compatible `pom.xml` file; for repo publishing
`deps`    downloads all dependencies if necessary

To perform ahead-of-time (AOT) compilation, an `:aot` slot needs to be added to the `defproject` macro with a value of `:all` to compile all namespaces in the project, or a vector of namespaces specifying which to compile.

*[AOT]: Ahead-of-Time

It may be useful to perform AOT compilation as a sanity check, but ignore the produced class files. While Maven has support for this, Leiningen doesn't and so the best approximation would be to perform a `compile` followed by a `clean`.

I encountered problems when setting `:aot` in the `dev` environment, it seems to particularly conflict with leiningen plugins. For this reason it's probably best to reserve this option for the `uberjar` or other release environment.

# Java Interoperability

The `doto` macro can perform a series of operations on an object, yielding the object at the end.

``` clojure
(doto (ArrayList.)
  (.add 1)
  (.add 2)
  (.add 3))
```

## Exception Handling

Exceptions are the same as Java's. The `try` form defines the scope of the exception-handling. It can contain a variable number of `catch` forms and optionally a single `finally` form. The `finally` form executes regardless of whether or not there were exceptions, and it's only useful for side-effecting actions as it doesn't affect the result of the `try` form.

The `throw` form can be used to throw an exception, which must take an instance of a class that is `Throwable`.

``` clojure
(defn as-int
  [s]
  (try
    (Integer/parseInt s)
    (catch NumberFormatException e
      (.printStackTrace e))
    (finally
      (println "Attempted to parse as integer: " s))))
```

Idiomatic Java uses `finally` [^try_with_resources] to ensure that resources are closed properly in the case of exceptions. Java 7 introduced try-with-statements which can do this more concisely. Similarly, Clojure provides the `with-open` form that does this as well, which can take a variable number of resource bindings.

[^try_with_resources]: Or more recently, the [`try-with-resources`](http://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html) statement.

``` clojure
(require '[clojure.java.io :as io])

(defn append-to
  [f text]
  (with-open [w (io/writer f :append true)]
    (doto w
      (.write text)
      .flush)))
```

## Type Hinting

Type hints are explicit indications of the object type to the Clojure compiler. When this isn't done, Clojure code that uses interop might perform reflection at run-time to determine the method to call, which can impact performance. Setting `*warn-on-reflection*` to `true` can yield reflection warnings which warn when a particular method can't be resolved statically.

Type hints can be added to return values by adding them to the argument vectors.

``` clojure
(defn file-extension
  ^String [^java.io.File f]
  (-> (re-seq #"\.(.+)" (.getName f))
    first
    second))
```

## Arrays

Primitive Java arrays are supported via a variety of functions.

``` clojure
; convert to array
(into-array [1 2])

; new Integer[10][100]
(make-array Integer 10 100)

; access some_array[4]
(aget some-array 4)

; set array some_array[4] = 3
(aset some-array 4 3)
```

There are also helper functions to create primitive arrays, such as `boolean-array`, `byte-array`, and so on, which takes as argument the desired size or a collection from which to source the array.

Sometimes it's necessary to get the class of an array type to, for example, implement a protocol for an array of that type. Classes for arrays are written using JVM notation with left brackets `[` for each array dimension followed by a letter denoting the array type.

Type       Letter
-----     --------
`boolean` Z
`byte`    B
`char`    C
`long`    J
`int`     I
`short`   S
`double`  D
`float`   F

The `Class/forName` function can then be used to yield a class from this notation.

``` clojure
(class (make-array Character/TYPE 0 0 0))
;= [[[C

(Class/forName "[[[C")
;= [[[C
```

There also exist type hints that are specifically for use with arrays.

* `^objects`
* `^booleans`
* `^bytes`
* `^chars`
* `^longs`
* `^ints`
* `^shorts`
* `^doubles`
* `^floats`

Using the `map` and `reduce` functions on arrays would box the resulting values. For this reason, the `amap` and `areduce` functions exist which avoid autoboxing. The `amap` function takes as first argument the name to give the array, followed by the name to give the index, followed by the name to give the result array, and finally the expression whose result will be used as the value at the current index in the result array.

``` clojure
(let [a (int-array (range 10))]
  (amap a i res
    (inc (aget a i))))
```

With multidimensional arrays, the intermediate levels are arrays of objects.

The `aget` and `aset` functions support multidimensional operations by recursively using `apply` to get/set each level of the array. The use of `apply` causes the primitives to get autoboxed, which would have a _significant_ impact on performance. The workaround for this is to bind each array dimension to a different name in order to be able to provide type hints.

``` clojure
(time (dotimes [i 1000]
        (dotimes [j 1000]
          (aset arr i j 1.0)
          (aget arr i j))))
; "Elapsed time: 50802.798 msecs"

(time (dotimes [i 1000]
        (dotimes [j 1000]
          (let [^doubles darr (aget ^objects arr i)]
            (aset darr j 1.0)
            (aget darr j)))))
; "Elapsed time: 21.543 msecs"
```

## Classes

The `proxy` form can be used to yield an instance of an anonymous class, like `reify`, except that it can be used to extend an existing base class or interfaces. It takes a vector containing the base class and/or interfaces, and another vector for the arguments to use for that class' constructor. The rest of the forms are method implementations.

``` clojure
(proxy [java.util.LinkedHashMap] [16 0.75 true]
  (removeEldestEntry [entry]
    (> (count this) max-size)))
```

Alternatively, the `gen-class` is more definitive in terms of defining a Java class, as it can define a class in any package with any name, extending a base class with access to its protected fields, implent interfaces, define a variable number of constructors, static and instance methods, and static main methods.

The `gen-class` form must be AOT compiled, which causes it to emit Java class files. The implementations of the methods specified in `gen-class` are delegated to regular Clojure functions, whose names start witha  prefix which by default is `-`. Method declarations specify their signature as a vector where the first element is the method name, the second element is a vector of the arguments' types, and the last and third element is the return type.

The namespace form `ns` can accept a `:gen-class` form which consists of a map that is forwarded to the `gen-class` form, where the class takes on the name of the namespace.

``` clojure
(gen-class
  :name TestClass
  :main true
  :methods [^:static [printTimes [String int] void]
            ^:static [add [int int] int]])

(defn- -printTimes
  [str times]
  (dotimes [_ times]
    (println str)))

(defn- -add
  [a b]
  (+ a b))

(defn -main
 []
 (println "nothing"))
```

## Annotations

Annotations can be created using regular metadata syntax on class-generation forms, such as `gen-class`.

``` clojure
(gen-class
  :name SomeTest
  ; this metadata is equivalent to Java's
  ; @org.junit.Test
  :methods [^{org.junit.Test true} simpleTest [] void])
```

# REPL

In the REPL, `*n` contains the `n`th recently evaluated expression's result, i.e. `*1` refers to the most recent result, `*e` refers to the last uncaught exception.

The `clojure.repl`{.path} namespace contains various REPL utilities. The `pst` function could be used to print the stack trace of the provided exception, or `*e` if not provided. The `apropos` function shows which functions in the loaded namespaces matches the provided regular expression or string, whereas `find-doc` is similar but it searches within the documentation. The `source` function can output the source code for the provided function. The `doc` function can print out the documentation for a given var, and `dir` can print a list of public vars in the given namespace.

``` clojure
(apropos #"^ref")
;= (ref-max-history refer-clojure ref-set
;=  ref-history-count ref ref-min-history refer)

(dir clojure.string)
; blank?
; capitalize
; escape
; ...
```

# Math

Like Java, Clojure has a mixed numerics model so that it can make use of primitives and boxed numbers, although the only primitives it supports are `long` and `double`. In fact, Clojure automatically widens return values of `float`, `int`, and `short` to their 64-bit or larger equivalents.

``` clojure
(class (inc (Integer. 5)))
;= java.lang.Long
```

When an mathematical operation overflows the bounds of the value type, an exception is thrown instead of silently wrapping around as in other languages. There are `unchecked-*` variants that don't perform this check. Alternatively, a global configuration value `*unchecked-math*` can be set to true so that all mathematical operations are unchecked.

There are additional "prime" autopromoting variants of core mathematical functions which will automatically promote results that would otherwise overflow.

``` clojure
(inc Long/MAX_VALUE)
;= ArithmeticException

(inc' Long/MAX_VALUE)
;= 9223372036854775808N
```

When comparing numbers, `=` returns true if the numbers are the same and within the same category, e.g. both are integers. The `==` function, on the other hand, returns true if the numbers are numerically equivalent, regardless of the categories they're in.

# Testing

The `clojure.test`{.path} namespace contains the official Clojure test framework. Assertions are possible with the `is` macro, which takes an expression it evaluates to determine if it is logically true and an optional message which will be printed if the assertion fails.

``` clojure
(use 'clojure.test)

(is
  (= 5 (+ 4 2))
  "Brush up on addition")
; FAIL in <location>
; Brush up on addition
; expected: (= 5 (+ 4 2))
;   actual: (not (= 5 6))
;= false
```

There are also assertions that can be used within expressions, such as `thrown?` which ensures that a certain type of exception is thrown during evaluation, or `thrown-with-msg?` which takes a regular expression to test the contents of the exception message.

The `testing` macro can be used to nest tests so that each test's description is printed out when certain test fails.

``` clojure
(testing "Strings"
  (testing "regex"
    (is (re-find #"foo" "foobar"))
    (is (re-find #"foo" "bar"))))

; FAIL <location>
; Strings regex
; ...
```

The `deftest` macro can be used to define tests such that they are marked as tests. This is done by defining a zero-argument function and adding a `^:test` metadata slot. This allows test runners to easily find tests.

``` clojure
(deftest test-foo
  (is (= 1 1)))

(:test (meta #'test-foo))
;= #<user$fn__366 ...>
```

The `with-test` macro can be used to define a function and its accompanying tests in a single location.

``` clojure
(with-test
  (defn hello [name]
    (str "Hello, " name))
  (is (= (hello "Brian") "Hello, Brian"))

; can be run explicitly
((:test (meta #'hello)))
; FAIL in <location>
; ...
```

The `run-tests` function can dynamically find tests in the specified namespaces by checking for the `^:test` metadata. If no namespaces are specified, it searches in the current namespace: `*ns*`.

It's possible to define test suites by simply having one test call another. However, this can cause the test runner to run the same tests multiple times. This can be avoided by simply omitting the `^:test` metadata for subordinate tests, i.e. not using `deftest`.

Another way to avoid this is by defining an entry point for tests by creating a function called `test-ns-hook` in the namespace, which, if found by `run-tests`, is the only test function run in that namespace, giving complete control over which tests are run and their order.

Fixtures---which allow set-up and tear-down routines to establish and take down an environment required for a test---are naturally possible via higher-order functions, where the argument to the fixture is the test to run within the context of the environment the fixture establishes. Fixtures are mutually exclusive with the `test-ns-hook` entry point for tests, so that if the entry point is defined, fixtures wont be run.

Fixtures are simply functions that take one argument---the test to run within its environment---and are registered using the `use-fixtures` function. It takes as its first argument the kind of life cycle to use:

* `:once` runs the fixture once and then runs all of the tests within that environment
* `:each` re-runs the fixture separately for each test

``` clojure
(defn some-fixture
  [f]
  (set-up)
  (f)
  (tear-down))

(use-fixtures :once some-fixture)
```

The `are` macro can templatize assertions in order to avoid repetition.

``` clojure
(deftest test-addition
  (are [x y z] (= x (+ y z))
    10  7  3
    20  10 10
    100 89 11))

; expands to
(do
  (is (= 10  (+ 7 3)))
  (is (= 20  (+ 10 10)))
  (is (= 100 (+ 89 11))))
```

The `assert` macro can be used to throw an error if the condition is not met, which is useful in instances where it's preferable to fail as early as possible. Assertions can be disabled globally by setting `*assert*` to `false`, which should be set before compilation of code that uses it.

``` clojure
(assert
  false
  "value should be true")
;= #<AssertionError ...>
;=   value should be true
```

The `fn` special form has support for testing preconditions and postconditions, such that an error is thrown if one isn't met. This is done by specifying a map with `:pre` and/or `:post` keys as the first value in the function body, which gets expanded to calls to `assert` when the function is compiled.

The values to the keys are vectors where each item is a separate assertion. Postcondition assertions can refer to the value to-be-returned as `%`.

``` clojure
(defn even-to-odd
  [num]
  {:pre  [(= (mod num 2) 0)]
   :post [(not= (mod % 2) 0)]}
  (inc num))

(even-to-odd 2)
; AssertionError
```

