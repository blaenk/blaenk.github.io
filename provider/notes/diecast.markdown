---
title: Diecast
published: November 7, 2014
excerpt: Static Site Generator written in Rust
comments: off
toc: right
---

* toc

A `Generator` generates a site, maintaining a notion of input and output directories and bindings used to compile trees in the input directory to the output directory.

A `Binding` is a relation between a `Pattern` (representing a section of the input directory), the `Compiler` with which to build it, and its dependencies if any.

A `Pattern` is used to match against every file in the input directory. Default instances exist for simple strings (exact matches), regular expressions, and globs. `Pattern`s can be conjoined, disjoined, and negated.

An `Item` represents a unit of compilation in Diecast and simply consists of a `Path` to the underlying file and an `AnyMap` representing arbitrary, type-checked metadata.

A `Compiler` is used to process an `Item`. Processing an item may involve performing side-effects based on an `Item`'s metadata or writing and updating metadata.

## Parallelization

One of the goals is to have parallelized compilation. In order to do this I believe it's necessary to determine the order in which `Item`s must be built so that dependency constraints are satisfied. Further, for this ordering to be processable in a parallelized fashion it's necessary to prevent dependents from being built before all of its dependencies are complete.

The ordering required is the topological ordering of a dependency graph. A dependency graph in this context is a directed acyclic graph (DAG) where an edge $a \to b$ means that $b$ depends on $a$, so that $a$ must be processed _before_ $b$. A topological sort provides this ordering, so that all dependencies come before their dependents.

In the dependency graph below, $5$ and $6$ must be built before $4$, that is, $4$ depends on $5$ and $6$. One of the valid topological orderings is [8, 7, 2, 3, 0, 5, 1, 6, 4, 9, 11, 12, 10], which is the topological ordering resulting from a reverse post-order depth-first search.


<img class="center" src="/images/notes/diecast/dependency-graph.png">

This ordering cannot be used directly as a work-stealing queue in a parallelized environment, however. Consider the first two items in the queue, 8 and 7. If one thread dequeues 8, another thread has no way of knowing that it shouldn't dequeue 7 because 7 depends on 8. There has to be a way to hide unfulfilled items so that fulfilled items are not blocked by them, which would lead to something similar to [head-of-line blocking], which would negate parallelization.

[head-of-line blocking]: http://en.wikipedia.org/wiki/Head-of-line_blocking

One solution to this problem is to give every `Item` a reference count corresponding to the number of dependencies it has. When a dependency finishes, every `Item` that depends on it should have its reference count decremented. An intelligent queue would then only serve those `Items` which have a reference count of 0.

A `Job` would contain an `Item`, along with its original topological ordering index and reference count. The original list of `Item`s would be partition into unfulfilled (refcount > 0) and fulfilled (refcount = 0). Every fullfilled `Job` would be pushed onto the priority queue based on its original topological ordering index so that related jobs are kept closer together, i.e. compiled soon after their dependencies. This isn't strictly necessary, and a regular FIFO queue can be used, including a channel.

It turns out that this approach has been described by others [^davis_paper] [^cs_se].

[^davis_paper]: Stanley Tzen, Brandon Lloyd, John D. Owens. [A GPU Task-Parallel Model with Dependency Resolution](http://www.idav.ucdavis.edu/func/return_pdf?pub_id=1091). Page 3, Section 5.
[^cs_se]: [Getting parallel items in dependency resolution](http://cs.stackexchange.com/questions/2524/getting-parallel-items-in-dependency-resolution) on the Computer Science StackExchange

## Ownership

A parallel approach would move each `Job` (and thus `Item`) into its assigned worker thread. This isn't possible because then other `Job`s in other worker threads would have no way of referencing the moved `Item`'s metadata.

It seems that there are two requirements:

1. allow a worker thread to mutate a specific `Item`
2. allow a worker thread to reference a specific `Item`

I think that the [`RWLock`](http://doc.rust-lang.org/std/sync/struct.RWLock.html) is pretty relevant for this purpose.

> A dual-mode reader-writer lock. The data can be accessed mutably or immutably, and immutably-accessing tasks may run concurrently.

One possible design is to move every `Job` from the work-stealing queue to its assigned worker thread. When the worker thread is done compiling the `Item`, it should then move it into a `RWLock`ed collection that can be looked-up using a `Pattern`.

It seems particularly tricky to reconcile the following requirements:

1. a graph which is used to obtain the initial topological order
2. the `Item` needs to be sent to the worker thread for processing
3. once processed it is sent back to the main thread where it needs to:
    * decrement its dependents in the staging queue, which is done by querying the graph
    * or be added back to a staging area when blocked by a `Barrier`

A possible approach to begin with is the design of the general flow of the `Item`s. The main thread would construct a work-stealing queue with the `Item`s in topological order. Each `TaskPool` task clones the stealer and steals a `Job` and proceeds to processing it.

After processing the `Job`, it's sent back through a channel to the main thread, which then determines what to do with it. If the `Job` is complete, the graph is queried for neighbors of the `Item` and decrements each of their reference counts which may mark them as ready and thus move them to the job queue. If the `Job` is incomplete, create a `Barrier`-tracking entry for it in the `Generator` if one doesn't already exist, and if it's the final `Job` to reach the `Barrier` then put all of the `Job`s associated with the bind name back onto the job queue.

It seems that the principal problem is that the `Job` needs to be mutable within its worker task, but the `Job` also needs to be kept somehow in the graph and other structures in the `Generator`. One possible approach is to associate the original index with each `Job`, and have the graph and other structures operate purely based on the index.

Under this approach, the above steps would be done as:

1. a graph is constructed using only the indices to output the topological order
2. the `Job`s are created from the `Item`s based on the topological order indices, storing the original index in a field in the `Job`
3. the `Job` is sent to the worker thread for processing
4. once processed, the `Job` is sent back to the main thread:
    * if the `Job` status is `Paused`, add it back to a staging area indexed by the original index
    * decrement its dependents in the staging queue based on the `Job` index, which is done by querying the graph

## Dependencies

An implicit dependency registration system would be more ergonomic and flexible, but I think it's either infeasible or intractable in a parallel environment [^further_investigation]. It appears to me that it is necessary to explicitly state the dependencies of each binding up front in order to build the dependency graph and by extension the dependency-respecting ordering, which in turn is necessary for parallelization. 

[^further_investigation]: This requires further investigation.

The API can be be made ergonomic despite requiring explicit dependency registration. Every binding could carry a friendly name which could then be referred to for purposes of dependency registration.

``` rust
generator
  .bind("posts",
    "posts/*.md",
    post_compiler,
    None)
  .create("post index",
    "post-index.html",
    post_index_compiler,
    Some(["posts"]));
```

The `Compiler` would then be passed a map of name to `Item`s, to prevent the `Compiler` from manually fetching `Item`s it probably didn't specify as dependencies.

``` rust
fn compile(item: &mut Item, dependencies: Dependencies) {
  let posts = dependencies["posts"].filter_map(|p| p.data.find::<Title>());

  for Title(title) in posts {
    // do something with `title`
  }
}
```

### Ordering Dependencies

How would this dependency system manage the scenario where a post contains links to the previous and next post? There is no way to specify a dependency on the next or previous post.

The simplest, but inflexible solution would be to maintain a notion of metadata (title, route, date, etc.) separate from all other data (post body, etc.). This way, metadata would be processed first and would be ready during regular compilation. The downside to this approach is that the same problem would arise for any other kind of data. However, if no alternatives seem feasible then this might be the only way forward.

Another approach would be to allow separate stages of compilation. Under this approach, "posts" would go through preliminary compilation, which might process the title and route, after which the second stage compilation would run which would allow the `Compiler` to refer to other posts. This would most naturally be facilitated by a `Barrier` `Compiler`, whose semantics would be much like a [memory barrier](http://en.wikipedia.org/wiki/Memory_barrier), forcing all items in the binding to be compiled up until that point before proceeding.

``` rust
let post_compiler =
  CompilerChain::new()
    // this reads the title
    .link(ReadMetadata)

    // force all posts to reach this point before proceeding
    .link(Barrier)

    // now we can refer to the title because we are guaranteed
    // that it has been processed for every other post
    .link(GetNextAndPreviousPosts);
```

Perhaps the `Barrier` should not be free-standing, and should instead be required to wrap the `Compiler` that has the ordering constraint? This would be much like the [`Mutex`](http://doc.rust-lang.org/std/sync/struct.Mutex.html) type in Rust.

``` rust
let post_compiler =
  CompilerChain::new()
    .link(ReadMetadata)
    .link(Barrier(GetNextAndPreviousPosts));
```

This wouldn't really work well if there are multiple `Compiler`s that require the `Barrier`.

``` rust
let post_compiler =
  CompilerChain::new()
    .link(ReadMetadata)
    .link(Barrier(GetNextPost))
    .link(GetPreviousPost);
```

The above code could be reformulated with a `CompilerChain`.

``` rust
let post_compiler =
  CompilerChain::new()
    .link(ReadMetadata)
    .link(
      Barrier(
        CompilerChain::new()
          .link(GetNextPost)
          .link(GetPreviousPost)));
```

However, it would probably be best represented as an inherent method in the `CompilerChain`, which is the only place where a `Barrier` makes sense; all other `Compiler`s are single `Compiler`s.

``` rust
let post_compiler =
  CompilerChain::new()
    .link(ReadMetadata)
    .barrier()
    .link(GetNextPost)
    .link(GetPreviousPost);
```

It's not possible to use an actual [`Barrier`](http://doc.rust-lang.org/std/sync/struct.Barrier.html) because there would be no way to know if what `Jobs` are currently on which tasks, or if they even all fit in the thread pool.

The implementation details of a system that would facilitate a `Barrier` `Compiler` seem complex. The worker must know when a `Barrier` is encountered, pause `Compiler` execution at that point, and put the `Job` back on the job queue. This might imply that a `Job` (or `Compiler`) has internal state about the compilation status, specifically whether it is `Paused` or `Done`, and if `Paused`, where it left off at.

Perhaps this should all only apply to the `CompilerChain`, in which case the chain should be represented as a queue which is dequeued as a `Compiler` is run, making the compilation state implicit: both completion (`Paused` or `Done`) and progress (which `Compiler` to continue on).

It could be that the worker thread invokes the `compile` method on the `Item`, and it could return an enum variant such as `Done` or `Paused`, which the worker would pattern match on to determine whether the `Job` needs to be re-inserted into the job queue.

Aside from pushing the `Job` back onto the job queue to continue compilation, the `Item` at that state needs to be pushed onto the `Item` store so that subsequent `Compiler`s could reference the data that was built up until the point of the barrier. This would represent a frozen state, so I believe it has to be made a copy via `clone`.

``` rust
match job.compile() {
  Paused => {
    store.insert(job.item.clone());
    job_queue.push(job);
  },
  Done => /* decrement dependents, add to store */,
}
```

I'm not sure if the clone could be avoided, perhaps through some kind of copy-on-write semantics, perhaps the new [`Cow`](https://github.com/rust-lang/rfcs/blob/master/text/0235-collections-conventions.md#clone-on-write-cow-pointers) type? [^further_investigation]. It could also be that cloning doesn't introduce too much overhead. At most there would be two copies of an `Item` at any given point: the master copy and the frozen one. This is not unlike the [double buffer] pattern used in much more performance-oriented games.

[double buffer]: http://gameprogrammingpatterns.com/double-buffer.html#not-just-for-graphics

Special care needs to be taken if multiple `Barrier`s are present in the same `Compiler`, as they could lead to race conditions. For example, consider `Item`s A and B using the same `Compiler` C. If C contains two `Barrier`s, then it could be that B's second `Barrier` is encountered before A has even had a chance to encounter the first `Barrier`. In this case, by the time A encounters the first `Barrier`, B's second `Barrier` would have overwritten the frozen state that A's first barrier was protecting.

1. B encounters first barrier, saves current state of B into read-only store
2. B encounters second barrier, saves current state of B into read-only store
3. A encounters first barrier
    1. subsequent `Compiler` for A expects B's data at step #1, but step #2 has overwritten it

This problem would occur if jobs were re-enqueued onto the job queue as soon as a `Barrier` is encountered. To counter this, the `Item`s shouldn't be pushed back onto the job queue until _all_ of `Item`s affected by the `Barrier` have reached the `Barrier`.

This can be accomplished through the use of a counter which is decremented as `Item`s reach the barrier. For this to work, the number of `Item`s represented by a given `Pattern` must be known, so every `Binding` should be augmented with the number of `Item`s that matched either explicitly as a separate field or implicitly if the `Item`s are stored in a collection. The `Generator` should then contain a map of active `Barrier`s mapping from the `Binding` name to a reference count denoting the number of `Item`s yet to reach a barrier.

As soon as a `Barrier` is encountered by an `Item`, the `Generator` should check to see if an entry already exists in the collection of active `Barrier`s:

* If an entry doesn't exist yet, this is the first `Item` to encounter the `Barrier`. Inserted an entry with an initial reference count of $N - 1$ where $N$ is the number of `Item`s associated with that `Binding`.
* If an entry does exist, then it means that this is just another `Item` that has reached the barrier. Decrement the reference count for the entry. If after this decrement the reference count reaches 0, then re-enqueue all of the `Item`s associated with the `Binding` onto the job queue.

In either case, the `Item` should not trigger dependency resolution, as it's not actually complete yet. This should be implicit since the `Compiler` wouldn't have completed by then.

## Caching

One of the goals of the caching implementation should be to keep each `Compiler`'s cache separate in order to prevent different `Compiler`s from clobbering each other's cache space.

Another goal should be to bypass as much work as possible. Given `Compiler` A and B where B depends on metadata inserted by A, if A avoids work because it determines that recompilation isn't necessary, then B should also avoid work since it depends on A.

For this to be possible, it would be necessary to allow subsequent compilers to test the cache of previous compilers, to determine whether or not the work that is depended on has changed.

Granular caching could be achieved by having the `Generator` maintain an `AnyMap` mapping the `Compiler` type to the actual cache, a [`TypeMap`] mapping a pair of type and a descriptor string to the actual cached value.

[`TypeMap`]: https://github.com/reem/rust-typemap

### Approach

The problem with the aforementioned ideas is that other compilers have to care about other compilers' cache. I think the simplest approach here is to ignore caching altogether. Each compiler will instead have the option/choice of caching things if they should want to do so in some unique directory (?) in some standard cache directory.

Perhaps provide some sort of uniform API for hooking into caching functionality. It would cache things in a binary encoded manner? Or would it cache as simple as JSON? JSON would be portable, but binary encoded might be faster. Portability is probably not a huge concern with something as ephemeral as the cache. This API would also not have to worry about what directory to write things in? Though the directory will be available should a particular compiler want it.

Look into [serde](https://github.com/erickt/rust-serde).

There should also be a way to handle in-memory caching? For use with the `live` command. Should probably have a way of pruning cruft when, e.g., a file is removed and its cached data is no longer needed.

## Error Handling

There should be some facility for handling errors in compilers. Presumably each compiler should return a `Result`. Errors should probably consist of `Box<Error>` since I can't think of any particular error that diecast might throw, instead it would be some underlying error, such as an `io::Error`.

Possible success return values would probably be `Continue` and `Pause`.

``` rust
fn some_compiler(_item: &mut Item) {
  // some work
  Ok(Continue)
}
```

But what would it mean for a compiler to choose to pause? Should it be interpreted as a `Barrier`? So that all items being compiled with that compiler are put on a barrier?

``` rust
fn barrier(_item: &mut Item) {
  Ok(Pause)
}
```

The problem with implementing `Compile` for `Compiler`, and also a problem with allowing any compiler to `Pause`, is that the compiler needs to store its position to know where to resume from later.

The first problem is that the `compile` method has an immutable `self`. This can probably be circumvented by storing the position in a `Cell`.

The second problem could perhaps be solved by only giving a `Pause` significance within the context of a `Compiler`. That is, the compiler would run a compiler and if it returns `paused` it would itself return paused.

``` rust
pub fn compile(&self, item: &mut Item) -> Result<Status, Box<Error>> {
  let position = self.position.get();

  for compiler in &self.chain[position ..] {
    self.position.set(self.position.get() + 1);

    match compiler.compile(item) {
      Ok(Continue) => (),
      Ok(Paused) => return Ok(Paused),
      Err(e) => return Err(e),
    }
  }

  Ok(Done)
}
```

What then should be returned when a compile chain is finished? It doesn't make sense to allow `Done` because then any compiler could return `Done` to short-circuit evaluation?

Maybe we can get rid of a `Compiler` and only keep `Chain`.

``` rust
Chain::new()
  .link(inject_with)
  .link(read)
  .link(render_markdown)
  .link(write)
  .build();
```

This would allow us to pass compilers directly to `Rule` constructors:

``` rust
Rule::matching(
  "pages",
  glob::Pattern::new("pages/*.md"),
  |item: &mut Item| -> Result<Status, Box<Error>> {
    // some work
    Ok(Continue)
  });
```

This would change the definition of `Rule` to be parameterized over the compiler.

``` rust
pub struct Rule<C> where C: Compile {
    pub name: &'static str,
    pub kind: Kind,
    pub compiler: C,
    pub dependencies: Vec<&'static str>,
}
```

This would necessitate creating a `Chain` for a sequence of compilers. The `Chain` would handle behavior with respect to keeping track of the position of the sequence and propagating pauses and errors.

One consequence of this is that it would be possible to nest `Chain`s, allowing for the pattern of packaging up common sequences of compilers into chains, such as Ring's [`wrap-defaults`{.path}](https://github.com/ring-clojure/ring-defaults#basic-usage).

``` rust
fn setup() -> Chain {
  Chain::new()
    .link(read)
    .link(parse_metadata)
    .link(parse_toml)
    .build();
}

Rule::matching(
  "pages",
  glob::Pattern::new("pages/*.md"),
  Chain::new()
    .link(setup())
    .link(my_own_stuff)
    .link(here)
    .build());
```

Note that this would be different from a function that contains calls to each compiler in succession, unless the return value of each compiler is properly handled:

``` rust
fn setup(item: &mut Item) -> Result<Status, Box<Error>> {
  // return values are discarded!
  read(item);
  parse_metadata(item);
  parse_toml(item);
}
```

A macro like `try!` should probably be created that early-returns on `Ok(Paused)` and `Ok(Done)`, or should we co-opt `Error` to do this? So that we have some `Error` enum that contains variants for `Paused` and `Done`, and one for `Other` which contains a `Box<Error>`.

Then it should be possible to rewrite the `Compile` implementation for `Chain` as:

``` rust
pub fn compile(&self, item: &mut Item) -> Result<Status, Box<Error>> {
  let position = self.position.get();

  for compiler in &self.chain[position ..] {
    self.position.set(self.position.get() + 1);
    compile!(compiler.compile(item));
  }

  Ok(Done)
}
```

However, the use of such a macro alone would not enable manual, sequential chaining of compilers in a function. If the `parse_metadata` compiler yielded `Ok(Pause)` it would correctly early-return from `setup`, but it would have no way of knowing where to resume.

If `setup` was called within a `Chain`, the chain would correctly resume compilation at the `setup` function, but the `setup` function would have no way of knowing where to resume compilation and so would re-run each of the functions.

``` rust
fn setup(item: &mut Item) -> Result<Status, Box<Error>> {
  // pause position discarded!
  compile!(read(item));
  compile!(parse_metadata(item));
  compile!(parse_toml(item));
}
```

Honestly I don't like allowing compilers to return `Paused` and `Done`. It doesn't make much sense. Instead this should be contained within a `Chain`. In this case the return value should just be `Result<(), Box<Error>>`, since I can't really think of what other values should be possible. It's always awkward to type `Ok(())` though, maybe type alias to `Continue`?

The problem is that `Job` needs to know what the return status of compilation is so it can know whether to re-enqueue the job or if it's done. If we contain those return values to `Chain` only, then it requires a `Chain` to be used no matter what. I guess those are the two options:

1. any compiler can use `Pause` and `Continue` so that `Chain` isn't mandatory
2. `Chain` is mandatory

Should we be wrapping compilers in an `Arc`? If not then `Compile` would need a `Clone` bound. Better yet, why not wrap them in an `Arc` within the evaluator, to then be sent off to the thread pool? This would relax the requirements on individual compilers and would remove the need for the `Compiler` + `Chain` split.

**EDIT**: This makes it difficult for `Chain` to track internal state. This seems to break at the seams of the special casing of `Chain`.

Perhaps the position paused at should be encoded in the `Pause` variant as a stack with which to retrace the steps:

``` rust
pub fn compile(&self, item: &mut Item) -> Result<Status, Box<Error>> {
  // would need some way to resume from position

  for (position, compiler)
  in self.chain[position ..].iter().enumerate() {
    match compiler.compile(item) {
      Ok(Continue) => (),
      Ok(Pause(mut stack)) => {
        stack.push(position);
        return Ok(Pause(stack));
      },
      Err(e) => return Err(e),
    }
  }

  Ok(Continue)
}
```

It makes pausing from a "leaf" position slightly less ergonomic, such as from a `barrier` function:

``` rust
fn barrier(_item: &mut Item) {
  Ok(Pause(Vec::new()))
}
```

Alternatively, a `Pause` variant can be used at leaf positions, and they're converted to `Paused(stack)` variants by `Chain`?

**Updated**: Compilers now return `Result<(), Box<Error>>`. Actual error handling is still not implemented because a thread pool that handles errors needs to be implemented.

## Barrier Reform

Barriers currently require all items in the binding to pass through the barrier. This presents a problem in conditional compilation. The barrier will deadlock because it'll be waiting for all items in the binding to reach the barrier, even though the barrier is only performed for items that satisfied the condition.

``` rust
Chain::new()
  .link(compiler::read)
  .link(
    compiler::only_if(
      publishable,
      Chain::new()
        .link(compiler::print)
        .barrier() // <-- deadlock!
        .link(compiler::write)));
```

The most preferable solution would be one that completely removes the concept of pausing from the `Site` type as it currently presents a special casing/handling of the `Chain` compiler, which is contrary to the impression it gives, that it's something that anyone can implement.

There needs to be a way to narrow the scope, perhaps a way of defining "sub-bindings." One example would be for `only_if` to perform a barrier at its beginning in order to ensure registration of each item that satisfied the condition. Subsequent barriers would then be based on the immediate parent binding.

The problem with this approach is that currently the `Site` is what does the book-keeping for barriers. However, these book-keeping structures only refer to the binding, so theoretically they can be stored some place else? But where? It wouldn't make sense to store them on an `Item` itself, unless it were a reference to the structure that's actually stored some place else (the `Site`?).

**UPDATE**: This is wrong. The book-keeping structure actually stores the `Item` itself, which would need to be revised since we can't move out of a `&mut Item`. Perhaps store the `Job.id` instead?

``` rust
paused: BTreeMap<&'static str, Vec<Job>>,
```

Perhaps a `barriers` structure would be kept in the `Site` and a reference to it would be stored on each `Item`? Or could it be created once and cloned into each `Item`? It would be created by the `Chain` itself and would be stored as a stack, as with `ChainPosition`.

``` rust
struct Chain {
  chain: Vec<Link>,

  // clone this into each `Item` in its compile fn,
  // if the compiler doesn't already contain it
  barriers: Arc<Mutex<BTreeMap<&'static str, Vec<JobId>>>>,
}
```

The `Item` would contain a stack of barriers. Each `Chain` compile fn would push the latest barriers layer and would not pop it until the `Chain` finished?

``` rust
// insert the barriers key if it doesn't already exist
let barriers = item.data.entry::<Barriers>().get()
  .unwrap_or_else(|v| {
    let barriers: Vec<Arc<Mutex<BTreeMap<&'static str, Vec<JobId>>>>> =
      Vec::new(Arc::new(Mutex::new(BTreeMap::new())));
    v.insert(barriers);
  });

// insert one if there isn't one already for this level
let barrier = if barriers.is_empty() {
  let bar = Arc::new(Mutex::new(BTreeMap::new()));
  barriers.push(bar.clone());
  bar
// if there is, get it
} else {
  barriers.pop().unwrap();
};
```

An alternative, easier approach would be to maintain a concept of a "target barrier count." Since by definition only one barrier could be active at any one moment, we will maintain a count of the amount of triggers required. By default this target count would be the total length of the binding, which would also be stored in a `BindCount` anymap entry, to facilitate resetting. When an `only_if` is triggered, it would perform a barrier to count the number of items that satisfy the condition, then after the barrier use that count to update the target barrier count. This way, subsequent barriers would _only_ perform on this subset of items.

_Side note_: Instead of maintaining a blind count, should we maintain a set of `Job` ids? This would facilitate the handling of errors in the rare event that a job slips through that shouldn't have?

One implementation possibility would be to maintain a stack of target barrier counts, so that a nested chain would pop the count off when it has finished, thereby reinstating the previous chain's target barrier count. This would facilitate the nesting of `only_if`, for example.

The `Site` type would then consult the top of this target barrier count (using teh `last` method)? This seems to be complicated because a barrier itself is required to perform this change:

1. chain (could be inside an `only_if`)
2. item adds its jobid to the new target barrier count
3. barrier
    1. as an item reaches this preliminary barrier, it would be checking the previous target barrier count (TBC)
4. swap target barrier counts. much like a double buffer in graphics rendering, swap the target barrier count with the new one if it hasn't been done already (if `!=`)

Some double-buffering like this would be required to avoid corrupting the target barrier count as the barrier itself is being performed to update the target barrier count.

## Resolve From

The dependency graph is able to resolve from a particular node, which fits perfectly with the concept of updating a single binding/item, which saves time when a single file is modified.

However, this feature cannot be leveraged while barriers exist in their current form. Consider a binding `posts` that contains a compiler chain with a barrier and a single `posts` item is modified. It would not be possible to _only_ rebuild the single `posts` item and the dependency chain sourced at the `posts` node in the dependency graph because the presence of the barrier(s) means that every other `posts` item depended on each other at different states. For example, given:

``` rust
Chain::new()
  .link(something)
  .link(other)
  .barrier()
  .link(depends_on_first_barrier)
  // ^--- this compiler may need access to each item
  //      at this exact state
  .link(blah)
  .barrier()
  .link(depends_on_second_barrier);
  // ^--- same here
```

If we would _only_ rebuild the single `posts` item that changed, it would not have access to the first and second barrier states of every other item.

One possible solution to this would be to save each individual barrier state.

An alternative solution which would resolve many problems would be to allow compilers to add items to bindings.

``` rust
Chain::new()
    .link(compiler::read)
    .link(compiler::parse_metadata)
    .link(compiler::render_markdown)
    .link(router::set_extension("html"))
    .link(compiler::render_template("article", article_handler))
    .link(compiler::only_if(publishable, move_to("publishable posts")));
    // can continue to work on item even though we shouldn't
    .link(something_to_moved_item)
```

I think it's easier to just use the current approach right now. However, to enable resolve-from we could have to maintain a tree of item states and then traverse them in-order when we encounter a barrier. This is too tedious and very special-cased for barriers/pausing.

## Unresolved Questions

* Maybe make it possible to set the sorting to use for `Item`s in a `Binding`. Consider the scenario of adding next and previous post links. To get the next and previous posts, the entire posts would need to be sorted in chronological order. If the `Item`s aren't already sorted in chronological order, then this sorting would occur for every single `Item` being compiled in the `Binding`.

    Alternatively, if it were possible to store `Binding`-level data, then a separate `Compiler` directly following the `Barrier` could sort the `Item`s in chronological order once, then construct a map of `Item`s to tuples of previous and next `Item`s. Subsequent `Compiler`s could then refer to this `Binding`-level data. (This doesn't really make sense, since this `Compiler` would run for every `Item`. There would then need to be some way to preprocess a `Binding`, i.e. a `Binding` `Compiler`.)

* Should it be possible to store `Binding`-level data? Would this require an `RWLock` of the `Binding`? It would also have to be passed to every `Compiler`?

    * this seems to be possible with `compiler::inject_with`

* How will drafts be handled?
    * conditional compilation

    ``` rust
    compiler::only_if(publishable, some_compiler)
    ```

* How will tags work?
    1. rule that depends on all items that contribute to the tags
    1. groups all items by tag
    1. injects some sort of tag info structure
    1. rule that builds tag index based on tag info structure (or merge this step into previous compiler?)

## Pagination

This would only require the index page to depend on `posts` and then split up the `posts` into `chunks(n)`, then output a separate file for each page such as `/posts/n`.

Is it a problem that such a rule would create separate files? Does this mean that the `rule::Kind::Create` is unnecessary? Well it's only really necessary to side-step the matching mechanism.

Hakyll is [able to do this](http://jaspervdj.be/hakyll/reference/src/Hakyll-Web-Paginate.html#buildPaginateWith) by querying the matches of the binding being paginated and then splitting that.

``` haskell
buildPaginateWith
    :: MonadMetadata m
    => ([Identifier] -> m [[Identifier]])  -- ^ Group items into pages
    -> Pattern                             -- ^ Select items to paginate
    -> (PageNumber -> Identifier)          -- ^ Identifiers for the pages
    -> m Paginate
buildPaginateWith grouper pattern makeId = do
    -- get the identifiers of the stuff being paginated
    ids      <- getMatches pattern
    -- group them using the provided grouper
    idGroups <- grouper ids
    -- create a set of the identifiers for dependency purposes
    let idsSet = S.fromList ids
    return Paginate
        -- map of page number -> identifiers in that page
        { paginateMap        = M.fromList (zip [1 ..] idGroups)
        , paginateMakeId     = makeId
        , paginateDependency = PatternDependency pattern idsSet
        }

paginateRules :: Paginate -> (PageNumber -> Pattern -> Rules ()) -> Rules ()
paginateRules paginator rules =
    -- create a new rule for each page
    forM_ (M.toList $ paginateMap paginator) $ \(idx, identifiers) ->
        -- this page's rule depends on the entire identifiers
        -- examples of why this is necessary: 
        --   * changed identifier metadata title, need to re-render
        --     the page the identifier is on
        --     this explains dependency on just the group of
        --     identifiers in this page
        --   * add or remove an identifier from the set
        --     this may change the number of pages and which
        --     identifiers end up in which pages
        rulesExtraDependencies [paginateDependency paginator] $
            -- create a new rule for the page
            -- the identifier of this rule is determined
            -- using the passed-in identifier-generating
            -- function
            create [paginateMakeId paginator idx] $
                -- create the rule for this page
                -- with the provided rule generating
                -- function `rules`
                rules idx $ fromList identifiers
```

For this to work, we need to have access to the matches of a previous rule. The matches could not be the `Item`s themselves, however, because.

## Current Blockers

* create rules from matches of other rules. e.g. for pagination
* conditional compilation

    It's very hacky how `only_if` is implemented. It requires multiple barriers and mutexes and atomic ints.

    In my opinion, a compiler should apply to the _complete_ binding, but this probably won't stop people from attempting to run barriers conditionally. This is part of my motivation for getting rid of barriers entirely, but what would be an alternative?

    The motivation for barriers was to give an item access to all other items at a certain point in time. In particular, it was to facilitate the next/previous links on each item. This could probably be restructured as a binding to process each item, then have another binding that depends on the first binding 

## Rust Gripes

* lack of polymorphic mutability
* can't iterate over one field while calling a function that takes a `&mut self`
    * hacks:
        * wrap field in `RefCell`
        * wrap field in `Option`
        * swap field out and then back with `mem::replace`. [playpen](http://is.gd/DDIzY2)

