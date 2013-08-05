---
title: Algorithms Refresher 
published: July 15, 2013
excerpt: Notes on common algorithms
tags: Algorithms, Notes
icon: lightbulb
comments: off
---

What follows are some notes on algorithms I've been reviewing from [Algorithms]() by Robert Sedgewick and Kevin Wayne as well as [The Algorithm Design Manual]() by Steven S. Skiena. I wanted to write some notes on the material so that I could easily look back on it, but mainly so that I could be sure that I understand the material -- since I have to understand it to explain it.

* toc-center

# Dynamic Connectivity

**Answers**: Is a pair of nodes connected?

**Data Structure**: Array, indexed by any given site to the value corresponding to the component its a part of: `id[site] = component`. All sites are initially set to be members of their own component -- i.e. `id[5] = 5`.

**General Flow**: Sites are all partitioned into singleton sets. Successive `union()` operations merge sets together. The `find()` operation determines if a given pair of sites are from the same component.

Terms:

Site
  ~ Element/Node
Component
  ~ Set/Graph
Connected
  ~ Sites are in the same component

## Quick-Find

<div class="right">

Operation    Growth
----------  --------
Find        $O(1)$
Union       $O(n)$

</div>

This algorithm favors a quick `find()` operation by sacrificing the `union()` operation.

Union operates as follows:

1. of the two sites $P$ and $Q$, _arbitrarily_ choose one to merge under the other
2. gets the associated components of $P$ and $Q$
2. goes through the whole array, setting sites which were part of $P$'s component to now be part of $Q$'s
3. decrements the number of components in the disjoint-set

~~~ {lang="java" text="quick-find"}
public int find(int site) { return id[site]; }

public void union(int a, int b) {
  int pID = find(p);
  int qID = find(q);

  if (pID == qID) return;

  for (int i = 0; i < id.length; i++)
    if (id[i] == pID) id[i] = qID;

  count--;
}
~~~

## Quick-Union

<div class="right">

Operation    Growth
----------  --------
Find        $tree\ height$
Union       $tree\ height$

</div>

This algorithm aims to speed up the `union()` operation by avoiding the act of going through the whole array to change the component of every affected site.

This is accomplished by creating a tree-like relationship between sites. With a tree representation, sites are added as direct leaves to the root node of the component to which they were merged.

As a result of this, the `find()` operation needs to walk up the tree from any given site to find the root note which designates the component to which the given site belongs to. The walk is terminated when it encounters a site whose component is itself.

~~~ {lang="java" text="quick-union"}
public int find(int p) {
  while (p != id[p]) p = id[p];
  return p;
}

public void union(int p, int q) {
  int i = find(p);
  int j = find(q);

  if (i == j) return;

  id[i] = j;

  count--;
}
~~~

## Weighted Quick-Union

<div class="right">

Operation         Growth
----------       --------
Find             $\lg(n)$
Union            $\lg(n)$

</div>

The problem with vanilla Quick-Union is that the trees are merged arbitrarily. This can cause bad performance depending on which tree is merged under the other.

Given the arbitrary form in which components are merged in Quick-Union, input of the form 0-1, 0-2, 0-3, ... 0-N can have worst-case effects:

1. 0-1 can connect component 0 under component 1
2. 0-2 can connect component 1 under component 2
3. 0-3 can connect component 2 under component 3

This input eventually creates a linked-list, where the deepest node in the tree incurs the cost of having to traverse the entire list of sites before determining the component to which it belongs.

Weighted Quick-Union fixes this by keeping track of each component's size in a separate array. With this information it then chooses to merge the smaller component under the larger one.

In the example above, by step 2, component 1 is size 2, so component 2, being size 1, is merged under component 1 and not the other way around.

~~~ {lang="java" text="weighted quick-union"}
public void union(int p, int q) {
  int i = find(p);
  int j = find(q);

  if (i == j) return;

  if (sz[i] < sz[j]) { id[i] = j; sz[j] += sz[i]; }
  else               { id[j] = i; sz[i] += sz[j]; }

  count--;
}
~~~

### Path Compression

<div class="right">

Operation         Growth
----------       --------
Union            $1\ (amortized)$

</div>

A further improvement can be done called **path compression** in which every site traversed due to a call to `find()` is directly linked to the component root.

~~~ {lang="java" text="path compression"}
public int find(int p) {
  if (p != id[p])
    id[p] = find(id[p]);

  return id[p];
}
~~~

# Sorting

Many problems can be reduced to sorting.

**Answers**: Ordering a sequence into a specified order.

**Data Structure**: Array or other sequence.

The following algorithms are described with the assumption that the sequence is an array of contiguous memory and constant access time. This is **noteworthy** because it is important to recognize algorithms can have different speeds depending on the underlying data structure.

For example, selection sort backed by a priority queue or balanced binary tree can help to speed up the operation of finding the smallest element in the unsorted region. Instead of being linear, the operation would be $\lg(n)$. Given that this is done at every element in the sequence, of which there are $N$, this means that selection sort backed by such a structure can be improved from $O(n^2)$ to $O(n\lg(n))$ [^sorting_improvements].

Terms:

Stability
  ~ maintaining relative order of equal keys

## Selection Sort

<div class="right">

Case    Growth
-----   --------
Any     $\Theta(n^2)$

</div>

This is a pretty naive algorithm that is mainly useful for didactic purposes.

Algorithm operation:

1. go through entire sequence to find smallest element
2. swap element with the left-most unsorted element
3. repeat until the end of the sequence

This essentially splits the sequence into a left sorted region and a right unsorted region.

~~~ {lang="java" text="selection sort <a href=\"http://www.sorting-algorithms.com/selection-sort\">visualize</a>"}
public void sort(Comparable[] seq) {
  int N = seq.length;

  for (int i = 0; i < N; i++) {
    int min = i;

    for (int j = i + 1; j < N; j++)
      if (seq[j] < seq[min]) min = j;

    swap(seq[i], seq[min]);
  }
}
~~~

## Insertion Sort

<div class="right">

Case   Growth
-----  --------
Best   $\Theta(n)$
Worst  $O(n^2)$

</div>

This is a **stable** algorithm that is still pretty straightforward but somewhat improves upon selection sort if the array is already sorted or if it's nearly sorted.

It operates as follows:

1. go through the entire sequence until an element is found which is **smaller than the previous element**
2. swap the smaller element with the one on the left until the element to its left is no longer larger than itself
3. repeat until the end of the sequence

The benefit of insertion sort is that if the sequence is already sorted then the algorithm operates in linear time. Similarly, if the sequence is nearly sorted, the algorithm will perform better than the worst case.

**Performance Factors**: order of the items

~~~ {lang="java" text="insertion sort <a href=\"http://www.sorting-algorithms.com/insertion-sort\">visualize</a>"}
public void sort(Comparable[] seq) {
  int N = seq.length;

  for (int i = 1; i < N; i++)
    for (int j = i; j > 0 && seq[j] < seq[j - 1]; j--)
      swap(seq[j], seq[j - 1]);
}
~~~

## Shell Sort

<div class="right">

Case   Growth
-----  --------
Worst  $O(n^{3/_2})$

</div>

While insertion sort can be faster than selection sort, one problem with it is that the swap operations are done one at a time. This means that in the worst case, when sorting position 1 of the array, the smallest element could be at the very end of the array, meaning a total of $N - 1$ swaps where $N$ is the length of the array.

Shell sort aims to mitigate this by doing the following:

1. pick a large number $H$ some constant factor less than the length of the sequence
2. consider every $H^{th}$ element in the sequence and apply insertion sort to those elements
3. now consider every $(H + 1)^{th}$ element and do the same
4. repeat incrementing $H$ until the end of the array is reached
5. repeat steps 2 - 4 but with $H$ reduced by some factor until the reduction reaches $1$
6. ultimately do regular insertion sort, i.e. $H = 1$

The value picked for $H$ and the factor which is used to reduce it form what is known as a [gap sequence](http://en.wikipedia.org/wiki/Shellsort#Gap_sequences). The overall worst-case time complexity **depends on the chosen gap sequence**. A commonly chosen gap sequence with a worst-case time complexity of $O(n^{3/_2})$ is:

$$\frac {(3^k - 1)} {2} \text{, not greater than}\ \lceil N/3 \rceil$$

This sequence begins at the largest increment less than $N/3$ and decreases to 1. This means that for a sequence of length $16$ the sequence is $13, 4, 1$.

The effect of shell sort is that it sorts elements that are $H$ elements apart with one swap instead of $H$. The granularity of the sorting operation increases as $H$ itself decreases such that every element is eventually sorted, but with the added benefit that as $H$ decreases, the distance of the longest-distance swap decreases.

~~~ {lang="java" text="shell sort <a href=\"http://www.sorting-algorithms.com/shell-sort\">visualize</a>"}
public void sort(Comparable[] seq) {
  int N = seq.length;
  int h = 1;

  while (h < N/3) h = 3 * h + 1;

  while (h >= 1) {
    for (int i = h; i < N; i++) {
      for (int j = i; j >= h && seq[j] < seq[j - h]; j -= h)
        swap(seq[j], seq[j - h]);
    }

    h = h / 3;
  }
}
~~~

## Merge Sort

<div class="right">

Case   Growth
-----  --------
Worst  $O(n\lg{n})$
Space  $O(n)$

</div>

This is a **stable** algorithm and the first algorithm that is linearithmic in complexity. The general idea is that the sequence is split into many pieces and then they're all merged back together. The sorting occurs during the merging phase. The merging algorithm works such that the resultant merged piece is sorted.

The main drawback is that it has $O(n)$ space complexity because an auxiliary sequence has to be created to facilitate the merging process.

~~~ {lang="java" text="merge algorithm"}
public void merge(Comparable[] seq, int lo, int mid, int hi) {
  int i = lo, j = mid + 1;

  for (int k = lo; k <= hi; k++)
    aux[k] = seq[k];

  for (int k = lo; k <= hi; k++)
    if      (i > mid)         seq[k] = aux[j++];
    else if (j > hi)          seq[k] = aux[i++];
    else if (aux[j] < aux[i]) seq[k] = aux[j++];
    else                      seq[k] = aux[i++];
}
~~~

### Top-Down

This is a recursive approach that works by splitting the array into two pieces until the pieces consist of pairs of elements. On each recurrence, the two pieces that were split for that recurrence are merged back.

~~~ {lang="java" text="merge sort – top-down"}
public void sort(Comparable[] seq) {
  aux = new Comparable[seq.length];
  sort(seq, 0, seq.length - 1);
}

private void sort(Comparable[] seq, int lo, int hi) {
  if (hi <= lo) return;

  int mid = lo + (hi - lo)/2;

  sort(seq, lo, mid);
  sort(seq, mid + 1, hi);

  merge(seq, lo, mid, hi);
}
~~~

#### Improvements {#merge-sort-improvements}

There are a couple of improvements that can be made to top-down merge sort:

* **use insertion sort for small sub-arrays**: create a cut-off, e.g. 15 elements, where the pieces are sorted with insertion sort instead of being broken down further
* **test if sequence is already in order**: skip the merging phase if `seq[mid] <= seq[mid + 1]`

### Bottom-Up

The other approach to merge sort is bottom-up, that is, starting with arrays consisting of one element and merging them together, then merging all of the arrays of size two, and so on until the entire array is merged.

1. increments a counter $SZ$ in the series of powers of two until $SZ < N$
2. merges every sub-array of length $2SZ$

One **advantage** of bottom-up merge sort is that it can be modified to perform on linked-lists **in place**.

~~~ {lang="java" text="merge sort – bottom-up"}
public void sort(Comparable[] seq) {
  int N = seq.length;
  aux = new Comparable[N];

  for (int sz = 1; sz < N; sz = sz + sz)
    for (int lo = 0; lo < N - sz; lo += sz + sz)
      merge(seq, lo, lo + sz - 1, min(lo + sz + sz - 1, N - 1));
}
~~~

## Quick Sort

<div class="right">

Case   Growth
-----  --------
Worst  $O(n\lg{n})$
Space  $O(\lg{n})$

</div>

This is a divide-and-conquer algorithm that works by essentially partitioning the array into two sub-arrays and sorting them independently. It is typically faster than merge sort because it does less data movement.

~~~ {lang="java" text="quick sort <a href=\"http://www.sorting-algorithms.com/quick-sort\">visualize</a>"}
public void sort(Comparable[] seq) {
  shuffle(seq);
  sort(seq, 0, seq.length - 1);
}

private void sort(Comparable[] seq, int lo, int hi) {
  if (hi <= lo) return;

  int j = partition(seq, lo, hi);

  sort(seq, lo, j - 1);
  sort(seq, j + 1, hi);
}
~~~

The partition algorithm is similar to merge in merge sort in that it is what actually does the sorting.

<img class="center" src="/images/algorithms/quicksort/partition.png">

1. choose a partition element separator $v$
2. scan through the array from $i$ to $j$ in both directions
    1. while $i < v$ do `i++`
    2. while $j > v$ do `j--`
    3. swap $i$ and $j$
3. repeat step 2 until the iterators $i$ and $j$ cross
4. swap the partition element $v$ with the final position of the right-side iterator $j$

The sorting algorithm then recurses on the two partitions.

~~~ {lang="java" text="partition algorithm"}
private int partition(Comparable[] seq, int lo, int hi) {
  int i = lo, j = hi + 1;
  Comparable v = a[lo];

  while (true) {
    while (seq[++i] < v       ) if (i == hi) break;
    while (v        < seq[--j]) if (j == lo) break;

    if (i >= j) break;

    swap(seq[i], seq[j]);
  }

  swap(seq[lo], seq[j]);
  return j;
}
~~~

### Improvements {#quick-sort-improvements}

* **use insertion sort for small sub-arrays**: Adding a cutoff size for which to apply insertion sort to small sub-arrays can improve the performance of the algorithm.

    Instead of:

    ~~~ {lang="java"}
    if (hi <= lo) return;
    ~~~

    use:

    ~~~ {lang="java"}
    if (hi <= lo + M) { insertionSort(seq, lo, hi); return; }
    ~~~

    where `M` is the cutoff. Recommended sizes are between 5 and 15.

* **median-of-three partitioning**: Choose a sample of size 3 from the sequence and choose the middle element as the partitioning element.

### Three-way Partitioning

<div class="right">

Case   Growth
-----  --------
Best   $O(n)$
Worst  $O(n\lg{n})$
Space  $O(\lg{n})$

</div>

One problem with quick sort as it is implemented above is that items with keys equal to that of the partition item are nonetheless swapped, unnecessarily. Three-way partitioning aims to resolve this by partitioning into three separate sub-arrays, the middle of which corresponds to those items with keys equal to the partition point. E. W. Dijkstra popularized this as the _Dutch National Flag_ problem.

**Performance Factors**: distribution of the keys

<img class="center" src="/images/algorithms/quicksort/3waypartition.png">

1. perform a 3-way comparison between element $i$ and $v$
    1. $seq[i] < v$: swap $lt$ and $i$ and `lt++` and `i++`
    2. $seq[i] > v$: swap $i$ and $gt$ and `gt--`
    3. $seq[i] = v$: `i++`
2. repeat step 1 until $i$ and $gt$ cross, i.e. while $i \leq gt$
3. recurse on the left and right segments

Quick sort performs a lot better than merge sort in sequences that have duplicate keys. Its time is reduced from linearithmic to linear for sequences with large numbers of duplicate keys.

~~~ {lang="java" text="three-way partitioning"}
private void sort(Comparable[] seq, int lo, int hi) {
  if (hi <= lo) return;

  int lt = lo, i = lo + 1, gt = hi;
  Comparable v = seq[lo];

  while (i <= gt) {
    int cmp = (seq[i] > seq[v]) - (seq[i] < seq[v]);

    if      (cmp < 0) swap(seq[lt++], seq[i++]);
    else if (cmp > 0) swap(seq[i],    seq[gt--]);
    else              i++;
  }

  sort(seq, lo, lt - 1);
  sort(seq, gt + 1, hi);
}
~~~

## Priority Queues

A priority queue is an abstract data type that allows adding elements and retrieving the smallest or largest element. Priority queues are useful for an unbounded sequence for which we want to retrieve the $M$ smallest elements at any given moment.

### Data Structure {#priority-queue-data-structure}

The data structure commonly used to back a priority queue is an array, with the first element `seq[0]` unused, embedding the contents of a **complete binary tree** in level-order that maintains two invariants:

1. the parent of $k$ is $k / 2$
2. the children of $k$ are at $2k$ and $2k + 1$

#### Insertion {#priority-queue-insertion}

<div class="right">

Case   Growth
-----  --------
Worst  $O(\lg{n})$

</div>

To insert into the heap:

1. add element to the end of the array
2. increment heap size
3. swim up the heap to restore heap order

~~~ {lang="java" text="swim"}
private void swim(Comparable[] seq, int k) {
  while (k > 1 && seq[k / 2] < seq[k]) {
    swap(seq[k / 2], seq[k]);
    k = k / 2;
  }
}
~~~

#### Removal {#priority-queue-removal}

<div class="right">

Case   Growth
-----  --------
Worst  $O(\lg{n})$

</div>

To remove the maximum from the heap:

1. take the largest item off of the top
2. put the item from the end of the heap at the top
3. decrement heap size
4. sink down the heap to restore heap order

~~~ {lang="java" text="sink"}
private void sink(Comparable[] seq, int k) {
  while (2 * k <= N) {
    int j = 2 * k;

    if (j < N && seq[j] < seq[j + 1]) j++;
    if (k >= j) break;

    swap(seq[k], seq[j]);
    k = j
  }
}
~~~

### Heap Sort

<div class="right">

Case   Growth
-----  --------
Worst  $O(n\lg{n})$

</div>

Heap sort is a sorting algorithm facilitated by a priority queue which performs well when backed by a binary heap. Heap sort more or less amounts to:

1. feeding the sequence into a priority queue
2. extracting the sequence out of the priority queue

However, there are certain details involved to make it operate faster. Usually these operations are performed in place to avoid using extra space.

First, the sequence has to be put into heap order, which is accomplished by walking up the tree (bottom-up) and sinking every root node with more than one child. The starting point for this is always $N / 2$.

Assuming a maximum-oriented priority queue, the sorting is then accomplished by:

1. remove the maximum, thus decrementing the heap size
2. swap the maximum with the last item in the heap
3. sink the new root
4. repeat 1-3 until the priority queue becomes empty

~~~ {lang="java" text="heap sort"}
public void sort(Comparable[] seq) {
  int N = seq.length;

  for (int k = N / 2; k >= 1; k--)
    sink(seq, k, N);

  while (N > 1) {
    swap(seq[1], seq[N--]);
    sink(seq, 1, N);
  }
}
~~~

## Selection

<div class="right">

Case    Growth
-----   --------
Average $\Theta(n)$

</div>

Selecting the $k$ smallest items in a sequence can be accomplished by using the quick sort algorithm's partition algorithm. This is guaranteed by the invariant held by quick sort's partition algorithm which states that given the partition index $j$, all elements to the left are less than or equal to $j$ and all elements to the right are greater than or equal to $j$, effectively making the sub-sequence up to $j$ consist of the smallest $j$ elements in the sequence.

~~~ {lang="java" text="selection"}
public Comparable select(Comparable[] seq, int k) {
  shuffle(seq);

  int lo = 0, hi = seq.length - 1;

  while (hi > lo) {
    int j = partition(seq, lo, hi);

    if      (j == k) return seq[k];
    else if  (j > k) hi = j - 1;
    else if  (j < k) lo = j + 1;
  }

  return seq[k];
}
~~~

# Searching

**Answers**: Finding a certain element in a collection.

## Binary Search Trees

<div class="right">

Case    Growth
-----   --------
Worst   $O(n)$

</div>

This is the classical data structure consisting of a binary tree where each node has two children. The sub-tree to the left of each node consists of elements smaller than the node and the sub-tree to the right of each node consists of elements greater than the node.

The performance of BSTs greatly depends on the shape of the tree, which is a result of the distribution and order of the elements that are input.

#### Deletion

Most operations such as insertion and lookup are very straightforward. Deletion is somewhat more involved.

To delete node $z$:

1. $z$ **has no children**: transplant it with a child, which is $nil$
2. $z$ **has just one child**: transplant it with the child
3. $z$ **has two children**: find successor $y$ -- which must be in $z$'s right subtree
    1. if $y$ is $z$'s right child then transplant $z$ by $y$, leaving $y$'s right child alone
    2. else transplant $y$ by its own right child, then transplant $z$ by $y$

The transplant operation can be handled by simply associating the parent with the new child and vice versa:

~~~ {lang="c"}
void replace_node(tree *t, node *u, node *v) {
  if (u->p == t->nil)
    t->root = v;
  else if (u == u->p->left)
    u->p->left = v;
  else
    u->p->right = v;

  // ignore this check in red-black trees
  if (v != NULL)
    v->p = u->p;
}
~~~

## 2-3 Search Trees {#two-three-search-trees}

While **2-3 search tree** can be implemented, they're mainly used to help understand the implementation of [Red-Black Trees](#red-black-trees), which have better performance.

A **2-3 tree** is either empty or:

* **2-node**: one key and two links
    * left for keys smaller than the left key
    * right for keys larger than the right key
* **3-node**: two keys and three links
    * left for keys smaller than the left key
    * middle for keys between the node's keys
    * right for keys larger than the right key

### Searching {#two-three-tree-searching}

Searching follows simply from the structure of the tree.

1. **search hit** if the key is in the node
2. if not, recurse into the appropriate link
3. **search miss** if a null link is reached

### Insertion {#two-three-tree-insertion}

Insertion needs to take into consideration the fact that the tree must remain balanced after the operation. The general procedure is that the key is searched for until a node with a null link is reached at the bottom of the tree.

* **single 2-node**
    1. replace the 2-node with a 3-node containing the new key
* **single 3-node**
    1. create two 2-nodes out of each of the two keys
    2. replace the 3-node with a 2-node consisting of the new key
    3. set the 2-node's links to the two new 2-nodes
* **3-node with 2-node parent** -- _same as above with slight variation_
    1. create two 2-nodes out of each of the two keys
    2. move the new key into the parent 2-node to make it a 3-node
    3. set the middle link to the 3-node's left key and right link to the right key
* **3-node with 3-node parent**
    1. propagate the above operation until the root or a 2-node is encountered
    2. if the root is encountered, split it as in the case of a single 3-node

Perfect balance is preserved because tree height increase occurs at the root, and additions at the bottom of the tree are performed in the form of splitting existing nodes such that the height remains the same.

The **problem** with implementing a direct representation of 2-3 trees is that there are many cases to handle and nodes have to be converted between various types. These operations can incur overhead that nullifies or even makes worse the performance of 2-3 trees compared to regular BSTs.

## Red-Black Trees

<div class="right">

Case    Growth
-----   --------
Worst   $O(2 \lg {n})$

</div>
[Red-Black trees](http://en.wikipedia.org/wiki/Red–black_tree) are trees that guarantee near-perfect balance by maintaining 5 invariants:

1. a node is either **red** or **black**
2. root is **black**
3. all leaves -- represented as nil -- are **black**
4. both children of every red node are **black**
5. every path from a given node to any of its descendant leaves contains the same number of **black** nodes

These properties allow red-black trees to be nearly balanced in even the worst case, allowing them more performance than regular BSTs. A very neat implementation is [available here](https://github.com/prasanthmadhavan/Red-Black-Tree/blob/master/rbtree.c).

### Insertion {#red-black-tree-insertion}

The inserted node is attached in the same manner as for BSTs, except that every node is painted **red** on insertion. However, the inserted node has the possibility of violating any one of the 5 invariants, in which case the situation must be remedied. The following code representing the different cases that must be remedied are split into corresponding individual functions for didactic purposes.

There are three main scenarios that may arise from adding a node:

1. first node added creates a **red** root, violating property **2** (root is **black**) 
2. node is added as child of **black** node, operation completes successfully
3. consecutive **red** nodes, violating properties **4** (both children of **red** nodes are **black**) and **5** (equal number of **black** nodes per path)

Note that scenarios 1 and 3 violate the properties of red-black trees.

**First**, the inserted node may be the only node in the tree, making it the root. Since all nodes are inserted as **red**, it should be repainted **black** to satisfy property **2** (root is **black**):

~~~ {lang="c"}
void insert_case1(node *n) {
  if (n->parent == NULL)
    n->color = BLACK;
  else
    insert_case2(n);
}
~~~

**Second**, if the parent of the inserted node is **black**, the insertion is complete because it is not possible for that to have violated any of the properties:

~~~ {lang="c"}
void insert_case2(node *n) {
  if (n->parent->color == BLACK)
    return;
  else
    insert_case3(n);
}
~~~

**Third**, it is possible that the inserted node creates two consecutive **red** nodes, violating property **3** (both children of **red** nodes are **black**). For this, there are three different scenarios:

A.  parent and uncle are both red
B.  direction in which new node and parent lean differ
C.  new node and parent lean in the same direction

**First**, if the parent and its uncle are **red**, flip their colors and make the grandparent **red** instead. This allows the newly added **red** node to satisfy all properties, since its parent is **black**. However, making the grandparent **red** may possibly violate properties **2** (root is **black**) and **4** (both children of **red** nodes are **black**), so recurse the enforcement algorithm on the grandparent starting from case 1:

<img src="/images/algorithms/red-black-trees/insert_1.png" class="center">

~~~ {lang="c"}
void insert_case3a(node *n) {
  node *u = uncle(n), *g;

  if (u != NULL && u->color == RED) {
    n->parent->color = BLACK;
    u->color = BLACK;

    g = grandparent(n);
    g->color = RED;

    insert_case1(g);
  } else
    insert_case4(n);
}
~~~

**Second**, the new node could be added diagonal to a **red** parent node, meaning for example the parent node being **red** and the **left child** of its parent and the new node could be **red** (as always) and the **right child** of its parent.

This is ultimately resolved by two rotations, but the first rotation is made to get the new node leaning in the same direction as its parent. This is accomplished by rotating the new node in the direction of the parent's direction from its parent. In the above example, the new node is its parent's **right child** and the parent is the grandparent's **left child**, so the new node is **rotated left**.

There are still consecutive **red** nodes after this rotation, albeit leaning in the same direction. This makes it simple for case 3c to handle, provided it is applied to the ex-parent, i.e. the now-bottom node, since case 3c operates in a more general sense from the perspective of the grandchild.

<img src="/images/algorithms/red-black-trees/insert_2.png" class="center">

~~~ {lang="c"}
void insert_case3b(node *n) {
  node *g = grandparent(n);

  if (n == n->parent->right && n->parent == g->left) {
    rotate_left(n->parent);
    n = n->left;
  } else if (n == n->parent->left && n->parent == g->right) {
    rotate_right(n->parent);
    n = n->right;
  }

  insert_case5(n);
}
~~~

**Third**, the new node could be added below a **red** parent node and leaning in the same direction. For example, the new node is the **left child** of its parent and its parent is the **left child** of its parent (grandparent of the new node) as well.

This is resolved by rotating the grandparent in the direction **opposite** to the direction in which the consecutive **red** links lean. This has the effect of making the parent be the new root of the subtree previously rooted by the grandparent.

The grandparent was known to be **black**, since the **red** parent could not have been a child of it otherwise. Knowing this, the parent -- now the root -- switches colors with the grandparent, such that the subtree now consists of the **black** root and two **red** children.

<img src="/images/algorithms/red-black-trees/insert_3.png" class="center">

~~~ {lang="c"}
void insert_case3c(node *n) {
  node *g = grandparent(n);

  n->parent->color = BLACK;
  g->color = RED;

  if (n == n->parent->left)
    rotate_right(g);
  else
    rotate_left(g);
}
~~~

### Deletion {#red-black-tree-deletion}

Deletion is handled similar to deletion in BSTs, but is a _lot_ more complicated because the tree has to be re-balanced if removing a node from the tree causes it to become unbalanced.

Every resource I looked at -- books, sites, university slides, etc. -- simply hand-waived the deletion process presumably due to its complexity. The one place that managed to somewhat explain it well was the classic CLRS book, but its implementation consisted of a big, difficult-to-follow while-loop. Instead I decided to go with [wikipedia's](http://en.wikipedia.org/wiki/Red%E2%80%93black_tree#Removal) long and dense explanation of its relatively simple implementation which even the [Linux kernel uses](https://github.com/torvalds/linux/blob/master/lib/rbtree.c).

**First**, if the node to be deleted has two children then it is replaced by its successor. The successor then has to be deleted, and by definition the successor will have at most one non-leaf child -- otherwise it would not be the minimum in that subtree and the left child would have been followed.

~~~ {lang="c"}
void delete(node *m, void *key) {
  if (node == NULL) return;

  if      (*key < m->key) delete(m->left,  key);
  else if (*key > m->key) delete(m->right, key);
  else {
    if (m->left != NULL && m->right != NULL) {
      // replace with successor
      node *c = minimum_node(m->right);
      m->key = c->key;
      delete(c, c->key);
~~~

**Second**, if the node to be deleted has a child, simply replace the successor with its child.

~~~ {lang="c"}
    } else if (m->left != NULL || m->right != NULL) {
      // replace with child, delete child
      delete_one_child(m);
~~~

**Third**, if the node to be deleted has no children, then it is possible to simply delete it.

~~~ {lang="c"}
    } else {
      // no children, just delete
      free(m);
    }
  }
}
~~~

#### Balance {#red-black-tree-deletion-balance}

If the node is replaced with a successor, that successor is essentially removed from its original location, thereby possibly causing tree unbalanced. For this reason, the original successor node is removed using `delete_one_child` which re-balances the tree if necessary.

* node $M$: successor to the node to be deleted
* node $C$: child of $M$, prioritized to be a non-leaf child if possible
* node $N$: child $C$ in its new position
* node $P$: $N$'s parent
* node $S$: $N$'s sibling
* nodes $S_{L}$ and $S_{R}$: $S$'s left and right child respectively

**First**, if $M$ is **red**, then simply replace it with its child $C$ which must be **black** by property 4 (both children of **red** nodes are **black**). Any paths that passed through the deleted node will simply pass through one fewer **red** node, maintaining balance:

~~~ {lang="c"}
void delete_one_child(node *n) {
  node *child = is_leaf(n->right) ? n->left : n->right;

  replace_node(n, child);
~~~

**Second**, if $M$ is **black** and $C$ is **red**, paint $C$ **black** and put it in $M$'s place. This preserves the same amount of **black** nodes along that path:

~~~ {lang="c"}
  if (n->color == BLACK)
    if (child->color == RED)
      child->color = BLACK;
~~~

**Third**, the most complex case is when both $M$ and $C$ are **black**. Replacing one with the other effectively removes one black node along that path, unbalancing the tree. Begin by replacing $M$ with its child $C$, then proceed to the first re-balancing case:

~~~ {lang="c"}
    else
      delete_case1(child);

  free(n);
}
~~~

When both $M$ and $C$ are **black** nodes, four situations [^case_merge] can arise that require re-balancing, unless $C$'s new position $N$ is the new root. If $C$ becomes the root it simply means that a **black** node was removed from all paths, effectively decreasing the black-height of every path by one and the tree therefore requires no re-balancing.

**First**: $N$'s sibling $S$ is **red**. In this case, reverse the colors of $P$ and $S$ and rotate $P$ left. Although all paths still have the same black-height, $N$'s sibling $S$ is now **black** and its parent $P$ is **red**, allowing fall-through to case 4, 5, or 6:

<img src="/images/algorithms/red-black-trees/delete_1.png" class="center">

~~~ {lang="c"}
void delete_case1(node *n) {
  if (n->parent == NULL) return;

  node *s = sibling(n);

  if (s->color == RED) {
    n->parent->color = RED;
    s->color = BLACK;

    if (n == n->parent->left)
      rotate_left(n->parent);
    else
      rotate_right(n->parent);
  }

  delete_case2(n);
}
~~~

**Second**: $P$, $S$, and $S$'s children are all **black**. Repaint $S$ **red** so that all paths passing through $S$ have the same black-height as those that go through $N$.

<img src="/images/algorithms/red-black-trees/delete_2a.png" class="center">

If $P$ is **red**, then the tree is violating property **4** (both children of **red** nodes are **black**), fix it by simply painting $P$ **black**.

<img src="/images/algorithms/red-black-trees/delete_2b.png" class="center">

Otherwise, if $P$ was already **black**, however, then after the painting of $S$ to **red**, $P$ now has effectively lost one level from its black-height, so case 1 should be applied to $P$:

~~~ {lang="c"}
void delete_case2(node *n) {
  node *s = sibling(n);

  if (s->color == BLACK &&
      s->left->color == BLACK &&
      s->right->color == BLACK) {
    s->color = RED;

    if (n->parent->color == RED)
      n->parent->color = BLACK
    else
      delete_case1(n->parent);
  } else
    delete_case3(n);
}
~~~

**Third**: $S$ is **black**, $S_{L}$ is **red**, $S_{R}$ is **black**, $N$ is left child of its $P$. Rotate $S$ right, then exchange colors of $S$ and its new parent. This case just prepares the tree for falling into case 6, since $N$ now has a **black** sibling -- $S_{L}$ -- whose right child is **red**.

<img src="/images/algorithms/red-black-trees/delete_3.png" class="center">

~~~ {lang="c"}
void delete_case3(node *n) {
  node *s = sibling(n);

  if (s->color == BLACK) {
    if (n == n->parent->left &&
        s->right->color == BLACK &&
        s->left->color == RED) {
      s->color = RED;
      s->left->color = BLACK;
      rotate_right(s);
    } else if (/* symmetric to above */) { }
  }

  delete_case4(n);
}
~~~

**Fourth**: $S$ is **black**, $S_{R}$ is **red**, $N$ is left child of its $P$. Rotate $P$ left, exchange colors of $P$ and $S$, and make $S_{R}$ **black**.

This unbalances the tree by increasing black-height of paths through $N$ by one because either $P$ became **black** or it was **black** and $S$ became a **black** grandparent.

<img src="/images/algorithms/red-black-trees/delete_4.png" class="center">

~~~ {lang="c"}
void delete_case4(node *n) {
  node *s = sibling(n);

  s->color = n->parent->color;
  n->parent->color = BLACK;

  if (n == n->parent->left) {
    s->right->color = BLACK;
    rotate_left(n->parent);
  } else {
    s->left->color = BLACK;
    rotate_right(n->parent);
  }
}
~~~

## Left-Leaning Red-Black Trees {#llrb-trees}

<div class="right">

Case    Growth
-----   --------
Worst   $O(2 \lg {n})$

</div>

Red-Black trees are like regular binary trees, except that they encode 3-nodes as two 2-nodes joined with a **red link**, where one of the 2-nodes is the **left child** of the other. The other kind of link, **black links**, act like regular links in a tree which point to children.

* **red links** lean left
* no node has two **red links** connected to it
* **perfect black balance**: every path from the root to a null link has the same number of **black links**. This can be observed if all red links are drawn horizontally

Considering that links are from parents to children, the color of the link is stored in the child, which can be thought of as "following the link to discover its color."

### Rotations {#llrb-tree-rotations}

Insertion algorithms may leave the tree such that it contains right-leaning **red links** or consecutive **red-links**, both of which violate the structural rules of red-black trees. Rotation operations form the basis of the method of fixing these situations.

**Rotating left** is used to fix the case where there is a **right-leaning red-link**. This is fixed by simply replacing the node with its successor and making the new node's left child be the original node attached with a **red-link**. The left child's right child becomes the old node's left child.

~~~ {lang="java" text="rotate left"}
Node rotateLeft(Node h) {
  Node x = h.right;
  h.right = x.left;

  x.color = h.color;
  h.color = RED;

  x.N = h.N;
  h.N = 1 + size(h.left) + size(h.right);

  return x;
}
~~~

**Rotate right** is used to fix the case where there are **consecutive red-links**. This rotation is the analog to the rotate left operation. The node is replaced by its left child, and the new node's right child becomes the original node.

### Insertion {#llrb-tree-insertion}

Nodes are always inserted at the bottom using red links.

* **2-node**
    1. if the new key is smaller than the root, then add normally, producing a left-leaning red-link
    2. otherwise, add the key normally, producing a right-leaning red-link, then rotate the root left
* **3-node**
    A.  new key is **larger** than both keys
        1. add it normally as the right child of the root with a red link
        2. **flip the colors** of the links from the root
    B.  new key is **smaller** than both keys
        1. add it normally as the left child of the left key
        2. this creates two **consecutive red links**
        3. **rotate** the middle node to the **right**
        4. continue at A.2
    C.  new key is **between** both keys
        1. add it normally as the right child of the left key
        2. this creates two **consecutive red links**
        3. **rotate** the middle node **left**
        4. continue at C.3

Flipping the colors of a node entails flipping the links from red to black, but also the color of the parent from black to red.

However, a root's color should always be kept black. When it is changed to red, it should be interpreted as the height of the tree increasing by 1, and then the color of the link should be changed back to black.

~~~ {lang="java" text="flipping colors"}
void flipColors(Node h) {
  h.color = RED;
  h.left.color = BLACK;
  h.right.color = BLACK;
}
~~~

### Deletion {#llrb-tree-deletion}

TODO

## Hash Tables

Hash tables consist of an array coupled with a **hash function** -- such as [MurmurHash](http://en.wikipedia.org/wiki/MurmurHash) or [CityHash](http://en.wikipedia.org/wiki/CityHash) -- and a **collision resolution** scheme, both of which help map the key to an index within the array.

### Hash Functions

Hash functions need to be consistent, efficient, and should uniformly distribute the set of keys.

A popular and simple hashing function is modular hashing of the form:

$$h(k) = k \bmod M$$

where $k$ is the key and $M$ is the array size, usually chosen to be prime. Multiple pieces of data can be combined into one hash by doing:

$$R * H + D \bmod M$$

where $R$ is a prime number such as a 31, $H$ is the hash as constructed so far (initially set to some prime number) and $D$ is the new piece of data.

### Separate Chaining

<div class="right">

Case    Growth
-----   --------
Worst   $O(\lg {n})$

</div>

This collision resolution strategy involves storing a linked-list at every entry in the array. The intent is to choose the size of the array large enough so that the linked-lists are sufficiently short.

Separate chaining consists of a two-step process:

1. hash the key to get the index to retrieve the list
2. sequentially search the list for the key

A property of separate chaining is that the average length of the lists is always $N/M$ in a hash table with $M$ lists and $N$ keys.

### Linear Probing

<div class="right">

Case    Growth
-----   --------
Worst   $O(c \lg {n})$

</div>

Linear probing is a form of open addressing that relies on empty entries in the array for collision resolution. Linear probing simply consists of:

1. hash the key to get the index
2. the element at the index determines three outcomes:
    1. if it's an empty position, insert the element
    2. if the position is not empty and the key is equal, replace the value
    3. if the key is not equal, try the next entry and repeat until it can be inserted

#### Deletion {#hash-table-deletion}

The insert and retrieval operations retrieve the index and perform the same operation until the entry is null. This has the consequence that deleting a node cannot _simply_ entail setting the entry to null, or it would prematurely stop the lookup of other keys.

As a result, after setting the entry to null, every key to the right of the removed key also has to be removed -- i.e. set to null -- and then re-inserted into the hash table using the regular insertion operation.

#### Load Factor {#hash-table-load-factor}

The **load factor** is defined by $\alpha = N/M$ where $\alpha$ is the percentage of table entries that are occupied, which can never be 1 since, if the table becomes full, a search miss would go into an infinite loop. Instead, array resizing is performed to ensure that the load factor is between $\frac {1} {8}$ and $\frac {1} {2}$.

The average number of compares, or _probes_, in a linear-probing hash table of size $M$ and $N = \alpha M$ keys is:

$$
\text {hits: ~} \frac {1} {2} \left( 1 + \frac {1} {1 - \alpha} \right) \\
\text {misses: ~} \frac {1} {2} \left( 1 + \frac {1} {\left( 1 - \alpha \right)^2} \right)
$$

Based on this, when $\alpha$ is about 0.5 there will be 1.5 compares for a search hit and 2.5 compares for a search miss on average. For this reason, $\alpha$ should be kept under 0.5 through the use of array resizing.

### Sparse Vectors

An application of hash tables can be to implement sparse vectors for the purpose of performing matrix-vector multiplications. In certain situations, the row-vector from a matrix can have a very small amount of non-zero elements. If the matrix was stored in a naive array format it would amount to an immense waste of space and computation.

Instead, sparse vectors are vectors backed by hash tables where the keys correspond to the index of a given element and the value corresponds to that element's value.

# Graphs

*[BST]: Binary Search Trees

[^sorting_improvements]: Skiena p. 109, § 4.3
[^case_merge]: The [Wikipedia implementation's](http://en.wikipedia.org/wiki/Red%E2%80%93black_tree#Removal) 6 cases were condensed to 4 as was done in the Linux kernel [Red-Black tree implementation](https://github.com/torvalds/linux/blob/master/lib/rbtree.c). Cases 1 and 2 were merged since case 1 is simply a check to see if the node is the root. Cases 3 and 4 were merged because they handle the same scenario, with case 4 simply being a handler for a special case of 3.
