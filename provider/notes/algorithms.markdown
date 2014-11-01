---
title: Algorithms
published: July 15, 2013
excerpt: Classical and crucial algorithms
comments: off
toc: left
---

What follows are some notes on algorithms I've been reviewing from [Algorithms](http://amzn.com/032157351X) by Robert Sedgewick and Kevin Wayne, [The Algorithm Design Manual](http://amzn.com/1849967202) by Steven S. Skiena, and other sources around the Internet [^mit] [^umd] [^umgd]. I wanted to write some notes on the material so that I could easily look back on it, but mainly so that I could be sure that I understand the material.

* toc

# Sorting

Many problems can be reduced to sorting.

**Answers**: Ordering a sequence into a specified order.

**Data Structure**: Array or other sequence.

The following algorithms are described with the assumption that the sequence is an array of contiguous memory and constant access time. This is _noteworthy_ because it is important to recognize algorithms can have different speeds depending on the underlying data structure.

For example, selection sort backed by a priority queue or balanced binary tree can help to speed up the operation of finding the smallest element in the unsorted region. Instead of being linear, the operation would be $\lg(n)$. Given that this is done at every element in the sequence, of which there are $N$, this means that selection sort backed by such a structure can be improved from $O(n^2)$ to $O(n\lg(n))$ [^sorting_improvements].

A sorting algorithm is known as _stable_ if it maintains the same relative order of equal keys as it was before the sorting operation.

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

This is a _stable_ algorithm that is still pretty straightforward but somewhat improves upon selection sort if the array is already sorted or if it's nearly sorted.

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

The value picked for $H$ and the factor which is used to reduce it form what is known as a [gap sequence](http://en.wikipedia.org/wiki/Shellsort#Gap_sequences). The overall worst-case time complexity _depends on the chosen gap sequence_. A commonly chosen gap sequence with a worst-case time complexity of $O(n^{3/_2})$ is:

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

This is a _stable_ algorithm and the first algorithm that is linearithmic in complexity. The general idea is that the sequence is split into many pieces and then they're all merged back together. The sorting occurs during the merging phase. The merging algorithm works such that the resultant merged piece is sorted.

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

<img class="center" src="/images/notes/algorithms/quicksort/partition.png">

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

<img class="center" src="/images/notes/algorithms/quicksort/3waypartition.png">

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

### Traversal {#binary-search-tree-traversal}

There are three main forms of traversing a BST. The _order_ refers to the order in which the current node $C$ is visited, that is, the time at which $C$ is visited is the only thing that varies, so $L$ is always visited before $R$.

Traversal  Order
---------- -----
pre-order  $C \to L \to R$
in-order   $L \to C \to R$
post-order $L \to R \to C$

### Deletion {#binary-search-tree-deletion}

Most operations such as insertion and lookup are very straightforward. Deletion is somewhat more involved.

To delete node $z$:

1. $z$ **has no children**: transplant it with a child, which is $nil$
2. $z$ **has just one child**: transplant it with the child
3. $z$ **has two children**: find successor $y$, which must be in $z$'s right subtree
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
* **3-node with 2-node parent** (slight variation of above)
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
3. all leaves---represented as nil---are **black**
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

<img src="/images/notes/algorithms/red-black-trees/insert_1.png" class="center">

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

<img src="/images/notes/algorithms/red-black-trees/insert_2.png" class="center">

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

The grandparent was known to be **black**, since the **red** parent could not have been a child of it otherwise. Knowing this, the parent---now the root---switches colors with the grandparent, such that the subtree now consists of the **black** root and two **red** children.

<img src="/images/notes/algorithms/red-black-trees/insert_3.png" class="center">

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

Every resource I looked at---books, sites, university slides---simply hand-waived the deletion process presumably due to its complexity. The one place that managed to somewhat explain it well was the classic CLRS book, but its implementation consisted of a big, difficult-to-follow while-loop. Instead I decided to go with [wikipedia's](http://en.wikipedia.org/wiki/Red%E2%80%93black_tree#Removal) long and dense explanation of its relatively simple implementation which even the [Linux kernel uses](https://github.com/torvalds/linux/blob/master/lib/rbtree.c).

**First**, if the node to be deleted has two children then it is replaced by its successor. The successor then has to be deleted, and by definition the successor will have at most one non-leaf child, otherwise it would not be the minimum in that subtree and the left child would have been followed.

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

**Second**, if the node to be deleted has one child, simply replace the successor with its child.

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

When both $M$ and $C$ are **black** nodes, four situations [^rbtree_case_merge] can arise that require re-balancing, unless $C$'s new position $N$ is the new root. If $C$ becomes the root it simply means that a **black** node was removed from all paths, effectively decreasing the black-height of every path by one and the tree therefore requires no re-balancing.

**First**: $N$'s sibling $S$ is **red**. In this case, reverse the colors of $P$ and $S$ and rotate $P$ left. Although all paths still have the same black-height, $N$'s sibling $S$ is now **black** and its parent $P$ is **red**, allowing fall-through to case 4, 5, or 6:

<img src="/images/notes/algorithms/red-black-trees/delete_1.png" class="center">

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

<img src="/images/notes/algorithms/red-black-trees/delete_2a.png" class="center">

If $P$ is **red**, then the tree is violating property **4** (both children of **red** nodes are **black**), fix it by simply painting $P$ **black**.

<img src="/images/notes/algorithms/red-black-trees/delete_2b.png" class="center">

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

**Third**: $S$ is **black**, $S_{L}$ is **red**, $S_{R}$ is **black**, $N$ is left child of its $P$. Rotate $S$ right, then exchange colors of $S$ and its new parent. This case just prepares the tree for falling into case 6, since $N$ now has a **black** sibling---$S_{L}$---whose right child is **red**.

<img src="/images/notes/algorithms/red-black-trees/delete_3.png" class="center">

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

<img src="/images/notes/algorithms/red-black-trees/delete_4.png" class="center">

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

## Interval Trees

Interval trees are useful for efficiently finding all intervals that overlap with any given interval or point.

To construct the tree, the median of the entire range of all of the set of ranges is found. Those ranges in the set that are intersected by the median are stored in the current node. Ranges that fall completely to the left of the median are stored in the left child node, and vice versa with the right node.

At any given node representing the set of ranges intersected by the median at that node, two sorted lists are maintained: one containing all beginning points and the other containing all end points.

### Intersection Queries

The general operation for queries is to test the set of ranges in a node and then test those in the appropriate child node if the query isn't equal to the median.

Given a _point_ query, the current node is compared with the median. If it's equal, then every range in that node matches and the search is complete. If the query is less than the median, then the list of beginning points is searched for those beginning points that start before the query point, all of which are matches. Then the search continues into the left child.

Given an _interval query_, the set of beginning and end points are searched to see if they fall within the query interval. These ranges are matches, and they have potential for duplicates if the matched interval begins and ends within the query interval. Finally, to match for ranges which possibly contain the query interval, a point is chosen in the query interval, perhaps the begin or end point, and that point is used as a point query as in the aforementioned point query algorithm.

## Hash Tables

Hash tables consist of an array coupled with a _hash function_---such as [MurmurHash](http://en.wikipedia.org/wiki/MurmurHash) or [CityHash](http://en.wikipedia.org/wiki/CityHash)---and a _collision resolution_ scheme, both of which help map the key to an index within the array.

### Hash Functions

Hash functions need to be consistent, efficient, and should uniformly distribute the set of keys.

A popular and simple hashing function is modular hashing of the form:

$$h(k) = k \bmod M$$

where $k$ is the key and $M$ is the array size, used to avoid integer overflow, usually chosen to be prime. Multiple pieces of data can be combined into one hash by doing:

$$(H * R + D) \bmod M$$

where $R$ is a prime number such as a 31, $H$ is the hash as constructed so far (initially set to some prime number) and $D$ is the new piece of data.

For example, given a three properties---day, month, and year---the following hash computation could be used:

``` java
int hash = 0;
int hash = (hash * R + day  ) % M;
int hash = (hash * R + month) % M;
int hash = (hash * R + year ) % M;

// or
int hash = (((((0 * R + day) % M) * R + month) % M) * R + year) % M;
```

Or to hash a given string:

``` java
int hash = 0;

for (int i = 0; i < s.length(); i++)
  hash = (R * hash + s.charAt(i)) % M;
```

A simpler hashing scheme that doesn't account for integer overflow is:

$$R * H$$

So for example, given a day, month, and year:

``` java
int hash = R + day;
int hash = hash * R + month;
int hash = hash * R + year;

int hash = ((R + day) * R + month) * R + year;
```

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

As a result, after setting the entry to null, every key to the right of the removed key also has to be removed, i.e. set to null, and then re-inserted into the hash table using the regular insertion operation.

#### Load Factor {#hash-table-load-factor}

The _load factor_ is defined by $\alpha = N/M$ where $\alpha$ is the percentage of table entries that are occupied, which can never be 1 since, if the table becomes full, a search miss would go into an infinite loop. Instead, array resizing is performed to ensure that the load factor is between $\frac {1} {8}$ and $\frac {1} {2}$.

The average number of compares, or _probes_, in a linear-probing hash table of size $M$ and $N = \alpha M$ keys is:

$$
\text {hits: ~} \frac {1} {2} \left( 1 + \frac {1} {1 - \alpha} \right) \\
\text {misses: ~} \frac {1} {2} \left( 1 + \frac {1} {\left( 1 - \alpha \right)^2} \right)
$$

Based on this, when $\alpha$ is about 0.5 there will be 1.5 compares for a search hit and 2.5 compares for a search miss on average. For this reason, $\alpha$ should be kept under 0.5 through the use of array resizing.

### Sparse Vectors

An application of hash tables can be to implement sparse vectors for the purpose of performing matrix-vector multiplications. In certain situations, the row-vector from a matrix can have a very small amount of non-zero elements. If the matrix was stored in a naive array format it would amount to an immense waste of space and computation.

Instead, sparse vectors are vectors backed by hash tables where the keys correspond to the index of a given element and the value corresponds to that element's value. This solution is used in Google's PageRank algorithm.

# Graphs

A _graph_ is a set of _vertices_ and a collection of _edges_ that each connect a pair of vertices. This definition allows for _self-loops_ (edges that connect a vertex to itself) and _parallel edges_ (multiple edges connecting the same vertex pair).

Graphs with parallel edges are sometimes known as _multigraphs_, whereas graphs with no parallel edges or self-loops are _simple graphs_.

Two vertices connected by an edge are _adjacent_, and the edge is _incident_ to both vertices. A vertex' _degree_ is the number of edges connected to it. A _subgraph_ is a sub-set of edges and associated vertices that still constitutes a graph.

Paths in graphs are sequences of vertices connected by edges. _Simple paths_ have no repeated vertices. A path forms a _cycle_ if it has at least one edge whose first and last vertices are the same, and a _simple cycle_ if the cycle consists of no repeated edges or vertices. The number of edges in a path determines its _length_.

A graph is _connected_ if a path exists from every vertex to every other vertex. A graph that isn't connected consists of _connected components_ which are connected subgraphs of the graph.

_Acyclic graphs_ are graphs with no cycles. A tree is an acyclic connected graph, and a disjoint set of trees is a _forest_.

A graph $G$ with $V$ vertices is a tree if any of the following are satisfied:

* $G$ has $V - 1$ edges and no cycles
* $G$ has $V - 1$ edges and is connected
* $G$ is connected but removing a single edge disconnects it
* $G$ is acyclic but adding any edge creates a cycle
* exactly one simple path connects each pair of vertices in $G$

A _spanning tree_ of a connected graph is a subgraph that contains all of the vertices as a single tree. A _spanning forest_ of a graph is the union of all spanning trees of its connected components.

A graph's _density_ is its proportion of possible paris of vertices that are connected. A _sparse_ graph has relatively few of the possible edges present, compared to a _dense_ one.

A _bipartite graph_ is one whose vertices can be divided into two sets such that all edges connect a vertex in one set with a vertex in the other.

**Answers**:

* is there a way to connect one item to another by following the connections?
* how many other items are connected to a given item?
* what is the shortest chain of connections between two items?

## Undirected Graphs

An _undirected graph_ is one in which the connections don't have an associated direction. There are various data structures that can be used represent graphs:

* **adjacency matrix**: a $V \times V$ boolean array where row $v$ and column $w$ are set to true if vertices $v$ and $w$ are connected with an edge.
* **array of adjacency lists**: a vertex-indexed array of lists of the vertices adjacent to each vertex, similar to hash tables with separate chaining
* **array of edges**: a collection of Edge objects each containing two instance variables for each of the connected vertices

Adjacency lists have the best balance between space and time performance. They have space usage proportional to $V + E$, constant time to add an edge, and time proportional to the degree of $v$ to iterate through adjacent vertices.

### Depth-First Search

Depth-First Search (DFS) is a graph traversal algorithm that visits a vertex, marks that vertex as visited, then visits all unmarked adjacent vertices.

~~~ {lang="cpp" text="depth-first search"}
void dfs(const Graph &G, int v) {
  marked[v] = true;
  count++;

  for (int w : G.adj(v))
    if (!marked[w]) {
      edgeTo[w] = v; // v connects to w, i.e. v-w
      dfs(G, w);
    }
}
~~~

To trace the paths in the graph, an array can be kept of size $V$ indexed by a given vertex whose value is the vertex that connects to it. This array of edges represents a tree rooted at the source vertex.

### Breadth-First Search

Breadth-First Search (BFS) traversal aids in finding the shortest path between two vertices. Its basic operation consists of:

1. enqueue the source vertex
2. dequeue the current vertex
3. mark and enqueue all adjacent vertices
4. repeat 2-3 until the queue is empty

~~~ {lang="cpp" text="breadth-first search"}
void bfs(const Graph &G, int s) {
  queue<int> vertexQueue;
  marked[s] = true;
  vertexQueue.enqueue(s);

  while (!vertexQueue.isEmpty()) {
    int v = vertexQueue.dequeue();

    for (int w : G.adj(v))
      if (!marked[w]) {
        edgeTo[w] = v;
        marked[w] = true;
        vertexQueue.enqueue(w);
      }
  }
}
~~~

### Connected Components

Depth-First Search can also be used to find connected components of a graph. This is accomplished by initiating DFS on every unmarked vertex and each time it is called on a vertex, set the vertex' connected component identifier.

A run of DFS finds, and thus marks, every vertex in a connected component. Upon completing such a run, a counter variable signifying the connected componenet identifier is incremented and then it is called on the next unmarked vertex in the graph, i.e. a vertex not in a connected component found so far.

~~~ {lang="cpp" text="finding connected components"}
void findConnectedComponents(const Graph &G) {
  vector<int> id(G.V());
  vector<bool> marked(G.V());
  int count = 0;

  for (int s = 0; s < G.V(); s++)
    if (!marked[s]) {
      dfs(G, s);
      count++;
    }
}

void dfs(const Graph &G, int v) {
  marked[v] = true;
  id[v] = count; // set connected component identifier

  for (int w : G.adj(v))
    if (!marked[w])
      dfs(G, w);
}
~~~

Compared to [Union-Find](#dynamic-connectivity), the DFS approach is theoretically faster because it provides a constant-time guarantee. However, in practice the difference is negligible and Union-Find tends to be faster because it doesn't have to build a full representation of a graph. Perhaps more importantly, the DFS approach has to preprocess the graph by running DFS on the separate connected components. As a result, Union-Find is an online algorithm where it can be queried even while new edges are added without having to re-preprocess the graph.

### Cycle Detection

DFS can also be used to determine if there are cycles present in a graph. This is accomplished by keeping track of the vertex previous to the one being focused on by the DFS. If one of the current vertex' neighbors is already marked and it is not the previous vertex, then it means that there is an edge to an already marked vertex, thus forming a cycle.

~~~ {lang="cpp" text="cycle detection"}
bool detectCycles(const Graph &G) {
  for (int s = 0; s < G.V(); s++)
    if (!marked[s])
      dfs(G, s, s);
}

bool dfs(const Graph &G, int v, int u) {
  marked[v] = true;

  for (int w : G.adj(v))
    if (!marked[w])
      dfs(G, w, v);
    else if (w != u)
      hasCycle = true;
}
~~~

### Bipartite Detection

DFS can also be used to determine whether or not the graph is bipartite. Another way to frame the question is: can the vertices of the graph be assigned one of two colors such that no edge connects vertices of the game color?

This is accomplished by maintaining a vertex-indexed array that will store that vertex' color. As DFS traverses the graph, it will alternate the color of every vertex it visits. The graph starts out as assumed to be bipartite, and only if DFS encounters a marked vertex whose color is the same as the current vertex does it conclude that the graph is not bipartite.

~~~ {lang="cpp" text="bipartite detection"}
bool bipartiteDetect(const Graph &G) {
  for (int s = 0; s < G.V(); s++)
    if (!marked[s])
      dfs(G, s);
}

bool dfs(const Graph &G, int v) {
  marked[v] = true;

  for (int w : G.adj(v))
    if (!marked[w]) {
      color[w] = !color[v];
      dfs(G, w);
    } else if (color[w] == color[v]) isBipartite = false;
}
~~~

## Directed Graphs

The edges in _directed graphs_ have an associated one-way direction, such that edges are defined by an ordered pair of vertices that define a one-way adjacency. A directed graph (or _digraph_) is a set of vertices and a collection of directed edges, each connecting an ordered pair of vertices. The _outdegree_ of a vertex is the number of edges pointing from it, while the _indegree_ is the number of edges pointing to it.

The first vertex in a directed edge is the _head_ and the second vertex is the _tail_. Edges are drawn as arrows pointing from head to tail, such as $v \rightarrow w$.

Directed graphs can be represented by adjacency lists with the stricter property that if node $w$ is present in the adjacency list corresponding to $v$, it simply means that there is a directed edge $v \rightarrow w$, but not vice versa unless explicitly defined.

### Reachability {#digraph-reachability}

The same exact implementation of reachability testing by DFS used in undirected graphs can be used for digraphs, and can be expanded to allow for reachability testing from multiple sources which has applications in regular expression matchers or mark-and-sweep garbage collection strategies, for example.

Mark-and-sweep garbage collection (GC) strategies typically reserve one bit per object for the purpose of garbage collection. The GC then periodically _marks_ a set of potentially accessible objects by running digraph reachability tests on the graph of object references, then it _sweeps_ through all of the unmarked objects, collecting them for reuse for new objects.

### Cycle Detection {#directed-cycle-detection}

A digraph with no directed cycles is known as a directed acyclic graph (DAG). For this reason, checking a digraph for directed cycles answers the question of whether the digraph is  DAG.

Directed cycle detection is accomplished by maintaining a boolean array representing whether or not a directed path belongs to the same connected component. Then during DFS if the encountered vertex is already marked and is part of the same component, it returns the path from the current vertex through the cycle back to the current vertex. If no such cycle exists, the graph is a DAG.

~~~ {lang="cpp" text="directed cycle detection"}
void dfs(const Graph &G, int v) {
  onStack[v] = true;
  marked[v] = true;

  for (int w : G.adj(v))
    if (hasCycle()) return;
    else if (!marked[w]) {
      edgeTo[w] = v;
      dfs(G, w);
    }
    else if (onStack[w]) {
      cycle = new stack<int>();

      for (int x = v; x != w; x = edgeTo[x])
        cycle.push_back(x);

      cycle.push_back(w);
      cycle.push_back(v);
    }

  onStack[v] = false;
}
~~~

### Topological Order

Topological sort puts the vertices of a digraph in order such that all of its directed edges point from a vertex earlier in the order to a vertex later in the order. Three different orders are possible, which are accomplished by saving each vertex covered by the DFS in a queue or stack, depending on the desired order:

* **preorder**: put the vertex on a queue before the recursive calls
* **postorder**: put the vertex on a queue after the recursive calls
* **reverse postorder**, aka _topological order_: put the vertex on a stack after the recursive calls

This ability of DFS follows from the fact that DFS covers each vertex exactly once when run on digraphs.

### Strong Connectivity

Two vertices $v$ and $w$ are _strongly connected_ if they are mutually reachable, i.e. $v \leftrightarrow w$. Consequently, an entire digraph is _strongly connected_ if _all_ of its vertices are strongly connected to one another. Further, _strong components_ are connected components of a graph that are strongly connected.

The [Kosaraju-Sharir](http://en.wikipedia.org/wiki/Kosaraju%27s_algorithm) algorithm is able to find strongly connected components in digraphs. The algorithm operates as follows:

1. given digraph $G$ and its reverse digraph $G^R$, compute the reverse postorder of $G^R$
2. run standard DFS on $G$ on the vertices in the order generated by step 1
3. all vertices visited on a recursive DFS call from the constructor are a strong component, so identify them

The algorithm can answer the following questions:

* are two given vertices strongly connected?
* how many strong components does the digraph contain?

~~~ {lang="cpp" text="kosaraju-sharir algorithm"}
void findStrongComponents(const Digraph &G) {
  Digraph reverse = G.reverse();

  for (int s : reverse.reversePost())
    if (!marked[s]) {
      dfs(G, s);
      count++;
    }
}

void dfs(const Digraph &G, int v) {
  marked[v] = true;
  id[v] = count;

  for (int w : G.adj(v))
    if (!marked[w])
      dfs(G, w);
}
~~~

The algorithm can be understood by considering a kernel DAG, or _condensation digraph_, associated with each digraph, formed by collapsing all vertices in each strong component to a single vertex. This DAG can then be put into reverse topological order. Remember that reverse postorder of a DAG is equivalent to topological sort.

The algorithm begins by finding a vertex that is in a sink component of the kernel DAG. A _sink component_ is one that has no edges pointing from it. Running DFS from this vertex only visits the vertices in that component. DFS then marks the vertices in that component, effectively removing them from further consideration in that digraph. It then repeats this by finding another sink component in the resulting kernel DAG.

The first vertex in a reverse postorder of $G$ is in a _source_ component of the kernel DAG, whereas the first vertex in a reverse postorder of the _reverse_ digraph $G^R$ is in a _sink_ component of the kernel DAG.

### All-Pairs Reachability

All-Pairs reachability asks: given a digraph, is there a directed path from a given vertex $v$ to another given vertex $w$? This can be answered by creating a separate graph representation known as a transitive closure, which allows for straightforward checking of which vertex is reachable by others.

<img src="/images/notes/algorithms/graphs/transitive-closure.png" class="right">

The _transitive closure_ of digraph $G$ is another digraph with the same set of vertices but with an edge from $v$ to $w$ in the transitive closure if and only if $w$ is reachable from $v$ in $G$. Transitive closures are generally represented as a matrix of booleans where row $v$ at column $w$ is true if $w$ is reachable from $v$ in the digraph.

Finding the transitive closure of a digraph can be accomplished by running DFS on every vertex of the digraph and storing the resulting reachability array for each each vertex from which DFS was run. However, it can be impractical for large graphs because it uses space proportional to $V^2$ and time proportional to $V(V + E)$.

## Dynamic Connectivity

**Answers**: Is a pair of nodes connected?

**Data Structure**: Array, indexed by any given site to the value corresponding to the component its a part of: `id[site] = component`. All sites are initially set to be members of their own component, i.e. `id[5] = 5`.

**General Flow**: Sites are all partitioned into singleton sets. Successive `union()` operations merge sets together. The `find()` operation determines if a given pair of sites are from the same component.

A _site_ is an element or node in a disjoint set. The disjoint set is known as a _component_, which typically models a set or graph. Two sites are _connected_ if they are part of the same component.

### Quick-Find

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

public void union(int p, int q) {
  int pID = find(p);
  int qID = find(q);

  if (pID == qID) return;

  for (int i = 0; i < id.length; i++)
    if (id[i] == pID) id[i] = qID;

  count--;
}
~~~

### Quick-Union

<div class="right">

Operation    Growth
----------  --------
Find        $\text{tree height}$
Union       $\text{tree height}$

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

### Weighted Quick-Union

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

#### Path Compression

<div class="right">

Operation         Growth
----------       --------
Union            $\approx 1$

</div>

A further improvement can be done called _path compression_ in which every site traversed due to a call to `find()` is directly linked to the component root.

~~~ {lang="java" text="path compression"}
public int find(int p) {
  if (p != id[p])
    id[p] = find(id[p]);

  return id[p];
}
~~~

## Minimum Spanning Trees

An _edge-weighted graph_ is a graph where the edges have associated weights or costs. Edge-weighted graphs can be represented with adjacency lists containing edge objects which contain the two vertices, one of which is the index of the adjacency list, as well as the weight for that edge.

A _spanning tree_ is a connected subgraph with no cycles that includes all of the vertices in the graph. A _minimum spanning tree_ (MST) is a spanning tree whose weight---the sum of all of its edges' weights---is no larger than the weight of any other spanning tree for that graph.

Adding an edge to a tree creates a cycle and removing an edge from a tree breaks it into two separate subtrees. Knowing this, a _cut_ of a graph is a partition of its vertices into two nonempty disjoint sets, connected by a _crossing edge_.

### Prim's Algorithm

<div class="right">

Case    Growth
-----   --------
Worst   $O(E \lg {E})$
Space   $O(E)$

</div>

This method of finding the MST operates by attaching a new edge to a growing tree at each step. Starting with any vertex from the graph to create a single-vertex tree, each time taking the minimum-weight edge that connects a vertex on the tree to a vertex not yet on the tree.

The vertices in the tree being built are represented using a vertex-indexed boolean array where an element is set to true if the vertex is in the tree. The edges in the tree can be represented with a queue that collects edges or a vertex-indexed array of edge objects. Crossing edges are held in a minimum priority queue, making the operation of choosing the edge with the lowest weight particularly straightforward.

The act of adding an edge to the tree corresponds to adding a vertex to the tree. When this occurs, all edges from the newly added vertex to all vertices not in the tree must be added to the crossing edges priority queue. Furthermore, any edges previously in the priority queue that connected the newly added vertex to a vertex already in the tree become _ineligible_---otherwise they would create a cycle---and should be ignored or removed.

~~~ {lang="java" text="prim's algorithm"}
void prim(EdgeWeightedGraph G) {
  visit(G, 0); // start at arbitrary vertex

  while (!pq.empty()) {
    Edge e = pq.delMin(); // fetch lowest weight edge from frontier
    int v = e.either(), w = e.other(v);

    if (marked[v] && marked[w]) continue; // skip ineligible edges

    mst.enqueue(e); // add edge to result

    // visit either v or w
    if (!marked[v]) visit(G, v);
    if (!marked[w]) visit(G, w);
  }
}

void visit(EdgeWeightedGraph G, int v) {
  marked[v] = true;

  for (Edge e : G.adj(v))
    if (!marked[e.other(v)]) pq.insert(e);
}
~~~

#### Eager Prim's Algorithm {#eager-prims}

<div class="right">

Case    Growth
-----   --------
Worst   $O(E \lg {E})$
Space   $O(V)$

</div>

The above implementation is lazy with respect to ignoring ineligible edges in the priority queue. That approach leaves ineligible edges in the priority queue until they're dequeued for consideration and discarded if they are ineligible.

By contrast, an _eager approach_ would make sure those edges aren't present in the priority queue from the beginning. The eager version of Prim's algorithm uses two vertex-index arrays:

* an array for the shortest edges to vertices which are reachable from the tree within one edge
* an array for the weight of the shortest edge stored in the aforementioned array

For each vertex present in the above arrays, the vertex index associated with its weight are stored in a minimum priority queue, such that when the minimum weight is removed the associated index is returned. The implication of maintaining the priority queue this way is that given the next minimum-weight crossing edge returned by the priority queue, its associated vertex is the next one to add to the MST.

An improvement from the lazy implementation is that the eager implementation uses space proportional to $V$ whereas the lazy implementation uses $E$.

~~~ {lang="java" text="eager prim's algorithm"}
void primEager(EdgeWeightedGraph G) {
  for (int v = 0; v < G.V(); v++) {
    distTo[v] = Double.POSITIVE_INFINITY;
  }

  distTo[0] = 0.0;
  pq.insert(0, 0.0);

  while (!pq.empty())
    visit(G, pq.delMin());
}

void visit(EdgeWeightedGraph G, int v) {
  marked[v] = true;

  for (Edge e : G.adj(v)) {
    int w = e.other(v);
    if (marked[w]) continue; // v-w is ineligible
    if (e.weight() < distTo[w]) {
      edgeTo[w] = e;
      distTo[w] = e.weight();

      if (pq.contains(w)) pq.changeKey(w, distTo[w]);
      else                pq.insert(w, distTo[w]);
    }
  }
}
~~~

### Kruskal's Algorithm

<div class="right">

Case    Growth
-----   --------
Worst   $O(E \lg {E})$
Space   $O(E)$

</div>

An alternative method for finding the MST is to process the edges in increasing order of their weight values, each time taking an edge for the MST that doesn't form a cycle, stopping once $V-1$ edges have been aggregated. The edges form a forest of trees, gradually growing into a single tree (the MST). The algorithm can be thought of as starting with a forest of $V$ single-vertex trees, and on each step finding an edge to connect two trees until there is only one left (the MST).

The implementation uses a priority queue of edges based on their weight, a union-find data structure to identify potential cycles, and a queue to collect edges for for the MST.

Despite the simplicity of Kruskal's algorithm, it is generally slower than Prim's because it has to check if an edge is already connected using the union-find data structure on each edge that is considered for the MST.

~~~ {lang="java" text="kruskal's algorithm"}
void kruskal(EdgeWeightedGraph G) {
  mst = new Queue<Edge>();
  pq = new MinPQ<Edge>();
  uf = new UF(G.V());

  for (Edge e : G.edges())
    pq.insert(e);

  while (!pq.empty() && mst.size() < G.V() - 1) {
    Edge e = pq.delMin(); // fetch edge with lowest weight
    int v = e.either(), w = e.other(v);
    if (uf.connected(v, w)) continue; // check if already connected
    uf.union(v, w); // if not, merge them in the union-find data structure
    mst.enqueue(e); // add the edge to result
  }
}
~~~

## Shortest Paths

The _shortest path_ from vertex $s$ to $t$ in an edge-weighted digraph is a directed path from $s$ to $t$ such that no other such path has a lower weight. A _shortest-path tree_ (SPT) for a source vertex $s$ is a subgraph containing $s$ and all the vertices reachable from $s$ that forms a directed tree rooted at $s$ such that every path is a shortest path in the digraph.

_Edge relaxation_ refers to replacing an existing edge that reaches $w$ with a new edge $v \rightarrow w$ if the new edge makes the path from the source vertex to $w$ be of lower cost than it was previously.

~~~ {lang="java" text="edge relaxation"}
void relax(DirectedEdge e) {
  int v = e.from(), w = e.to();

  if (distTo[w] > distTo[v] + e.weight()) {
    distTo[w] = distTo[v] + e.weight();
    edgeTo[w] = e;
  }
}
~~~

_Vertex relaxation_ is similar to edge relaxation except that it relaxes all of the edges pointing from a given vertex.

~~~ {lang="java" text="vertex relaxation"}
void relax(EdgeWeightedDigraph G, int v) {
  for (DirectedEdge e : G.adj(v)) {
    int w = e.to();

    if (distTo[w] > distTo[v] + e.weight()) {
      distTo[w] = distTo[v] + e.weight();
      edgeTo[w] = e;
    }
  }
}
~~~

### Dijkstra's Algorithm

<div class="right">

Case    Growth
-----   --------
Worst   $O(E \lg {V})$
Space   $O(V)$

</div>

Dijkstra's alrogithm is similar to Prim's algorithm for finding the MST. Dijkstra's algorithm finds the SPT by finding the lowest-weight non-tree vertex as provided by an index minimum-priority queue and relaxing that vertex.

Dijkstra's algorithm _requires_ that edges be non-negative.

~~~ {lang="java" text="dijkstra's algorithm"}
void dijkstra(EdgeWeightedDigraph G, int s) {
  for (int v = 0; v < G.V(); v++)
    distTo[v] = Double.POSITIVE_INFINITY;
  distTo[s] = 0.0;

  pq.insert(s, 0.0);

  while (!pg.empty())
    relax(G, pq.delMin());
}

void relax(EdgeWeightedDigraph G, int v) {
  for (DirectedEdge e : G.adj(v)) {
    int w = e.to();

    if (distTo[w] > distTo[v] + e.weight()) {
      distTo[w] = distTo[v] + e.weight();
      edgeTo[w] = e;

      if (pq.contains(w)) pq.changeKey(w, distTo[w]);
      else                pq.insert(w, distTo[w]);
    }
  }
}
~~~

To specifically find the shortest path from the source vertex to an arbitrary vertex, simply terminate the search as soon as the target vertex comes off of the priority queue.

### Topological Sort

<div class="right">

Case    Growth
-----   --------
Worst   $O(E + V)$
Space   $O(V)$

</div>

Shortest paths can be found much more efficiently in acyclic graphs, specifically, the single-source problem can be solved in linear time, negative edge weights are easily handled, and other related problems such as finding the longest paths are solvable. This is possible by relaxing vertices in topological order.

~~~ {lang="java" text="shortest-paths in DAG"}
void shortestPathAcyclic(EdgeWeightedDigraph G, int s) {
  for (int v = 0; v < G.V(); v++) 
    distTo[v] = Double.POSITIVE_INFINITY;
  distTo[s] = 0.0;

  for (int v : G.topologicalOrder())
    relax(G, v);
}
~~~

This approach can be used for finding the longest path between two vertices in a DAG, accomplished by creating a copy of the DAG and negating the weight of every edge.

### Parallel Job Scheduling

The _critical path method_ for parallel job scheduling consists of encoding the constraints of the scheduling problem in a DAG. Both a source vertex $s$ and a sink vertex $t$ are created on either ends of the graph. Jobs are encoded in the graph as a pair of nodes connected by an edge whose weight corresponds to that job's duration. For each precedence constraint $v \rightarrow w$, add a zero-weight edge from $v$ to $w$. Finally, add a zero-weight edge from the source to every job's start vertex and from every job's end vertex to the sink.

When the scheduling problem is encoded in this manner, it can be solved by scheduling each job at the time corresponding to its longest path from the source vertex.

Relative deadlines can be encoded as a negative weighted edge going from the constrained job (vertex) to the job (vertex) which the deadline is relative to. However, relative deadlines can quickly make solutions infeasible with the aforementioned algorithms (Dijkstra's and Acyclic Shortest Paths).

### Bellman-Ford Algorithm

<div class="right">

Case    Growth
-----   --------
Worst   $O(VE)$
Average $O(E + V)$
Space   $O(V)$

</div>

The problem of finding the shortest paths can be generalized to graphs containing negative cycles. The Bellman-Ford algorithm accomplishes this by adding the source vertex to a queue and entering a loop where a vertex is dequeued and relaxed, and any vertex affected by that relaxation gets enqueued.

A _negative cycle_ is a directed cycle with net negative weight. No shortest path between $s$ and $v$ can consist of a vertex that lies within a negative cycle, or the weight of the path can be made arbitrarily low and a _shortest_ path would "never" be achieved.

To prevent the Bellman-Ford algorithm from looping infinitely due to negative cycles, it has to ensure to terminate after $V$ passes either by keeping track with a counter or by detecting negative cycles within a subgraph.

~~~ {lang="java" text="bellman-ford algorithm"}
void bellmanFord(EdgeWeightedDigraph G, int s) {
  queue.enqueue(s);
  onQ[s] = true;

  while (!queue.empty() && !this.hasNegativeCycle()) {
    int v = queue.dequeue();
    onQ[v] = false;
    relax(G, v);
  }
}

void relax(EdgeWeightedDigraph G, int v) {
  for (DirectedEdge e : G.adj(v)) {
    int w = e.to();

    if (distTo[w] > distTo[v] + e.weight()) {
      distTo[w] = distTo[v] + e.weight();
      edgeTo[w] = e;

      if (!onQ[w]) {
        queue.enqueue(w);
        onQ[w] = true;
      }
    }

    if (cost++ % G.V() == 0)
      findNegativeCycle();
  }
}
~~~

If the queue is not empty after $V$ passes through each edge then there is a negative cycle. By extension, if a negative cycle is present in a graph, the Bellman-Ford algorithm can end up in an infinite loop, continuously lowering the weight of each affected path.

This is mitigated by checking for negative cycles on every $V^{th}$ call to relax, as on line 26 of the above code listing. On every such interval, a [cycle finder](#directed-cycle-detection) is initiated on the sub-graph denoted by the edges so-far considered by Bellman-Ford.

~~~ {lang="java" text="negative cycle finder"}
void findNegativeCycle() {
  int V = edgeTo.length;
  EdgeWeightedDigraph spt = new EdgeWeightedDigraph(V);

  for (int v = 0; v < V; v++)
    if (edgeTo[v] != null)
      spt.addEdge(edgeTo[v]);

  EdgeWeightedCycleFinder cf = new EdgeWeightedCycleFinder(spt);

  cycle = cf.cycle();
}
~~~

## Constraint Satisfaction Problems

_Constraint Satisfaction Problems_ (CSP) [^cs188_csp] are a special subset of search problems where the state is defined by variables $X_i$ with corresponding values from a domain $D$ (which may depend on $i$), and the goal test is a set of constraints specifying the allowable combinations of values for the variables. A solution in this case is simply an assignment to all variables which satisfies the constraints.

*[CSP]: Constraint Satisfaction Problems

[^cs188_csp]: See [Week 3](https://courses.edx.org/courses/BerkeleyX/CS188.1x/2012_Fall/courseware/Week_3/Lecture_4_CSPs/) of CS 188.1x for more information.

Example problems that may be modeled as CSPs are map coloring, N-Queens, and Sudoku. [Map coloring] consists of coloring in different regions in a map such that their bordering regions don't have the same color. In this case, the variables would be the individual regions and the domain would consist of the possible set of colors, e.g. $D = \{\text{red}, \text{green}, \text{blue}\}$. The constraints could then be modeled implicitly in the form Region1 ≠ Region2 where Region2 borders Region1, or by explicitly specifying every legitimate configuration.

[Map coloring]: http://en.wikipedia.org/wiki/Map_coloring

[N-Queens] looks for a possible configuration of an N×N chess board with N queens on it such that there is one queen on each row and none of them threaten each other, i.e. they cannot be on the same row, column, or diagonal. This problem can be modeled so that there is one variable $Q_k$ for each queen taking on a value from the domain $D = \{1, 2, \ldots N\}$ which corresponds to the column the queen is on. The constraints can be modeled implicitly with $\forall_{i,j}\ \text {non-threatening} (Q_i, Q_j)$.

[N-Queens]: http://en.wikipedia.org/wiki/Eight_queens_puzzle

### Backtracking Search

<div class="right">

Case    Growth
-----   -------
Worst   $O(d^n)$

Table: $d:$ domain size

</div>

Given a state tree of the constraint satisfaction problem, all of the solutions would be at the bottom, so BFS would experience the worst-case. DFS with its backtracking gets to the bottom quicker, but it must be adapted to the context of CSPs in order to be optimal.

This adaptation is known as _backtracking search_. Backtracking search only considers one variable at a time and checks the constraints at each step, so that only values that don't conflict with previous assignments are considered. Backtracking naturally occurs if there are no more successors. A naive implementation of this, that will be optimized later, follows:

1. start with an empty solution
2. if the solution is complete, return it
3. select an unassigned variable
4. try giving it a value from its domain that hasn't been tried:
    1. if there are no more values in the domain, return failure (no successors). This goes back to the previous variable, i.e. backtracking, so that it may try another value for it (and backtracking again if there are no more).
    2. if the value satisfies the constraints, set it
    3. recurse starting at #2 and get its result
        1. if the result didn't fail, return it
        2. otherwise unset the variable and go to #4 to try another value

This algorithm can be optimized further by ordering the variables in a specific way, filtering out values from domains as other variables are set in order to detect failure earlier, and exploiting the problem's structure.

_Forward checking_ keeps track of domains for unassigned variables and removes from them values which would violate a constraint when added to the existing assignment. This is done whenever a new variable is assigned. For example, in a map coloring problem, if the domain is $D = \{\text{red}, \text{green}, \text{blue}\}$ and Region1 is set to red, then red would be removed from the domain of Region2 which borders it, since setting Region2 to red would violate the constraints.

_Constraint propagation_ takes this further by propagating these effects farther, in order to detect potential failures earlier. This is done by having a notion of an _arc_ which leads from other variables on the constraint graph to the variable in question, so that the _head_ of the arc is the variable in question and the tail is the other variable. Then it is said that a given arc $X \to Y$ is _consistent_ iff for _every_ $x$ in the tail's domain, there is some $y$ in the head's domain which could be assigned without violating the constraint.

_Forward checking_ uses this concept so that, when a new variable is assigned, arc consistency is enforced for each variable by removing values from their domain which would otherwise make them inconsistent. Naturally, when a value is removed from a varible's domain, all neighbors of that variable (incoming arcs) have to be re-enforced. Arc consistency is run after every assignment in backtracking search.

The algorithm, known as the [AC-3 algorithm](http://en.wikipedia.org/wiki/AC-3_algorithm) for enforcing arc consistency follows (specifically for binary CSPs, where there are at most two variables per constraint):

1. create a queue containing all of the arcs in the CSP
2. while the queue is not empty:
    1. retrieve an arc from the queue
    2. for each value $x$ in the tail's domain:
        1. if no value $y$ in the head's domain satisfies the constraints given $x$:
            1. delete $x$ from the tail's domain
    3. if there were values removed, then add an arc to the queue for each neighbor (i.e. each incoming arc)

_Variable Ordering_ refers to optimizing by prioritizing some variables over others. _Minimum Remaining Values_ (MRV) consists of prioritizing variables which have the fewest legal values left in their domain. This is so that, if backtracking becomes necessary, the amount of backtracking will be much less.

_Value Ordering_ refers to optimizing by prioritizing certain values in a domain. _Least Constraining Value_ refers to choosing the value which rules out the fewest values in the remaining variables. Knowledge of this may require re-running filtering.

### K-Consistency

There are increasing degrees of consistency. For example, _1-Consistency_ (Node Consistency) is when each single variable's (node) domain has a value which meets that node's unary constraints. _2-Consistency_ (Arc Consistency) is when any consistent assignment for one variable can be extended to the other for each pair of nodes. _K-Consistency_ is the generalized notion where any consistent assignment to $k - 1$ variables can be extended to the $k^{th}$ node for each $k$ nodes, i.e. whatever is done at the tail $k - 1$ variables can be extended to the head.

_Strong N-Consistency_ requires that all of the lower orders of K-Consistency are also satisfied, e.g. $k - 1$, $k - 2$, etc. This would mean that the CSP could be solved without backtracking, since the constraints could be enforced further and further until the entire constraint graph is enforced. Naturally this is very difficult to accomplish, though a good middle ground is where $k = 3$, referred to as _path consistency_.

### Tree-Structured CSPs

<div class="right">

Case    Growth
-----   -------
Worst   $O(n d^2)$

Table: $d:$ domain size

</div>

The CSP can be solved _much_ faster if there are no cycles in the constraint graph, specifically linear in the size of the graph and quadratic in the size of the domains.

The tree must first be re-ordered by choosing a root variable so that all parents precede children by replacing the undirected connections with directed connections. Once the constraint graph is structured in this manner, the algorithm is simple:

1. all nodes are traversed one level at a time, starting at the lowest level and going towards but not including the root
    1. for a given node, its incoming arc's consistency is enforced
2. set all of the nodes starting at the root. Each node is guaranteed by step #1 to have at least one valid value

### Cutset Conditioning

<div class="right">

Case    Growth
-----   -------
Worst   $O(d^c\ (n - c)\ d^2)$

Table: $c:$ cutset size

</div>

This optimization only applies to tree-structured CSPs, but not all problems are tree-structured. However, sometimes a constraint graph can easily be converted into a tree-structured CSP by removing a particular set of nodes. This is accomplished by setting the value of the variable and then severing the connection to its neighbors, imposing an additional unary constraint on the neighbors reflecting the value the node was set to, essentially removing the now-invalid values from the domains of the neighbors.

_Cutset conditioning_ is an algorithm that accomplishes this transformation, which essentially works by instantiating (in all ways) a set of variables so that the remaining constraint graph is a tree.

1. choose a cutset
2. instantiate the cutset in all possible ways
3. compute residual CSP by removing instantiated nodes and replacing their constraints with smaller constraints over remaining neighboring variables (NP-Hard)
4. solve residual tree-structured CSP

### Iterative Algorithms

Iterative algorithms begin with a constraint graph where every variable is set to a value, whether or not the value satisfies the constraints.

1. while not solved:
    1. select a conflicted variable
    2. choose a new value (min-conflicts heuristic)
        1. choose value that violates the fewest constraints (i.e. hill climb with h(n) = total number of violated constraints)

This approach to CSP solving is _very_ performant for any randomly-generated CSP particularly if there are many variables but few constraints or vice versa, but _not_ when both are the case:

$$ R = \frac {\text {# of constraints}} {\text {# of variables}} $$

# Strings

Strings have special properties which necessitate more efficient algorithms for sorting and searching. Other subjects concerning strings include tries, regular expressions, and data compression.

## Sorting {#string-sorting}

Certain properties of strings and alphabets can make for more efficient sorting algorithms for strings.

### Counting Sort

Counting sort, also known as key-indexed counting, essentially involves computing a histogram of the number of occurrences of each character, then regenerating the array in sorted order using that information.

~~~ {lang="java" text="counting sort"}
int N = a.length;

int[] aux = new String[N];
int[] count = new int[R + 1];

// count occurrences
for (int i = 0; i < N; i++)
  count[a[i].key() + 1]++;

// compute key ranges
for (int r = 0; r < R; r++)
  count[r + 1] += count[r];

// populate sorted array
for (int i = 0; i < N; i++)
  aux[count[a[i].key()]++] = a[i];

// copy back to original array
for (int i = 0; i < N; i++)
  a[i] aux[i];
~~~

### Least Significant Digit Sort

<div class="right">

Case    Growth
-----   --------
Worst   $O(NW)$
Space   $O(N)$

</div>

Least Significant Digit (LSD) sort works by sorting the strings based on the last character and then repeating this operation up until the first character. This is accomplished by modifying the counting sort algorithm so that it does a pass for every character in the string. This is mainly useful if all strings are the same length $W$ and relatively small alphabet size $R$.

~~~ {lang="java" text="least significant digit sort"}
void sort(String[] a, int W) {
  int N = a.length;
  int R = 256;

  String[] aux = new String[N];

  for (int d = W - 1; d >= 0; d--) {
    int[] count = new int[R + 1];

    // count occurrences
    for (int i = 0; i < N; i++)
      count[a[i].charAt(d) + 1]++;

    // compute key ranges
    for (int r = 0; r < R; r++)
      count[r + 1] += count[r];

    // populate sorted array
    for (int i = 0; i < N; i++)
      aux[count[a[i].charAt(d)]++] = a[i];

    // copy back to original array
    for (int i = 0; i < N; i++)
      a[i] aux[i];
  }
}
~~~

### Most Significant Digit Sort

<div class="right">

Case    Growth
-----   --------
Best    $\Omega (N)$
Worst   $O(Nw)$
Space   $O(N + WR)$

Table: $w:$ average string length

</div>

Most Significant Digit (MSD) sort is similar to LSD except that it operates in left-to-right order instead, meaning it works fine for variable-length strings. This is accomplished by performing counting sort to sort the array of strings based on their first character, then recursively performing the same operation on the sub-array of strings with the same first letter.

Because MSD works left-to-right and strings may be of variable length, the possibility of reaching the end of the string requires special handling. This is solved by observing the fact that a smaller string $S_1$ that is a prefix of larger string $S_2$ should naturally come before it in lexicographically sorted order. For example, _sea_ should come before _seashore_.

This order is maintained by keeping a separate count of such strings that have had all of their characters sorted. This count is held at `count[1]`. A string has had all of its characters sorted if the character position currently being sorted is past the length of the string currently being considered. Once the counts are converted to key ranges, such strings will naturally be inserted at the beginning of the sorted sub-array.

On each recursion of the sorting operation, an array for counts is allocated whose size is proportional to the alphabet size, occurrences are counted, transformed to key ranges, and so on. The point is that these operations can come to dominate the sorting operation, which makes having a cutoff for small sub-arrays crucial. After the cutoff, insertion sort takes over, with the slight modification that it only operates on the $d^{th}$ character position onward.

~~~ {lang="java" text="most significant digit sort"}
void charAt(String s, int d) {
  if (d < s.length())
    return s.charAt(d);
  else
    return -1;
}

void sort(String[] a) {
  int N = a.length;
  aux = new String[N];
  sort(a, 0, N - 1, 0);
}

void sort(String[] a, int lo, int hi, int d) {
  // cut off point for just running insertion sort
  if (hi <= lo + M) {
    Insertion.sort(a, lo, hi, d);
    return;
  }

  int[] count = new int[R + 2];

  // count occurrences
  for (int i = lo; i <= hi; i++)
    count[charAt(a[i], d) + 2]++;

  // compute key ranges
  for (int r = 0; r < R + 1; r++)
    count[r + 1] += count[r];

  // populate sorted array
  for (int i = lo; i <= hi; i++)
    aux[count[charAt(a[i], d) + 1]++] = a[i];

  // copy back to original array
  for (int i = lo; i <= hi; i++)
    a[i] = aux[i - lo];

  // recurse for each remaining character value
  for (int r = 0; r < R; r++)
    sort(a, lo + count[r], lo + count[r + 1] - 1, d + 1);
}
~~~

### Three-way String QuickSort

<div class="right">

Case    Growth
-----   --------
Best    $\Omega (N)$
Worst   $O(Nw \lg {R})$
Space   $O(W + \lg {N})$

Table: $w:$ average string length

</div>

Three-way quicksort can be adapted to work on a per-character basis similar to MSD. The advantages of this are that the algorithm doesn't use extra space---unlike MSD---and that the number of sub-arrays per recurse is bounded at three.

A direct result of only splitting into three sub-arrays is that more data movements are required to get elements into their correct position compared to MSD. However, three-way quicksort's three-way splits adapt well to handling equal keys, keys with small arrays, and keys that fall into a small range.

Research has shown that no algorithm can beat 3-way string quicksort by more than a constant factor.

~~~ {lang="java" text="string quicksort"}
void stringQuickSort(String[] a, int lo, int hi, int d) {
  if (hi <= lo) return;

  int lt = lo, gt = hi;
  int v = charAt(a[lo], d);
  int i = lo + 1;

  while (i <= gt) {
    int t = charAt(a[i], d);

    if      (t < v) exch(a, lt++, i++);
    else if (t > v) exch(a, i, gt--);
    else            i++;
  }

  sort(a, lo, lt - 1, d);
  if (v >= 0) sort(a, lt, gt, d + 1);
  sort(a, gt + 1, hi, d);
}
~~~

## Tries

Trie structures exploit string properties to provide much faster string search, with hits taking time proportional to the length of the key and where misses require examining only a few characters.

<img src="/images/notes/algorithms/tries/trie.png" class="right">

The structure of tries is comprised of a tree where every node has $R$ _links_ where $R$ is the size of the alphabet. Every node also has an associated _label_ corresponding to the character value consumed to reach the node. The root node has no such label as there is no link pointing to it. Every node also also has an associated _value_ corresponding to the value associated with the key denoted by the path ending at the particular node.

A _search hit_ occurs when the trie search arrives at the final node and that node's value is not empty. A _search hit_ occurs both if the final node's value is empty or if the search terminated on a null link.

~~~ {lang="java" text="trie search"}
Value get(String key) {
  Node x = get(root, key, 0);
  if (x == null) return null;
  return x.val;
}

Node get(Node x, String key, int d) {
  if (x == null) return null;
  if (d == key.length()) return x;
  char c = key.charAt(d);
  return get(x.next[c], key, d + 1);
}
~~~

Trie insertion simply consists of searching for the key and setting the value. If the key does not already exist, then create nodes for every character not yet in the trie.

~~~ {lang="java" text="trie insertion"}
void put(String key, Value val) { root = put(root, key, val, 0); }

Node put(Node x, String key, Value val, int d) {
  if (x == null) x = new Node();
  if (d == key.length()) { x.val = val; return x; }
  char c = key.charAt(d);
  x.next[c] = put(x.next[c], key, val, d + 1);
  return x;
}
~~~

Tries also allow operations for collecting keys with a common prefix. This is accomplished by finding the node at the end of the prefix' path and then recursively performing BFS on every node and enqueueing any node that has a non-empty value.

~~~ {lang="java" text="trie key collection"}
Queue<String> keysWithPrefix(String prefix) {
  Queue<String> q = new Queue<String>();
  collect(get(root, prefix, 0), prefix, q);
  return q;
}

void collect(Node x, String prefix, Queue<String> q) {
  if (x == null) return;
  if (x.val != null) q.enqueue(prefix);

  for (char c = 0; c < R; c++)
    collect(x.next[c], prefix + c, q);
}
~~~

This can also be modified to allow wildcard pattern matches, for example, keys that match `fin.` could include `fine`, `find`, etc.

~~~ {lang="java" text="trie key wildcards"}
Queue<String> keysWithPrefix(String pattern) {
  Queue<String> q = new Queue<String>();
  collect(root, "", pattern, q);
  return q;
}

void collect(Node x, String prefix, String pattern, Queue<String> q) {
  int d = pre.length();

  if (x == null) return;
  if (d == pattern.length() && x.val != null) q.enqueue(prefix);
  if (d == pattern.length()) return;

  char next = pattern.charAt(d);
  for (char c = 0; c < R; c++)
    if (next == '.' || next == c)
      collect(x.next[c], prefix + c, pattern, q);
}
~~~

### Deletion {#trie-deletion}

Deletion is a straightforward process in tries, simply involving finding the node and emptying its value. If this operation makes the node's parent's children all be null, then the same operation must be run on the parent.

~~~ {lang="java" text="trie deletion"}
void delete(String key) { root = delete(root, key, 0); }

Node delete(Node x, String key, int d) {
  if (x == null) return null;
  if (d == key.length())
    x.val = null;
  else {
    char c = key.charAt(d);
    x.next[c] = delete(x.next[c], key, d + 1);
  }

  if (x.val != null) return x;

  for (char c = 0; c < R; c++)
    if (x.next[c] != null)
      return x;

  return null;
}
~~~

### Ternary Search Trees

Ternary Search Trees (TSTs) seek to avoid the excessive space cost of regular R-way tries demonstrated above. TSTs are structured such that each node has only three links for characters less than, equal to, and greater than the node.

R-way tries can provide the fastest search, finishing the operation with a constant number of compares. However, space usage increases rapidly with larger alphabets TSTs are preferable, sacrificing a constant number of compares for a logarithmic number of compares.

~~~ {lang="java" text="ternary search tree search"}
Node get(Node x, String key, int d) {
  if (x == null) return null;
  char c = key.charAt(d);

  if      (c < x.c) return get(x.left,  key, d);
  else if (c > x.c) return get(x.right, key, d);
  else if (d < key.length() - 1)
                    return get(x.mid,   key, d);
  else return x;
}
~~~

Insertion is similar to insertion with tries except that only one of three links can be taken, instead of $R$ links.

~~~ {lang="java" text="ternary search tree insertion"}
void put(String key, Value val) { root = put(root, key, val, 0); }

Node put(Node x, String key, Value val, int d) {
  char c = key.charAt(d);

  if (x == null) { x = new Node(); x.c = c; }

  if      (c < x.c) x.left  = put(x.left,  key, val, d);
  else if (c > x.c) x.right = put(x.right, key, val, d);
  else if (d < key.length() - 1)
                    x.mid   = put(x.mid,   key, val, d + 1);
  else x.val = val;
  return x;
}
~~~

## Substring Search

Searching for a string within another string is a very common operation that can also benefit from exploiting certain properties of strings.

### Brute-Force {#brute-force-substring-search}

The most straightforward approach is a brute-force algorithm where every character in the text is checked to see if the pattern's first character matches, and if so, checks to see if the second character in the pattern matches, and so on.

If any character in the pattern matches during this check, the pattern iterator is not incremented and instead the text iterator is set back the amount of spaces equal to the pattern iterator, which essentially moves the text iterator one position past the position where the match checking was initiated. The pattern iterator is then reset to zero.

~~~ {lang="java" text="brute-force substring search"}
int search(String pattern, String text) {
  int j, M = pattern.length();
  int i, N = text.length();

  for (i = 0, j = 0; i < N && j < M; i++) {
    if (text.charAt(i) == pattern.charAt(j))
      j++;
    else {
      i -= j;
      j = 0;
    }
  }

  if (j == M) return i - M;
  else        return N;
}
~~~

### Knuth-Morris-Pratt

The Knuth-Morris-Pratt (KMP) substring search algorithm considers that it's probably not necessary to backtrack all the way to the beginning, since the characters along that stretch of the sequence have already been seen. One way to know the correct distance to backtrack is accomplished using a Deterministic Finite-State Automaton (DFA). There are other methods that either [build an NFA](http://algs4.cs.princeton.edu/53substring/KMPplus.java.html) or build a [partial-match table](http://www.inf.fh-flensburg.de/lang/algorithmen/pattern/kmpen.htm).

#### DFA Composition {#kmp-dfa-composition}

The DFA is constructed such that every state corresponds to the characters in the patterns, storing their position in the pattern. At each state there exists a transition to the next state corresponding with the character consumed in the pattern. At each state there are also transitions going back to previous states, corresponding to backtracking on a pattern mismatch. Finally, the end state corresponds to the halt state and as such has no transitions leaving it.

The DFA is essentially represented by a table `dfa[c][j]` such that `c` corresponds to the character in the text currently being considered and `j` corresponds to the position of the character currently being considered in the pattern, i.e. the state in the DFA. In effect, `dfa[c][j]` determines which state to proceed to when at state `j` considering character `c`.

The value stored at `dfa[c][j]` therefore is the identifier of the state that the algorithm should jump to, which could mean either backtracking in the case of a mismatch when $C \neq pattern[J]$ or a progression to the next state when $C = pattern[J]$.

#### Preventing Backtracking {#kmp-prevent-backtracking}

In a normal brute-force algorithm when a pattern matching a segment of the text starting at `t[i]` mismatches at position `j`, the entire pattern is re-checked starting on the character to the right: `t[i + 1]`, effectively having to re-check characters `t[i + 1]` to `t[i + j - 1]`.

For example, the following mismatches at position 4:

~~~
0 1 2 3 4 5
A B C D E F
A B C D F
~~~

So in a brute-force algorithm the pattern would have to be shifted to the right by one position:

~~~
0 1 2 3 4 5
A B C D E F
  A B C D F
~~~

However, this essentially means that the text segment from position 1 to 3 has to be rechecked, which we would prefer to avoid. The important observation to make is that the text had _already matched_ the pattern _up to_ (but not including) position `j` where the mismatch occurred. That is, the text segment `t[i .. i + j - 1]` is equal to `p[0 .. j - 1]` where `p` is the pattern. Since we would have to shift to the right one character, this means that the text that would have to be rechecked corresponds to `p[1 .. j - 1]`. Feeding this to the DFA takes us to the state where we can appropriately handle `t[i + j]`.

_Based on this observation_, we can conclude that at every state we can add transitions for mismatch cases based on the transitions that would be made for the equivalent mismatch that would occur at the state we would arrive at if we had fed the input `p[0 .. j - 1]` to the DFA. For this reason, a "pointer" to this state is kept at every iteration of the DFA construction, where each iteration is comprised of defining all transitions for a given state.

#### DFA Construction {#kmp-dfa-construction}

Given the important observation above, the construction of the DFA is very straightforward. A pointer to a fall-back state `X` is maintained to appropriately establish transitions in the event of a mismatch.

1. the first transition is established: `dfa[p[0]][0] = 1`
2. for each character in the pattern, a state is created
    1. for every character in the alphabet, a transition is established based on the transition that would be taken at state `X`, since these are the mismatch transitions
    2. a match transition is created for the current pattern character
    3. the pointer to the fall-back state is updated to the state arrived at by following the transition corresponding to the current pattern character from the previous fall-back state

~~~ {lang="java" text="DFA construction"}
void constructDFA(int[][] dfa, String pattern) {
  dfa[pattern.charAt(0)][0] = 1;

  for (int X = 0, j = 1; j < M; j++) {
    for (int c = 0; c < R; c++)
      dfa[c][j] = dfa[c][X];

    dfa[pattern.charAt(j)][j] = j + 1;
    X = dfa[pattern.charAt(j)][X];
  }
}
~~~

#### KMP Search {#kmp-search}

Now that the DFA is constructed, a string can be searched easily. It simply iterates the text pointer on each iteration, while the pattern's pointer iterates based on the output from the DFA given the current text character as input. Iteration ends when the full length of either the text or the pattern is exhausted. If the full pattern was consumed then there was a match and the pointer to the start of the match is returned.

~~~ {lang="java" text="KMP search"}
int search(String text, String pattern) {
  int i, j, N = text.length(), M = pattern.length();
  
  for (i = 0, j = 0; i < N && j < M; i++)
    j = dfa[text.charAt(i)][j];

  if (j == M) return i - M;
  else        return N;
}
~~~

### Boyer-Moore

The Boyer-Moore substring search algorithm works by reading the pattern for comparison in reverse order while skipping through the text accordingly to facilitate this. When a comparison mismatches, the algorithm looks in a skip table to determine how far ahead to jump forward to begin the next match attempt. This behavior is known as the mismatched character heuristic.

#### Skip Table {#bm-skip-table}

The mismatched character heuristic makes use of the aforementioned skip table. The table is indexed by a character from the alphabet and gives the index of its rightmost occurrence in the pattern, or -1 if not present. That very value defines how far ahead to skip if that character from the text caused the mismatch.

The table is constructed by first setting all entries to -1, then for every character in the pattern, set that character's entry to its position in the pattern.

~~~ {lang="java" text="skip table construction"}
void constructSkipTable(String pattern) {
  int[] right = new int[R];

  for (int c = 0; c < R; c++)
    right[c] = -1;

  for (int j = 0; j < M; j++)
    right[pattern.charAt(j)] = j;
}
~~~

#### Search {#bm-search}

The searching algorithm, as previously stated, iterates the text pointer `i` from left-to-right and the pattern pointer `j` right-to-left. If there is a mismatch with character `c` in the text, then one of three things can occur:

1. **if `c` is not in the pattern**: increment `i` by `j + 1` to effectively skip that segment of the text that will not match
2. **if `c` is in the pattern**: use the `right` array to line up the pattern with the text such that the right-most occurrence of `c` in the pattern is lined up with `c` in the text
3. **if `i` is not increased due to the above case**: then just increment `i` instead so that the pattern always slides at least one position to the right

The above cases are handled with the simple statement `skip = j - right[text.charAt(i + j)]`. Case 1 is handled because characters not present in the pattern are stored as -1 in the table, thereby turning the statement into `skip = j + 1`. Case 2 is handled normally by finding the right-most occurrence' position of `c` in the table and subtracting that from `j`. Case 3 is handled by simply checking if `skip` is less than one and if so setting it to one. If `skip` was never changed from its initial value of zero, then a match was found.

~~~ {lang="java" text="boyer-moore search"}
int search(String text, String pattern) {
  int N = text.length();
  int M = pattern.length();
  int skip;

  for (int i = 0; i <= N - M; i += skip) {
    skip = 0;

    for (int j = M - 1; j >= 0; j--)
      if (pattern.charAt(j) != text.charAt(i + j)) {
        skip = j - right[text.charAt(i + j)]; // determine skip distance
        if (skip < 1) skip = 1; // ensure text traversal
        break; // mismatch; stop trying to match the rest
      }
  
    // no skip distance set, therefore text matched
    // i is position where the match began
    if (skip == 0) return i;
  }

  return N;
}
~~~

### Rabin-Karp

The Rabin-Karp algorithm conceptually works by computing a hash of the pattern and then hashing every equal-lengthed substring in the text to find a match. The key idea is that a string of length $M$ corresponds to an $M$-digit base-$R$ number. So a proper hash function would convert an $M$-digit base-$R$ number to an integer value between $0$ and $Q - 1$ where $Q$ is some very large prime number. This is possible with a simple modular hashing scheme, by taking the remainder of dividing the number by $Q$.

~~~ {lang="java" text="modular hash function via horner's method"}
long hash(String key, int M) {
  long h = 0;
  for (int j = 0; j < M; j++)
    h = (R * h + key.charAt(j)) % Q;
  return h;
}
~~~

The problem with using the above approach for the text is that it incurs the cost of multiplication, addition, and remainder calculations for _each character_. Instead, for an $M$-character substring of the text where $t_i$ corresponds to `text.charAt(i)` the hash $x_i$ can be computed as:

$$ x_i = t_i R^{M - 1} + t_{i + 1} R^{M - 2} + \ldots + t_{i + M - 1} R^0 $$

From the above formula it's apparent that the hash is constructed by individual _hash components_ derived from each character in the text. It stands to reason, then, that the hash of the text shifted one character to the right is:

$$ x_{i + 1} = \left( x_i - t_i R^{M - 1} \right) R + t_{i + M} $$

That is, the original hash minus the hash component of the first character of the previous text, plus the hash component of the new ending character.

~~~ {lang="java" text="rabin-karp"}
int search(String pattern, String text) {
  int M = pattern.length();
  long Q = longRandomPrime();
  long RM = 1;
  for (int i = 1; i <= M - 1; i++)
    RM = (R * RM) % Q; // compute R^(M - 1) % Q
  long patHash = hash(pattern, M);

  int N = text.length();
  long txtHash = hash(txt, M);

  if (patHash == txtHash) return 0; // match

  for (int i = M; i < N && check(0); i++) {
    txtHash = (txtHash + Q - RM * text.charAt(i - M) % Q) % Q;
    txtHash = (txtHash * R + text.charAt(i)) % Q;

    if (patHash == txtHash && check(i - M + 1))
       return i - M + 1; // match
  }

  return N;
}

// return true for Monte Carlo
// or check pattern vs text[i .. i - M + 1] for Las Vegas
boolean check(int i) { return true; }
~~~

## Regular Expressions

A Regular Expression pattern can be represented as a Non-Deterministic Finite-State Automaton (NFA) where every character in the pattern corresponds to a state in the NFA, followed by an accept state. Characters from the alphabet have an outgoing edge (match transition) going to the next state (character) in the pattern. Metacharacters such as parentheses, pipes, and asterisks have at least one outgoing edge ($\epsilon$-transition) going to another state that represents their purpose.

NFA traversal in this context occurs as follows:

* **match transitions**: if current state corresponds to a character in the alphabet and the current character in the text matches it, the automaton can transition from it, i.e. consume the character
* **$\epsilon$-transitions**: if no match is made in the pattern, any transition can be taken from a metacharacter, so called for effectively matching the empty string $\epsilon$

The traversal of the NFA is handled in the following manner:

1. **at the start state**: find all set of states reachable via $\epsilon$ transitions
2. consume pattern character if there's a match in one of the possible states
3. **from each match state**:
    1. add set of states reachable via match transitions
    2. add set of states reachable via $\epsilon$ transitions
4. repeat at 2

As the text input is fed to the NFA, on input character the following conditions can arise:

* **set of states contains accept state**: the NFA therefore _accepts_ the text, i.e. there was a match
* **set of states doesn't contain the accept state**: feed it the next character
* **the end of the text has been reached**: there was no match

The NFA is simply represented by the pattern string and a digraph representing the $\epsilon$-transitions.

### Match Checking {#regex-match-checking}

From this information, it is possible to create an algorithm that determines whether a regular expression matches the provided text. Reachability is determined by a Directed DFS implementation [^directed_dfs]. This is straightforward because the DFS would only operate on the digraph, which only represents $\epsilon$-transitions.

First, the set of states reachable via $\epsilon$-transitions from the start state are collected:

~~~ {lang="java"}
boolean match(String text) {
  Bag<Integer> pc = new Bag<Integer>();
  DirectedDFS dfs = new DirectedDFS(G, 0);

  for (int v = 0; v < G.V(); v++)
    if (dfs.marked(v)) pc.add(v);

  for (int i = 0; i < text.length(); i++) {
    Bag<Integer> matches = new Bag<Integer>();
~~~

As the text is fed into the NFA one character at a time, the set of reachable states is checked for a match with the current character. For each match, its next state is added to the collection of matches representing the set of states reachable from the current state(s).

~~~ {lang="java"}
    for (int v : pc)
      if (v < M && re[v] == text.charAt(i) || re[v] == '.')
        matches.add(v + 1);
~~~

Each of the states reachable via $\epsilon$-transitions from each of the states collected are added to the collection:

~~~ {lang="java"}
    pc = new Bag<Integer>();
    dfs = new DirectedDFS(G, matches);

    for (int v = 0; v < G.V(); v++)
      if (dfs.marked(v))
        pc.add(v);
  }
~~~

Once the entire text has been consumed, the final iteration of the above loop would leave the final set of reachable states intact. If this set contains the final, _accept_ state, then the NFA accepts the text. Otherwise, there wasn't a match.

~~~ {lang="java" text="regular expression matching"}
  for (int v : pc)
    if (v == M)
      return true;

  return false;
}
~~~

### NFA Construction {#regex-nfa-construction}

The construction of the NFA is accomplished similar to how Djikstra's [shunting-yard algorithm](http://en.wikipedia.org/wiki/Shunting-yard_algorithm) works for evaluating mathematical expressions in infix notation by using two stacks: one for operators and another for values.

In this context, a stack is maintained for the operators and a digraph the size of the length of the pattern plus one (to account for the accept state) is maintained to represent the NFA's $\epsilon$-transitions. _Concatenation_ is already handled implicitly by nature of how the pattern is stored.

~~~ {lang="java"}
Digraph NFA(String regex) {
  Stack<Integer> ops = new Stack<Integer>();
  re = regex.toCharArray();
  M = re.length();
  G = new Digraph(M + 1); // +1 for accept state

  for (int i = 0; i < M; i++) {
    int lp = i;
~~~

For _parentheses_ and _or expressions_, the position of the `(` or `|` is pushed.

~~~ {lang="java"}
    if (re[i] == '(' || re[i] == '|')
      ops.push(i);
~~~

If a `)` is encountered and it signified the end of an _or expression_, then the appropriate edges must be created. A regex `(A | B)` is handled by adding two $\epsilon$-transitions: one from the `(` to the `B` and the other from the `|` to the `)`. Push the position of the `|` (having previously pushed the `(`).

~~~ {lang="java"}
    else if (re[i] == ')') {
      int or = ops.pop();

      if (re[or] == '|') {
        lp = ops.pop();
        G.addEdge(lp, or + 1);
        G.addEdge(or, i);
      } else lp = or;
    }
~~~

_Closures_ are detected by looking ahead of the current state (if possible). If one is found, then an edge is created to the `*` and another is created from the `*` to the current state.

~~~ {lang="java"}
    if (i < M - 1 && re[i + 1] == '*') {
      G.addEdge(lp, i + 1);
      G.addEdge(i + 1, lp);
    }
~~~

Finally, `)`, `*`, and `)` each also have an $\epsilon$-transition leading to the next state in the pattern.

~~~ {lang="java" text="NFA construction"}
    if (re[i] == '(' || re[i] == '*' || re[i] == ')')
      G.addEdge(i, i + 1);
  }

  return G;
}
~~~

## Data Compression

Universally good lossless data compression is impossible because, for example, it would mean that data could be compressed over and over again until eventually reaching a compressed length of 0. Instead, lossless compression aims to exploit the known structure of the target data for the best compression ratio.

### Run-Length Encoding

Run-Length Encoding (RLE) is a classic method of encryption that replaces repeat occurrences of characters with their repeat count. For example, the following consists of 15 zeros, 7 ones, 7 zeros, and 11 ones:

~~~
0000000000000001111111000000011111111111
~~~

With RLE, given a count size of 4 bits, it can be replaced with 15 (`1111`), 7 (`0111`), 7, and 11 (`1011`):

~~~
1111011101111011
~~~

In general, each count is encoded in one byte. If a run of repeated characters is greater than the maximum size representable by the count size (i.e. 255), the first 255 is encoded, then a zero-lengthed run of the alternate character, then again the next chunk of the original long repeated character.

### Huffman Compression

Huffman Compression exploits the frequency of individual characters. For example, in `ABRACADABRA!`, the most frequently occurring character `A` could be represented by `0`, `B` by `1`, `R` with `00`, `C` with `01`, `D` with `10`, and `!` with `11`, resulting in `01000010100100011`.

The problem with the above representation is that the interpretation of the above encoded data is ambiguous because the characters aren't delimited and some of the characters' codes are prefixes of others. For example, `A` is `0`, `B` is `1`, and `C` is `01`, so when `01` is read, it isn't clear if it is meant to be interpreted as `AB` or `C`.

Instead, a property known as _prefix-free code_ is enforced for the encodings, which prevents any code from being a prefix of another. In the above, a possible representation could be `A` with `0`, `B` with `1111`, `C` with `110`, `D` with `100`, `R` with `1110`, and `!` with `101`, yielding the encoding `011111110011001000111111100101`. While this is a slightly longer representation, it is unambiguous.

Prefix-free codes can be easily represented using a trie where left links are `0` and right links are `1`. Leave nodes contain the character represented by the bits of the edges of the path used to reach them. Each node in the trie has an associated frequency (used during construction) and character (for leaves).

Constructing the trie consists of first creating a forest of 1-node trees---all of which are leaves---one for each character in the input, with its frequency variable set to the number of times it appears in the input. The trie is then constructed from the bottom-up by merging the two least frequent characters (nodes) with a new parent node with its frequency set to their sum. This is greatly facilitated by a priority queue:

~~~ {lang="java" text="trie construction"}
Node buildTrie(int[] freq) {
  MinPQ<Node> pq = new MinPQ<Node>();

  for (char c = 0; c < R; c++)
    if (freq[c] > 0)
      pq.insert(new Node(c, freq[c], null, null));

  while (pq.size() > 1) {
    Node x = pq.delMin();
    Node y = pq.delMin();

    Node parent = new Node('\0', x.freq + y.freq, x, y);
    pq.insert(parent);
  }

  return pq.delMin();
}
~~~

The way in which the trie is constructed ensures that the more frequent characters (nodes) are closer to the root, and as a result are encoded with fewer bits.

One thing to recognize is that the trie has to somehow be encoded in the compressed data so that it can then be decompressed. The trie can be encoded in a bitstream by performing pre-order traversal (root - left - right), and at each node:

* if the node is a leaf, output a `1` and then the binary representation of the character
* otherwise, write a `0` then recurse on the left node then the right (i.e. pre-order)

Reading the trie into an actual trie structure is just as straightforward, where the type of node to create is determined by the leading bit.

_Decompression_ consists of simply traversing the trie as each bit is read. If a leaf is encountered, output the character and restart traversal from the root.

_Compression_ requires the existence of a code table mapping each character to the appropriate code. This table is derived from the trie by traversing the trie, keeping track of the bitstring along its path, and when a leaf node is encountered, the bitstring is associated with that character in the code table. Compression then simply requires looking up each character from the data in the code table and outputting the appropriate code.

### LZW Compression

LZW _compression_ works by having variable-length code words for fixed-length input patterns. Code words are kept in a trie as with Huffman compression. A code counter is maintained and incremented after each new code is added to the trie. The initial trie is constructed from the alphabet, one node being created from each character with its code stored within. The rest of the trie is constructed as the input is read:

1. the longest prefix of the input present in the trie is found and its value output to the compressed stream
2. if the length of the prefix is shorter than the remaining input length, a new code is added for the string consisting of the prefix concatenated with the next character in the input stream. This is a simple operation, essentially done by adding a new node with the new code to the node at which the prefix ends

~~~ {lang="java" text="LZW compression"}
void compress(String input) {
  TST<Integer> st = new TST<Integer>();

  for (int i = 0; i < R; i++)
    st.put("" + (char)i, i);

  int code = R + 1;

  while (input.length() > 0) {
    String s = st.longestPrefixOf(input);
    BinaryStdOut.write(st.get(s), W);
    int t = s.length();

    if (t < input.length() && code < L)
      st.put(input.substring(0, t + 1), code++);

    input = input.substring(t);
  }

  BinaryStdOut.write(R, W);
  BinaryStdOut.close();
}
~~~

_Decompression_ depends on a table indexed by codes and valued by strings (prefixes), this is constructed from the alphabet. The code of the first character in the input stream is read and its associated string is retrieved from the table. Decompression continues until the EOF character is encountered, on each iteration doing the following:

1. the string associated with the code is output
2. another code is read, break if EOF
3. the string associated with the code is retrieved
4. if the current code counter is equal to the next (lookahead) code---therefore making it impossible to read what the next code's first character is, since it's in the process of being constructed---then first character of the string currently being constructed is appended to its end, following basic logic
5. a new code is added to the table at an incremented code corresponding to the previously read string concatenated with the first character of the current string's first character

~~~ {lang="java" text="LZW decompression"}
void decompress() {
  String[] st = new String[L];
  int codeword = BinaryStdIn.readInt(W);
  int i;

  for (i = 0; i < R; i++)
    st[i] = "" + (char)i;

  st[i++] = " ";

  String val = st[codeword];

  while (true) {
    BinaryStdOut.write(val);
    codeword = BinaryStdIn.readInt(W);

    if (codeword == R) break;

    String s = st[codeword];

    if (i == codeword)
      s = val + val.charAt(0);

    if (i < L)
      st[i++] = val + s.charAt(0);

    val = s;
  }

  BinaryStdOut.close();
}
~~~

# Context

Miscellaneous algorithms follow.

## B-Trees

A B-Trees of order $M$ is a tree consisting of internal and external $k$-nodes each consisting of $k$ keys where $2 \leq k \leq M - 1$ at the root and $M/2 \leq k \leq M - 1$ at every other node. _Internal nodes_ contain copies of keys, where every key is greater than or equal to its parent node's associated key, but not greater than the parent node's next largest key. _External nodes_ are the leaves of the tree that associate keys with data. A _sentinel key_ is created to be less than all other keys and is the first key in the root node.

### Insertion {#b-tree-insertion}

To insert a key, the tree is recursively descended by following the link pertaining to the interval upon which the inserted key falls until an external node is reached. The tree is balanced on the way up the tree after the recursive call. If a node is full it is split into two $M/2$-nodes and attached to a parent 2-node (if at the root) or a $(k + 1)$-node where $k$ was the original size of the full node's parent. Whenever a node is split, the smallest key in the new node (or both smallest keys from both nodes if at the root) is inserted into the parent node.

~~~ {lang="java" text="B-Tree insertion"}
void add(Key key) {
  add(root, key);

  if (root.isFull()) {
    Page left = root;
    Page right = root.split();

    root = new Page();
    root.add(left);
    root.add(right);
  }
}

void add(Page h, Key key) {
  if (h.isExternal()) { h.add(key); return; }

  Page next = h.next(key);
  add(next, key);

  if (next.isFull())
    h.add(next.split());

  next.close();
}
~~~

## Suffix Arrays

Suffix arrays are arrays of suffixes of a given text which help with procedures such as finding the longest repeated substring in some text.

~~~ {lang="java" text="suffix array"}
class SuffixArray {
  private final String[] suffixes;
  private final int N;

  public SuffixArray(String s) {
    N = s.length();
    suffixes = new String[N];
    for (int i = 0; i < N; i++) suffixes[i] = s.substring(i);
    Array.sort(suffixes);
  }

  public int lcp(String s, String t) {
    int N = Math.min(s.length(), t.length());
    for (int i = 0; i < N; i++) if (s.charAt(i) != t.charAt(i)) return i;
    return N;
  }

  public int lcp(int i) { return lcp(suffixes[i], suffixes[i - 1]); }
  public int rank(String key) { /* binary search */ }
  public String select(int i) { return suffixes[i]; }
  public int index(int i) { return N - suffixes[i].length(); }
}
~~~

Using this suffix array class, the longest repeated substring can be found efficiently:

~~~ {lang="java" text="longest repeated substring"}
void main(String[] args) {
  String text = StdIn.readAll();
  int N = text.length();
  SuffixArray sa = new SuffixArray(text);
  String lrs = "";

  for (int i = 1; i < N; i++) {
    int length = sa.lcp(i);
    if (length > lrs.length())
      lrs = sa.select(i).substring(0, length);
  }

  StdOut.println(lrs);
}
~~~

## Network-Flow

The Network-Flow problem concerns itself with finding the settings in a network that maximize the flow from source to sink. At each junction in the network there are switches that control the flow's distribution between it's outgoing edges. The problem can be modeled as an edge-weighted digraph with a single source and sink pair, where the weights correspond to the capacity of the edge.

An _st-flow_ is a set of edge flows for the network that represent the distribution of flow values for each edge. An _st-flow value_ is the sink's inflow. The network-flow problem can be described as finding an st-flow such that no other st-flow has a larger st-flow value. Such an st-flow can be referred to as a _maxflow_.

### Ford-Fulkerson

The Ford-Fulkerson algorithm, also known as the _augmenting-path algorithm_, works by increasing flows incrementally along paths from the source to the sink. It works by considering that each edge consists of a _forward edge_ and a _backward edge_.

A path is found in the network in which there are no full forward edges and no empty backward edges. The flow of the network can then be increased by an amount $X$, by increasing flow in forward edges by $X$ and decreasing flow in backward edges by $X$ in this path. The value of $X$ is the minimum of the unused capacities in forward edges and backward edges in the path. This path that can be used to increase flow in the network is known as an _augmenting path_.

Following from this, the maxflow can be found by starting with zero flow everywhere and gradually increase the flow along any augmenting path from source to sink until there are no more augmenting paths.

A _residual network_ has the same vertices as the original. For every edge in the original network: if its flow is positive, an edge should be created in the residual with an opposite direction and capacity equal to the flow. Also, if its flow is less than its capacity, an edge should be added in the same direction as the original edge with capacity equal to the difference between its capacity and flow.

This means that if, in the original, an edge's flow is zero then there'll only be one edge (in the same direction) and if instead the flow is full there'll only be one edge (in the opposite direction).

The residual network is useful because any path in it from source to sink corresponds directly to an augmenting path in the original network. As an augmenting path's flow is incremented, when an edge in the path becomes full or empty, it corresponds to changing direction or disappearing in the residual network.

The _shortest-augmenting-path_ method finds the maxflow by finding an augmenting path using BFS and incrementing it.

## NP-Complete Problems

* **clique problem**: find complete subgraphs, or _cliques_, in a graph
* **vertex cover**: find a set of vertices in a graph such that each edge in the graph is incident to at least one vertex in the set
* **travelling salesman problem**: find the shortest possible path cycle that visits every vertex in a graph
* **graph coloring**: color every vertex--edge in a graph such that no two adjacent vertices--edges have the same color
* **knapsack**: given a set of items with different values and a container of a maximum capacity, find the combination of items that fits in the container and has the largest total value.

*[BFS]: Breadth-First Search
*[BST]: Binary Search Trees
*[DAG]: Directed Acyclic Graph
*[DFA]: Deterministic Finite-State Automaton
*[DFS]: Depth-First Search
*[GC]: Garbage Collector
*[KMP]: Knuth-Morris-Pratt
*[LSD]: Least Significant Digit
*[MSD]: Most Significant Digit
*[MST]: Minimum Spanning Tree
*[NFA]: Non-Deterministic Finite-State Automaton
*[RLE]: Run-Length Encoding
*[SPT]: Shortest-Paths Tree
*[TST]: Ternary Search Trees

[^mit]: [MIT CSAIL 6.861: Advanced Data Structures](http://courses.csail.mit.edu/6.851/spring12/)
[^umd]: [University of Maryland, CMSC 420](http://www.cs.umd.edu/class/spring2008/cmsc420/)
[^umgd]: [Universität Magdeburg, Geometric Datastructures](http://wwwisg.cs.uni-magdeburg.de/ag/lehre/WS1011/GDS/)
[^sorting_improvements]: Skiena p. 109, § 4.3
[^rbtree_case_merge]: The [Wikipedia implementation's](http://en.wikipedia.org/wiki/Red%E2%80%93black_tree#Removal) 6 cases were condensed to 4 as was done in the Linux kernel [Red-Black tree implementation](https://github.com/torvalds/linux/blob/master/lib/rbtree.c). Cases 1 and 2 were merged since case 1 is simply a check to see if the node is the root. Cases 3 and 4 were merged because they handle the same scenario, with case 4 simply being a handler for a special case of 3.
[^directed_dfs]: Sedgewick p. 570, algorithm 4.4
