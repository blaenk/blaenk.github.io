---
title: Algorithms Refresher 
published: July 15, 2013
excerpt: Notes on common algorithms
tags: Algorithms, Notes
icon: lightbulb
comments: off
---

What follows are some notes on algorithms I've been reviewing from [Algorithms]() by Robert Sedgewick and Kevin Wayne as well as [The Algorithm Design Manual]() by Steven S. Skiena. I wanted to write some notes on the material so that I could easily look back on it, but mainly so that I can be sure I understand the material -- since I have to understand it to explain it.

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

For example, selection sort backed by a priority queue or balanced binary tree can help to speed up the operation of finding the smallest element in the unsorted region. Instead of being linear, the operation would be $\lg(n)$. Given that this is done at every element in the sequence, of which there are $N$, this means that selection sort backed by such a structure can be improved from $O(n^2)$ to $O(n\lg(n))$ [^data_structures].

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
Worst  $O(n^2)$
Best   $\Theta(n)$

</div>

This is a **stable** algorithm that is still pretty straightforward but somewhat improves upon selection sort if the array is already sorted or if it's nearly sorted.

It operates as follows:

1. go through the entire sequence until an element is found which is **smaller than the previous element**
2. swap the smaller element with the one on the left until the element to its left is no longer larger than itself
3. repeat until the end of the sequence

The benefit of insertion sort is that if the sequence is already sorted then the algorithm operates in linear time. Similarly, if the sequence is nearly sorted, the algorithm will perform better than the worst case.

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

<img class="center" src="/images/algorithms/partition.png">

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

One problem with quick sort as it is implemented above is that items with keys equal to that of the partition item are nonetheless swapped, unnecessarily. Three-way partitioning aims to resolve this by partitioning into three separate sub-arrays, the middle of which corresponds to those items with keys equal to the partition point. E. W. Dijkstra popularized this as the _Dutch National Flag_ problem.

<img class="center" src="/images/algorithms/3waypartition.png">

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

# Searching

[^data_structures]: Skiena p. 109, § 4.3