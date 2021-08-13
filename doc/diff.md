# Myers

- Original paper, ["An O(ND) Difference Algorithm and Its Variations"](http://www.xmailserver.org/diff2.pdf), by Eugene Myers.
- ["Myers Diff Algorithm - Code & Interactive Visualization"](https://blog.robertelder.org/diff-algorithm/), by Robert Elder.
- Nicholas Butler's blog series (this doesn't rank as highly in Google's search results, but I think it's a clearer explanation than the one offered in the top hit, from James Coglan, below):
  - ["Myers' Diff Algorithm : The basic greedy algorithm"](http://simplygenius.net/Article/DiffTutorial1).
  - ["Myers' Diff Algorithm : The linear space refinement"](http://simplygenius.net/Article/DiffTutorial2).
- James Coglan's blog series:
  - ["The Myers diff algorithm: part 1"](https://blog.jcoglan.com/2017/02/12/the-myers-diff-algorithm-part-1/)
  - ["The Myers diff algorithm: part 2"](https://blog.jcoglan.com/2017/02/15/the-myers-diff-algorithm-part-2/)
  - ["The Myers diff algorithm: part 3"](https://blog.jcoglan.com/2017/02/17/the-myers-diff-algorithm-part-3/)
  - ["Myers diff in linear space: theory"](https://blog.jcoglan.com/2017/03/22/myers-diff-in-linear-space-theory/)
  - ["Myers diff in linear space: implementation"](https://blog.jcoglan.com/2017/04/25/myers-diff-in-linear-space-implementation/)
- JGit implementation, [`MyersDiff.java`](https://github.com/spearce/jgit/blob/master/org.eclipse.jgit/src/org/eclipse/jgit/diff/MyersDiff.java).
- Binary implementation with some neat optimizations, [`HFByteArrayEditScript.m`](https://github.com/HexFiend/HexFiend/blob/master/framework/sources/HFByteArrayEditScript.m).

# Patience

- ["Patience Diff Advantages"](https://bramcohen.livejournal.com/73318.html).
- ["Patience Diff, a brief summary"](https://alfedenzo.livejournal.com/170301.html).
- James Coglan's blog series:
  - ["The patience diff algorithm"](https://blog.jcoglan.com/2017/09/19/the-patience-diff-algorithm/).
  - ["Implementing patience diff"](https://blog.jcoglan.com/2017/09/28/implementing-patience-diff/).

# Histogram

## Resources

- StackOverflow question, ["What's the difference between `git diff --patience` and `git diff --histogram`?"](https://stackoverflow.com/questions/32365271/whats-the-difference-between-git-diff-patience-and-git-diff-histogram).
- Original contribution to Git: [8c912eea94, "teach --histogram to diff"](https://github.com/git/git/commit/8c912eea94a2138e8bc608f7c390eb0b313effb0).
- Git implementation, [`xhistogram.c`](https://github.com/git/git/blob/master/xdiff/xhistogram.c).
- JGit implementation, [`HistogramDiff.java`](https://github.com/spearce/jgit/blob/master/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiff.java).
- ["How different are different diff algorithms in Git?"](https://link.springer.com/article/10.1007/s10664-019-09772-z).

## Analysis

The two "reference" implementations (from JGit and Git) that I looked at to guide my implementation here have some oddities. In the following analysis I'll link to parts of those implementations to support my observations.

The algorithm consists of two phases:

1. [Scanning](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L183-L231) over sequence `a`, populating a "histogram" that records the frequency with which elements appear in the sequence. For each element, a hash is computed and used to index into a hash table. If the slot is already occupied and the new element is identical, the element is added to extend the chain of identical elements. Conversely, if the slot is empty or the element does not match, a new chain is started in the next available hash table slot.
2. [Scanning](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L177-L178) over sequence `b`, [looking at the corresponding element in the histogram](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L233-L307) (if any) identifying the one which is [most infrequent _or_ which corresponds to the largest matching region](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L275) in the two sequences. This best found region is used as a split region around which to recursively apply the algorithm. For example, if the string "foo" appears only once in `a`, it may be selected as a split region. Alternatively, if the string "bar" occurs _more_ frequently _but_ has a series of matching lines before and after that make it the longest matching region, we may use that instead. Because `b` is scanned from beginning to end, the last-seen "best match" wins in the event of a tie; for example, in the following sequences, where "foo" is the rarest string in `a` (with one occurrence) and "bar" is the second-rarest string (with two occurences, although only first occurence shown), "foo" will initially be selected as best matching region, later to be superseded by "bar" and the matching lines that immediately preceed and follow it, because that region is larger:

  ```
  sequence A:                                   sequence B:
  ===========                                   ===========

                                                different
  some                                          stuff
  random                                        on
  words                                         this
  here                                          side

  foo             <---------------------------> foo

  now                                           nothing
  we                                            interesting
  continue
  on
  for
  a
  bit

  common          <---------------------------> common
  lines           <---------------------------> lines
  and             <---------------------------> and
  things          <---------------------------> things
  bar             <---------------------------> bar
  region          <---------------------------> region
  continues       <---------------------------> continues

  diverging                                     going
  henceforth                                    elsewhere
  ...                                           ...
  ```

## Collisions

Hash collisions of the full 64-bit hash are unlikely (see [commit 4f40fcedb9fb](https://github.com/wincent/docvim/commit/4f40fcedb9fb8769adf6eee9e753c1cfb616bf64) but we do truncate the hash value to map it onto hash table slots. Git [rounds up the length of sequence `a` to the nearest power of two](https://github.com/git/git/blob/5d213e46bb7b880238ff5ea3914e940a50ae9369/xdiff/xhistogram.c#L272-L277) to deterimine the number of hash table slots. For example, a sequence with 200 lines would be mapped onto a hash table with 256 slots. Given [the birthday paradox](https://en.wikipedia.org/wiki/Birthday_problem), we can reasonably expect some modest number of collisions as a matter of routine.

When two elements hash to the same slot during scanning and actually match, [the algorithm prepends the new element before the first, to form (or extend) a chain](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L195-L206). If the hash codes match but the elements are non-identical, the algorithm checks [the next element in the chain](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L208); given that we know the next element(s) in the chain must be identical, by definition, we know the algorithm must eventually reach the end of the non-matching chain and [fall through to create a new chain](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L226-L228).

The first anomaly to note is this: consider what happens if we have multiple instances of "foo" that happen to hash into slot `N` of the table, and multiple instances of "bar" that also hash into slot `N`. As we encounter each new instance of "foo", we will end up prepending it to the chain in slot `N`. When we see "bar", we will compare it to every element in the chain at `N` and see that it doesn't match, so we create a new chain at slot `N`, [overwriting the previous occupant of the slot](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L228). Subsequent instances of "bar" will get prepended to this new chain, but if we see "foo" again, it will end up creating a new chain that overwrites the "bar" chain in slot `N`. This means that we wind up with some number of records that cannot be directly reached by hash table lookup, because no chain reachable from the table contains them.

Unless I'm mistaken, this seems to be a bug, and it appears to have been carried over faithfully to [the Git implementation](https://github.com/git/git/blob/5d213e46bb7b880238ff5ea3914e940a50ae9369/xdiff/xhistogram.c#L115-L151) as well. Experience would suggest that I probably _am_ mistaken, but I haven't been able to convince myself that the bug isn't real yet.

Consider how this plays out in the second phase of the algorithm, then, while iterating over `b`. Here again, [the algorithm looks up a hash table slot by hash code](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L235). If the slot contains a chain, [the algorithm visits each record in the chain](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L236). It [skips any record with a chain length longer than the one used in the best matching region we've seen so far](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L240). Additionally, it [skips any record that doesn't exactly match the item from `b`](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L247-L248). So, in the case of perfectly matching elements, the algorithm soon hits [the `TRY_LOCATIONS` loop](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L251), whose purpose is to [look for matching lines before and after the items](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L258-L271) and [see whether the matching region thus constructed is the largest seen so far](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L275).

But in the case of hash collisions, [the algorithm will `continue`](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L248) until it gets to the end of the chain, at which point [it will give up](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L306) and has no way of finding the orphaned records not reachable from a chain. In other words, it will effectively disregard the item at that point in sequence `b` and try again starting at the next line. This again seems like a bug, but I might be mistaken. If it is a bug, it may have gone undetected for so long because the behavior degrades quite gracefully even in the presence of the bug: because the purpose of the algorithm is to find optimal splitting points around which to apply itself recursively, the worst that can happen is that the splitting points are not chosen optimally; this doesn't stop a correct diff from being generated. The Myers diff algorithm is always applied as a fallback anyway, and that will produce valid diffs even if applied to suboptimally derived subproblems. Furthermore, although collisions may occur, it's possible they don't occur often enough to produce anomalies that would be noticeable by humans.

Now, might this analysis be wrong? It certainly seems that [the intent](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiff.java#L72-L77) of the algorithm is very different from the behavior I think I'm seeing:

> To prevent the algorithm from having an `O(N^2)` running time, an upper limit on the number of unique elements in a histogram bucket is configured by `setMaxChainLength(int)`. If sequence A has more than this many elements that hash into the same hash bucket, the algorithm passes the region to `setFallbackAlgorithm(DiffAlgorithm)`. If no fallback algorithm is configured, the region is emitted as a replace edit.

That sure makes it sound like colliding elements are _supposed_ to be stored in the same chain, with the overall length should be capped at 64, but that's not what the code looks to be doing.

## Performance

[The `recIdx` in JGit](https://github.com/spearce/jgit/blob/1513a5632dcaf8c6e2d6998427087e11ba35566d/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiffIndex.java#L136) and the [corresponding `line_map` in the Git implementation](https://github.com/git/git/blob/5d213e46bb7b880238ff5ea3914e940a50ae9369/xdiff/xhistogram.c#L57) don't seem to be necessary, because scanning proceeds orderly in reverse and a new record is created for each item. For any given sequence, the line map is going to resemble this (using Rust syntax):

```
[Some(6), Some(5), Some(4), Some(3), Some(2), Some(1), Some(0)]
```

This means that to locate a record based on its line's location within sequence `a`, we don't need to do a map lookup: we can just do `record index = sequence length - line index`.
