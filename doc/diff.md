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

# Patience

- ["Patience Diff Advantages"](https://bramcohen.livejournal.com/73318.html).
- ["Patience Diff, a brief summary"](https://alfedenzo.livejournal.com/170301.html).
- James Coglan's blog series:
  - ["The patience diff algorithm"](https://blog.jcoglan.com/2017/09/19/the-patience-diff-algorithm/).
  - ["Implementing patience diff"](https://blog.jcoglan.com/2017/09/28/implementing-patience-diff/).

# Histogram

- StackOverflow question, ["What's the difference between `git diff --patience` and `git diff --histogram`?"](https://stackoverflow.com/questions/32365271/whats-the-difference-between-git-diff-patience-and-git-diff-histogram).
- Original contribution to Git: [8c912eea94, "teach --histogram to diff"](https://github.com/git/git/commit/8c912eea94a2138e8bc608f7c390eb0b313effb0).
- Git implementation, [`xhistogram.c`](https://github.com/git/git/blob/master/xdiff/xhistogram.c).
- JGit implementation, [`HistogramDiff.java`](https://github.com/spearce/jgit/blob/master/org.eclipse.jgit/src/org/eclipse/jgit/diff/HistogramDiff.java).
- ["How different are different diff algorithms in Git?"](https://link.springer.com/article/10.1007/s10664-019-09772-z).
