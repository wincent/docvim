# Notes on hash function selection

## Criteria

- **Speed:** We want something faster than the default hash function because we control all the inputs and don't wish to pay the cost of a DoS-resitant hash.
- **Simplicity:** We want something simple enough for us to actually implement.
- **Collision-resitance:** We want something good enough for hash table use, but we don't need cryptographic hash.
- **Good for short strings:** We'll mostly be hashing source code (or source-code-like) things, so that means relatively short strings of 0 to 80 characters in length.

Reviewing the resources (see below), we can see that there are a bunch of complicated trade-offs to be evaluated, and different commentators arrive at totally contradictory conclusions based on evaluation of the same candidate functions.

## Resources

> If you just want to have a good hash function, and cannot wait, djb2 is one of the best string hash functions i know. it has excellent distribution and speed on many different sets of keys and table sizes. you are not likely to do better with one of the "well known" functions such as PJW, K&R[1], etc.
>
> this algorithm (k=33) was first reported by dan bernstein many years ago in comp.lang.c. another version of this algorithm (now favored by bernstein) uses xor: `hash(i) = hash(i - 1) * 33 ^ str[i];` the magic of number 33 (why it works better than many other constants, prime or not) has never been adequately explained.
>
>     unsigned long
>     hash(unsigned char *str)
>     {
>         unsigned long hash = 5381;
>         int c;
>
>         while (c = *str++)
>             hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
>
>         return hash;
>     }

Source: ["Hash Functions"](http://www.cse.yorku.ca/~oz/hash.html)

On the source of the constants 33 and 5381:

> Again, practically any good multiplier works. I think you're worrying about the fact that 31c + d doesn't cover any reasonable range of hash values if c and d are between 0 and 255. That's why, when I discovered the 33 hash function and started using it in my compressors, I started with a hash value of 5381. I think you'll find that this does just as well as a 261 multiplier.

["hash function for mac needed (comp.lang.c, Jun 25, 1991)](https://groups.google.com/g/comp.lang.c/c/lSKWXiuNOAk/m/zstZ3SRhCjgJ)

See also ["Reason for 5381 number in DJB hash function?"](https://stackoverflow.com/a/13809282) and ["Why are 5381 and 33 so important in the djb2 algorithm?"](https://stackoverflow.com/q/1579721) on Stack Overflow.

> Compute the hash value of the key in the record. The hash value modulo 256 is the number of a hash table. The hash value divided by 256, modulo the length of that table, is a slot number. Probe that slot, the next higher slot, and so on, until you find the record or run into an empty slot.
>
> The cdb hash function is ``h = ((h << 5) + h) ^ c'', with a starting hash of 5381.

Source: ["A structure for constant databases"](http://cr.yp.to/cdb/cdb.txt)

For more on the difference between "djb2" (which uses an addition) and "djb2a" (which uses an XOR), see ["Which hashing algorithm is best for uniqueness and speed?"](https://softwareengineering.stackexchange.com/a/145633)

> # Superior Simple Algorithms
>
> The FNV, Sedgewicks, SBDM and K&R algorithms were designed to be as simple and as fast as possible. They all produce no short collisions and their distributions range from good to bad for shorter strings (if this matters). FNV-1a seems to be the best choice and it seems to be respected, popular and has a good theoretical basis
>
> # Inferior Simple Algorithms
>
> The Partow, Sobel, Bernstein and Knuth algorithms were also designed to be simple and fast, but they have terrible behaviour and should not be considered practically useful. Use one of the superior simple algorithms instead.

Source: ["Hashing Short Strings - Overview"](http://www.orthogonal.com.au/computers/hashstrings/)

>  Even if those worse hash functions will lead to more collisions, the overall speed advantage and inline-ability beats the slightly worse quality. See e.g. [A Seven-Dimensional Analysis of Hashing Methods and its Implications on Query Processing](https://bigdata.uni-saarland.de/publications/p249-richter.pdf) for a concise overview of the best hash table strategies, confirming that the simplest Mult hashing (bernstein, FNV*, x17, sdbm) always beat "better" hash functions (Tabulation, Murmur, Farm, ...) when used in a hash table.
>
> The fast hash functions tested here are recommendable as fast for file digests and maybe bigger databases, but not for 32bit hash tables. The "Quality problems" lead to less uniform distribution, i.e. more collisions and worse performance, but are rarely related to real security attacks, just the 2nd sanity AppendZeroes test against \0 invariance is security relevant.

Source: ["SMhasher"](https://github.com/rurban/smhasher) hash comparisons.

## Conclusion

As noted in the introduction, this is a complex topic with a lot of trade-offs (speaking of which, see the decision graph at the end of ["A Seven-Dimensional Analysis of Hashing Methods and its Implications on Query Processing"](https://bigdata.uni-saarland.de/publications/p249-richter.pdf)).

In the end, I'm going with djb2a (although djb2) would be fine too because it is:

- Collision-resistant enough for use in a hash table that won't be subjected to DoS attacks.
- Simple enough that I can implement it.
- Adequate for short strings.
- Fast enough, even without advanced SSE2 tricks (my very unscientific testing on short strings shows that it gives us about a 17% speed boost over Rust's default hash, which evidently is sped up because it probably _does_ use SSE2-style tricks _but_ must be DoS resistant).
- Easily switched out for another hash function in the future.

## See also

- ["FNV Hash"](http://www.isthe.com/chongo/tech/comp/fnv/).
- ["What is the best 32bit hash function for short strings (tag names)?"](https://stackoverflow.com/q/2351087).
