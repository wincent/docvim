/*
 * ```
 * cc collisions.c
 * cat words.txt | ./a.out | sort -k 2 | uniq -D -f 1 | sed '$!N;s/\n/ /'
 * ```
 */

#include <stdio.h>
#include <ctype.h>

#define DJB2A
#define DJB2A_BIG

#ifdef DJB2A_BIG
#include <stdint.h>
#define DJB2A_SIZE uint64_t
#define HASH_FMT "%lu"
#else
#define DJB2A_SIZE unsigned int
#define HASH_FMT "%u"
#endif

DJB2A_SIZE hash(const char *word)
{
    DJB2A_SIZE hash = 5381;
    int c;
    while ((c = *word++)) {
#ifdef DJB2A
        hash = ((hash << 5) + hash) ^ c;
#else
        hash = ((hash << 5) + hash) + c;
#endif
    }
    return hash;
}

int main() {
    char word[1024];
    FILE *f = stdin;
    while (fscanf(f, "%1023s", word) == 1) {
        printf("%s " HASH_FMT "\n", word, hash(word));
    }
}
