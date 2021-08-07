/*
 * ```
 * cc hash.c
 * ./a.out
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
    printf(HASH_FMT "\n", hash("hetairas"));
    printf(HASH_FMT "\n", hash("mentioner"));
    printf(HASH_FMT "\n", hash("haggadot"));
    printf(HASH_FMT "\n", hash("loathsomenesses"));

    // djb2 (32)    djb2a (32)   djb2 (64)              djb2a (64)
    //
    // 3940087062   254653076    7572452189730070       7569463502025364
    // 3940087062   1605867752   249897947298854166     249788221435123944
    // 2567235620   379918194    7572446521911332       7569467922257778
    // 3968138338   379918194    2706329446310542434    10696802194950526834
    //
    // u64 Rust implementation does indeed agree with the last two columns.
}
