/*
  This is a reduced-functionality copy of Bob Jenkins' perfect hash algorithm.
  The intent is to compile CASE statements without ever causing a jump table
  to explode into a huge number of cases due to poor sxhash distribution.
  Using a perfect hash, if all symbols' hashes are distinct, then we can make
  a perfect table because the perfect hash will figure out which bits to extract
  from symbol-hash to discriminate N symbols using an N-way branch.
  Additionally we might perfectly map slot-name to metatdata for
  layout-slot-table and -struct-slot-map (which really should be combined)

  Behavioral changes:
  * Remove support for strings as keys, restrict keys to uint32
  * Drop support for "(A,B)" mode
  * Eliminate all modes of file input
  * Add a callable API that returns the calculation as a char*
  * Write to stdout instead of phash.c in command-line mode
  * Produce the hash function in either C or Lisp
  * Print failures to stderr instead of stdout

  Structural changes:
  * Rename source files and strip trailing whitespace
  * Discard recycle.c and lookupa.c
  * Convert from K&R C to ANSI C
  * Fix C style warnings
  * Fold standard.h into this file and eliminate its use by the C hash fun

  Generating a hash of fewer than 5 keys is temporarily disabled,
  pending some necessary repairs (too many warnings to deal with)

  Full sources are available at https://burtleburtle.net/bob/hash/perfect.html
*/

/*
------------------------------------------------------------------------------
perfect.c: code to generate code for a hash for perfect hashing.
(c) Bob Jenkins, September 1996, December 1999
You may use this code in any way you wish, and it is free.  No warranty.
I hereby place this in the public domain.
Source is http://burtleburtle.net/bob/c/perfect.c

This generates a minimal perfect hash function.  That means, given a
set of n keys, this determines a hash function that maps each of
those keys into a value in 0..n-1 with no collisions.

The perfect hash function first uses a normal hash function on the key
to determine (a,b) such that the pair (a,b) is distinct for all
keys, then it computes a^scramble[tab[b]] to get the final perfect hash.
tab[] is an array of 1-byte values and scramble[] is a 256-term array of
2-byte or 4-byte values.  If there are n keys, the length of tab[] is a
power of two between n/3 and n.

I found the idea of computing distinct (a,b) values in "Practical minimal
perfect hash functions for large databases", Fox, Heath, Chen, and Daoud,
Communications of the ACM, January 1992.  They found the idea in Chichelli
(CACM Jan 1980).  Beyond that, our methods differ.

The key is hashed to a pair (a,b) where a in 0..*alen*-1 and b in
0..*blen*-1.  A fast hash function determines both a and b
simultaneously.  Any decent hash function is likely to produce
hashes so that (a,b) is distinct for all pairs.  I try the hash
using different values of *salt* until all pairs are distinct.

The final hash is (a XOR scramble[tab[b]]).  *scramble* is a
predetermined mapping of 0..255 into 0..smax-1.  *tab* is an
array that we fill in in such a way as to make the hash perfect.

First we fill in all values of *tab* that are used by more than one
key.  We try all possible values for each position until one works.

This leaves m unmapped keys and m values that something could hash to.
If you treat unmapped keys as lefthand nodes and unused hash values
as righthand nodes, and draw a line connecting each key to each hash
value it could map to, you get a bipartite graph.  We attempt to
find a perfect matching in this graph.  If we succeed, we have
determined a perfect hash for the whole set of keys.

*scramble* is used because (a^tab[i]) clusters keys around *a*.
------------------------------------------------------------------------------
*/

/*
------------------------------------------------------------------------------
Standard definitions and types, Bob Jenkins
------------------------------------------------------------------------------
*/
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>

typedef  uint32_t  ub4;   /* unsigned 4-byte quantities */
#define UB4BITS 32
typedef  uint16_t  ub2;
#define UB2MAXVAL 0xffff
typedef  uint8_t   ub1;
#define UB1MAXVAL 0xff

#define bis(target,mask)  ((target) |=  (mask))
#define bic(target,mask)  ((target) &= ~(mask))
#define bit(target,mask)  ((target) &   (mask))

#define TRUE  1
#define FALSE 0

#include "perfecthash.h"

static int verbose = 0;

/*
------------------------------------------------------------------------------
Find the mapping that will produce a perfect hash
------------------------------------------------------------------------------
*/

/* return the ceiling of the log (base 2) of val */
static ub4 mylog2(ub4 val)
{
  ub4 i;
  for (i=0; ((ub4)1<<i) < val; ++i)
    ;
  return i;
}

/* compute p(x), where p is a permutation of 0..(1<<nbits)-1 */
/* permute(0)=0.  This is intended and useful. */
static ub4  permute(
    ub4 x,                                   /* input, a value in some range */
    ub4 nbits)                             /* input, number of bits in range */
{
  int i;
  int mask   = ((ub4)1<<nbits)-1;                                /* all ones */
  int const2 = 1+nbits/2;
  int const3 = 1+nbits/3;
  int const4 = 1+nbits/4;
  int const5 = 1+nbits/5;
  for (i=0; i<20; ++i)
  {
    x = (x+(x<<const2)) & mask;
    x = (x^(x>>const3));
    x = (x+(x<<const4)) & mask;
    x = (x^(x>>const5));
  }
  return x;
}

/* initialize scramble[] with distinct random values in 0..smax-1 */
static void scrambleinit(
    ub4      *scramble,                        /* hash is a^scramble[tab[b]] */
    ub4       smax)                /* scramble values should be in 0..smax-1 */
{
  ub4 i;

  /* fill scramble[] with distinct random integers in 0..smax-1 */
  for (i=0; i<SCRAMBLE_LEN; ++i)
  {
    scramble[i] = permute(i, mylog2(smax));
  }
}

/*
 * Check if key1 and key2 are the same.
 * We already checked (a,b) are the same.
 */
static void checkdup(
    key      *key1,
    key      *key2,
    __attribute__((unused)) hashform *form)
{
    if (key1->hash_k == key2->hash_k)
    {
      if (verbose) fprintf(stderr, "perfect.c: Duplicate keys!  %.8x\n", key1->hash_k);
      abort();
    }
}


/*
 * put keys in tabb according to key->b_k
 * check if the initial hash might work
 */
static int inittab(
    bstuff   *tabb,                 /* output, list of keys with b for (a,b) */
    ub4       blen,                                        /* length of tabb */
    key      *keys,                           /* list of keys already hashed */
    hashform *form,                                       /* user directives */
    int       complete)    /* TRUE means to complete init despite collisions */
{
  int  nocollision = TRUE;
  key *mykey;

  memset((void *)tabb, 0, (size_t)(sizeof(bstuff)*blen));

  /* Two keys with the same (a,b) guarantees a collision */
  for (mykey=keys; mykey; mykey=mykey->next_k)
  {
    key *otherkey;

    for (otherkey=tabb[mykey->b_k].list_b;
         otherkey;
         otherkey=otherkey->nextb_k)
    {
      if (mykey->a_k == otherkey->a_k)
      {
        nocollision = FALSE;
        checkdup(mykey, otherkey, form);
        if (!complete)
          return FALSE;
      }
    }
    ++tabb[mykey->b_k].listlen_b;
    mykey->nextb_k = tabb[mykey->b_k].list_b;
    tabb[mykey->b_k].list_b = mykey;
  }

  /* no two keys have the same (a,b) pair */
  return nocollision;
}

#include "perfecthex.inc"
/*
 * Run a hash function on the key to get a and b
 * Returns:
 *   0: didn't find distinct (a,b) for all keys
 *   1: found distinct (a,b) for all keys, put keys in tabb[]
 *   2: found a perfect hash, no need to do any more work
 */
static int initkey(
    key      *keys,                                      /* list of all keys */
    ub4       nkeys,                                 /* total number of keys */
    bstuff   *tabb,                                    /* stuff indexed by b */
    ub4       alen,                /* (a,b) has a in 0..alen-1, a power of 2 */
    ub4       blen,                /* (a,b) has b in 0..blen-1, a power of 2 */
    ub4       smax,                       /* range of computable hash values */
    ub4       salt,                  /* used to initialize the hash function */
    hashform *form,                                       /* user directives */
    gencode  *final)                                  /* code for final hash */
{
  /* Do the initial hash of the keys */
  int finished = inithex(keys, nkeys, alen, blen, smax, salt, final, form);
  if (finished < 0) return -1;
  if (finished) return 2;
  return inittab(tabb, blen, keys, form, FALSE);
}

/* Print an error message and exit if there are duplicates */
static void duplicates(
    bstuff   *tabb,                /* array of lists of keys with the same b */
    ub4       blen,                          /* length of tabb, a power of 2 */
    key      *keys,
    hashform *form)                                       /* user directives */
{
  ub4  i;
  key *key1;
  key *key2;

  (void)inittab(tabb, blen, keys, form, TRUE);

  /* for each b, do nested loops through key list looking for duplicates */
  for (i=0; i<blen; ++i)
    for (key1=tabb[i].list_b; key1; key1=key1->nextb_k)
      for (key2=key1->nextb_k; key2; key2=key2->nextb_k)
        checkdup(key1, key2, form);
}


/* Try to apply an augmenting list */
static int apply(
    bstuff *tabb,
    hstuff *tabh,
    qstuff *tabq,
    ub4     blen,
    ub4    *scramble,
    ub4     tail,
    int     rollback)      /* FALSE applies augmenting path, TRUE rolls back */
{
  ub4     hash;
  key    *mykey;
  bstuff *pb;
  ub4     child;
  ub4     parent;
  ub4     stabb;                                         /* scramble[tab[b]] */

  /* walk from child to parent */
  for (child=tail-1; child; child=parent)
  {
    parent = tabq[child].parent_q;                    /* find child's parent */
    pb     = tabq[parent].b_q;             /* find parent's list of siblings */

    /* erase old hash values */
    stabb = scramble[pb->val_b];
    for (mykey=pb->list_b; mykey; mykey=mykey->nextb_k)
    {
      hash = mykey->a_k^stabb;
      if (mykey == tabh[hash].key_h)
      {                            /* erase hash for all of child's siblings */
        tabh[hash].key_h = (key *)0;
      }
    }

    /* change pb->val_b, which will change the hashes of all parent siblings */
    pb->val_b = (rollback ? tabq[child].oldval_q : tabq[child].newval_q);

    /* set new hash values */
    stabb = scramble[pb->val_b];
    for (mykey=pb->list_b; mykey; mykey=mykey->nextb_k)
    {
      hash = mykey->a_k^stabb;
      if (rollback)
      {
        if (parent == 0) continue;                  /* root never had a hash */
      }
      else if (tabh[hash].key_h)
      {
        /* very rare: roll back any changes */
        (void) apply(tabb, tabh, tabq, blen, scramble, tail, TRUE);
        return FALSE;                                  /* failure, collision */
      }
      tabh[hash].key_h = mykey;
    }
  }
  return TRUE;
}


/*
-------------------------------------------------------------------------------
augment(): Add item to the mapping.

Construct a spanning tree of *b*s with *item* as root, where each
parent can have all its hashes changed (by some new val_b) with
at most one collision, and each child is the b of that collision.

I got this from Tarjan's "Data Structures and Network Algorithms".  The
path from *item* to a *b* that can be remapped with no collision is
an "augmenting path".  Change values of tab[b] along the path so that
the unmapped key gets mapped and the unused hash value gets used.

Assuming 1 key per b, if m out of n hash values are still unused,
you should expect the transitive closure to cover n/m nodes before
an unused node is found.  Sum(i=1..n)(n/i) is about nlogn, so expect
this approach to take about nlogn time to map all single-key b's.
-------------------------------------------------------------------------------
*/
static int augment(
 bstuff   *tabb,                                       /* stuff indexed by b */
 hstuff   *tabh, /* which key is associated with which hash, indexed by hash */
 qstuff   *tabq,           /* queue of *b* values, this is the spanning tree */
 ub4       blen,                                           /* length of tabb */
 ub4      *scramble,                     /* final hash is a^scramble[tab[b]] */
 ub4       smax,                                /* highest value in scramble */
 bstuff   *item,                          /* &tabb[b] for the b to be mapped */
 ub4       nkeys,                        /* final hash must be in 0..nkeys-1 */
 ub4       highwater,       /* a value higher than any now in tabb[].water_b */
 hashform *form)              /* TRUE if we should do a minimal perfect hash */
{
  ub4  q;                      /* current position walking through the queue */
  ub4  tail;              /* tail of the queue.  0 is the head of the queue. */
  ub4  limit=((blen < USE_SCRAMBLE) ? smax : UB1MAXVAL+1);
  ub4  highhash = ((form->perfect == MINIMAL_HP) ? nkeys : smax);
  int  trans = (form->speed == SLOW_HS || form->perfect == MINIMAL_HP);

  /* initialize the root of the spanning tree */
  tabq[0].b_q = item;
  tail = 1;

  /* construct the spanning tree by walking the queue, add children to tail */
  for (q=0; q<tail; ++q)
  {
    bstuff *myb = tabq[q].b_q;                        /* the b for this node */
    ub4     i;                              /* possible value for myb->val_b */

    if (!trans && (q == 1))
      break;                                  /* don't do transitive closure */

    for (i=0; i<limit; ++i)
    {
      bstuff *childb = (bstuff *)0;             /* the b that this i maps to */
      key    *mykey;                       /* for walking through myb's keys */

      for (mykey = myb->list_b; mykey; mykey=mykey->nextb_k)
      {
        key    *childkey;
        ub4 hash = mykey->a_k^scramble[i];

        if (hash >= highhash) break;                        /* out of bounds */
        childkey = tabh[hash].key_h;

        if (childkey)
        {
          bstuff *hitb = &tabb[childkey->b_k];

          if (childb)
          {
            if (childb != hitb) break;            /* hit at most one child b */
          }
          else
          {
            childb = hitb;                        /* remember this as childb */
            if (childb->water_b == highwater) break;     /* already explored */
          }
        }
      }
      if (mykey) continue;             /* myb with i has multiple collisions */

      /* add childb to the queue of reachable things */
      if (childb) childb->water_b = highwater;
      tabq[tail].b_q      = childb;
      tabq[tail].newval_q = i;     /* how to make parent (myb) use this hash */
      tabq[tail].oldval_q = myb->val_b;            /* need this for rollback */
      tabq[tail].parent_q = q;
      ++tail;

      if (!childb)
      {                                  /* found an *i* with no collisions? */
        /* try to apply the augmenting path */
        if (apply(tabb, tabh, tabq, blen, scramble, tail, FALSE))
          return TRUE;        /* success, item was added to the perfect hash */

        --tail;                    /* don't know how to handle such a child! */
      }
    }
  }
  return FALSE;
}


/* find a mapping that makes this a perfect hash */
static int perfect(
    bstuff   *tabb,
    hstuff   *tabh,
    qstuff   *tabq,
    ub4       blen,
    ub4       smax,
    ub4      *scramble,
    ub4       nkeys,
    hashform *form)
{
  ub4 maxkeys;                           /* maximum number of keys for any b */
  ub4 i, j;

  /* clear any state from previous attempts */
  memset((void *)tabh, 0,
         (size_t)(sizeof(hstuff)*
                  ((form->perfect == MINIMAL_HP) ? nkeys : smax)));
  memset((void *)tabq, 0, (size_t)(sizeof(qstuff)*(blen+1)));

  for (maxkeys=0,i=0; i<blen; ++i)
    if (tabb[i].listlen_b > maxkeys)
      maxkeys = tabb[i].listlen_b;

  /* In descending order by number of keys, map all *b*s */
  for (j=maxkeys; j>0; --j)
    for (i=0; i<blen; ++i)
      if (tabb[i].listlen_b == j)
        if (!augment(tabb, tabh, tabq, blen, scramble, smax, &tabb[i], nkeys,
                     i+1, form))
        {
          if (verbose) fprintf(stderr, "fail to map group of size %d for tab size %d\n", j, blen);
          return FALSE;
        }

  /* Success!  We found a perfect hash of all keys into 0..nkeys-1. */
  return TRUE;
}


/* guess initial values for alen and blen */
static void initalen(
    ub4      *alen,                                  /* output, initial alen */
    ub4      *blen,                                  /* output, initial blen */
    ub4      *smax,/* input, power of two greater or equal to max hash value */
    ub4       nkeys,                          /* number of keys being hashed */
    hashform *form)                                       /* user directives */
{
  /*
   * Find initial *alen, *blen
   * Initial alen and blen values were found empirically.  Some factors:
   *
   * If smax<256 there is no scramble, so tab[b] needs to cover 0..smax-1.
   *
   * alen and blen must be powers of 2 because the values in 0..alen-1 and
   * 0..blen-1 are produced by applying a bitmask to the initial hash function.
   *
   * alen must be less than smax, in fact less than nkeys, because otherwise
   * there would often be no i such that a^scramble[i] is in 0..nkeys-1 for
   * all the *a*s associated with a given *b*, so there would be no legal
   * value to assign to tab[b].  This only matters when we're doing a minimal
   * perfect hash.
   *
   * It takes around 800 trials to find distinct (a,b) with nkey=smax*(5/8)
   * and alen*blen = smax*smax/32.
   *
   * Values of blen less than smax/4 never work, and smax/2 always works.
   *
   * We want blen as small as possible because it is the number of bytes in
   * the huge array we must create for the perfect hash.
   *
   * When nkey <= smax*(5/8), blen=smax/4 works much more often with
   * alen=smax/8 than with alen=smax/4.  Above smax*(5/8), blen=smax/4
   * doesn't seem to care whether alen=smax/8 or alen=smax/4.  I think it
   * has something to do with 5/8 = 1/8 * 5.  For example examine 80000,
   * 85000, and 90000 keys with different values of alen.  This only matters
   * if we're doing a minimal perfect hash.
   *
   * When alen*blen <= 1<<UB4BITS, the initial hash must produce one integer.
   * Bigger than that it must produce two integers, which increases the
   * cost of the hash per character hashed.
   */
  if (form->perfect == NORMAL_HP)
  {
    if ((form->speed == FAST_HS) && (nkeys > *smax*0.8))
    {
      *smax = *smax * 2;
    }

    *alen = ((form->hashtype==INT_HT) && *smax>131072) ?
      ((ub4)1<<(UB4BITS-mylog2(*blen))) :   /* distinct keys => distinct (A,B) */
      *smax;                         /* no reason to restrict alen to smax/2 */
    if ((form->hashtype == INT_HT) && *smax < 32)
      *blen = *smax;                      /* go for function speed not space */
    else if (*smax/4 <= (1<<14))
      *blen = ((nkeys <= *smax*0.56) ? *smax/32 :
               (nkeys <= *smax*0.74) ? *smax/16 : *smax/8);
    else
      *blen = ((nkeys <= *smax*0.6) ? *smax/16 :
               (nkeys <= *smax*0.8) ? *smax/8 : *smax/4);

    if ((form->speed == FAST_HS) && (*blen < *smax/8))
      *blen = *smax/8;

    if (*alen < 1) *alen = 1;
    if (*blen < 1) *blen = 1;
  }
  else
  {
    switch(mylog2(*smax))
    {
    case 0:
      *alen = 1;
      *blen = 1;
      // I think there was a missing 'break' here. Fallthrough would defeat the
      // purpose of having this case exist at all.
      break;
    case 1: case 2: case 3: case 4: case 5: case 6: case 7: case 8:
      *alen = (form->perfect == NORMAL_HP) ? *smax : *smax/2;
      *blen = *smax/2;
      break;
    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 15:
    case 16:
    case 17:
      if (form->speed == FAST_HS)
      {
        *alen = *smax/2;
        *blen = *smax/4;
      }
      else if (*smax/4 < USE_SCRAMBLE)
      {
        *alen = ((nkeys <= *smax*0.52) ? *smax/8 : *smax/4);
        *blen = ((nkeys <= *smax*0.52) ? *smax/8 : *smax/4);
      }
      else
      {
        *alen = ((nkeys <= *smax*(5.0/8.0)) ? *smax/8 :
                 (nkeys <= *smax*(3.0/4.0)) ? *smax/4 : *smax/2);
        *blen = *smax/4;                /* always give the small size a shot */
      }
      break;
    case 18:
      if (form->speed == FAST_HS)
      {
        *alen = *smax/2;
        *blen = *smax/2;
      }
      else
      {
        *alen = *smax/8;                 /* never require the multiword hash */
        *blen = (nkeys <= *smax*(5.0/8.0)) ? *smax/4 : *smax/2;
      }
      break;
    case 19:
    case 20:
      *alen = (nkeys <= *smax*(5.0/8.0)) ? *smax/8 : *smax/2;
      *blen = (nkeys <= *smax*(5.0/8.0)) ? *smax/4 : *smax/2;
      break;
    default:
      *alen = *smax/2;              /* just find a hash as quick as possible */
      *blen = *smax/2;     /* we'll be thrashing virtual memory at this size */
      break;
    }
  }
}

/*
** Try to find a perfect hash function.
** Return the successful initializer for the initial hash.
** Return 0 if no perfect hash could be found.
*/
int findhash(
    bstuff  **tabb,       /* output, tab[] of the perfect hash, length *blen */
    ub4      *alen,             /* output, 0..alen-1 is range for a of (a,b) */
    ub4      *blen,             /* output, 0..blen-1 is range for b of (a,b) */
    ub4      *salt,                      /* output, initializes initial hash */
    gencode  *final,                                  /* code for final hash */
    ub4      *scramble,                  /* input, hash = a^scramble[tab[b]] */
    ub4      *smax,                       /* input, scramble[i] in 0..smax-1 */
    key      *keys,                                   /* input, keys to hash */
    ub4       nkeys,                   /* input, number of keys being hashed */
    hashform *form)                                       /* user directives */
{
  ub4 bad_initkey;                       /* how many times did initkey fail? */
  ub4 bad_perfect;                       /* how many times did perfect fail? */
  ub4 trysalt;                        /* trial initializer for initial hash */
  ub4 maxalen;
  hstuff *tabh;                       /* table of keys indexed by hash value */
  qstuff *tabq;    /* table of stuff indexed by queue value, used by augment */

  /* guess initial values for smax, alen and blen */
  *smax = ((ub4)1<<mylog2(nkeys));
  if (verbose) fprintf(stderr, "nkeys=%d log2=%d guess=%d\n", nkeys, mylog2(nkeys), *smax);
  initalen(alen, blen, smax, nkeys, form);

  scrambleinit(scramble, *smax);

  maxalen = (form->perfect == MINIMAL_HP) ? *smax/2 : *smax;

  /* allocate working memory */
  *tabb = (bstuff *)malloc((size_t)(sizeof(bstuff)*(*blen)));
  tabq  = (qstuff *)malloc(sizeof(qstuff)*(*blen+1));
  tabh  = (hstuff *)malloc(sizeof(hstuff)*(form->perfect == MINIMAL_HP ? nkeys : *smax));

  /* Actually find the perfect hash */
  *salt = 0;
  bad_initkey = 0;
  bad_perfect = 0;
  for (trysalt=1; ; ++trysalt)
  {
    int rslinit;
    /* Try to find distinct (A,B) for all keys */

    rslinit = initkey(keys, nkeys, *tabb, *alen, *blen, *smax, trysalt,
                      form, final);
    if (rslinit < 0) goto fail;

    if (rslinit == 2)
    {      /* initkey actually found a perfect hash, not just distinct (a,b) */
      *salt = 1;
      *blen = 0;
      break;
    }
    else if (rslinit == 0)
    {
      /* didn't find distinct (a,b) */
      if (++bad_initkey >= RETRY_INITKEY)
      {
        /* Try to put more bits in (A,B) to make distinct (A,B) more likely */
        if (*alen < maxalen)
        {
          *alen *= 2;
        }
        else if (*blen < *smax)
        {
          *blen *= 2;
          free(tabq);
          free(*tabb);
          *tabb  = (bstuff *)malloc((size_t)(sizeof(bstuff)*(*blen)));
          tabq  = (qstuff *)malloc((size_t)(sizeof(qstuff)*(*blen+1)));
        }
        else
        {
          duplicates(*tabb, *blen, keys, form);      /* check for duplicates */
       // printf("fatal error: Cannot perfect hash: cannot find distinct (A,B)\n");
          goto fail;
        }
        bad_initkey = 0;
        bad_perfect = 0;
      }
      continue;                             /* two keys have same (a,b) pair */
    }

    if (verbose) fprintf(stderr, "found distinct (A,B) on attempt %d\n", trysalt);

    /* Given distinct (A,B) for all keys, build a perfect hash */
    if (!perfect(*tabb, tabh, tabq, *blen, *smax, scramble, nkeys, form))
    {
      if ((form->hashtype != INT_HT && ++bad_perfect >= RETRY_PERFECT) ||
          (form->hashtype == INT_HT && ++bad_perfect >= RETRY_HEX))
      {
        if (*blen < *smax)
        {
          *blen *= 2;
          free(*tabb);
          free(tabq);
          *tabb  = (bstuff *)malloc((size_t)(sizeof(bstuff)*(*blen)));
          tabq  = (qstuff *)malloc((size_t)(sizeof(qstuff)*(*blen+1)));
          --trysalt;               /* we know this salt got distinct (A,B) */
        }
        else
        {
       // printf("fatal error: Cannot perfect hash: cannot build tab[]\n");
          goto fail;
        }
        bad_perfect = 0;
      }
      continue;
    }

    *salt = trysalt;
    break;
  }

  if (verbose) fprintf(stderr, "built perfect hash table of size %d\n", *blen);

  /* free working memory */
  free((void *)tabh);
  free((void *)tabq);
  return 1; // success

 fail:
  free(*tabb);
  free(tabq);
  free(tabh);
  return -1;
}

struct mem_stream {
    int size;
    char* buffer;
    int position;
};

struct mem_stream* make_mem_stream() {
    int size = 1024;
    struct mem_stream* stream = malloc(sizeof(struct mem_stream));
    stream->size = size;
    stream->position = 0;
    stream->buffer = malloc(size);
    return stream;
}

void grow_mem_stream (struct mem_stream * stream) {
    if (stream->position >= stream->size) {
        int new_size = stream->size * 2;
        stream->buffer = realloc(stream->buffer, new_size);
        stream->size = new_size;
    }
}

int mem_stream_printf(struct mem_stream * stream, char *fmt, ...) {
    va_list args;

    int written;
    int position = stream->position;
    int length;
    do {
        grow_mem_stream(stream);
        length = stream->size - position;
        va_start(args, fmt);
        written = vsnprintf(stream->buffer + position, length, fmt, args);
        va_end(args);
        stream->position = position + written;
    } while (written >= length);

    return written;
}

/*
------------------------------------------------------------------------------
Input/output type routines
------------------------------------------------------------------------------
*/

/* make the .h file */
static void make_h(
    ub4  blen,
    ub4  smax,
    ub4  nkeys,
    ub4  salt)
{
  FILE *f;
  f = fopen("phash.h", "w");
  fprintf(f, "/* Perfect hash definitions */\n");
  fprintf(f, "#ifndef STANDARD\n");
  fprintf(f, "#include \"standard.h\"\n");
  fprintf(f, "#endif /* STANDARD */\n");
  fprintf(f, "#ifndef PHASH\n");
  fprintf(f, "#define PHASH\n");
  fprintf(f, "\n");
  if (blen > 0)
  {
    if (smax <= UB1MAXVAL+1 || blen >= USE_SCRAMBLE)
      fprintf(f, "extern ub1 tab[];\n");
    else
    {
      fprintf(f, "extern ub2 tab[];\n");
      if (blen >= USE_SCRAMBLE)
      {
        if (smax <= UB2MAXVAL+1)
          fprintf(f, "extern ub2 scramble[];\n");
        else
          fprintf(f, "extern ub4 scramble[];\n");
      }
    }
    fprintf(f, "#define PHASHLEN 0x%x   /* length of hash mapping table */\n",
            blen);
  }
  fprintf(f, "#define PHASHNKEYS %d   /* How many keys were hashed */\n",
          nkeys);
  fprintf(f, "#define PHASHRANGE %d   /* Range any input might map to */\n",
          smax);
  fprintf(f, "#define PHASHSALT 0x%.8x  /* internal, initialize normal hash */\n",
          salt*0x9e3779b9);
  fprintf(f, "\n");
  fprintf(f, "ub4 phash();\n");
  fprintf(f, "\n");
  fprintf(f, "#endif  /* PHASH */\n");
  fprintf(f, "\n");
  fclose(f);
}

/* make the .c file */
static void make_c(
    bstuff   *tab,                                     /* table indexed by b */
    ub4       smax,                                   /* range of scramble[] */
    ub4       blen,                            /* b in 0..blen-1, power of 2 */
    ub4      *scramble,                                /* used in final hash */
    gencode  *final,                              /* code for the final hash */
    hashform *form,                                       /* user directives */
    struct mem_stream *output)
{
  ub4   i;
  struct mem_stream *f = output;
  int infix = form->infix;
  if (infix) {
    mem_stream_printf(f, "/* table for the mapping for the perfect hash */\n");
    mem_stream_printf(f, "#include <stdint.h>\n\
typedef uint32_t ub4;\n\
typedef uint16_t ub2;\n\
typedef uint8_t  ub1;\n");
    mem_stream_printf(f, "/* The hash function */\n");
    mem_stream_printf(f, "ub4 phash(ub4 val) {\n");
  } else {
      mem_stream_printf(f, "(");
  }
  int extra_parens = 0;
  if (blen >= USE_SCRAMBLE)
  {
    mem_stream_printf(f, infix ? "/* A way to make the 1-byte values in tab bigger */\n"
                      : "(let ((scramble #a((256) (unsigned-byte ");

    if (smax > UB2MAXVAL+1)
    {
      mem_stream_printf(f, infix ? "ub4 scramble[] = {\n" : "32)");
      for (i=0; i<=UB1MAXVAL; i+=4)
        mem_stream_printf(f,
                infix ? "0x%.8x, 0x%.8x, 0x%.8x, 0x%.8x,\n" : " #x%8x #x%8x #x%8x #x%8x\n",
                scramble[i+0], scramble[i+1], scramble[i+2], scramble[i+3]);
    }
    else
    {
      mem_stream_printf(f, infix ? "ub2 scramble[] = {\n" : "16)");
      for (i=0; i<=UB1MAXVAL; i+=8)
        mem_stream_printf(f,
                infix ? "0x%.4x, 0x%.4x, 0x%.4x, 0x%.4x, 0x%.4x, 0x%.4x, 0x%.4x, 0x%.4x,\n"
                : " #x%x #x%x #x%x #x%x #x%x #x%x #x%x #x%x\n",
                scramble[i+0], scramble[i+1], scramble[i+2], scramble[i+3],
                scramble[i+4], scramble[i+5], scramble[i+6], scramble[i+7]);
    }
    mem_stream_printf(f, infix ? "};\n\n" : ")))\n");
    ++extra_parens;
  }
  if (blen > 0)
  {
    mem_stream_printf(f, infix ? "/* small adjustments to _a_ to make values distinct */\n"
                      : "(let ((tab #a((%d) (unsigned-byte ", blen);

    if (smax <= UB1MAXVAL+1 || blen >= USE_SCRAMBLE) {
      mem_stream_printf(f, infix ? "ub1 tab[] = {\n" : "8)");
    } else {
      mem_stream_printf(f, infix ? "ub2 tab[] = {\n": "16)");
    }

    // When emitting sexprs instead of C, assume that the Lisp pretty-printer
    // can nicely reformat the string after reading it.
    if (blen < 16)
    {
      for (i=0; i<blen; ++i) mem_stream_printf(f, infix?"%3d,":" %d", scramble[tab[i].val_b]);
    }
    else if (blen <= 1024)
    {
      for (i=0; i<blen; i+=16)
        mem_stream_printf(f, infix ? "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,\n"
                        : " %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                scramble[tab[i+0].val_b], scramble[tab[i+1].val_b],
                scramble[tab[i+2].val_b], scramble[tab[i+3].val_b],
                scramble[tab[i+4].val_b], scramble[tab[i+5].val_b],
                scramble[tab[i+6].val_b], scramble[tab[i+7].val_b],
                scramble[tab[i+8].val_b], scramble[tab[i+9].val_b],
                scramble[tab[i+10].val_b], scramble[tab[i+11].val_b],
                scramble[tab[i+12].val_b], scramble[tab[i+13].val_b],
                scramble[tab[i+14].val_b], scramble[tab[i+15].val_b]);
    }
    else if (blen < USE_SCRAMBLE)
    {
      for (i=0; i<blen; i+=8)
        mem_stream_printf(f, infix ? "%d,%d,%d,%d,%d,%d,%d,%d,\n" : " %d %d %d %d %d %d %d %d",
                scramble[tab[i+0].val_b], scramble[tab[i+1].val_b],
                scramble[tab[i+2].val_b], scramble[tab[i+3].val_b],
                scramble[tab[i+4].val_b], scramble[tab[i+5].val_b],
                scramble[tab[i+6].val_b], scramble[tab[i+7].val_b]);
    }
    else
    {
      for (i=0; i<blen; i+=16)
        mem_stream_printf(f, infix ? "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,\n"
                        : " %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                tab[i+0].val_b, tab[i+1].val_b,
                tab[i+2].val_b, tab[i+3].val_b,
                tab[i+4].val_b, tab[i+5].val_b,
                tab[i+6].val_b, tab[i+7].val_b,
                tab[i+8].val_b, tab[i+9].val_b,
                tab[i+10].val_b, tab[i+11].val_b,
                tab[i+12].val_b, tab[i+13].val_b,
                tab[i+14].val_b, tab[i+15].val_b);
    }
    mem_stream_printf(f, infix ? "};\n\n" : ")))\n");
    ++extra_parens;
  }
  int indent = 0, newline = 0;
  char *comment = 0;
  for (i=0; i<final->used; ++i) {
    char* line = final->line[i];
    if (!line[0]) continue; // empty line
    if (newline) mem_stream_printf(f,"\n");
    newline = 0;
    int j; for(j=0;j<indent;++j) mem_stream_printf(f," ");
    mem_stream_printf(f, "  ");

    comment = strchr(line, ';');
    if (comment && !form->comments) { // strip the comment
        mem_stream_printf(f, "%.*s", comment-line, line);
        comment = 0;
    } else
        mem_stream_printf(f, "%s", line);
    // Delay the newline in lisp mode so we prettily
    // close all the parens on the last line.
    if (infix) mem_stream_printf(f, ";\n");
    else newline = 1;
    if (!strncmp(line,"(let",4)) ++indent;
  }
  if (infix) {
    mem_stream_printf(f, "  return rsl;\n}\n");
  } else {
    if (comment) mem_stream_printf(f,"\n");
    indent += 1 + extra_parens;
    while (indent--) mem_stream_printf(f,")");
    mem_stream_printf(f,"\n");
  }
}

/*
------------------------------------------------------------------------------
Read in the keys, find the hash, and write the .c and .h files
------------------------------------------------------------------------------
*/
static int driver(
        hashform *form,                                   /* user directives */
        key* keys,
        int nkeys,
        struct mem_stream* scratchfile)
{
  bstuff   *tab;                                       /* table indexed by b */
  ub4       smax;            /* scramble[] values in 0..smax-1, a power of 2 */
  ub4       alen;                            /* a in 0..alen-1, a power of 2 */
  ub4       blen;                            /* b in 0..blen-1, a power of 2 */
  ub4       salt;                       /* a parameter to the hash function */
  gencode   final;                                    /* code for final hash */
  ub4       i;
  ub4       scramble[SCRAMBLE_LEN];           /* used in final hash function */
  char      buf[10][80];                        /* buffer for generated code */
  char     *buf2[10];                             /* also for generated code */

  /* set up code for final hash */
  final.line = buf2;
  final.used = 0;
  final.len  = 10;
  for (i=0; i<10; ++i) final.line[i] = buf[i];

  /* find the hash */
  if (findhash(&tab, &alen, &blen, &salt, &final,
               scramble, &smax, keys, nkeys, form) < 0) return -1;

  /* generate the phash.h file */
  if (form->infix) {
    make_h(blen, smax, nkeys, salt);
    if (verbose) fprintf(stderr, "Wrote phash.h\n");
  }

  /* generate the phash.c file */
  make_c(tab, smax, blen, scramble, &final, form, scratchfile);

  /* clean up memory sources */
  free((void *)tab);
  return 0;
}

char* lisp_perfhash_with_options(int flags, unsigned int *key_array, int nkeys)
{
  key* keylist = 0;
  key* keyspace = calloc(nkeys, sizeof (key));
  int i;
  for (i=0; i<nkeys; ++i) {
    key* this = &keyspace[i];
    this->hash_k = key_array[i];
    this->next_k = keylist;
    keylist = this;
  }
  hashform form = {
    .hashtype = INT_HT,
    .perfect = (flags & 1) ? MINIMAL_HP : NORMAL_HP,
    .speed = (flags & 2) ? FAST_HS : SLOW_HS,
    .infix = 0,
    .comments = 1
  };
  struct mem_stream * scratchfile = make_mem_stream();
  char* result = 0;
  if (driver(&form, keylist, nkeys, scratchfile) < 0) {
    free(scratchfile->buffer);
  } else {
    result = realloc(scratchfile->buffer, scratchfile->position + 1);
  }
  //fprintf(stderr, "#|\n%s|#\n", result);
  free(scratchfile);
  free(keyspace);
  return result;
}
char* generate_perfhash_sexpr(unsigned int *key_array, int nkeys) {
  return lisp_perfhash_with_options(1, key_array, nkeys); // MINIMAL
}

#ifdef DEFINE_MAIN
int main(int argc, char *argv[])
{
  hashform form = { .hashtype = INT_HT, .perfect = MINIMAL_HP, .speed = SLOW_HS,
                    .infix = 0, .comments = 0 };
  if (argc == 2 && !strcmp(argv[1],"infix")) form.infix = 1;
  key* keylist = 0;
  int keycount = 0;
  char line[100];
  key keybuffer[1000];
  while (fgets(line, sizeof line, stdin)) {
      key *this = (keycount < 1000) ? &keybuffer[keycount]  : (key*)malloc(sizeof (key));
      memset(this, 0, sizeof *this);
      this->hash_k = strtoul(line, 0, 16);
      this->next_k = keylist;
      keylist = this;
      ++keycount;
  }
  struct mem_stream * scratchfile = make_mem_stream();
  driver(&form, keylist, keycount, scratchfile);
  fwrite(scratchfile->buffer, 1, scratchfile->position, stdout);
  return 0;
}
#endif
