/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <stdlib.h>

#include "sbcl.h"
#include "runtime.h"
#include "vars.h"
#include "os.h"

#define NAME_BUCKETS 31
#define OBJ_BUCKETS 31

static struct var *NameHash[NAME_BUCKETS], *ObjHash[OBJ_BUCKETS];
static int tempcntr = 1;

struct var {
    lispobj obj;
    lispobj (*update_fn)(struct var *var);
    char *name;
    sword_t clock;
    boolean map_back, permanent;

    struct var *nnext; /* Next in name list */
    struct var *onext; /* Next in object list */
};

static int hash_name(char *name)
{
    uword_t value = 0;

    while (*name != '\0') {
        value = (value << 1) ^ *(unsigned char *)(name++);
        value = (value & (1-(1<<24))) ^ (value >> 24);
    }

    return value % NAME_BUCKETS;
}

static int hash_obj(lispobj obj)
{
    return (uword_t)obj % OBJ_BUCKETS;
}

void flush_vars()
{
    int index;
    struct var *var, *next, *perm = NULL;

    /* Note: all vars in the object hash table also appear in the name hash
     * table, so if we free everything in the name hash table, we free
     * everything in the object hash table. */

    for (index = 0; index < NAME_BUCKETS; index++)
        for (var = NameHash[index]; var != NULL; var = next) {
            next = var->nnext;
            if (var->permanent) {
                var->nnext = perm;
                perm = var;
            }
            else {
                free(var->name);
                free(var);
            }
        }
    memset(NameHash, 0, sizeof(NameHash));
    memset(ObjHash, 0, sizeof(ObjHash));
    tempcntr = 1;

    for (var = perm; var != NULL; var = next) {
        next = var->nnext;
        index = hash_name(var->name);
        var->nnext = NameHash[index];
        NameHash[index] = var;
        if (var->map_back) {
            index = hash_obj(var->obj);
            var->onext = ObjHash[index];
            ObjHash[index] = var;
        }
    }
}

struct var *lookup_by_name(name)
char *name;
{
    struct var *var;

    for (var = NameHash[hash_name(name)]; var != NULL; var = var->nnext)
        if (strcmp(var->name, name) == 0)
            return var;
    return NULL;
}

struct var *lookup_by_obj(obj)
lispobj obj;
{
    struct var *var;

    for (var = ObjHash[hash_obj(obj)]; var != NULL; var = var->onext)
        if (var->obj == obj)
            return var;
    return NULL;
}

static struct var *make_var(char *name, boolean perm)
{
    struct var *var = (struct var *)malloc(sizeof(struct var));
    char buffer[256];
    int index;

    if (name == NULL) {
        sprintf(buffer, "%d", tempcntr++);
        name = buffer;
    }
    var->name = (char *)malloc(strlen(name)+1);
    strcpy(var->name, name);
    var->clock = 0;
    var->permanent = perm;
    var->map_back = 0;

    index = hash_name(name);
    var->nnext = NameHash[index];
    NameHash[index] = var;

    return var;
}

struct var *define_var(char *name, lispobj obj, boolean perm)
{
    struct var *var = make_var(name, perm);
    int index;

    var->obj = obj;
    var->update_fn = NULL;

    if (lookup_by_obj(obj) == NULL) {
        var->map_back = 1;
        index = hash_obj(obj);
        var->onext = ObjHash[index];
        ObjHash[index] = var;
    }

    return var;
}

struct var *define_dynamic_var(char *name, lispobj updatefn(struct var *),
                               boolean perm)
{
    struct var *var = make_var(name, perm);

    var->update_fn = updatefn;

    return var;
}

char *var_name(struct var *var)
{
    return var->name;
}

lispobj var_value(struct var *var)
{
    if (var->update_fn != NULL)
        var->obj = (*var->update_fn)(var);
    return var->obj;
}

sword_t var_clock(struct var *var)
{
    return var->clock;
}

void var_setclock(struct var *var, sword_t val)
{
    var->clock = val;
}
