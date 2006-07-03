/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef _AL60L_SLIST_H_
#define _AL60L_SLIST_H_

#include "pd.h"

typedef struct struct_slist_t { } slist_t;

/// Create fresh linked list.
slist_t * new_slist (void);

/// Destroy label list.  Doesn't touch the objects collected inside,
/// just the list.
void delete_slist (slist_t * list);

/// Convert void* to slist, if it is slist, or return NULL.
slist_t * slist (void * ptr)
     ARG_NONNULL(1);

/// Add new object onto the end of the list.
void slist_pushback (slist_t * list, void * object);

/// Add new object onto the beginning of the list.
void slist_pushfront (slist_t * list, void * object);

/// Pop an object off the beginning of the list.  No checking is done
/// whether the list contains any elements.  Note that popback isn't
/// applicable due to singly-linked implementation of list.
void * slist_popfront (slist_t * list);

/// Answer 1 if the slist doesn't contain any elements.  Otherwise
/// answer 0.
int slist_empty (slist_t * list);

/// Apply the function `fn' to each element of the list `list'.  The
/// argument `userdata' will be passed over to fn verbatim.  The
/// arguments of `fn' are, respectively, the list that is iterated
/// over, the object that is currently under the cursor, and the
/// userdata passed to slist_map.
void slist_each (
  slist_t * list,
  void (*fn)(slist_t * /*list*/, void * /*object*/, void * /*userdata*/),
  void * userdata);

/// Filter the slist according to predicate pred.  This function
/// creates a new list, that contains only the objects, for which the
/// predicate holds.  The argument `userdata' will be passed over to
/// pred verbatim.  The arguments of `pred' are, respectively, the
/// list that is iterated over, the object that is currently under the
/// cursor, and the userdata passed to slist_filter.
slist_t * slist_filter (
   slist_t * list,
   int (*pred)(slist_t * /*list*/, void * /*object*/, void * /*userdata*/),
   void * userdata);

#endif//_AL60L_SLIST_H_
