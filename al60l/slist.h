/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef _AL60L_SLIST_H_
#define _AL60L_SLIST_H_

#include "pd.h"

typedef struct struct_slist_t { } slist_t;
typedef struct struct_slist_it_t { } slist_it_t;

/// Create fresh linked list.
slist_t * new_slist (void);

/// Create a linked list that keeps track of the type of objects that
/// are put inside.  The `test' parameter is function that takes void*
/// argument, and answers NULL if the object isn't of required type.
/// If the test doesn't pass on object creation time, an assertion
/// fails.  The `userdata' parameter is passed verbatim to `test'.
/// In NDEBUG mode, typed lists are the same as untyped, with no extra
/// overhead, both memory and time.
slist_t * new_slist_typed (void* (*test)(void * obj, void * user), void * userdata);

/// Create new slist with `num' elements defined as extra arguments.
slist_t * new_slist_from (int num, ...);

/// Clone existing slist.
slist_t * clone_slist (slist_t * slist);

/// For purposes of testing type, this adaptor is provided.  The
/// `test' has to be something*(*test)(whatever*), but is declared as
/// mere void* to make it possible to pass over functions operating
/// over various somthings and whatevers.  `obj' is object that's
/// passed to the testing function.
/// Usage: new_slist_typed (adapt_test, my_test);
void * adapt_test (void * obj, void * test);

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

/// Get iterator.  For explicit iteration over the list.
slist_it_t * slist_iter (slist_t * list);

/// Query the validity of the iterator.  Return non-zero when the
/// iterator points to the list, or zero when the iteration ended.
int slist_it_has (slist_it_t * it);

/// Get the object that it pointed to by this iterator.
void * slist_it_get (slist_it_t * it);

/// Advance to next item.
void slist_it_next (slist_it_t * it);

/// Destroy the iterator.
void delete_slist_it (slist_it_t * it);

#endif//_AL60L_SLIST_H_
