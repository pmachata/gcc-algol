/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef _AL60L_SLIST_H_
#define _AL60L_SLIST_H_

#include "pd.h"
#include "slist.i"

/// Create fresh linked list.
slist_t * new_slist (void)
  ATTRIBUTE_MALLOC;

/// Create a linked list that keeps track of the type of objects that
/// are put inside.  The `test' parameter is function that takes void*
/// argument, and answers NULL if the object isn't of required type.
/// If the test doesn't pass on object creation time, an assertion
/// fails.  The `userdata' parameter is passed verbatim to `test'.
/// In NDEBUG mode, typed lists are the same as untyped, with no extra
/// overhead, both memory and time.
slist_t * new_slist_typed (void* (*test)(void * obj, void * user), void * userdata)
  ATTRIBUTE_MALLOC
  ATTRIBUTE_NONNULL(1);

/// Create new slist with `num' elements defined as extra arguments.
slist_t * new_slist_from (unsigned num, ...)
  ATTRIBUTE_MALLOC;

/// Create new typed slist with `num' elements defined as extra arguments.
slist_t * new_slist_typed_from (void* (*test)(void * obj, void * user), void * userdata, unsigned num, ...)
  ATTRIBUTE_MALLOC
  ATTRIBUTE_NONNULL(1);

/// Clone existing slist.
slist_t * clone_slist (slist_t const * slist)
  ATTRIBUTE_MALLOC
  ATTRIBUTE_NONNULL(1);

/// Clone existing slist into new typed slist.
slist_t * clone_slist_typed (void* (*test)(void * obj, void * user), void * userdata, slist_t const * slist)
  ATTRIBUTE_MALLOC
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(3);

/// For purposes of testing type, this adaptor is provided.  The
/// `test' has to be something*(*test)(whatever*), but is declared as
/// mere void* to make it possible to pass over functions operating
/// over various somthings and whatevers.  `obj' is object that's
/// passed to the testing function.
/// Usage: new_slist_typed (adapt_test, my_test);
void * adapt_test (void * obj, void * test)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

/// Destroy label list.  Doesn't touch the objects collected inside,
/// just the list.
void delete_slist (slist_t * list);

/// Convert void* to slist, if it is slist, or return NULL.
slist_t * slist (void * ptr)
  ATTRIBUTE_NONNULL(1);

/// Add new object onto the end of the list.
void slist_pushback (slist_t * list, void * object)
  ATTRIBUTE_NONNULL(1);

/// Add new object onto the beginning of the list.
void slist_pushfront (slist_t * list, void * object)
  ATTRIBUTE_NONNULL(1);

/// Pop an object off the beginning of the list.  No checking is done
/// whether the list contains any elements.  Note that popback isn't
/// applicable due to singly-linked implementation of list.
void * slist_popfront (slist_t * list)
  ATTRIBUTE_NONNULL(1);

/// Get the object stored at the end of the list.  Again, no
/// checking if the list is non-empty.
void * slist_back (slist_t * list)
  ATTRIBUTE_NONNULL(1);

/// Get the object stored at the front of the list.  Again, no
/// checking if the list is non-empty.
void * slist_front (slist_t * list)
  ATTRIBUTE_NONNULL(1);

/// Answer 1 if the slist doesn't contain any elements.  Otherwise
/// answer 0.
int slist_empty (slist_t * list)
  ATTRIBUTE_NONNULL(1);

/// Answer the length of the list.  Note that this is O(n) operation.
int slist_length (slist_t * list)
  ATTRIBUTE_NONNULL(1);

/// Apply the function `fn' to each element of the list `list'.  The
/// argument `userdata' will be passed over to fn verbatim.  The
/// arguments of `fn' are, respectively, the list that is iterated
/// over, the object that is currently under the cursor, and the
/// userdata passed to slist_map.
void slist_each (
  slist_t * list,
  void (*fn)(slist_t * /*list*/, void * /*object*/, void * /*userdata*/),
  void * userdata)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

/// Filter the slist according to predicate pred.  This function
/// creates a new list, that contains only the objects, for which the
/// predicate holds.  The argument `userdata' will be passed over to
/// pred verbatim.  The arguments of `pred' are, respectively, the
/// list that is iterated over, the object that is currently under the
/// cursor, and the userdata passed to slist_filter.
slist_t * slist_filter (
   slist_t * list,
   int (*pred)(slist_t * /*list*/, void * /*object*/, void * /*userdata*/),
   void * userdata)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

/// Get iterator.  For explicit iteration over the list.
slist_it_t * slist_iter (slist_t * list)
  ATTRIBUTE_MALLOC
  ATTRIBUTE_NONNULL(1);

/// Query the validity of the iterator.  Return non-zero when the
/// iterator points to the list, or zero when the iteration ended.
int slist_it_has (slist_it_t * it)
  ATTRIBUTE_NONNULL(1);

/// Get the object that it pointed to by this iterator.
void * slist_it_get (slist_it_t * it)
  ATTRIBUTE_NONNULL(1);

/// Put given object to the position pointed to by this iterator.
void slist_it_put (slist_it_t * it, void * object)
  ATTRIBUTE_NONNULL(1);

/// Advance to next item.
void slist_it_next (slist_it_t * it)
  ATTRIBUTE_NONNULL(1);

/// Destroy the iterator.
void delete_slist_it (slist_it_t * it);

#endif//_AL60L_SLIST_H_
