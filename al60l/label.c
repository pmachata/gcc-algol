/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef SELF_TEST

#include "label.h"
#include "estring.h"
#include <assert.h>

static char const* private_label_signature = "label";

typedef struct struct_label_rep_t
{
  char const* signature;
} label_rep_t;

label_t *
new_label (void)
{
  label_rep_t * ret = malloc (sizeof (label_rep_t));
  ret->signature = private_label_signature;
  return (void*)ret;
}

void
delete_label (label_t * _label)
{
  if (_label != NULL)
    {
      label_rep_t * label = (void*)_label;
      free (label);
    }
}

label_t *
label (void * ptr)
{
  if (((label_rep_t*)ptr)->signature == private_label_signature)
    return ptr;
  else
    return NULL;
}

#else /* SELF_TEST */

#include "label.h"
#include <stdio.h>
#include <assert.h>

int
main (void)
{
  label_t * l1 = new_label ();
  label_t * l2 = new_label ();
  label_t * l3 = new_label ();
  label_t * l4 = new_label ();
  label_t * l5 = new_label ();

  assert (label (l1));
  assert (l1 != l2);
  assert (l1 != l3);
  assert (l1 != l4);
  assert (l1 != l5);

  assert (label (l2));
  assert (l2 != l3);
  assert (l2 != l4);
  assert (l2 != l5);

  assert (label (l3));
  assert (l3 != l4);
  assert (l3 != l5);

  assert (label (l4));
  assert (l4 != l5);

  assert (label (l5));

  printf ("All passed.\n");
  return 0;
}

#endif
