/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef SELF_TEST

#include "type.h"
#include <assert.h>

typedef struct struct_type_rep_t
{
  type_kind_t kind;
  struct struct_type_rep_t const* link;
  struct struct_type_rep_t const* host;
} type_rep_t;

static type_rep_t const* basic_types[type_kind_t_count] = {};

static type_rep_t *
private_alloc_type (type_kind_t kind)
{
  type_rep_t * ret = malloc (sizeof (type_rep_t));
  ret->kind = kind;
  ret->host = NULL;
  ret->link = NULL;
  return ret;
}

void
private_delete_type (type_rep_t * type)
{
  free (type);
}

int
types_same (type_t const* _lhs, type_t const* _rhs)
{
  assert (_lhs != NULL);
  type_rep_t const* lhs = (void*)_lhs;

  assert (_rhs != NULL);
  type_rep_t const* rhs = (void*)_rhs;

  // if kinds differ, they're not same
  if (lhs->kind != rhs->kind)
    return 0;

  // kinds are same, and there are no hosting types that'd make the
  // difference, so lhs and rhs are same
  if ((lhs->host == NULL) && (rhs->host == NULL))
    return 1;

  // one of the types has host, the other not, so they aren't the same
  if ((lhs->host == NULL) != (rhs->host == NULL))
    return 0;

  // compare the hosting types
  return types_same ((void*)lhs->host, (void*)rhs->host);
}

type_t const*
type_basic (type_kind_t kind)
{
  assert (kind == t_int
	  || kind == t_real
	  || kind == t_string
	  || kind == t_bool
	  || kind == t_label
	  || !"Only real, string, bool and label types are considered basic.");
  if (basic_types[kind] == NULL)
    basic_types[kind] = private_alloc_type (kind);

  assert (basic_types[kind] != NULL);
  return (void*)basic_types[kind];
}

type_t const*
type_array (type_t const* _host)
{
  assert (_host != NULL);
  type_rep_t const* host = (void*)_host;

  type_rep_t * ret = private_alloc_type (t_array);
  ret->host = host;

  static type_rep_t const* types = NULL;
  for (type_rep_t const* it = types;
       it != NULL; it = it->link)
    {
      if (types_same ((void*)it, (void*)ret))
	{
	  private_delete_type (ret);
	  return (void*)it;
	}
    }

  ret->link = types;
  types = ret;
  return (void*)ret;
}

type_t *
type_proc (type_t const* return_type)
{
  return NULL;
}

void
type_proc_add_param (type_t * type, symbol_t const* param)
{
}

#else /* SELF_TEST */

#include "type.h"
#include <stdio.h>
#include <assert.h>

int
main (void)
{
  type_t const* t1a = type_basic (t_int);
  type_t const* t1b = type_basic (t_int);
  assert (t1a == t1b);

  type_t const* t2a = type_basic (t_real);
  type_t const* t2b = type_basic (t_real);
  assert (t2a == t2b);
  assert (t1a != t2a);

  type_t const* t3a = type_basic (t_bool);
  type_t const* t3b = type_basic (t_bool);
  assert (t3a == t3b);
  assert (t1a != t3a);
  assert (t2a != t3a);

  type_t const* t4a = type_basic (t_label);
  type_t const* t4b = type_basic (t_label);
  assert (t4a == t4b);
  assert (t1a != t4a);
  assert (t2a != t4a);
  assert (t3a != t4a);

  type_t const* t5a = type_basic (t_string);
  type_t const* t5b = type_basic (t_string);
  assert (t5a == t5b);
  assert (t1a != t5a);
  assert (t2a != t5a);
  assert (t3a != t5a);
  assert (t4a != t5a);

  type_t const* t6a = type_array (type_basic (t_int));
  type_t const* t6b = type_array (t1a);
  assert (t6a == t6b);
  assert (t1a != t6a);
  assert (t2a != t6a);
  assert (t3a != t6a);
  assert (t4a != t6a);
  assert (t5a != t6a);

  type_t const* t7a = type_array (type_array (type_basic (t_int)));
  type_t const* t7b = type_array (type_array (t1a));
  type_t const* t7c = type_array (t6a);
  type_t const* t7d = type_array (t6b);
  assert (t7a == t7b);
  assert (t7a == t7c);
  assert (t7a == t7d);
  assert (t1a != t7a);
  assert (t2a != t7a);
  assert (t3a != t7a);
  assert (t4a != t7a);
  assert (t5a != t7a);
  assert (t6a != t7a);

  printf ("All passed.\n");
  return 0;
}

#endif
