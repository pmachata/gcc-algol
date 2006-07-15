/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef SELF_TEST

#include "type.h"
#include <assert.h>
#include <stdlib.h>

static char const* private_type_signature = "type";

typedef enum enum_type_kind_t
{
  t_unknown,
  t_any,
  t_int,
  t_real,
  t_string,
  t_bool,
  t_procedure,
  t_label,
  t_array,
  t_own,
  type_kind_t_count
} type_kind_t;

typedef struct struct_type_rep_t
{
  char const* signature;
  type_kind_t kind;
  struct struct_type_rep_t const* link;
  struct struct_type_rep_t const* host;
} type_rep_t;

static type_rep_t const* basic_types[type_kind_t_count] = {};

static type_rep_t *
private_alloc_type (type_kind_t kind)
{
  type_rep_t * ret = malloc (sizeof (type_rep_t));
  ret->signature = private_type_signature;
  ret->kind = kind;
  ret->host = NULL;
  ret->link = NULL;
  return ret;
}

static void
private_delete_type (type_rep_t * type)
{
  free (type);
}

static type_t const*
private_new_type_basic (type_kind_t kind)
{
  assert (kind == t_int
	  || kind == t_real
	  || kind == t_string
	  || kind == t_bool
	  || kind == t_label
	  || kind == t_unknown
	  || kind == t_any
	  || !"Only real, string, bool, label, any and unknown types are considered basic.");

  if (basic_types[kind] == NULL)
    basic_types[kind] = private_alloc_type (kind);

  assert (basic_types[kind] != NULL);
  return (void*)basic_types[kind];
}

type_t const*
type_unknown (void)
{
  return private_new_type_basic (t_unknown);
}

type_t const*
type_any (void)
{
  return private_new_type_basic (t_any);
}

type_t const*
type_int (void)
{
  return private_new_type_basic (t_int);
}

type_t const*
type_real (void)
{
  return private_new_type_basic (t_real);
}

type_t const*
type_string (void)
{
  return private_new_type_basic (t_string);
}

type_t const*
type_bool (void)
{
  return private_new_type_basic (t_bool);
}

type_t const*
type_label (void)
{
  return private_new_type_basic (t_label);
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

type_t const*
type_own (type_t const* _host)
{
  assert (_host != NULL);
  type_rep_t const* host = (void*)_host;
  assert (host->kind != t_own);

  type_rep_t * ret = private_alloc_type (t_own);
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

type_t const*
type_proc (type_t const* return_type, ...)
{
  return NULL;
}

type_t const*
type (void const* ptr)
{
  if (((type_rep_t const*)ptr)->signature == private_type_signature)
    return ptr;
  else
    return NULL;
}

estring_t *
type_str (type_t const* _type, estring_t * buf)
{
  assert (_type != NULL);
  type_rep_t const* type = (void*)_type;

  if (buf == NULL)
    buf = new_estring ();

  switch (type->kind) {
  case t_unknown:
    estr_assign_cstr (buf, "'unknown'");
    return buf;

  case t_any:
    estr_assign_cstr (buf, "'any'");
    return buf;

  case t_int:
    estr_assign_cstr (buf, "'integer'");
    return buf;

  case t_real:
    estr_assign_cstr (buf, "'real'");
    return buf;

  case t_string:
    estr_assign_cstr (buf, "'string'");
    return buf;

  case t_bool:
    estr_assign_cstr (buf, "'Boolean'");
    return buf;

  case t_procedure:
    estr_assign_cstr (buf, "<proc>-NYI");
    return buf;

  case t_label:
    estr_assign_cstr (buf, "<label>");
    return buf;

  case t_array:
    type_str ((type_t const*)type->host, buf);
    estr_append_cstr (buf, " 'array'");
    return buf;

  case t_own:
    type_str ((type_t const*)type->host, buf);
    estr_prepend_cstr (buf, "'own' ");
    return buf;

  case type_kind_t_count:
    ;
  };
  assert (!"should never get there!");
  return NULL;
}

int
types_same (type_t const* _lhs, type_t const* _rhs)
{
  assert (_lhs != NULL);
  type_rep_t const* lhs = (void*)_lhs;

  assert (_rhs != NULL);
  type_rep_t const* rhs = (void*)_rhs;

  // if kinds differ, the types aren't the same
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

int
types_match (type_t const* _lhs, type_t const* _rhs)
{
  assert (_lhs != NULL);
  type_rep_t const* lhs = (void*)_lhs;

  assert (_rhs != NULL);
  type_rep_t const* rhs = (void*)_rhs;

  // any matches everything
  if (lhs->kind == t_any
      || rhs->kind == t_any)
    return 1;

  // unknown matches nothing
  if (lhs->kind == t_unknown
      || rhs->kind == t_unknown)
    return 0;

  // two arrays match, if their hosting types match
  if (lhs->kind == t_array
      && rhs->kind == t_array)
    return types_match ((type_t const*)lhs->host, (type_t const*)rhs->host);

  // matching of two procedures in NYI, but the rules will be along
  // these lines:
  //  + returning types have to match
  //  + both procedures have to have the same number of parameters
  //  + the types of arguments have to match
  if (lhs->kind == t_procedure
      && rhs->kind == t_procedure)
    assert (!"NYI!");

  // if no pattern was found so far, we will do simple comparison and
  // see if it holds
  return types_same (_lhs, _rhs);
}

#else /* SELF_TEST */

#include "type.h"
#include <stdio.h>
#include <assert.h>

int
main (void)
{
  printf (" + basic types\n");
  type_t const* t1a = type_int ();
  type_t const* t1b = type_int ();
  assert (type (t1a));
  assert (type (t1b));
  assert (t1a == t1b);
  assert (types_same (t1a, t1b));
  assert (types_match (t1a, t1b));

  type_t const* t2a = type_real ();
  type_t const* t2b = type_real ();
  assert (type (t2a));
  assert (type (t2b));
  assert (t2a == t2b);
  assert (types_same (t2a, t2b));
  assert (types_match (t2a, t2b));
  assert (t1a != t2a);
  assert (!types_same (t1a, t2a));
  assert (!types_match (t1a, t2a));

  type_t const* t3a = type_bool ();
  type_t const* t3b = type_bool ();
  assert (type (t3a));
  assert (type (t3b));
  assert (t3a == t3b);
  assert (types_same (t3a, t3b));
  assert (types_match (t3a, t3b));
  assert (t1a != t3a);
  assert (!types_same (t1a, t3a));
  assert (!types_match (t1a, t3a));
  assert (t2a != t3a);
  assert (!types_same (t2a, t3a));
  assert (!types_match (t2a, t3a));

  type_t const* t4a = type_label ();
  type_t const* t4b = type_label ();
  assert (type (t4a));
  assert (type (t4b));
  assert (t4a == t4b);
  assert (types_same (t4a, t4b));
  assert (types_match (t4a, t4b));
  assert (t1a != t4a);
  assert (!types_same (t1a, t4a));
  assert (!types_match (t1a, t4a));
  assert (t2a != t4a);
  assert (!types_same (t2a, t4a));
  assert (!types_match (t2a, t4a));
  assert (t3a != t4a);
  assert (!types_same (t3a, t4a));
  assert (!types_match (t3a, t4a));

  type_t const* t5a = type_string ();
  type_t const* t5b = type_string ();
  assert (type (t5a));
  assert (type (t5b));
  assert (t5a == t5b);
  assert (types_same (t5a, t5b));
  assert (types_match (t5a, t5b));
  assert (t1a != t5a);
  assert (!types_same (t1a, t5a));
  assert (!types_match (t1a, t5a));
  assert (t2a != t5a);
  assert (!types_same (t2a, t5a));
  assert (!types_match (t2a, t5a));
  assert (t3a != t5a);
  assert (!types_same (t3a, t5a));
  assert (!types_match (t3a, t5a));
  assert (t4a != t5a);
  assert (!types_same (t4a, t5a));
  assert (!types_match (t4a, t5a));

  printf (" + arrays\n");
  type_t const* t6a = type_array (type_int ());
  type_t const* t6b = type_array (t1a);
  assert (type (t6a));
  assert (type (t6b));
  assert (t6a == t6b);
  assert (types_same (t6a, t6b));
  assert (types_match (t6a, t6b));
  assert (t1a != t6a);
  assert (!types_same (t1a, t6a));
  assert (!types_match (t1a, t6a));
  assert (t2a != t6a);
  assert (!types_same (t2a, t6a));
  assert (!types_match (t2a, t6a));
  assert (t3a != t6a);
  assert (!types_same (t3a, t6a));
  assert (!types_match (t3a, t6a));
  assert (t4a != t6a);
  assert (!types_same (t4a, t6a));
  assert (!types_match (t4a, t6a));
  assert (t5a != t6a);
  assert (!types_same (t5a, t6a));
  assert (!types_match (t5a, t6a));

  type_t const* t7a = type_array (type_array (type_int ()));
  type_t const* t7b = type_array (type_array (t1a));
  type_t const* t7c = type_array (t6a);
  type_t const* t7d = type_array (t6b);
  assert (type (t7a));
  assert (type (t7b));
  assert (type (t7c));
  assert (type (t7d));
  assert (t7a == t7b);
  assert (types_same (t7a, t7b));
  assert (types_match (t7a, t7b));
  assert (t7a == t7c);
  assert (types_same (t7a, t7c));
  assert (types_match (t7a, t7c));
  assert (t7a == t7d);
  assert (types_same (t7a, t7d));
  assert (types_match (t7a, t7d));
  assert (t1a != t7a);
  assert (!types_same (t1a, t7a));
  assert (!types_match (t1a, t7a));
  assert (t2a != t7a);
  assert (!types_same (t2a, t7a));
  assert (!types_match (t2a, t7a));
  assert (t3a != t7a);
  assert (!types_same (t3a, t7a));
  assert (!types_match (t3a, t7a));
  assert (t4a != t7a);
  assert (!types_same (t4a, t7a));
  assert (!types_match (t4a, t7a));
  assert (t5a != t7a);
  assert (!types_same (t5a, t7a));
  assert (!types_match (t5a, t7a));
  assert (t6a != t7a);
  assert (!types_same (t6a, t7a));
  assert (!types_match (t6a, t7a));

  printf (" + any\n");
  type_t const* t8a = type_any ();
  type_t const* t8b = type_any ();
  assert (type (t8a));
  assert (type (t8b));
  assert (t8a == t8b);
  assert (types_same (t8a, t8b));
  assert (types_match (t8a, t8b));
  assert (t1a != t8a);
  assert (!types_same (t1a, t8a));
  assert (types_match (t1a, t8a));
  assert (t2a != t8a);
  assert (!types_same (t2a, t8a));
  assert (types_match (t2a, t8a));
  assert (t3a != t8a);
  assert (!types_same (t3a, t8a));
  assert (types_match (t3a, t8a));
  assert (t4a != t8a);
  assert (!types_same (t4a, t8a));
  assert (types_match (t4a, t8a));
  assert (t5a != t8a);
  assert (!types_same (t5a, t8a));
  assert (types_match (t5a, t8a));
  assert (t6a != t8a);
  assert (!types_same (t6a, t8a));
  assert (types_match (t6a, t8a));
  assert (t7a != t8a);
  assert (!types_same (t7a, t8a));
  assert (types_match (t7a, t8a));

  // this test is slightly different from the rest, it checks whether
  // 'array of any' will match 'array of int' and 'array of array of
  // int'
  type_t const* t81a = type_array (t8a);
  type_t const* t81b = type_array (type_any ());
  type_t const* t81c = type_array (type_int ());
  type_t const* t81d = type_array (type_array (type_int ()));
  assert (type (t81a));
  assert (type (t81b));
  assert (type (t81c));
  assert (type (t81d));
  assert (t81a == t81b);
  assert (types_same (t81a, t81b));
  assert (t81a != t81c);
  assert (!types_same (t81a, t81c));
  assert (t81a != t81d);
  assert (!types_same (t81a, t81d));
  assert (t81c != t81d);
  assert (!types_same (t81c, t81d));

  assert (types_match (t8a, t81a)); // a,b,c,d matches ANY
  assert (types_match (t8a, t81b));
  assert (types_match (t8a, t81c));
  assert (types_match (t8a, t81d));
  assert (types_match (t81a, t81c)); // array of any matches array of int
  assert (types_match (t81a, t81d)); // array of any matches array of array
  assert (!types_match (t81a, t1a)); // array of any doesn't match basic
  assert (!types_match (t81a, t2a));
  assert (!types_match (t81a, t3a));
  assert (!types_match (t81a, t4a));
  assert (!types_match (t81a, t5a));

  printf (" + unknown\n");
  type_t const* t9a = type_unknown ();
  type_t const* t9b = type_unknown ();
  assert (type (t9a));
  assert (type (t9b));
  assert (t9a == t9b);
  assert (types_same (t9a, t9b));
  assert (!types_match (t9a, t9b)); // don't match yourself
  assert (t1a != t9a);
  assert (!types_same (t1a, t9a));
  assert (!types_match (t1a, t9a));
  assert (t2a != t9a);
  assert (!types_same (t2a, t9a));
  assert (!types_match (t2a, t9a));
  assert (t3a != t9a);
  assert (!types_same (t3a, t9a));
  assert (!types_match (t3a, t9a));
  assert (t4a != t9a);
  assert (!types_same (t4a, t9a));
  assert (!types_match (t4a, t9a));
  assert (t5a != t9a);
  assert (!types_same (t5a, t9a));
  assert (!types_match (t5a, t9a));
  assert (t6a != t9a);
  assert (!types_same (t6a, t9a));
  assert (!types_match (t6a, t9a));
  assert (t7a != t9a);
  assert (!types_same (t7a, t9a));
  assert (!types_match (t7a, t9a));
  assert (t8a != t9a);
  assert (!types_same (t8a, t9a));
  assert (types_match (t8a, t9a)); // match ANY

  printf ("All passed.\n");
  return 0;
}

#endif
