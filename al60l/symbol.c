/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef SELF_TEST

#include "estring.h"
#include "symbol.h"
#include "type.h"
#include <assert.h>

static char const* private_symbol_signature = "symbol";

typedef struct struct_symbol_rep_t
{
  char const* signature;
  estring_t * name;
  statement * stmt;
  type_t const* type;
} symbol_rep_t;


symbol_t *
new_symbol (estring_t const* name)
{
  symbol_rep_t * ret = malloc (sizeof (symbol_rep_t));
  if (ret == NULL)
    return NULL;

  ret->signature = private_symbol_signature;
  ret->name = clone_estring (name);
  ret->stmt = NULL;
  ret->type = NULL;

  return (void*)ret;
}

void
delete_symbol (symbol_t * _symbol)
{
  if (_symbol != NULL)
    {
      symbol_rep_t * symbol = (void*)_symbol;
      delete_estring (symbol->name);
      free (symbol);
    }
}

symbol_t *
symbol (void * ptr)
{
  if (((symbol_rep_t*)ptr)->signature == private_symbol_signature)
    return ptr;
  else
    return NULL;
}

void
symbol_assign_stmt (symbol_t * _symbol, statement * stmt)
{
  assert (_symbol != NULL);
  symbol_rep_t * symbol = (void*)_symbol;
  assert (symbol->stmt == NULL);
  assert (stmt != NULL);
  symbol->stmt = stmt;
}

void
symbol_assign_type (symbol_t * _symbol, type_t * type)
{
  assert (_symbol != NULL);
  symbol_rep_t * symbol = (void*)_symbol;
  assert (symbol->type == NULL);
  assert (type != NULL);
  symbol->type = type;
}

estring_t const*
symbol_name (symbol_t * _symbol)
{
  assert (_symbol != NULL);
  symbol_rep_t * symbol = (void*)_symbol;
  return symbol->name;
}

type_t const*
symbol_type (symbol_t * _symbol)
{
  assert (_symbol != NULL);
  symbol_rep_t * symbol = (void*)_symbol;
  return (void*)symbol->type;
}

statement const*
symbol_stmt (symbol_t * _symbol)
{
  assert (_symbol != NULL);
  symbol_rep_t * symbol = (void*)_symbol;
  return (void*)symbol->stmt;
}

#else /* SELF_TEST */

#include "symbol.h"
#include "estring.h"
#include <assert.h>
#include <stdio.h>

int
main (void)
{
  estring_t * e = new_estring_from ("some_name");
  symbol_t * sym1 = new_symbol (e);
  symbol_t * sym2 = new_symbol (e);
  assert (symbol (sym1));
  assert (symbol (sym2));

  delete_estring (e);
  assert (estr_compare (symbol_name (sym1), symbol_name (sym2)) == 0);

  delete_symbol (sym1);
  assert (estr_compare_cstr (symbol_name (sym2), "some_name") == 0);

  delete_symbol (sym2);

  printf ("All passed.\n");
  return 0;
}

#endif
