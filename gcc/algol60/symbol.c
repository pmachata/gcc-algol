#include <stdlib.h>
#include <assert.h>
#include "meta.h"

#include "symbol.h"
#include "label.h"
#include "type.h"
#include "statement.h"
#include "estring.h"

static char const * private_symbol_signature = "symbol";

typedef struct struct_symbol_rep_t
{
  char const * signature;
  label_t const * label;
  statement_t * stmt;
  type_t * type;
  int hidden;
  void * extra;
}
symbol_rep_t;

symbol_t *
new_symbol (label_t const * name)
{
  symbol_rep_t * ret = malloc (sizeof (symbol_rep_t));
  ret->signature = private_symbol_signature;
  ret->label = name;
  ret->stmt = NULL;
  ret->type = NULL;
  ret->hidden = 0;
  ret->extra = NULL;
  return (void*)ret;
}

void
delete_symbol (symbol_t * self)
{
  free (self);
}

symbol_t *
symbol (void * ptr)
{
  A60_CHECKED_CONVERSION (symbol, ptr);
}

label_t const *
symbol_label (symbol_t const * _self)
{
  A60_USER_TO_REP(symbol, self, const *);
  return self->label;
}

estring_t *
symbol_to_str (symbol_t const * _self, estring_t * buf)
{
  A60_USER_TO_REP(symbol, self, const *);
  return label_to_str (self->label, buf);
}

void
symbol_set_type (symbol_t * _self, type_t * type)
{
  A60_USER_TO_REP(symbol, self, *);
  assert (type != NULL);
  self->type = type;
}

type_t *
symbol_type (symbol_t const * _self)
{
  A60_USER_TO_REP(symbol, self, const *);
  return self->type;
}

void
symbol_set_stmt (symbol_t * _self, statement_t * stmt)
{
  A60_USER_TO_REP(symbol, self, *);
  assert (stmt != NULL);
  self->stmt = stmt;
}

statement_t *
symbol_stmt (symbol_t const * _self)
{
  A60_USER_TO_REP(symbol, self, const *);
  return self->stmt;
}

void
symbol_set_hidden (symbol_t * _self, int hidden)
{
  A60_USER_TO_REP(symbol, self, *);
  self->hidden = hidden;
}

int
symbol_hidden (symbol_t const * _self)
{
  A60_USER_TO_REP(symbol, self, const *);
  return self->hidden;
}

void
symbol_set_extra (symbol_t * _self, void * extra)
{
  A60_USER_TO_REP(symbol, self, *);
  self->extra = extra;
}

void *
symbol_extra (symbol_t const * _self)
{
  A60_USER_TO_REP(symbol, self, const *);
  return self->extra;
}
