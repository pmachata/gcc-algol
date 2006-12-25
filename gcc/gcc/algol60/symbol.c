#include <stdlib.h>
#include <assert.h>
#include "meta.h"

#include "symbol.h"
#include "label.h"
#include "type.h"
#include "statement.h"
#include "estring.h"

static char const * private_symbol_signature = "symbol";

struct struct_symbol_t
{
  char const * signature;
  label_t const * label;
  statement_t * stmt;
  type_t * type;
  int hidden;
  void * extra;
};

static symbol_t *
private_alloc_symbol (label_t const * label)
{
  symbol_t * ret = malloc (sizeof (symbol_t));
  ret->signature = private_symbol_signature;
  ret->label = label;
  return ret;
}

symbol_t *
new_symbol (label_t const * label)
{
  assert (label != NULL);

  symbol_t * ret = private_alloc_symbol (label);
  ret->stmt = NULL;
  ret->type = NULL;
  ret->hidden = 0;
  ret->extra = NULL;
  return ret;
}

symbol_t *
clone_symbol (symbol_t const * self)
{
  return clone_symbol_with_name (self, self->label);
}

symbol_t *
clone_symbol_with_name (symbol_t const * self, label_t const * label)
{
  assert (self != NULL);
  assert (label != NULL);

  symbol_t * ret = private_alloc_symbol (label);
  ret->stmt = self->stmt;
  ret->type = self->type;
  ret->hidden = self->hidden;
  ret->extra = self->extra;
  return ret;
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
symbol_label (symbol_t const * self)
{
  assert (self != NULL);
  return self->label;
}

estring_t *
symbol_to_str (symbol_t const * self, estring_t * buf)
{
  assert (self != NULL);
  return label_to_str (self->label, buf);
}

void
symbol_set_type (symbol_t * self, type_t * type)
{
  assert (self != NULL);
  assert (type != NULL);
  self->type = type;
}

type_t *
symbol_type (symbol_t const * self)
{
  assert (self != NULL);
  return self->type;
}

void
symbol_set_stmt (symbol_t * self, statement_t * stmt)
{
  assert (self != NULL);
  assert (stmt != NULL);
  self->stmt = stmt;
}

statement_t *
symbol_stmt (symbol_t const * self)
{
  assert (self != NULL);
  return self->stmt;
}

void
symbol_set_hidden (symbol_t * self, int hidden)
{
  assert (self != NULL);
  self->hidden = hidden;
}

int
symbol_hidden (symbol_t const * self)
{
  assert (self != NULL);
  return self->hidden;
}

void
symbol_set_extra (symbol_t * self, void * extra)
{
  assert (self != NULL);
  self->extra = extra;
}

void *
symbol_extra (symbol_t const * self)
{
  assert (self != NULL);
  return self->extra;
}
