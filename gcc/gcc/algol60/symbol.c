#include <stdlib.h>
#include <assert.h>
#include "meta.h"

#include "symbol.h"
#include "label.h"
#include "type.h"
#include "statement.h"
#include "estring.h"
#include "visitor.h"
#include "logger.h"
#include "visitor-impl.h"

static char const * private_symbol_signature = "symbol";

typedef struct struct_sym_var_t
{
}
sym_var_t;

typedef struct struct_sym_func_t
{
  /// List typed with a60_as_symbol, contains explicit parameters of
  /// the function.
  slist_t * params;

  /// List typed with a60_as_symbol, contains symbols that are
  /// unresolved in function body, and as such form implicit
  /// parameters of the function.
  slist_t * implied_params;

  /// Body of this function.
  statement_t * body;
}
sym_func_t;

typedef enum enum_symbol_kind_t
{
  sk_var,
  sk_func,
}
symbol_kind_t;

struct struct_symbol_t
{
  visitable_t base;
  label_t const * label;
  statement_t * stmt;
  type_t * type;
  int hidden;
  void * extra;

  union {
    sym_var_t sym_var;
    sym_func_t sym_func;
  };
};

static symbol_t *
private_alloc_symbol (label_t const * label, symbol_kind_t kind)
{
  symbol_t * ret = malloc (sizeof (symbol_t));
#ifndef NDEBUG
  ret->base.signature = private_symbol_signature;
#endif
  ret->base.kind = kind;
  ret->label = label;
  ret->stmt = NULL;
  ret->type = NULL;
  ret->hidden = 0;
  ret->extra = NULL;
  return ret;
}

symbol_t *
new_symbol_var (label_t const * label)
{
  assert (label != NULL);

  symbol_t * ret = private_alloc_symbol (label, sk_var);
  return ret;
}

symbol_t *
new_symbol_func (label_t const * label)
{
  assert (label != NULL);

  symbol_t * ret = private_alloc_symbol (label, sk_func);
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

  symbol_t * ret = private_alloc_symbol (label, self->base.kind);
  ret->stmt = self->stmt;
  ret->type = self->type;
  ret->hidden = self->hidden;
  ret->extra = self->extra;

  switch (self->base.kind)
    {
    case sk_var:
      break;

    case sk_func:
      break;
    }

  return ret;
}

void
delete_symbol (symbol_t * self)
{
  if (self != NULL)
    {
      free (self);

      switch (self->base.kind)
	{
	case sk_var:
	  break;

	case sk_func:
	  break;
	}
    }
}

symbol_t *
a60_as_symbol (void * obj)
{
#ifndef NDEBUG
  a60_check_access (obj, private_symbol_signature);
#endif
  return (symbol_t *)obj;
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
symbol_resolve_symbols (symbol_t * self, container_t * context, logger_t * log)
{
  switch (self->base.kind)
    {
    case sk_var:
      type_resolve_symbols (self->type, context, log);
      return;

    case sk_func:
      // @TODO: Do the right thing :)
      log_printf (log, ll_info, "note: skipping resolve of function symbol.");
      return;
    }
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

callback_t
a60_symbol_callback (void *(*cb)(symbol_t *, void *))
{
  return (callback_t)cb;
}
