#include "desig-expr.h"
#include "symbol.h"
#include "expression.h"
#include "meta.h"
#include "estring.h"
#include "logger.h"
#include "type.h"
#include "statement.h"

#include <stdlib.h>
#include <assert.h>

char const * const private_desig_expr_signature = "desig_expr";

typedef struct struct_desig_expr_label_rep_t
{
  label_t * lbl;
  symbol_t * sym;
}
desig_expr_label_rep_t;

typedef struct struct_desig_expr_cond_rep_t
{
  expression_t * cond;
  desig_expr_t * exp_t;
  desig_expr_t * exp_f;
}
desig_expr_cond_rep_t;

typedef struct struct_desig_expr_switch_rep_t
{
  label_t * lbl;
  expression_t * index;
  symbol_t * sym;
}
desig_expr_switch_rep_t;

typedef enum enum_desig_expr_kind_t
{
  dek_label,
  dek_cond,
  dek_switch,
}
desig_expr_kind_t;

struct struct_desig_expr_t
{
  char const * signature;
  desig_expr_kind_t kind;
  cursor_t * cursor;
  union {
    desig_expr_label_rep_t elbl;
    desig_expr_cond_rep_t econd;
    desig_expr_switch_rep_t eswitch;
  };
};

static desig_expr_t *
private_new_desig_expr (desig_expr_kind_t kind, cursor_t * location)
{
  desig_expr_t * ret = malloc (sizeof (desig_expr_t));
  ret->signature = private_desig_expr_signature;
  ret->kind = kind;
  ret->cursor = location;
  return ret;
}

desig_expr_t *
new_desig_expr_label (cursor_t * location, label_t * lbl)
{
  assert (lbl != NULL);

  desig_expr_t * ret = private_new_desig_expr (dek_label, location);
  ret->elbl.lbl = lbl;
  ret->elbl.sym = NULL;
  return ret;
}

desig_expr_t *
new_desig_expr_if (cursor_t * location, expression_t * cond, desig_expr_t * exp_t, desig_expr_t * exp_f)
{
  assert (cond != NULL);
  assert (exp_t != NULL);
  assert (exp_f != NULL);

  desig_expr_t * ret = private_new_desig_expr (dek_cond, location);
  ret->econd.cond = cond;
  ret->econd.exp_t = exp_t;
  ret->econd.exp_f = exp_f;
  return ret;
}

desig_expr_t *
new_desig_expr_switch (cursor_t * location, label_t * lbl, expression_t * index)
{
  assert (lbl != NULL);
  assert (index != NULL);

  desig_expr_t * ret = private_new_desig_expr (dek_switch, location);
  ret->eswitch.lbl = lbl;
  ret->eswitch.sym = NULL;
  ret->eswitch.index = index;
  return ret;
}

desig_expr_t *
clone_desig_expr (desig_expr_t const * self)
{
  assert (self != NULL);

  desig_expr_t * ret = private_new_desig_expr (self->kind, self->cursor);

  switch (self->kind)
    {
    case dek_label:
      ret->elbl.lbl = self->elbl.lbl;
      ret->elbl.sym = clone_symbol (self->elbl.sym);
      break;

    case dek_cond:
      ret->econd.cond = clone_expression (self->econd.cond);
      ret->econd.exp_t = clone_desig_expr (self->econd.exp_t);
      ret->econd.exp_f = clone_desig_expr (self->econd.exp_f);
      break;

    case dek_switch:
      ret->eswitch.lbl = self->eswitch.lbl;
      ret->eswitch.sym = clone_symbol (self->eswitch.sym);
      ret->eswitch.index = clone_expression (self->eswitch.index);
      break;
    };

  return ret;
}

desig_expr_t *
desig_expr (void * ptr)
{
  A60_CHECKED_CONVERSION (desig_expr, ptr);
}

static void
private_desig_expr_dump (desig_expr_t const * self, estring_t * buf)
{
  assert (self != NULL);

  switch (self->kind)
    {
    case dek_label:
      {
	estring_t * tmp = NULL;
	tmp = symbol_to_str (self->eswitch.sym, tmp);
	estr_append (buf, tmp);
	delete_estring (tmp);
	return;
      }

    case dek_cond:
      {
	estring_t * tmp = NULL;

	estr_append_cstr (buf, "'if' ");
	tmp = expr_to_str (self->econd.cond, tmp);
	estr_append (buf, tmp);

	estr_append_cstr (buf, " 'then' ");
	if (self->econd.exp_t->kind != dek_label)
	  estr_push (buf, '(');
	private_desig_expr_dump (self->econd.exp_t, buf);
	if (self->econd.exp_t->kind != dek_label)
	  estr_push (buf, ')');

	estr_append_cstr (buf, " 'else' ");
	if (self->econd.exp_f->kind != dek_label)
	  estr_push (buf, '(');
	private_desig_expr_dump (self->econd.exp_t, buf);
	if (self->econd.exp_f->kind != dek_label)
	  estr_push (buf, ')');

	delete_estring (tmp);
	return;
      }

    case dek_switch:
      {
	estring_t * tmp = NULL;

	tmp = symbol_to_str (self->eswitch.sym, tmp);
	estr_append (buf, tmp);

	estr_push (buf, '[');
	tmp = expr_to_str (self->eswitch.index, tmp);
	estr_append (buf, tmp);
	estr_push (buf, ']');

	delete_estring (tmp);
	return;
      }
    };

  assert (!"Should never get there!");
}

estring_t *
desig_expr_to_str (desig_expr_t const * self, estring_t * buf)
{
  assert (self != NULL);

  if (buf == NULL)
    buf = new_estring ();
  else
    estr_clear (buf);

  private_desig_expr_dump (self, buf);

  return buf;
}

void
desig_expr_resolve_symbols (desig_expr_t * self, container_t * context, logger_t * log)
{
  assert (self != NULL);
  assert (log != NULL);

  switch (self->kind)
    {
    case dek_label:
      {
	self->elbl.sym
	  = container_find_name_rec_add_undefined (context, self->elbl.lbl,
						   type_label (),
						   log, self->cursor);
	return;
      }

    case dek_cond:
      {
	expr_resolve_symbols (self->econd.cond, context, log);
	desig_expr_resolve_symbols (self->econd.exp_t, context, log);
	desig_expr_resolve_symbols (self->econd.exp_f, context, log);

	if (!types_same (expr_type (self->econd.cond), type_bool ()))
	  {
	    estring_t * es = desig_expr_to_str (self, NULL);
	    estring_t * tmp = type_to_str (expr_type (self->econd.cond), NULL);
	    estr_prepend_cstr (es, "type mismatch in expression `");
	    estr_append_cstr (es, "': ");
	    estr_append (es, tmp);
	    estr_append_cstr (es, " should be 'Boolean'");

	    log_printfc (log, ll_error, self->cursor, "%s", estr_cstr (es));

	    delete_estring (es);
	    delete_estring (tmp);
	  }
	return;
      }

    case dek_switch:
      {
	// @@@TODO
	assert (!"NYI!");
	return;
      }
    }

  assert (!"Should never get there!");
}

cursor_t *
desig_expr_cursor (desig_expr_t const * self)
{
  assert (self != NULL);
  return self->cursor;
}

expression_t *
desig_expr_if_cond (desig_expr_t const * self)
{
  assert (self != NULL);
  assert (self->kind == dek_cond);
  return self->econd.cond;
}

desig_expr_t *
desig_expr_if_trueb (desig_expr_t const * self)
{
  assert (self != NULL);
  assert (self->kind == dek_cond);
  return self->econd.exp_t;
}

desig_expr_t *
desig_expr_if_falseb (desig_expr_t const * self)
{
  assert (self != NULL);
  assert (self->kind == dek_cond);
  return self->econd.exp_f;
}

symbol_t *
desig_expr_symbol (desig_expr_t const * self)
{
  assert (self != NULL);

  if (self->kind == dek_label)
    return self->elbl.sym;
  else
    {
      assert (self->kind == dek_switch);
      return self->eswitch.sym;
    }
}

expression_t *
desig_expr_switch_index (desig_expr_t const * self)
{
  assert (self != NULL);
  assert (self->kind == dek_switch);
  return self->eswitch.index;
}

#ifndef IN_GCC
static void ATTRIBUTE_NORETURN
no_glue_built (void)
{
  fprintf (stderr, "Dummy expr_*_build_generic called.\n");
  abort ();
}
void * desig_expr_label_build_generic (desig_expr_t * self, void * data) { no_glue_built (); }
void * desig_expr_if_build_generic (desig_expr_t * self, void * data) { no_glue_built (); }
void * desig_expr_switch_build_generic (desig_expr_t * self, void * data) { no_glue_built (); }
#endif


void *
desig_expr_build_generic (desig_expr_t * self, void * data)
{
  assert (self != NULL);
  switch (self->kind)
    {
    case dek_label:
      return desig_expr_label_build_generic (self, data);
    case dek_cond:
      return desig_expr_if_build_generic (self, data);
    case dek_switch:
      return desig_expr_switch_build_generic (self, data);
    };
  assert (!"Should never get there!");
}
