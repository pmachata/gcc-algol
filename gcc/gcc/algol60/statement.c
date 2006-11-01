#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "statement.h"
#include "cursor.h"
#include "slist.h"
#include "expression.h"
#include "logger.h"
#include "symbol.h"
#include "label.h"
#include "type.h"
#include "estring.h"
#include "boundspair.h"
#include "gcc.h"
#include "meta.h"

char const * private_statement_signature = "statement";

typedef struct struct_stmt_dummy_rep_t
{
}
stmt_dummy_rep_t;

typedef struct struct_stmt_assign_rep_t
{
  expression_t * rhs;
  slist_t * lhss;
}
stmt_assign_rep_t;

typedef struct struct_stmt_call_rep_t
{
  expression_t * call;
}
stmt_call_rep_t;

/// Used for blocks as well as for toplevs.
typedef struct struct_container_rep_t
{
  slist_t * statements;
  slist_t * symtab;
}
container_rep_t;

typedef enum enum_stmt_kind_t
{
  sk_dummy,
  sk_assign,
  sk_call,
  sk_block,
  sk_toplev
}
stmt_kind_t;

struct struct_statement_t
{
  char const * signature;
  stmt_kind_t kind;

  cursor_t * cursor;
  container_t * parent;
  slist_t * labels;
  union {
    stmt_dummy_rep_t dummy;
    stmt_assign_rep_t assign;
    stmt_call_rep_t call;
    container_rep_t block; ///< Used by both block and toplev.
  };
};

static void *
private_check_symbol_label (void * ptr, void * data ATTRIBUTE_UNUSED)
{
  symbol_t * sym = symbol (ptr);
  if (sym && !types_same (symbol_type (sym), type_label ()))
    sym = NULL;
  return sym;
}

static statement_t *
private_new_statement (stmt_kind_t kind, cursor_t * cursor, container_t * parent)
{
  statement_t * ret = malloc (sizeof (statement_t));
  ret->signature = private_statement_signature;
  ret->kind = kind;
  ret->cursor = cursor;
  ret->parent = parent;
  ret->labels = new_slist_typed (private_check_symbol_label, NULL);
  return ret;
}

statement_t *
new_stmt_dummy (cursor_t * cursor)
{
  return private_new_statement (sk_dummy, cursor, NULL);
}

static void *
private_check_expr_lvalue (void * ptr, void * data ATTRIBUTE_UNUSED)
{
  expression_t * expr = expression (ptr);
  if (expr && !expr_is_lvalue (expr))
    expr = NULL;
  return expr;
}

statement_t *
new_stmt_assign (cursor_t * cursor, slist_t * lhss, expression_t * rhs)
{
  assert (lhss != NULL);
  assert (rhs != NULL);

  statement_t * ret = private_new_statement (sk_assign, cursor, NULL);
  slist_set_type (lhss, private_check_expr_lvalue, NULL);
  ret->assign.lhss = lhss;
  ret->assign.rhs = rhs;

  return ret;
}

statement_t *
new_stmt_call (cursor_t * cursor, expression_t * call)
{
  assert (call != NULL);

  statement_t * ret = private_new_statement (sk_call, cursor, NULL);
  ret->call.call = call;
  return ret;
}

static container_t *
private_new_container (stmt_kind_t kind, cursor_t * cursor)
{
  statement_t * ret = private_new_statement (kind, cursor, NULL);
  ret->block.statements = new_slist_typed (adapt_test, statement);
  ret->block.symtab = new_slist_typed (adapt_test, symbol);
  return (container_t *)ret;
}

container_t *
new_stmt_block (cursor_t * cursor)
{
  return private_new_container (sk_block, cursor);
}

container_t *
new_stmt_toplev (cursor_t * cursor)
{
  return private_new_container (sk_toplev, cursor);
}

statement_t *
statement (void * ptr)
{
  A60_CHECKED_CONVERSION (statement, ptr);
}

container_t *
container (void * ptr)
{
  statement_t * st;
  if ((st = statement (ptr))
      && (st->kind == sk_block
	  || st->kind == sk_toplev))
    return (void*)st;
  else
    return NULL;
}

statement_t *
as_statement (container_t * container)
{
  return statement (container);
}

container_t *
as_container (statement_t * stmt)
{
  return container (stmt);
}

static char const *
padding (int level)
{
  static char const* padding =
    "                                                            "
    "                                                            "
    "                                                            "
    "                                                            "
    "                                                            ";
  if (level > 300)
    level = 300;
  return padding + 300 - level;
}


static void private_stmt_dump (statement_t const * self, estring_t * buf, int level);

static void
private_dump_container (statement_t const * self, estring_t * buf, int level)
{
  estring_t * buf0 = new_estring ();

  // dump all variables but labels and hidden symbols
  slist_it_t * it = slist_iter (self->block.symtab);
  for (; slist_it_has (it); slist_it_next (it))
    {
      symbol_t * sym = slist_it_get (it);
      type_t const * symtype = symbol_type (sym);
      if (types_same (symtype, type_label ())
	  || symbol_hidden (sym))
	continue;

      type_to_str_canon (symtype, buf0);
      estr_append_cstr (buf, padding (level));
      estr_append (buf, buf0);
      estr_push (buf, ' ');
      estr_append (buf, label_id (symbol_label (sym)));
      if (type_is_own (symtype))
	symtype = type_host (symtype);
      if (type_is_array (symtype))
	{
	  estr_push (buf, '[');
	  int first = 1;
	  for (; type_is_array (symtype); symtype = type_host (symtype))
	    {
	      boundspair_t * bp = t_array_bounds (symtype);
	      if (first)
		first = 0;
	      else
		estr_push (buf, ',');
	      expr_to_str (boundspair_lo (bp), buf0);
	      estr_append (buf, buf0);
	      estr_push (buf, ':');
	      expr_to_str (boundspair_hi (bp), buf0);
	      estr_append (buf, buf0);
	    }
	  estr_push (buf, ']');
	}
      estr_append_cstr (buf, ";\n");
    }
  delete_slist_it (it);

  // dump all statemenets
  it = slist_iter (self->block.statements);
  for (;slist_it_has (it);)
    {
      statement_t * stmt = slist_it_get (it);

      // look for any labels pointing to this command
      slist_it_t * lt = slist_iter (stmt_labels (stmt));
      for (; slist_it_has (lt); slist_it_next (lt))
	{
	  symbol_t * sym = slist_it_get (lt);
	  estr_append_cstr (buf, padding (level));
	  estr_append (buf, label_id (symbol_label (sym)));
	  estr_append_cstr (buf, ":\n");
	}
      delete_slist_it (lt);

      slist_it_next (it);

      // dump the statement itself.  Don't dump it, if there are no
      // more statements in container (i.e. `stmt' is last statement)
      // and `stmt' is dummy statement.
      if (slist_it_has (it)
	  || ((statement_t *)stmt)->kind != sk_dummy)
	private_stmt_dump (stmt, buf, level);
      else
	break;
    }
  delete_slist_it (it);

  delete_estring (buf0);
}

static void
private_dump_assign (statement_t const * self, estring_t * buf, int level)
{
  slist_it_t * it = slist_iter (self->assign.lhss);
  estring_t * buf0 = new_estring ();
  estr_append_cstr (buf, padding (level));
  for (; slist_it_has (it); slist_it_next (it))
    {
      expression_t * expr = slist_it_get (it);
      estr_append (buf, expr_to_str (expr, buf0));
      estr_append_cstr (buf, " := ");
    }
  estr_append (buf, expr_to_str (self->assign.rhs, buf0));
  estr_append_cstr (buf, ";\n");
  delete_estring (buf0);
  delete_slist_it (it);
}

static void
private_stmt_dump (statement_t const * self, estring_t * buf, int level)
{
  switch (self->kind)
    {
    case sk_dummy:
      return;

    case sk_assign:
      private_dump_assign (self, buf, level);
      return;

    case sk_call:
      {
	estring_t * buf0 = expr_to_str (self->call.call, NULL);
	estr_append_cstr (buf, padding (level));
	estr_append (buf, buf0);
	estr_append_cstr (buf, ";\n");
	delete_estring (buf0);
	return;
      }

    case sk_block:
      estr_append_cstr (buf, padding (level));
      estr_append_cstr (buf, "'begin'\n");
      private_dump_container (self, buf, level+1);
      estr_append_cstr (buf, padding (level));
      estr_append_cstr (buf, "'end';\n");
      return;

    case sk_toplev:
      private_dump_container (self, buf, level);
      return;
    };

  assert (!"Should never get there!");
}

estring_t *
stmt_to_str (statement_t const * self, estring_t * buf)
{
  assert (self != NULL);

  if (buf == NULL)
    buf = new_estring ();
  else
    estr_clear (buf);

  private_stmt_dump (self, buf, 0);
  return buf;
}

static void
private_resolve_symbols_assign (statement_t * self, logger_t * log)
{
  expr_resolve_symbols (self->assign.rhs, self->parent, log);
  slist_it_t * it = slist_iter (self->assign.lhss);
  for (; slist_it_has (it); slist_it_next (it))
    expr_resolve_symbols (slist_it_get (it), self->parent, log);
  delete_slist_it (it);

  type_t * tt = expr_type (self->assign.rhs);
  it = slist_iter (self->assign.lhss);
  for (; slist_it_has (it); slist_it_next (it))
    {
      expression_t * lhs = slist_it_get (it);
      type_t * t1 = expr_type (lhs);
      if (!types_match (tt, t1))
	{
	  estring_t * s1 = expr_to_str (lhs, NULL);
	  estring_t * s2 = expr_to_str (self->assign.rhs, NULL);
	  estring_t * s3 = type_to_str (t1, NULL);
	  estring_t * s4 = type_to_str (tt, NULL);
	  log_printfc (log, ll_error, self->cursor,
		       "type mismatch in assignment %s := %s (%s doesn't match %s)",
		       estr_cstr (s1), estr_cstr (s2), estr_cstr (s3), estr_cstr (s4));
	  delete_estring (s1);
	  delete_estring (s2);
	  delete_estring (s3);
	  delete_estring (s4);
	}
      // Shouldn't be necessary to check this, since we now have
      // strongly typed slist that only holds lvalue expressions, and
      // this error is reported during syntax analysis.  But better
      // safe than sorry.
      if (!expr_is_lvalue (lhs))
	{
	  estring_t * s1 = expr_to_str (lhs, NULL);
	  log_printfc (log, ll_error, self->cursor,
		      "%s has to be an lvalue",
		      estr_cstr (s1));
	  delete_estring (s1);
	}
    }
  delete_slist_it (it);
}

static void
private_resolve_symbols_block (statement_t * self, logger_t * log)
{
  assert (container (self));
  slist_it_t * it;

  it = slist_iter (self->block.symtab);
  for (; slist_it_has (it); slist_it_next (it))
    {
      symbol_t * sym = slist_it_get (it);
      type_resolve_symbols (symbol_type (sym), container (self), log);
    }
  delete_slist_it (it);

  it = slist_iter (self->block.statements);
  for (; slist_it_has (it); slist_it_next (it))
    stmt_resolve_symbols (slist_it_get (it), log);
  delete_slist_it (it);
}

void
stmt_resolve_symbols (statement_t * self, logger_t * log)
{
  assert (self != NULL);
  assert (log != NULL);

  switch (self->kind)
    {
    case sk_dummy: return;

    case sk_assign:
      private_resolve_symbols_assign (self, log);
      return;

    case sk_call:
      expr_resolve_symbols (self->call.call, self->parent, log);
      return;

    case sk_block:
    case sk_toplev:
      private_resolve_symbols_block (self, log);
      return;
    };

  assert (!"Should never get there!");
}

container_t *
stmt_parent (statement_t const * self)
{
  assert (self != NULL);
  return self->parent;
}

container_t *
container_parent (container_t const * _self)
{
  assert (_self != NULL);
  A60_USER_TO_REP (statement, self, const *);
  assert (self->kind == sk_block || self->kind == sk_toplev);
  return self->parent;
}

void
stmt_set_parent (statement_t * self, container_t * parent)
{
  assert (self != NULL);
  self->parent = parent;
}

void
container_set_parent (container_t * _self, container_t * parent)
{
  assert (_self != NULL);
  A60_USER_TO_REP (statement, self, *);
  assert (self->kind == sk_block || self->kind == sk_toplev);

  self->parent = parent;
}

void
container_add_stmt (container_t * _self, statement_t * stmt)
{
  assert (_self != NULL);
  assert (stmt != NULL);
  A60_USER_TO_REP (statement, self, *);
  assert (self->kind == sk_block || self->kind == sk_toplev);

  slist_pushback (self->block.statements, stmt);
  stmt->parent = _self;
}

slist_t *
stmt_assign_lhss (statement_t const * self)
{
  assert (self != NULL);
  assert (self->kind == sk_assign);
  return self->assign.lhss;
}

expression_t *
stmt_assign_rhs (statement_t const * self)
{
  assert (self != NULL);
  assert (self->kind == sk_assign);
  return self->assign.rhs;
}

expression_t *
stmt_call_call (statement_t const * self)
{
  assert (self != NULL);
  assert (self->kind == sk_call);
  return self->call.call;
}

void
stmt_add_label (statement_t * self, symbol_t * label)
{
  assert (self != NULL);
  assert (label != NULL);
  slist_pushback (self->labels, label);
}

slist_t *
stmt_labels (statement_t const * self)
{
  assert (self != NULL);
  return self->labels;
}

slist_t *
container_symtab (container_t const * _self)
{
  assert (_self != NULL);
  A60_USER_TO_REP (statement, self, *);
  assert (self->kind == sk_block || self->kind == sk_toplev);
  return self->block.symtab;
}

slist_t *
container_stmts (container_t const * _self)
{
  assert (_self != NULL);
  A60_USER_TO_REP (statement, self, *);
  assert (self->kind == sk_block || self->kind == sk_toplev);
  return self->block.statements;
}

int
container_add_symbol (container_t * _self, symbol_t * sym,
		      symtab_entry_kind_t internal)
{
  assert (_self != NULL);
  assert (sym != NULL);
  A60_USER_TO_REP (statement, self, *);
  assert (container (self));

  if (internal == sek_ordinary
      && container_find_name (_self, symbol_label (sym), type_any ()) != NULL)
    return -1;

  slist_pushback (self->block.symtab, sym);
  return 0;
}

symbol_t *
container_find_name (container_t * _self, label_t const * lbl, type_t const * atype)
{
  assert (_self != NULL);
  assert (lbl != NULL);
  A60_USER_TO_REP (statement, self, *);
  assert (container (self));

  slist_it_t * it = slist_iter (self->block.symtab);
  for (; slist_it_has (it); slist_it_next (it))
    {
      symbol_t * sym = slist_it_get (it);
      type_t * symtype = symbol_type (sym);
      if (label_eq (symbol_label (sym), lbl)
	  && (symtype == NULL || types_match (symtype, atype)))
	return sym;
    }
  return NULL;
}

symbol_t *
container_find_name_rec (container_t * _self, label_t const * lbl, type_t const * atype)
{
  assert (_self != NULL);
  assert (lbl != NULL);
  A60_USER_TO_REP (statement, self, *);
  assert (container (self));

  symbol_t * sym = container_find_name (_self, lbl, atype);
  if (sym == NULL && self->parent != NULL)
    return container_find_name_rec (self->parent, lbl, atype);
  else
    return sym;
}

symbol_t *
container_find_name_rec_add_undefined (container_t * self, label_t const * lbl,
				       type_t * atype, logger_t * log,
				       cursor_t * cursor)
{
  assert (self != NULL);
  assert (lbl != NULL);
  assert (atype != NULL);
  assert (log != NULL);
  assert (container (self));

  symbol_t * found = container_find_name_rec (self, lbl, atype);
  if (found == NULL)
    {
      if (types_same (atype, type_any ()))
	log_printfc (log, ll_error, cursor,
		     "(1) unknown symbol named `%s'",
		     estr_cstr (label_id (lbl)));
      else
	{
	  // second chance: look up any symbol of that name
	  found = container_find_name_rec (self, lbl, type_any ());
	  if (found == NULL)
	    {
	      log_printfc (log, ll_error, cursor,
			   "(2) unknown symbol named `%s'",
			   estr_cstr (label_id (lbl)));
	    }
	  else
	    {
	      estring_t * t1s = type_to_str (atype, NULL);
	      estring_t * t2s = type_to_str (symbol_type (found), NULL);
	      estring_t * fmt =
		new_estring_fmt ("type mismatch for symbol `%s': "
				 "requested type `%s', found type `%s'",
				 estr_cstr (label_id (lbl)),
				 estr_cstr (t1s), estr_cstr (t2s));
	      log_printfc (log, ll_error, cursor, "%s", estr_cstr (fmt));
	      delete_estring (fmt);
	      delete_estring (t2s);
	      delete_estring (t1s);
	    }
	}
      int was_there = container_add_symbol (self, new_symbol (lbl), sek_ordinary);
      assert (!was_there);
      found = container_find_name (self, lbl, atype);
      // mark a type at new symbol, either fallback type_int, or
      // atype, if it has suitable type
      symbol_set_type (found, is_metatype (atype) ? type_int () : atype);
    }
  assert (found != NULL);

  return found;
}

void
stmt_toplev_define_internals (container_t * _self)
{
  assert (_self != NULL);
  A60_USER_TO_REP (statement, self, *);
  assert (container (self));
  assert (self->kind == sk_toplev);

  struct interfun {
    char const* n;
    type_t * t;
  } builtins [] =
  {
    // Algol 60 intrinsics
    {"abs",  type_proc_real_real ()},
    {"abs",  type_proc_int_int ()},
    {"sign", type_proc_int_real ()},
    {"sign", type_proc_int_int ()},
    {"sqrt", type_proc_real_real ()},
    {"sin",  type_proc_real_real ()},
    {"cos",  type_proc_real_real ()},
    {"arctan", type_proc_real_real ()},
    {"ln",   type_proc_real_real ()},
    {"exp",  type_proc_real_real ()},
    {"entier", type_proc_int_real ()},
    {"entier", type_proc_real_int ()},
    // Extensions
    {"exit", type_proc_void_int ()},
    {"puts", type_proc_int_string ()},
    //
    {NULL,   NULL}
  };

  struct interfun * ptr = builtins;
  for (; ptr->n != NULL; ptr++)
    {
      symbol_t * s = new_symbol (new_label (new_estring_from (ptr->n)));
      symbol_set_type (s, ptr->t);
      symbol_set_hidden (s, 1);
      int fail = container_add_symbol (_self, s, sek_internal);
#ifdef IN_GCC
      symbol_set_extra (s, builtin_decl_get_generic (s));
#endif
      assert (fail == 0);
    }
}

#ifndef IN_GCC
static void ATTRIBUTE_NORETURN
no_glue_built (void)
{
  fprintf (stderr, "Dummy stmt_*_build_generic called.\n");
  abort ();
}
void * stmt_dummy_build_generic (statement_t * self ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built (); }
void * stmt_assign_build_generic (statement_t * self ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built (); }
void * stmt_call_build_generic (statement_t * self ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built (); }
void * container_build_generic (statement_t * self ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built (); }

void * ATTRIBUTE_NORETURN builtin_decl_get_generic (symbol_t * sym ATTRIBUTE_UNUSED) {
  fprintf (stderr, "Dummy builtin_decl_get_generic called.\n");
  abort ();
}
#endif

void *
stmt_build_generic (statement_t * self, void * data)
{
  assert (self != NULL);
  switch (self->kind)
    {
    case sk_dummy:
      return stmt_dummy_build_generic (self, data);

    case sk_assign:
      return stmt_assign_build_generic (self, data);

    case sk_call:
      return stmt_call_build_generic (self, data);

    case sk_block:
    case sk_toplev:
      return stmt_container_build_generic (as_container (self), data);
    };

  assert (!"Should never get there!");
  return NULL;
}
