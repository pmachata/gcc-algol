#include "expression.h"
#include "cursor.h"
#include "estring.h"
#include "logger.h"
#include "statement.h"
#include "label.h"
#include "slist.h"
#include "type.h"
#include "symbol.h"
#include "meta.h"

#include <stdlib.h>
#include <assert.h>

char const * const private_expression_signature = "expression";

typedef struct struct_expr_int_rep_t
{
  int value;
}
expr_int_rep_t;

typedef struct struct_expr_real_rep_t
{
  estring_t const * value;
}
expr_real_rep_t;

typedef struct struct_expr_string_rep_t
{
  estring_t const * value;
}
expr_string_rep_t;

typedef struct struct_expr_bool_rep_t
{
  int value;
}
expr_bool_rep_t;

typedef struct struct_expr_idref_rep_t
{
  label_t * lbl;
  symbol_t * sym;
}
expr_idref_rep_t;

typedef struct struct_expr_if_rep_t
{
  expression_t * cond;
  expression_t * exp_t;
  expression_t * exp_f;
  type_t * result_type;
}
expr_if_rep_t;

typedef struct struct_expr_bin_rep_t
{
  expr_binop_t op;
  expression_t * left;
  expression_t * right;
}
expr_bin_rep_t;

typedef struct struct_expr_un_rep_t
{
  expr_binop_t op;
  expression_t * operand;
}
expr_un_rep_t;

typedef struct struct_expr_call_rep_t
{
  label_t * lbl;

  /// Typed slist with expressions used as arguments in call.
  slist_t * arguments;
  symbol_t * sym;
}
expr_call_rep_t;

typedef struct struct_expr_subscript_rep_t
{
  label_t * lbl;
  slist_t * indices;
  symbol_t * sym;
}
expr_subscript_rep_t;

typedef enum enum_expr_kind_t
{
  ek_int,
  ek_real,
  ek_string,
  ek_bool,
  ek_idref,
  ek_if,
  ek_binary,
  ek_unary,
  ek_call,
  ek_subscript
}
expr_kind_t;

struct struct_expression_t
{
  char const * signature;
  expr_kind_t kind;
  cursor_t * cursor;
  union {
    expr_int_rep_t eint;
    expr_real_rep_t ereal;
    expr_string_rep_t estring;
    expr_bool_rep_t ebool;
    expr_idref_rep_t idref;
    expr_if_rep_t eif;
    expr_bin_rep_t binary;
    expr_un_rep_t unary;
    expr_call_rep_t call;
    expr_subscript_rep_t subscript;
  };
};

static char const * const expr_bin_op_str[] = {
#define A60_DEFBINOP(OP, STR) STR,
# include "expr-binop.def"
#undef A60_DEFBINOP
};

static char const * const expr_un_op_str[] = {
#define A60_DEFUNOP(OP, STR) STR,
# include "expr-unop.def"
#undef A60_DEFUNOP
};



static expression_t *
private_new_expr (cursor_t * location, expr_kind_t kind)
{
  expression_t * ret = calloc (1, sizeof (expression_t));
  ret->signature = private_expression_signature;
  ret->kind = kind;
  ret->cursor = location;
  return ret;
}

expression_t *
new_expr_int (cursor_t * location, int value)
{
  expression_t * ret = private_new_expr (location, ek_int);
  ret->eint.value = value;
  return ret;
}

expression_t *
new_expr_real (cursor_t * location, estring_t const * value)
{
  assert (value != NULL);
  expression_t * ret = private_new_expr (location, ek_real);
  ret->ereal.value = value;
  return ret;
}

expression_t *
new_expr_string (cursor_t * location, estring_t const * value)
{
  assert (value != NULL);
  expression_t * ret = private_new_expr (location, ek_string);
  ret->estring.value = value;
  return ret;
}

expression_t *
new_expr_bool (cursor_t * location, int value)
{
  expression_t * ret = private_new_expr (location, ek_bool);
  ret->ebool.value = value;
  return ret;
}

expression_t *
new_expr_idref (cursor_t * location, label_t * lbl)
{
  assert (lbl != NULL);
  expression_t * ret = private_new_expr (location, ek_idref);
  ret->idref.lbl = lbl;
  return ret;
}

expression_t *
new_expr_if (cursor_t * location, expression_t * cond, expression_t * exp_t, expression_t * exp_f)
{
  assert (cond != NULL);
  assert (exp_t != NULL);
  assert (exp_f != NULL);

  expression_t * ret = private_new_expr (location, ek_if);
  ret->eif.cond = cond;
  ret->eif.exp_t = exp_t;
  ret->eif.exp_f = exp_f;
  return ret;
}

expression_t *
new_expr_binary (cursor_t * location, expr_binop_t binop, expression_t * left, expression_t * right)
{
  assert (left != NULL);
  assert (right != NULL);

  expression_t * ret = private_new_expr (location, ek_binary);
  ret->binary.op = binop;
  ret->binary.left = left;
  ret->binary.right = right;
  return ret;
}

expression_t *
new_expr_unary (cursor_t * location, expr_unop_t unop, expression_t * operand)
{
  assert (operand != NULL);

  expression_t * ret = private_new_expr (location, ek_unary);
  ret->unary.op = unop;
  ret->unary.operand = operand;
  return ret;
}

expression_t *
new_expr_call (cursor_t * location, label_t * label, slist_t * arguments)
{
  assert (label != NULL);
  assert (arguments != NULL);

  expression_t * ret = private_new_expr (location, ek_call);
  ret->call.lbl = label;
  slist_set_type (arguments, adapt_test, expression);
  ret->call.arguments = arguments;
  return ret;
}

expression_t *
new_expr_call_sym (cursor_t * location, label_t * label, slist_t * arguments,
		   symbol_t * symbol)
{
  assert (label != NULL);
  assert (arguments != NULL);
  assert (symbol != NULL);

  expression_t * ret = new_expr_call (location, label, arguments);
  ret->call.sym = symbol;
  return ret;
}

expression_t *
new_expr_subscript (cursor_t * location, label_t * label, slist_t * indices)
{
  assert (label != NULL);
  assert (indices != NULL);

  expression_t * ret = private_new_expr (location, ek_subscript);
  ret->subscript.lbl = label;
  slist_set_type (indices, adapt_test, expression);
  ret->subscript.indices = indices;
  return ret;
}

expression_t *
expression (void * ptr)
{
  A60_CHECKED_CONVERSION (expression, ptr);
}


static void
private_render_exprs_slist (slist_t * slist, estring_t * buf,
                            char const * open, char const * close)
{
  if (!slist_empty (slist))
    {
      estr_append_cstr (buf, open);
      slist_it_t * it = slist_iter (slist);
      estring_t * tmp = new_estring ();
      while (1)
	{
	  expression_t * arg = slist_it_get (it);
	  tmp = expr_to_str (arg, tmp);
	  estr_append (buf, tmp);
	  slist_it_next (it);
	  if (!slist_it_has (it))
	    break;
	  estr_append_cstr (buf, ", ");
	}
      delete_slist_it (it);
      delete_estring (tmp);
      estr_append_cstr (buf, close);
    }
}

estring_t *
expr_to_str (expression_t const * self, estring_t * buf)
{
  assert (self != NULL);

  if (buf == NULL)
    buf = new_estring ();

  switch (self->kind)
    {
    case ek_int:
      estr_printf (buf, "%d", self->eint.value);
      goto done;

    case ek_real:
      estr_assign (buf, self->ereal.value);
      goto done;

    case ek_string:
      estr_assign (buf, self->estring.value);
      goto done;

    case ek_bool:
      estr_assign_cstr (buf, self->ebool.value ? "'true'" : "'false'");
      goto done;

    case ek_idref:
      buf = symbol_to_str (self->idref.sym, buf);
      goto done;

    case ek_if:
      {
	buf = expr_to_str (self->eif.cond, buf);
	estr_prepend_cstr (buf, "'if' ");
	estr_append_cstr (buf, " 'then' ");
	estring_t * s = expr_to_str (self->eif.exp_t, NULL);
	estr_append (buf, s);
	estr_append_cstr (buf, " 'else' ");
	expr_to_str (self->eif.exp_f, s);
	estr_append (buf, s);
	delete_estring (s);
      }
      goto done;

    case ek_binary:
      {
	buf = expr_to_str (self->binary.left, buf);
	estr_prepend_cstr (buf, "(");
	estr_push (buf, ' ');
	estr_append_cstr (buf, expr_bin_op_str[self->binary.op]);
	estr_push (buf, ' ');
	estring_t * s = expr_to_str (self->binary.right, NULL);
	estr_append (buf, s);
	estr_push (buf, ')');
	delete_estring (s);
      }
      goto done;

    case ek_unary:
      {
	buf = expr_to_str (self->unary.operand, buf);
	estr_prepend_cstr (buf, expr_un_op_str[self->unary.op]);
	estr_prepend_cstr (buf, "(");
	estr_push (buf, ')');
      }
      goto done;

    case ek_call:
      buf = label_to_str (self->call.lbl, buf);
      private_render_exprs_slist (self->call.arguments, buf, "(", ")");
      goto done;

    case ek_subscript:
      buf = label_to_str (self->subscript.lbl, buf);
      private_render_exprs_slist (self->subscript.indices, buf, "[", "]");
      goto done;
    };
  assert (!"Should never get there!");
  return NULL;

 done:
  return buf;
}

static type_t *
expr_bin_type (expr_binop_t op, type_t * first, type_t * second)
{
  if (types_same (first, second))
    {
      // int X int -> int for X in [add sub mul idiv pow]
      // int rdiv int -> real
      if (types_same (first, type_int ()))
	switch (op) {
	case ebk_aadd:
	case ebk_asub:
	case ebk_amul:
	case ebk_aidiv:
	case ebk_apow:
	  return type_int ();
	case ebk_ardiv:
	  return type_real ();
	default:
	  ;
	};

      // real X real -> real for X in [add sub mul rdiv pow]
      if (types_same (first, type_real ()))
	switch (op) {
	case ebk_aadd:
	case ebk_asub:
	case ebk_amul:
	case ebk_ardiv:
	case ebk_apow:
	  return type_real ();
	default:
	  ;
	}

      // A X A -> bool for A in [int real] for X in [eq neq lt lte gt gte]
      if (types_same (first, type_int ())
	  || types_same (first, type_real ()))
	switch (op) {
	case ebk_req:
	case ebk_rneq:
	case ebk_rgt:
	case ebk_rgte:
	case ebk_rlt:
	case ebk_rlte:
	  return type_bool ();
	default:
	  ;
	};

      // bool X bool -> bool for X in [imp leq or and]
      if (types_same (first, type_bool ()))
	switch (op) {
	case ebk_limp:
	case ebk_leq:
	case ebk_lor:
	case ebk_land:
	  return type_bool ();
	default:
	  ;
	};
    }
  else if ((types_same (first, type_int ()) && types_same (second, type_real ()))
	   || (types_same (first, type_real ()) && types_same (second, type_int ())))
    {
      switch (op) {
      case ebk_ardiv:
      case ebk_apow:
	return type_real ();
	default:
	  ;
      };
    }

  return type_unknown ();
}

static type_t *
expr_un_type (expr_unop_t op, type_t * t)
{
  if (op == euk_uminus
      && (types_same (t, type_int ()) || types_same (t, type_real ())))
    return t;
  else if (op == euk_not && types_same (t, type_bool ()))
    return t;

  return type_unknown ();
}


type_t *
expr_type (expression_t const * self)
{
  assert (self != NULL);

  switch (self->kind)
    {
    case ek_int:
      return type_int ();

    case ek_real:
      return type_real ();

    case ek_string:
      return type_string ();

    case ek_bool:
      return type_bool ();

    case ek_idref:
      return symbol_type (self->idref.sym);

    case ek_if:
      return self->eif.result_type;

    case ek_binary:
      return expr_bin_type (self->binary.op,
			    expr_type (self->binary.left),
			    expr_type (self->binary.right));

    case ek_unary:
      return expr_un_type (self->unary.op,
			   expr_type (self->unary.operand));

    case ek_call:
      {
	type_t * t = symbol_type (self->call.sym);
	if (type_is_proc (t))
	  return t_proc_return_type (t);
	else
	  // Might happen if the symbol was added artificially, or as a
	  // result of an error.  Clench teeth and carry on...
	  return t;
      }

    case ek_subscript:
      // Assume that the user provided just the right number of
      // indices. We are actually checking for that in
      // expr_resolve_symbols.
      return type_get_root (symbol_type (self->subscript.sym));
    };
  assert (!"Should never get there!");
  return NULL;
}

static void
private_resolve_symbols_idref (expression_t * self, container_t * block,
			       logger_t * log)
{
  type_t * match_type = new_t_proc (type_any (), new_slist ());
  symbol_t * found = container_find_name_rec (block, self->idref.lbl, match_type);
  if (found == NULL)
    found = container_find_name_rec_add_undefined (block, self->idref.lbl, type_any (),
						   log, self->cursor);
  self->idref.sym = found;
}

static void
private_resolve_symbols_if (expression_t * self, container_t * block,
			    logger_t * log)
{
  expr_resolve_symbols (self->eif.cond, block, log);
  expr_resolve_symbols (self->eif.exp_t, block, log);
  expr_resolve_symbols (self->eif.exp_f, block, log);

  type_t * ct = expr_type (self->eif.cond);
  type_t * tt = expr_type (self->eif.exp_t);
  type_t * ft = expr_type (self->eif.exp_f);

  if (!types_same (ct, type_bool ()))
    {
      log_printfc (log, ll_error, self->cursor,
		   "`if' expression condition has to be boolean");
      self->eif.cond = expr_primitive_for_type (type_bool ());
    }

  self->eif.result_type = tt;
  if (!types_same (tt, ft)) // match is not enough
    {
      log_printfc (log, ll_error, self->cursor,
                   "both `if' branches have to have same types");
      self->eif.exp_f = expr_primitive_for_type (tt);
    }
}

static void
private_resolve_symbols_binary (expression_t * self,
				container_t * block, logger_t * log)
{
  expr_resolve_symbols (self->binary.left, block, log);
  expr_resolve_symbols (self->binary.right, block, log);

  type_t * tt = expr_type (self);
  if (type_is_unknown (tt))
    {
      // @TODO: there should be some error recovery; this way our node
      // ends up being type_unknown, and avalanche of errors is
      // blurted on the user

      estring_t * es = expr_to_str (self, NULL);
      estring_t * tmp = type_to_str (expr_type (self->binary.left), NULL);
      estr_prepend_cstr (es, "type mismatch in expression `");
      estr_append_cstr (es, "': ");
      estr_append (es, tmp);
      estr_push (es, ' ');
      estr_append_cstr (es, expr_bin_op_str [self->binary.op]);
      estr_push (es, ' ');
      type_to_str (expr_type (self->binary.right), tmp);
      estr_append (es, tmp);

      log_printfc (log, ll_error, self->cursor, "%s", estr_cstr (es));

      delete_estring (es);
      delete_estring (tmp);
    }
}

static void
private_resolve_symbols_unary (expression_t * self,
			       container_t * block, logger_t * log)
{
  expr_resolve_symbols (self->unary.operand, block, log);

  type_t * tt = expr_type (self);
  if (type_is_unknown (tt))
    {
      // @TODO: there should be some error recovery; this way our node
      // ends up being type_unknown, and avalanche of errors is
      // blurted on the user

      estring_t * es = expr_to_str (self, NULL);
      estr_prepend_cstr (es, "type mismatch in expression `");
      estr_append_cstr (es, "': ");
      estr_append_cstr (es, expr_un_op_str [self->unary.op]);
      estring_t * tmp = type_to_str (expr_type (self->unary.operand), NULL);
      estr_append (es, tmp);

      log_printfc (log, ll_error, self->cursor, "%s", estr_cstr (es));

      delete_estring (es);
      delete_estring (tmp);
    }
}

static void
private_resolve_symbols_call (expression_t * self,
			      container_t * block, logger_t * log)
{
  slist_t * argtypes = new_slist ();
  slist_it_t * it = slist_iter (self->call.arguments);
  for (; slist_it_has (it); slist_it_next (it))
    {
      expression_t * arg = slist_it_get (it);
      expr_resolve_symbols (arg, block, log);
      type_t * argtype = expr_type (arg);
      slist_pushback (argtypes, argtype);
    }
  delete_slist_it (it);

  type_t * match_type = new_t_proc (type_any (), argtypes);
  self->call.sym =
    container_find_name_rec_add_undefined (block, self->call.lbl,
					   match_type, log,
					   self->cursor);
  assert (self->call.sym != NULL);
}

static void
private_resolve_symbols_subscript (expression_t * self,
				   container_t * block, logger_t * log)
{
  slist_it_t * it = slist_iter (self->subscript.indices);
  for (; slist_it_has (it); slist_it_next (it))
    {
      expression_t * e = slist_it_get (it);
      expr_resolve_symbols (e, block, log);
    }
  delete_slist_it (it);

  self->subscript.sym =
    container_find_name_rec_add_undefined (block, self->subscript.lbl,
					   type_array_any (), log,
					   self->cursor);

  type_t * t = symbol_type (self->subscript.sym);
  if (types_match (t, type_array_any ()))
    // this may not be true if the symbol was added artificially by
    // container_find_name_rec_add_undefined.  In such a case an error
    // message was emitted already, and we don't have to care.
    {
      int num_indices = slist_length (self->subscript.indices);
      int num_dimensions = 0;
      if (type_is_own (t))
	t = type_host (t);
      for (; type_is_array (t); t = type_host (t))
	++num_dimensions;

      if (num_dimensions != num_indices)
	{
	  log_printfc (log, ll_error, self->cursor,
		       "%d indices provided for %dD array",
		       num_indices, num_dimensions);
	}
    }
}

void
expr_resolve_symbols (expression_t * self, container_t * context, logger_t * logger)
{
  assert (self != NULL);
  assert (logger != NULL);

  switch (self->kind)
    {
    case ek_int:
    case ek_real:
    case ek_string:
    case ek_bool:
      // nothing
      return;

    case ek_idref:
      private_resolve_symbols_idref (self, context, logger);
      return;

    case ek_if:
      private_resolve_symbols_if (self, context, logger);
      return;

    case ek_binary:
      private_resolve_symbols_binary (self, context, logger);
      return;

    case ek_unary:
      private_resolve_symbols_unary (self, context, logger);
      return;

    case ek_call:
      private_resolve_symbols_call (self, context, logger);
      return;

    case ek_subscript:
      private_resolve_symbols_subscript (self, context, logger);
      return;
    };
  assert (!"Should never get there!");
}

int
expr_is_lvalue (expression_t const * self)
{
  assert (self != NULL);
  return (self->kind == ek_idref
	  || self->kind == ek_subscript);
}

cursor_t *
expr_cursor (expression_t const * self)
{
  assert (self != NULL);
  return self->cursor;
}

int
expr_int_value (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_int);
  return self->eint.value;
}

int
expr_bool_value (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_bool);
  return self->ebool.value;
}

estring_t const *
expr_real_value (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_real);
  return self->ereal.value;
}

estring_t const *
expr_string_value (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_string);
  return self->estring.value;
}

symbol_t *
expr_symbol (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_idref
	  || self->kind == ek_call
	  || self->kind == ek_subscript);
  switch (self->kind)
    {
    case ek_idref: return self->idref.sym;
    case ek_call: return self->call.sym;
    case ek_subscript: return self->subscript.sym;
    default:
      assert (!"Should never get there!");
      return NULL;
    };
}

expr_binop_t
expr_binary_op (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_binary);
  return self->binary.op;
}

expression_t *
expr_binary_left (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_binary);
  return self->binary.left;
}

expression_t *
expr_binary_right (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_binary);
  return self->binary.right;
}

expr_unop_t
expr_unary_op (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_unary);
  return self->unary.op;
}

expression_t *
expr_unary_operand (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_unary);
  return self->unary.operand;
}

expression_t *
expr_if_cond (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_if);
  return self->eif.cond;
}

expression_t *
expr_if_trueb (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_if);
  return self->eif.exp_t;
}

expression_t *
expr_if_falseb (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_if);
  return self->eif.exp_f;
}

slist_t *
expr_call_args (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_call);
  return self->call.arguments;
}

slist_t *
expr_subscript_indices (expression_t const * self)
{
  assert (self != NULL);
  assert (self->kind == ek_subscript);
  return self->subscript.indices;
}

#ifndef IN_GCC
static void ATTRIBUTE_NORETURN
no_glue_built (void)
{
  fprintf (stderr, "Dummy expr_*_build_generic called.\n");
  abort ();
}
void * expr_int_build_generic (expression_t * self, void * data) { no_glue_built (); }
void * expr_real_build_generic (expression_t * self, void * data) { no_glue_built (); }
void * expr_string_build_generic (expression_t * self, void * data) { no_glue_built (); }
void * expr_bool_build_generic (expression_t * self, void * data) { no_glue_built (); }
void * expr_idref_build_generic (expression_t * self, void * data) { no_glue_built (); }
void * expr_if_build_generic (expression_t * self, void * data) { no_glue_built (); }
void * expr_binary_build_generic (expression_t * self, void * data) { no_glue_built (); }
void * expr_unary_build_generic (expression_t * self, void * data) { no_glue_built (); }
void * expr_call_build_generic (expression_t * self, void * data) { no_glue_built (); }
void * expr_subscript_build_generic (expression_t * self, void * data) { no_glue_built (); }
#endif


void *
expr_build_generic (expression_t * self, void * data)
{
  assert (self != NULL);
  switch (self->kind)
    {
    case ek_int:
      return expr_int_build_generic (self, data);
    case ek_real:
      return expr_real_build_generic (self, data);
    case ek_string:
      return expr_string_build_generic (self, data);
    case ek_bool:
      return expr_bool_build_generic (self, data);
    case ek_idref:
      return expr_idref_build_generic (self, data);
    case ek_if:
      return expr_if_build_generic (self, data);
    case ek_binary:
      return expr_binary_build_generic (self, data);
    case ek_unary:
      return expr_unary_build_generic (self, data);
    case ek_call:
      return expr_call_build_generic (self, data);
    case ek_subscript:
      return expr_subscript_build_generic (self, data);
    };
  assert (!"Should never get there!");
}
