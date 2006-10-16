#ifndef _AL60L_EXPRESSION_H_
#define _AL60L_EXPRESSION_H_

///
/// \file expression.h
///

#include "expression.i"
#include "cursor.i"
#include "estring.i"
#include "logger.i"
#include "statement.i"
#include "label.i"
#include "slist.i"
#include "type.i"
#include "symbol.i"
#include "pd.h"

/// Create an expression representing integer literal.
expression_t * new_expr_int (cursor_t * location, int value)
  ATTRIBUTE_MALLOC;

/// Create an expression representing floating-point literal.
expression_t * new_expr_real (cursor_t * location, estring_t const* value)
  ATTRIBUTE_NONNULL(2)
  ATTRIBUTE_MALLOC;

/// Create an expression representing string literal.
expression_t * new_expr_string (cursor_t * location, estring_t const* value)
  ATTRIBUTE_MALLOC;

/// Create an expression representing boolean literal.
expression_t * new_expr_bool (cursor_t * location, int value)
  ATTRIBUTE_MALLOC;

/// Create an expression representing variable reference.
expression_t * new_expr_idref (cursor_t * location, label_t * lbl)
  ATTRIBUTE_MALLOC;

/// Create an `if-then-else` expression.
expression_t * new_expr_if (cursor_t * location, expression_t * cond, expression_t * exp_t, expression_t * exp_f)
  ATTRIBUTE_NONNULL(2)
  ATTRIBUTE_NONNULL(3)
  ATTRIBUTE_NONNULL(4)
  ATTRIBUTE_MALLOC;

typedef enum enum_expr_binop_t
{
#define A60_DEFBINOP(OP,STR) OP,
# include "expr-binop.def"
#undef A60_DEFBINOP
}
expr_binop_t;

/// Create a binary expression.
expression_t * new_expr_binary (cursor_t * location, expr_binop_t binop, expression_t * left, expression_t * right)
  ATTRIBUTE_NONNULL(3)
  ATTRIBUTE_NONNULL(4)
  ATTRIBUTE_MALLOC;

typedef enum enum_expr_unop_t
{
#define A60_DEFUNOP(OP,STR) OP,
# include "expr-unop.def"
#undef A60_DEFUNOP
}
expr_unop_t;

/// Create an unary expression.
expression_t * new_expr_unary (cursor_t * location, expr_unop_t unop, expression_t * operand)
  ATTRIBUTE_NONNULL(3)
  ATTRIBUTE_MALLOC;

/// Create a function call expression.
/// @TODO: arguments should be copied for internal use, in typed slist.
expression_t * new_expr_call (cursor_t * location, label_t * label, slist_t * arguments)
  ATTRIBUTE_NONNULL(2)
  ATTRIBUTE_NONNULL(3)
  ATTRIBUTE_MALLOC;

/// Create a function call expression with explicitly set symbol.
expression_t * new_expr_call_sym (cursor_t * location, label_t * label, slist_t * arguments, symbol_t * symbol)
  ATTRIBUTE_NONNULL(2)
  ATTRIBUTE_NONNULL(3)
  ATTRIBUTE_NONNULL(4)
  ATTRIBUTE_MALLOC;

/// Create an array access expression.
/// @TODO: indices should be copied for internal use, in typed slist.
expression_t * new_expr_subscript (cursor_t * location, label_t * label, slist_t * indices)
  ATTRIBUTE_NONNULL(2)
  ATTRIBUTE_NONNULL(3)
  ATTRIBUTE_MALLOC;

/// Convert void* to expression, if it is expression, or return NULL.
expression_t * expression (void * ptr)
  ATTRIBUTE_NONNULL(1);

/// Dump the expression to string.  Returns 'buf', or allocated buffer
/// if 'buf' was NULL.  Contents of non-NULL 'buf' is overwritten.
estring_t * expr_to_str (expression_t const * self, estring_t * buf)
  ATTRIBUTE_NONNULL(1);

/// Get the type of the expression.
type_t * expr_type (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Resolve symbols used in this expression.  By the time this gets
/// called, symtabs should be already filled.  Typechecking should be
/// also performed here.
void expr_resolve_symbols (expression_t * self, container_t * context, logger_t * logger)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(3);

/// Answer whether this expression is lvalue.
int expr_is_lvalue (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the cursor associated with this expression.
cursor_t * expr_cursor (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the value of integer literal expression.
int expr_int_value (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the value of boolean literal expression.
int expr_bool_value (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the value of real literal expression.
estring_t const * expr_real_value (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the value of string literal expression.
estring_t const * expr_string_value (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the symbol associated with this expression.  Valid only for
/// idref, call, subscript, and maybe some similar expressions in
/// future.  Generally those that take `label' argument upon
/// construction.
symbol_t * expr_symbol (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the operator assigned with binary operation.
/// Assert if it's not binary expression.
expr_binop_t expr_binary_op (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer left operand of given binary expression.
/// Assert if it's not binary expression.
expression_t * expr_binary_left (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer right operand of given binary expression.
/// Assert if it's not binary expression.
expression_t * expr_binary_right (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the operator assigned with unary operation.
/// Assert if it's not unary expression.
expr_unop_t expr_unary_op (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the operand of given unary expression.
/// Assert if it's not unary expression.
expression_t * expr_unary_operand (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer condition of if-expression.
/// Assert if it's not if-expression.
expression_t * expr_if_cond (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer true branch of if-expression.
/// Assert if it's not if-expression.
expression_t * expr_if_trueb (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer false branch of if-expression.
/// Assert if it's not if-expression.
expression_t * expr_if_falseb (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the argument-list of given call expression.
/// Assert if it's not a call expression.
slist_t * expr_call_args (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the index-list of given subscript expression.
/// Assert if it's not a subscript expression.
slist_t * expr_subscript_indices (expression_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Return GENERIC for given expression.
/// Calls specific building function depending on the kind of
/// expression in hand.  `data` is passed verbatim into callbacks.
void * expr_build_generic (expression_t * self, void * data)
  ATTRIBUTE_NONNULL (1);

// Callbacks for expr_build_generic follow... when IN_GCC is NOT
// defined, those have dummy definitions in expression.c.  Otherwise
// you have to roll your own.

void * expr_int_build_generic (expression_t * self, void * data);
void * expr_real_build_generic (expression_t * self, void * data);
void * expr_string_build_generic (expression_t * self, void * data);
void * expr_bool_build_generic (expression_t * self, void * data);
void * expr_idref_build_generic (expression_t * self, void * data);
void * expr_if_build_generic (expression_t * self, void * data);
void * expr_binary_build_generic (expression_t * self, void * data);
void * expr_unary_build_generic (expression_t * self, void * data);
void * expr_call_build_generic (expression_t * self, void * data);
void * expr_subscript_build_generic (expression_t * self, void * data);

#endif//_AL60L_EXPRESSION_H_
