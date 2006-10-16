/// This file contains the binding between the GCC part and Al60l
/// parsing library.

#include "config.h" /* system.h needs to know if it's safe to include
		       <string.h>/<strings.h> stuff; this is
		       host-specific, and config.h knows it */

#include "system.h" /* stdlib, strings... system includes... for some
		       reason they can't be included directly, has
		       something to do with host/build/target
		       trichotomy. */

#include "coretypes.h" /* contains definition of `tree', required by
			  tree.h */

#include "tree.h"   /* treecodes used in tree.def, here required to be
		       able to call actual tree-building functions */

#include "real.h"   /* methods for real number manipulations, such as
		       build_real */

#include "tree-gimple.h" /* alloc_stmt_list, append_to_statement_list */
#include "toplev.h" /* rest_of_decl_compilation, pedwarn */

#include "algol-tree.h"
#include "al60l-bind.h"

#include "util.h"
#include "meta.h"

#include "slist.h"
#include "estring.h"
#include "boundspair.h"
#include "label.h"
#include "symbol.h"
#include "type.h"
#include "statement.h"
#include "expression.h"

typedef struct struct_al60l_bind_state_rep_t
{
  /// Function declarations stack.  As the layers of nesting are
  /// uncovered, new decls are pushed on and popped off this stack.
  /// slist_front always yields the most enclosing decl.
  slist_t * fundecls;

  /// Result declarations stack.  This is always kept in sync with
  /// fundecls, n-th resultdecl from top of this stack matches n-th
  /// fundecl from top of fundecl stack.
  slist_t * resultdecls;

  /// Stack of subblocks on current binding level.  This is slist of
  /// tree lists.  It's pushed-on in push_function and when handling
  /// container.
  slist_t * subblocks;
} al60l_bind_state_rep_t;

#ifndef IN_GCC
# error It makes no sense to build this file outside the GCC
#endif

// ------------------------------------
//   AUXILIARIES
// ------------------------------------

al60l_bind_state_t *
new_bind_state (void)
{
  al60l_bind_state_rep_t * ret = xmalloc (sizeof (al60l_bind_state_rep_t));
  if (ret == NULL)
    return NULL;

  jmp_buf buf;
  if (setjmp (buf) == 0)
    {
      guard_ptr (buf, 1, ret->fundecls = new_slist ());
      guard_ptr (buf, 1, ret->resultdecls = new_slist ());
      guard_ptr (buf, 1, ret->subblocks = new_slist ());
      return (void*)ret;
    }
  else
    {
      delete_bind_state ((al60l_bind_state_t*)ret);
      return NULL;
    }
}

void
delete_bind_state (al60l_bind_state_t * _state)
{
  al60l_bind_state_rep_t * state = (void*)_state;
  if (state != NULL)
    {
      delete_slist (state->fundecls);
      delete_slist (state->resultdecls);
      delete_slist (state->subblocks);
      free (state);
    }
}

void
bind_state_push_function (al60l_bind_state_t * _state, tree resultdecl, tree decl)
{
  al60l_bind_state_rep_t * state = (void*)_state;
  gcc_assert (state != NULL);
  slist_pushfront (state->fundecls, decl);
  slist_pushfront (state->resultdecls, resultdecl);
  slist_pushfront (state->subblocks, NULL_TREE);
}

tree
bind_state_enclosing_decl (al60l_bind_state_t const* _state)
{
  al60l_bind_state_rep_t const* state = (void*)_state;
  gcc_assert (state != NULL);
  gcc_assert (!slist_empty (state->fundecls));
  return slist_front (state->fundecls);
}

tree
bind_state_enclosing_resultdecl (al60l_bind_state_t const* _state)
{
  al60l_bind_state_rep_t const* state = (void*)_state;
  gcc_assert (state != NULL);
  gcc_assert (!slist_empty (state->resultdecls));
  return slist_front (state->resultdecls);
}

void
bind_state_pop_function (al60l_bind_state_t * _state)
{
  al60l_bind_state_rep_t * state = (void*)_state;
  gcc_assert (state != NULL);
  gcc_assert (!slist_empty (state->fundecls));
  gcc_assert (!slist_empty (state->resultdecls));
  gcc_assert (!slist_empty (state->subblocks));
  slist_popfront (state->fundecls);
  slist_popfront (state->resultdecls);
  slist_popfront (state->subblocks);
}


// ------------------------------------
//   STATEMENT
// ------------------------------------

void *
stmt_dummy_build_generic (statement_t * self ATTRIBUTE_UNUSED,
			  void * state ATTRIBUTE_UNUSED)
{
  return build_empty_stmt ();
}

void *
stmt_assign_build_generic (statement_t * self, void * data)
{
  // @FIXME: limit ourselves to simple case `x := something`.
  slist_t * lhss = stmt_assign_lhss (self);
  tree op1 = expr_build_generic (slist_front (lhss), data);
  tree op2 = expr_build_generic (stmt_assign_rhs (self), data);
  tree ret = build2 (MODIFY_EXPR, void_type_node, op1, op2);
  return ret;
}

void *
stmt_call_build_generic (statement_t * self, void * state)
{
  return expr_build_generic (stmt_call_call (self), state);
}

void *
stmt_container_build_generic (container_t * self, void * _state)
{
  al60l_bind_state_rep_t * state = _state;
  slist_it_t * it;

  // Open new subblock level.  All substatements that are containers
  // will add their bind_exprs to the _front of state->subblocks.
  slist_pushfront (state->subblocks, NULL_TREE);

  tree vars = NULL_TREE;
  it = slist_iter (container_symtab (self));
  for (; slist_it_has (it); slist_it_next (it))
    {
      symbol_t * sym = slist_it_get (it);
      gcc_assert (symbol_extra (sym) == NULL);

      tree decl = symbol_decl_for_type (symbol_type (sym), sym, _state);
      symbol_set_extra (sym, decl);

      // Add variable to the chain.  This really has to be chain of
      // variables, not a list of conses.
      TREE_CHAIN (decl) = vars;
      vars = decl;
    }
  delete_slist_it (it);

  tree stmts = alloc_stmt_list ();
  it = slist_iter (container_stmts (self));
  for (; slist_it_has (it); slist_it_next (it))
    {
      statement_t * st = slist_it_get (it);
      tree stmt = stmt_build_generic (st, state);
      append_to_statement_list (stmt, &stmts);
    }
  delete_slist_it (it);

  // Collect the subblocks that might have been added during
  // translation of container statements.
  tree subblocks = slist_popfront (state->subblocks);
  if (subblocks)
    subblocks = nreverse (subblocks);

  // Create new block and make it a superblock of all subblocks.
  tree block = build_block (vars, subblocks, NULL_TREE, NULL_TREE);
  if (subblocks)
    {
      tree subblock_node;
      for (subblock_node = subblocks; subblock_node != NULL_TREE;
	   subblock_node = TREE_CHAIN (subblock_node))
	{
	  tree subblock = TREE_VALUE (subblock_node);
	  BLOCK_SUPERCONTEXT (subblock) = block;
	}
    }

  // Then add the block to super's subblocks.
  it = slist_iter (state->subblocks);
  tree super_subs = slist_it_get (it);
  super_subs = tree_cons (NULL_TREE, block, super_subs);
  slist_it_put (it, super_subs);

  // Finally return the binding expression that represents our block.
  tree bind = build3 (BIND_EXPR, void_type_node,
		      BLOCK_VARS (block), stmts, block);
  TREE_USED (block) = 1;

  return bind;
}


// ------------------------------------
//   EXPRESSION
// ------------------------------------

void *
expr_int_build_generic (expression_t * self, void * data ATTRIBUTE_UNUSED)
{
  tree ret = build_int_cst (integer_type_node, expr_int_value (self));
  return ret;
}

void *
expr_real_build_generic (expression_t * self, void * data)
{
  REAL_VALUE_TYPE real;
  type_t * t = expr_type (self);
  tree ttt = type_build_generic (t, data);

  real_from_string3 (&real, estr_cstr (expr_real_value (self)), TYPE_MODE (ttt));

  if (REAL_VALUE_ISINF (real))
    pedwarn ("floating constant exceeds range of %qT", ttt);

  tree ret = build_real (ttt, real);
  return ret;
}

void *
expr_string_build_generic (expression_t * self, void * data ATTRIBUTE_UNUSED)
{
  estring_t const * s = expr_string_value (self);
  int len = estr_length (s) + 1; // +1 for trailing zero
  tree ret = build_string_literal (len, estr_cstr (s));
  return ret;
}

void *
expr_bool_build_generic (expression_t * self, void * data ATTRIBUTE_UNUSED)
{
  tree ret = build_int_cst (boolean_type_node, expr_bool_value (self));
  return ret;
}

void *
expr_idref_build_generic (expression_t * self, void * data ATTRIBUTE_UNUSED)
{
  // see if the idref was resolved
  symbol_t * sym = expr_symbol (self);
  gcc_assert (sym != NULL);

  // see if it has declaration, and answer it, if it has
  tree decl = symbol_extra (sym);
  gcc_assert (decl);
  return decl;
}

void *
expr_if_build_generic (expression_t * self, void * data)
{
  type_t * t = expr_type (self);
  tree ttt  = type_build_generic (t, data);
  tree cond = expr_build_generic (expr_if_cond (self), data);
  tree expt = expr_build_generic (expr_if_trueb (self), data);
  tree expf = expr_build_generic (expr_if_falseb (self), data);
  tree ret  = build3 (COND_EXPR, ttt, cond, expt, expf);
  return ret;
}

static tree
private_expr_build_binary_generic (expression_t * self, void * data, int op)
{
  type_t * t = expr_type (self);
  tree ttt = type_build_generic (t, data);
  tree op1 = expr_build_generic (expr_binary_left (self), data);
  tree op2 = expr_build_generic (expr_binary_right (self), data);
  tree ret = build2 (op, ttt, op1, op2);
  return ret;
}

static tree
private_expr_build_unary_generic (expression_t * self, void * data, int op)
{
  type_t * t = expr_type (self);
  tree ttt = type_build_generic (t, data);
  tree op1 = expr_build_generic (expr_unary_operand (self), data);
  tree ret = build1 (op, ttt, op1);
  return ret;
}

static void *
private_expr_build_ardiv (expression_t * self, void * data)
{
  tree exp =  private_expr_build_binary_generic (self, data, RDIV_EXPR);
  // int / int also yields real.  We must cast the operands explicitly
  // to allow for this.
  if (types_same (expr_type (expr_binary_left (self)), type_int ()))
    {
      tree tmp = TREE_OPERAND (exp, 0);
      TREE_OPERAND (exp, 0) = build1 (FLOAT_EXPR, TREE_TYPE (exp), tmp);
    }

  if (types_same (expr_type (expr_binary_right (self)), type_int ()))
    {
      tree tmp = TREE_OPERAND (exp, 1);
      TREE_OPERAND (exp, 1) = build1 (FLOAT_EXPR, TREE_TYPE (exp), tmp);
    }

  return exp;
}

static void *
private_expr_build_apow (expression_t * self, void * data)
{
  // Arithmetic power expressions are translated to function call into
  // runtime library.  The resulting symbol is named _a60_pow_<a>_<b>,
  // where `a' and `b' are either `i' or `r', depending on the type of
  // each operand.

  // @TODO: as of now, there is no runtime library in place yet.  The
  // resulting binary has to be linked with apow.c and -lm explicitly.

  type_t * tl = expr_type (expr_binary_left (self));
  type_t * tr = expr_type (expr_binary_right (self));

  static type_t * types[4] = {}; // NULLs by default
  static int initialized = 0;
  static char const sigs[] = {'i', 'r'};

  if (! initialized)
    {
      types[0] = type_proc_int_int_int ();
      types[1] = type_proc_real_int_real ();
      types[2] = type_proc_real_real_int ();
      types[3] = type_proc_real_real_real ();
      initialized = 1;
    }

  // sl, sr are indices into the sigs array, and are used to compute
  // index into types array
  int sl = (types_same (tl, type_int ()) ? 0
	    : (gcc_assert (types_same (tl, type_real ())), 1));
  int sr = (types_same (tr, type_int ()) ? 0
	    : (gcc_assert (types_same (tr, type_real ())), 1));
  int typeidx = 2*sl + sr; // index into types array

  label_t * l = new_label (new_estring_fmt ("_a60_pow_%c_%c", sigs[sl], sigs[sr]));
  slist_t * args = new_slist_from (2,
				   expr_binary_left (self),
				   expr_binary_right (self));
  cursor_t * c = expr_cursor (self);

  // Don't forget to do what resolve would do for us.  We can omit
  // typechecking, and subexpression resolving has already been done
  // by this point.
  symbol_t * sym = new_symbol (l);
  symbol_set_type (sym, types[typeidx]); // function type
  symbol_set_hidden (sym, 1);
  symbol_set_extra (sym, builtin_decl_get_generic (sym));
  expression_t * e = new_expr_call_sym (c, l, args, sym);

  return expr_build_generic (e, data);
}

static void *
private_expr_build_limp (expression_t * self, void * data)
{
  // `a => b` translates as `or (not (a), b)`
  // @@@TODO: btw, this is candidate to custom tree node!
  cursor_t * c = expr_cursor (self);
  expression_t * e1 = new_expr_unary (c, euk_not, expr_binary_left (self));
  expression_t * e2 = new_expr_binary (c, ebk_lor, e1, expr_binary_right (self));
  return expr_build_generic (e2, data);
}

void *
expr_binary_build_generic (expression_t * self, void * data)
{
  static int treecode[] = {
    [ebk_aadd] = PLUS_EXPR,  [ebk_asub] = MINUS_EXPR,
    [ebk_amul] = MULT_EXPR,  [ebk_aidiv] = TRUNC_DIV_EXPR, // @TODO: is this the right operator?
    [ebk_req] = EQ_EXPR,     [ebk_rneq] = NE_EXPR,
    [ebk_rlt] = LT_EXPR,     [ebk_rlte] = LE_EXPR,
    [ebk_rgt] = GT_EXPR,     [ebk_rgte] = GE_EXPR,
    [ebk_leq] = EQ_EXPR,     [ebk_lor] = TRUTH_OR_EXPR,
    [ebk_land] = TRUTH_AND_EXPR
  };

  expr_binop_t op = expr_binary_op (self);
  switch (op)
    {
    case ebk_aadd:    case ebk_asub:    case ebk_amul:
    case ebk_aidiv:   case ebk_req:     case ebk_rneq:
    case ebk_rlt:     case ebk_rlte:    case ebk_rgt:
    case ebk_rgte:    case ebk_leq:     case ebk_lor:
    case ebk_land:
      return private_expr_build_binary_generic (self, data, treecode[op]);

    case ebk_ardiv:
      return private_expr_build_ardiv (self, data);
    case ebk_apow:
      return private_expr_build_apow (self, data);
    case ebk_limp:
      return private_expr_build_limp (self, data);
    };
  gcc_unreachable ();
}

void *
expr_unary_build_generic (expression_t * self, void * data)
{
  expr_unop_t op = expr_unary_op (self);
  switch (op)
    {
    case euk_uminus:
  return private_expr_build_unary_generic (self, data, NEGATE_EXPR);
    case euk_not:
  return private_expr_build_unary_generic (self, data, TRUTH_NOT_EXPR);
    };
  gcc_unreachable ();
}

void *
expr_call_build_generic (expression_t * self, void * state)
{
  symbol_t * sym = expr_symbol (self);
  gcc_assert (sym != NULL); // i.e. resolve_symbols was called already

  tree proc_decl = symbol_extra (sym);
  gcc_assert (proc_decl != NULL);

  tree arg_list = NULL_TREE;
  slist_it_t * it = slist_iter (expr_call_args (self));
  for (; slist_it_has (it); slist_it_next (it))
    {
      expression_t * expr = slist_it_get (it);
      tree arg_expr = expr_build_generic (expr, state);
      arg_list = tree_cons (NULL_TREE, arg_expr, arg_list);
    }
  delete_slist_it (it);
  arg_list = nreverse (arg_list);

  tree call_expr = build_function_call_expr (proc_decl, arg_list);
  TREE_SIDE_EFFECTS (call_expr) = 1;
  TREE_USED (call_expr) = 1;

  return call_expr;
}

void *
expr_subscript_build_generic (expression_t * self, void * data)
{
  tree array = symbol_extra (expr_symbol (self));
  type_t * t = symbol_type (expr_symbol (self));
  gcc_assert (array != NULL);
  gcc_assert (t != NULL);

  slist_it_t * it = slist_iter (expr_subscript_indices (self));
  for (; slist_it_has (it); slist_it_next (it))
    {
      expression_t * idx_expr = slist_it_get (it);
      tree idx_tree = expr_build_generic (idx_expr, data);
      t = type_host (t);
      tree type = type_build_generic (t, data);
      array = build4 (ARRAY_REF, type, array, idx_tree, NULL_TREE, NULL_TREE);
    }
  return array;
}


void *
builtin_decl_get_generic (symbol_t * sym)
{
  type_t * t = symbol_type (sym);

  int own = 0;
  if (type_is_own (t))
    {
      own = 1;
      t = type_host (t);
    }
  tree decl = symbol_decl_for_type (t, sym, NULL);

  // Builtins have file scope.
  DECL_CONTEXT (decl) = NULL_TREE;

  // In a VAR_DECL or FUNCTION_DECL, nonzero means external reference:
  // do not allocate storage, and refer to a definition elsewhere.
  DECL_EXTERNAL (decl) = 1; // all builtins are external

  // In a VAR_DECL, FUNCTION_DECL, NAMESPACE_DECL or TYPE_DECL,
  // nonzero means name is to be accessible from outside this module.
  TREE_PUBLIC (decl) = 1;

  // In a VAR_DECL, nonzero means allocate static storage.
  // In a FUNCTION_DECL, nonzero if function has been defined.
  if (type_is_proc (t))
    TREE_STATIC (decl) = 0; // builtin functions are undefined
  else
    TREE_STATIC (decl) = own;

  rest_of_decl_compilation (decl, /*top_level=*/1, /*at_end=*/0);

  return decl;
}

void *
type_unknown_build_generic (type_t * self ATTRIBUTE_UNUSED,
			    void * data ATTRIBUTE_UNUSED)
{
  gcc_assert (!"You shouldn't ask for GENERIC of `unknown'.");
  gcc_unreachable ();
}

void *
type_any_build_generic (type_t * self ATTRIBUTE_UNUSED,
			void * data ATTRIBUTE_UNUSED)
{
  gcc_assert (!"You shouldn't ask for GENERIC of `any'.");
  gcc_unreachable ();
}

void *
type_own_build_generic (type_t * self, void * data)
{
  tree ret = type_build_generic (type_host (self), data);
  TREE_STATIC (ret) = 1;
  return ret;
}

void *
type_int_build_generic (type_t * self ATTRIBUTE_UNUSED,
			void * data ATTRIBUTE_UNUSED)
{
  return integer_type_node;
}

void *
type_void_build_generic (type_t * self ATTRIBUTE_UNUSED,
			 void * data ATTRIBUTE_UNUSED)
{
  return void_type_node;
}

void *
type_real_build_generic (type_t * self ATTRIBUTE_UNUSED,
			 void * data ATTRIBUTE_UNUSED)
{
  return double_type_node;
}

void *
type_string_build_generic (type_t * self ATTRIBUTE_UNUSED,
			   void * data ATTRIBUTE_UNUSED)
{
  return string_type_node;
}

void *
type_bool_build_generic (type_t * self ATTRIBUTE_UNUSED,
			 void * data ATTRIBUTE_UNUSED)
{
  return boolean_type_node;
}


// ------------------------------------
//   SYMBOL
// ------------------------------------

/// Build GENERIC for declaration of given symbol.  This function is
/// called by many symbol_decl_for_<type> methods. Some of them may
/// wish to handle the decl building themselves though.
static tree
private_decl_for_ordinary_symbol (symbol_t * sym, void * data)
{
  // Handle only regular symbols
  label_t const * lbl = symbol_label (sym);
  tree id = get_identifier (estr_cstr (label_id (lbl)));
  tree tt = type_build_generic (symbol_type (sym), data);
  return build_decl (VAR_DECL, id, tt);
}

void *
symbol_decl_for_unknown (symbol_t * sym ATTRIBUTE_UNUSED,
			 void * data ATTRIBUTE_UNUSED)
{
  gcc_assert (!"You shouldn't ask for GENERIC of `unknown'.");
  gcc_unreachable ();
}

void *
symbol_decl_for_any (symbol_t * sym ATTRIBUTE_UNUSED,
		     void * data ATTRIBUTE_UNUSED)
{
  gcc_assert (!"You shouldn't ask for GENERIC of `any'.");
  gcc_unreachable ();
}

void *
symbol_decl_for_own (symbol_t * sym, void * data)
{
  return private_decl_for_ordinary_symbol (sym, data);
}

void *
symbol_decl_for_int (symbol_t * sym, void * data)
{
  return private_decl_for_ordinary_symbol (sym, data);
}

void *
symbol_decl_for_void (symbol_t * sym, void * data)
{
  return private_decl_for_ordinary_symbol (sym, data);
}

void *
symbol_decl_for_real (symbol_t * sym, void * data)
{
  return private_decl_for_ordinary_symbol (sym, data);
}

void *
symbol_decl_for_string (symbol_t * sym, void * data)
{
  return private_decl_for_ordinary_symbol (sym, data);
}

void *
symbol_decl_for_bool (symbol_t * sym, void * data)
{
  return private_decl_for_ordinary_symbol (sym, data);
}

void *
symbol_decl_for_label (symbol_t * sym ATTRIBUTE_UNUSED,
		       void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

void *
symbol_decl_for_array (symbol_t * sym, void * data)
{
  return private_decl_for_ordinary_symbol (sym, data);
}

void *
symbol_decl_for_proc (symbol_t * sym, void * data)
{
  type_t * t = symbol_type (sym);
  tree fn_type = type_proc_build_generic (t, data);
  char const* name = estr_cstr (label_id (symbol_label (sym)));
  tree fn_decl = build_fn_decl (name, fn_type);

  /*
  // build declaration parameters
  tree param_decls = NULL_TREE;

  char buf[15];
  int num = 0;
  slist_it_t * it = slist_iter (ast_as (t_proc, sym->type)->arg_types);
  for (; slist_it_has (it); slist_it_next (it))
    {
      type * t = slist_it_get (it);
      tree argt = type_build_generic (t, data);
      snprintf (buf, sizeof (buf), ".arg.%d", num);
      tree parm = build_decl (PARM_DECL, get_identifier (buf), argt);
      DECL_CONTEXT (parm) = fn_decl;
      param_decls = tree_cons (NULL_TREE, parm, param_decls);
      ++num;
    }
  delete_slist_it (it);
  param_decls = nreverse (param_decls);

  DECL_ARGUMENTS (fn_decl) = param_decls;
  */

  return fn_decl;
}


// ------------------------------------
//   TYPE
// ------------------------------------

void *
type_label_build_generic (type_t * self ATTRIBUTE_UNUSED,
			  void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

void *
type_array_build_generic (type_t * self, void * data)
{
  tree emtt = type_build_generic (type_host (self), data);
  boundspair_t * bp = t_array_bounds (self);
  gcc_assert (bp != NULL);
  expression_t * h = boundspair_hi (bp);
  expression_t * l = boundspair_lo (bp);
  tree lowb = expr_build_generic (l, data);
  tree highb = expr_build_generic (h, data);
  tree arridxt = type_build_generic (type_int (), data);
  tree arrbdst = build_range_type (arridxt, lowb, highb);
  tree ret = build_array_type (emtt, arrbdst);
  return ret;
}

void *
type_proc_build_generic (type_t * self, void * data)
{
  tree fn_ret_type = type_build_generic (t_proc_return_type (self), data);
  tree param_types = NULL_TREE;
  slist_it_t * it = slist_iter (t_proc_arg_types (self));
  for (; slist_it_has (it); slist_it_next (it))
    {
      tree t1 = type_build_generic (slist_it_get (it), data);
      param_types = tree_cons (NULL_TREE, t1, param_types);
    }
  delete_slist_it (it);
  param_types = nreverse (param_types);

  tree fn_type = build_function_type (fn_ret_type, param_types);
  return fn_type;
}
