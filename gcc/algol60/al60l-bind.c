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

#include "ast-tab.h"
#include "slist.h"

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


void *
stmt_dummy_build_generic (stmt_dummy * self ATTRIBUTE_UNUSED,
			  void * state ATTRIBUTE_UNUSED)
{
  return build_empty_stmt ();
}

void *
stmt_assign_build_generic (stmt_assign * self, void * data)
{
  // @FIXME: limit ourselves to simple case `x := something`.  General
  // case is pretty simple right now that we don't support array
  // access.  But compilation of `b := a[b] := b + 1;` will be rather
  // tricky in that we will have to extract the array indices into
  // temporaries as part of array access expression compilation.
  gcc_assert (slist_front (self->lhss) == slist_back (self->lhss));
  tree op1 = expr_build_generic (slist_front (self->lhss), data);
  tree op2 = expr_build_generic (self->rhs, data);
  tree ret = build2 (MODIFY_EXPR, void_type_node, op1, op2);
  return ret;
}

void *
stmt_call_build_generic (stmt_call * self, void * state)
{
  expression * expr = self->call;
  return expr_build_generic (expr, state);
}

void *
stmt_container_build_generic (container * self, void * _state)
{
  al60l_bind_state_rep_t * state = _state;
  slist_it_t * it;

  // Open new subblock level.  All substatements that are containers
  // will add their bind_exprs to the _front of state->subblocks.
  slist_pushfront (state->subblocks, NULL_TREE);

  tree vars = NULL_TREE;
  it = slist_iter (self->symtab);
  for (; slist_it_has (it); slist_it_next (it))
    {
      symbol * sym = slist_it_get (it);
      gcc_assert (sym->extra == NULL);

      // Handle only regular symbols, no labels for now.
      label_id * lbl = ast_as (label_id, sym->lbl);
      gcc_assert (lbl != NULL);
      tree id = get_identifier (estr_cstr (lbl->id));

      tree tt = type_build_generic (sym->type, _state);
      tree decl = build_decl (VAR_DECL, id, tt);
      sym->extra = decl;

      // Add variable to the chain.  This really has to be chain of
      // variables, not a list of conses.
      TREE_CHAIN (decl) = vars;
      vars = decl;
    }
  delete_slist_it (it);

  tree stmts = alloc_stmt_list ();
  it = slist_iter (self->statements);
  for (; slist_it_has (it); slist_it_next (it))
    {
      statement * st = slist_it_get (it);
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

void *
expr_int_build_generic (expr_int * self, void * data ATTRIBUTE_UNUSED)
{
  tree ret = build_int_cst (integer_type_node, self->value);
  return ret;
}

void *
expr_real_build_generic (expr_real * self, void * data)
{
  REAL_VALUE_TYPE real;
  type * t = expr_type (ast_as (expression, self));
  tree ttt = type_build_generic (t, data);

  real_from_string3 (&real, estr_cstr (self->value), TYPE_MODE (ttt));

  if (REAL_VALUE_ISINF (real))
    pedwarn ("floating constant exceeds range of %qT", ttt);

  tree ret = build_real (ttt, real);
  return ret;
}

void *
expr_string_build_generic (expr_string * self, void * data ATTRIBUTE_UNUSED)
{
  int len = estr_length (self->value) + 1; // +1 for trailing zero
  tree ret = build_string_literal (len, estr_cstr (self->value));
  return ret;
}

void *
expr_bool_build_generic (expr_bool * self, void * data ATTRIBUTE_UNUSED)
{
  tree ret = build_int_cst (boolean_type_node, self->value);
  return ret;
}

void *
expr_idref_build_generic (expr_idref * self, void * data ATTRIBUTE_UNUSED)
{
  // see if the idref was resolved
  symbol * sym = self->sym;
  gcc_assert (sym != NULL);

  // see if it has declaration, and answer it, if it has
  tree * decl = sym->extra;
  gcc_assert (decl);
  return decl;
}

void *
expr_if_build_generic (expr_if * self ATTRIBUTE_UNUSED,
		       void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

static tree
private_expr_build_arith_generic (expression * self, void * data, int op)
{
  gcc_assert (ast_isa (self, expr_bin));
  type * t = expr_type (self);
  expr_bin * e = ast_as (expr_bin, self);
  tree ttt = type_build_generic (t, data);
  tree op1 = expr_build_generic (e->left, data);
  tree op2 = expr_build_generic (e->right, data);
  tree ret = build2 (op, ttt, op1, op2);
  return ret;
}

void *
expr_aadd_build_generic (expr_aadd * self, void * data)
{
  return private_expr_build_arith_generic (ast_as (expression, self), data, PLUS_EXPR);
}

void *
expr_asub_build_generic (expr_asub * self, void * data)
{
  return private_expr_build_arith_generic (ast_as (expression, self), data, MINUS_EXPR);
}

void *
expr_amul_build_generic (expr_amul * self, void * data)
{
  return private_expr_build_arith_generic (ast_as (expression, self), data, MULT_EXPR);
}

void *
expr_aidiv_build_generic (expr_aidiv * self, void * data)
{
  // @TODO: check algol 60 reference if this is the right operator
  return private_expr_build_arith_generic (ast_as (expression, self), data, TRUNC_DIV_EXPR);
}

void *
expr_ardiv_build_generic (expr_ardiv * self, void * data)
{
  return private_expr_build_arith_generic (ast_as (expression, self), data, RDIV_EXPR);
}

void *
expr_apow_build_generic (expr_apow * self ATTRIBUTE_UNUSED,
			 void * data ATTRIBUTE_UNUSED)
{
  // @TODO: this requires a funcall, leaving alone for now
  gcc_unreachable ();
}

void *
expr_rel_build_generic (expr_rel * self ATTRIBUTE_UNUSED,
			void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

void *
expr_log_build_generic (expr_log * self ATTRIBUTE_UNUSED,
			void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

void *
expr_un_build_generic (expr_un * self ATTRIBUTE_UNUSED,
		       void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

void *
expr_call_build_generic (expr_call * self, void * state)
{
  symbol * sym = self->sym;
  gcc_assert (sym != NULL); // i.e. resolve_symbols was called already

  tree proc_decl = sym->extra;
  gcc_assert (proc_decl != NULL);

  tree arg_list = NULL_TREE;
  slist_it_t * it = slist_iter (self->arguments);
  for (; slist_it_has (it); slist_it_next (it))
    {
      expression * expr = slist_it_get (it);
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
builtin_decl_get_generic (symbol * sym)
{
  label * lbl = sym->lbl;
  type * t = sym->type;
  gcc_assert (ast_isa (lbl, label_id));

  int own = 0;
  if (ast_isa (t, t_own))
    {
      own = 1;
      t = ast_as (t_own, t)->host;
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
  if (ast_isa (t, t_proc))
    TREE_STATIC (decl) = 0; // builtin functions are undefined
  else
    TREE_STATIC (decl) = own;

  rest_of_decl_compilation (decl, /*top_level=*/1, /*at_end=*/0);

  return decl;
}

void *
type_int_build_generic (t_int * self ATTRIBUTE_UNUSED,
			void * data ATTRIBUTE_UNUSED)
{
  return integer_type_node;
}
void *
symbol_decl_for_int (symbol * sym ATTRIBUTE_UNUSED,
		     void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}


void *
type_void_build_generic (t_void * self ATTRIBUTE_UNUSED,
			 void * data ATTRIBUTE_UNUSED)
{
  return void_type_node;
}

void *
symbol_decl_for_void (symbol * sym ATTRIBUTE_UNUSED,
		      void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}


void *
type_real_build_generic (t_real * self ATTRIBUTE_UNUSED,
			 void * data ATTRIBUTE_UNUSED)
{
  return double_type_node;
}

void *
symbol_decl_for_real (symbol * sym ATTRIBUTE_UNUSED,
		      void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}


void *
type_string_build_generic (t_string * self ATTRIBUTE_UNUSED,
			   void * data ATTRIBUTE_UNUSED)
{
  return string_type_node;
}

void *
symbol_decl_for_string (symbol * sym ATTRIBUTE_UNUSED,
			void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}


void *
type_bool_build_generic (t_bool * self ATTRIBUTE_UNUSED,
			 void * data ATTRIBUTE_UNUSED)
{
  return boolean_type_node;
}

void *
symbol_decl_for_bool (symbol * sym ATTRIBUTE_UNUSED,
		      void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}


void *
type_label_build_generic (t_label * self ATTRIBUTE_UNUSED,
			  void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

void *
symbol_decl_for_label (symbol * sym ATTRIBUTE_UNUSED,
		       void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}


void *
type_array_build_generic (t_array * self ATTRIBUTE_UNUSED,
			  void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

void *
symbol_decl_for_array (symbol * sym ATTRIBUTE_UNUSED,
		       void * data ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}


void *
type_proc_build_generic (t_proc * self, void * data)
{
  tree fn_ret_type = type_build_generic (self->ret_type, data);
  tree param_types = NULL_TREE;
  slist_it_t * it = slist_iter (self->arg_types);
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

void *
symbol_decl_for_proc (symbol * sym, void * data)
{
  gcc_assert (ast_isa (sym->type, t_proc));
  tree fn_type = type_proc_build_generic ((t_proc*)sym->type, data);

  // build declaration; assume procedures have string names
  gcc_assert (ast_isa (sym->lbl, label_id));
  char const* name = estr_cstr (ast_as (label_id, sym->lbl)->id);
  tree fn_decl = build_fn_decl (name, fn_type);

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

  //DECL_ARGUMENTS (fn_decl) = param_decls;
  return fn_decl;
}
