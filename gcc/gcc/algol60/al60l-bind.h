#ifndef AL60L_GCC_BIND_H
#define AL60L_GCC_BIND_H

/// \file
/// Al60l binder contains the glue between Al60l parsing library and
/// GCC.  The code that does AST-to-GENERIC translation is here.
/// al60l-bind.c implements the callback methods dispatched by
/// stmt_build_generic and friends.

/// \note
/// The GCC framework necessary to setup types isn't included here.
/// Instead, it's assumed that the unit that needs to use binder will
/// also need to use GCC framework, and will have to include it anyway.

typedef struct struct_al60l_bind_state_t al60l_bind_state_t;

/// Create new state for al60l binder.
al60l_bind_state_t * new_bind_state (void)
  ATTRIBUTE_MALLOC;

/// Destroy the state.
void delete_bind_state (al60l_bind_state_t * state);

/// Push new function context to the state.  `resultdecl' is tree
/// representing the value that holds return value of function.  `decl' is
/// the declaration of the function.
void bind_state_push_function (al60l_bind_state_t * state, tree resultdecl, tree decl)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2)
  ATTRIBUTE_NONNULL(3);

/// Get decl of most enclosing function.
tree bind_state_enclosing_decl (al60l_bind_state_t const* state)
  ATTRIBUTE_NONNULL(1);

/// Get resultdecl of most enclosing function.
tree bind_state_enclosing_resultdecl (al60l_bind_state_t const* state)
  ATTRIBUTE_NONNULL(1);

/// Pop one function context off the stack.
void bind_state_pop_function (al60l_bind_state_t * state)
  ATTRIBUTE_NONNULL(1);

#endif//AL60L_GCC_BIND_H
