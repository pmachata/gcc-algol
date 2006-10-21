#ifndef _AL60L_STATEMENT_H_
#define _AL60L_STATEMENT_H_

///
/// \file statement.h
///
/// Hierarchy of Algol 60 statements, important part of Abstract
/// Syntax Tree code.  There are two principal abstractions: a
/// statement, which is a parental class of all statements (each
/// concrete statement is a subclass of statement); and container,
/// which is itself subclass of statement, but has additional
/// capability to hold list of other statements, and a symbol table.
///

#include "statement.i"
#include "cursor.i"
#include "slist.i"
#include "expression.i"
#include "logger.i"
#include "symbol.i"
#include "label.i"
#include "type.i"
#include "estring.i"
#include "pd.h"

/// Create new dummy statement.
statement_t * new_stmt_dummy (cursor_t * cursor)
  ATTRIBUTE_MALLOC;

/// Create new assignment statement.  The list of left sides `lhss'
/// has to contain all expressions, and all have to be lvalues.
/// It will be cloned for purposes of storage inside stmt_assign
/// privates.
statement_t * new_stmt_assign (cursor_t * cursor, slist_t const * lhss, expression_t * rhs)
  ATTRIBUTE_MALLOC
  ATTRIBUTE_NONNULL (2)
  ATTRIBUTE_NONNULL (3);

/// Create new procedure call statement.
statement_t * new_stmt_call (cursor_t * cursor, expression_t * call)
  ATTRIBUTE_MALLOC
  ATTRIBUTE_NONNULL (2);

/// Create new generic statement block.
container_t * new_stmt_block (cursor_t * cursor)
  ATTRIBUTE_MALLOC;

/// Create new toplevel block.
container_t * new_stmt_toplev (cursor_t * cursor)
  ATTRIBUTE_MALLOC;

/// Convert void* to statement, if it is statement, or return NULL.
statement_t * statement (void * ptr)
  ATTRIBUTE_NONNULL(1);

/// Convert void* to container, if it is container, or return NULL.
container_t * container (void * ptr)
  ATTRIBUTE_NONNULL(1);

/// Like function `statement', but with typed argument for explicit
/// container->statement upcast.
statement_t * as_statement (container_t * container)
  ATTRIBUTE_NONNULL(1);

/// Like function `container', but with typed argument for explicit
/// statement->container downcast.
container_t * as_container (statement_t * statement)
  ATTRIBUTE_NONNULL(1);

/// Rewrite the tree into string in its original shape (as close as
/// possible).  `buf' is a string into which the dump should be done.
/// If NULL, new string will be allocated.  Either passed-in, or newly
/// allocated buffer is returned.
estring_t * stmt_to_str (statement_t const * self, estring_t * buf)
  ATTRIBUTE_NONNULL(1);

/// Resolve symbols in and typecheck this statement.  By the time this
/// gets called, symtabs should be already filled.
void stmt_resolve_symbols (statement_t * self, logger_t * log)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

/// Answer the parent of given statement.
container_t * stmt_parent (statement_t const * self)
  ATTRIBUTE_NONNULL(1);

/// The same as stmt_parent, just with differently typed argument.
container_t * container_parent (container_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Set the parent of statement.
void stmt_set_parent (statement_t * self, container_t * parent)
  ATTRIBUTE_NONNULL(1);

/// The same as stmt_set_parent, just with differently typed argument.
void container_set_parent (container_t * self, container_t * parent)
  ATTRIBUTE_NONNULL(1);

/// Add new statement into given container.
void container_add_stmt (container_t * self, statement_t * stmt)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

/// Answer the vector of left sides of given assignment statement.
slist_t * stmt_assign_lhss (statement_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the right hand side of given assignment statement.
expression_t * stmt_assign_rhs (statement_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the call expression representing this call statement.
expression_t * stmt_call_call (statement_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the symtab associated with the container.
slist_t * container_symtab (container_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Answer the statement list associated with the container.
slist_t * container_stmts (container_t const * self)
  ATTRIBUTE_NONNULL(1);

typedef enum enum_symtab_entry_kind_t
{
  sek_internal,
  sek_ordinary
} symtab_entry_kind_t;

/// Add given symbol to the container and return 0.  It there already
/// is other symbol with the same name, do nothing and return -1.
/// `internal' is sek_internal for internal symbols (internal symbols
/// may be overloaded) and sek_ordinary for ordinary symbols (which
/// may not).
int container_add_symbol (container_t * self, symbol_t * sym, symtab_entry_kind_t internal)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

/// Look up given symbol in the container.  Answer first symbol that
/// matches restriction `atype', or first symbol with matching name,
/// if its type is NULL (in that case type can't be checked).  Return
/// NULL if not found.
symbol_t * container_find_name (container_t * self, label_t const * lbl, type_t const * atype)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

/// Lookup given symbol in the container, and if it fails, recursively
/// in all parental containers. Answer first symbol that matches
/// restriction `atype', or first symbol with matching name, if its
/// type is NULL (in that case type can't be checked). Return NULL if
/// not found.
symbol_t * container_find_name_rec (container_t * self, label_t const * lbl, type_t const * atype)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

/// See if given symbol with requested matching type, is already
/// defined somewhere in the scope.  If it's not, create new
/// definition in most enclosing scope.  Answer either found, or newly
/// created symbol.
symbol_t * container_find_name_rec_add_undefined (container_t * self, label_t const * lbl, type_t * atype, logger_t * log, cursor_t * cursor)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2)
  ATTRIBUTE_NONNULL(3)
  ATTRIBUTE_NONNULL(4);

/// Populate given container with algol internal functions.  The
/// container must be `stmt_toplev`.
void stmt_toplev_define_internals (container_t * self)
  ATTRIBUTE_NONNULL(1);

/// Return GENERIC for given statement and all its substatements.
/// Calls specific building function depending on the type of
/// statement in hand.  `data` is passed verbatim into callbacks.
void * stmt_build_generic (statement_t * self, void * data)
  ATTRIBUTE_NONNULL (1);

// Callbacks for stmt_build_generic follow... when IN_GCC is NOT
// defined, those have dummy definitions in statement.c.  Otherwise
// you have to roll your own.

void * stmt_dummy_build_generic (statement_t * self, void * data)
  ATTRIBUTE_NONNULL (1);

void * stmt_assign_build_generic (statement_t * self, void * data)
  ATTRIBUTE_NONNULL (1);

void * stmt_call_build_generic (statement_t * self, void * data)
  ATTRIBUTE_NONNULL (1);

void * stmt_container_build_generic (container_t * self, void * data)
  ATTRIBUTE_NONNULL (1);

/// Special function, not callback.  Returns GENERIC for given builtin
/// declaration.  Just like callbacks, it has implicit dummy
/// definition when IN_GCC isn't defined, otherwise has to be defined
/// explicitly.
void * builtin_decl_get_generic (symbol_t * sym);

#endif//_AL60L_STATEMENT_H_
