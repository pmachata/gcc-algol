#ifndef _AL60L_SYMTAB_H_
#define _AL60L_SYMTAB_H_

///
/// \file symtab.h
///
/// Symtab represents a collection of symbols.  Symtabs are used by
/// containers and function symbols to store symbols declared in their
/// bodies.
///

#include "a60_symtab.i"
#include "symbol.i"
#include "label.i"
#include "type.i"
#include "statement.i"
#include "estring.i"
#include "logger.i"
#include "cursor.i"
#include "pd.h"
#include "visitor.i"

/// Create new symtab with optional parental symtab `parent'.
a60_symtab_t * a60_new_symtab (void)
  ATTRIBUTE_MALLOC;

/// Set the parental symtab.
void a60_symtab_set_parent (a60_symtab_t * self, a60_symtab_t * parent)
  ATTRIBUTE_NONNULL(1);

/// Delete this symtab.
void a60_delete_symtab (a60_symtab_t * self);

/// Convert void* to statement, if it is statement, or abort.
a60_symtab_t * a60_as_symtab (void * ptr)
  ATTRIBUTE_NONNULL(1);

typedef enum enum_a60_symtab_entry_kind_t
{
  sek_internal,
  sek_ordinary
}
a60_symtab_entry_kind_t;

/// Add given symbol to the container and return 0.  It there already
/// is other symbol with the same name, do nothing and return -1.
/// `internal' is sek_internal for internal symbols (internal symbols
/// may be overloaded) and sek_ordinary for ordinary symbols (which
/// may not).
int a60_symtab_add_symbol (a60_symtab_t * self, symbol_t * sym, a60_symtab_entry_kind_t internal)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

/// Remove the symbol from symtab.  The symbol must be present in
/// symtab.
symbol_t * a60_symtab_erase_symbol (a60_symtab_t * self, symbol_t * sym)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

/// Answer non-zero if the symtab is empty.  Answer zero if there is
/// at least one symbol in table.
int a60_symtab_empty (a60_symtab_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Resolve all symbols in the symbol table.  This calls
/// symbol_resolve_symbols for each symbol in table.
void a60_symtab_resolve_symbols (a60_symtab_t * self, container_t * context, logger_t * log)
  ATTRIBUTE_NONNULL(1);

/// Look up given symbol in the table.  Answer first symbol that
/// matches restriction `atype', or first symbol with matching name,
/// if its type is NULL (in that case type can't be checked).  Return
/// NULL if not found.
symbol_t * a60_symtab_find_name (a60_symtab_t * self, label_t const * lbl, type_t const * atype)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

/// Lookup given symbol in the table, and if it fails, recursively
/// in all parental tables. Answer first symbol that matches
/// restriction `atype', or first symbol with matching name, if its
/// type is NULL (in that case type can't be checked). Return NULL if
/// not found.
symbol_t * a60_symtab_find_name_rec (a60_symtab_t * self, label_t const * lbl, type_t const * atype)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

/// See if given symbol with requested matching type is already
/// defined somewhere in the scope.  If it's not, create new
/// definition in most enclosing scope.  Answer either found, or newly
/// created symbol.
symbol_t * a60_symtab_find_name_rec_add_undefined (a60_symtab_t * self, label_t const * lbl, type_t * atype, logger_t * log, cursor_t * cursor)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2)
  ATTRIBUTE_NONNULL(3)
  ATTRIBUTE_NONNULL(4);

/// Populate given symtab with algol internal functions.
void a60_symtab_toplev_define_internals (a60_symtab_t * self)
  ATTRIBUTE_NONNULL(1);

/// Rewrite the table into string.  `buf' is a string into which the
/// dump should be done. If NULL, new string will be allocated.
/// Either passed-in, or newly allocated buffer is returned.
estring_t * a60_symtab_to_str (a60_symtab_t const * self, estring_t * buf)
  ATTRIBUTE_NONNULL(1);

/// Call a given callback at each symbol of symbol table.  Callback
/// will be passed visited symbol as a first argument, and `data' as a
/// second argument.  Return value of callback is ignored.
void a60_symtab_each (a60_symtab_t * self, callback_t callback, void * data)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

#endif//_AL60L_SYMTAB_H_
