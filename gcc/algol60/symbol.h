#ifndef _AL60L_SYMBOL_H_
#define _AL60L_SYMBOL_H_

///
/// \file symbol.h
///
/// Symbol is an AST node that represents a symbol in a program.
/// Symbols are used to track variables and functions in Algol
/// programs.  Each symbol has a label, that denotes its name, and
/// several optional features, such as assigned type, statement, etc.
///

#include "symbol.i"
#include "label.i"
#include "type.i"
#include "statement.i"
#include "estring.i"
#include "cursor.i"
#include "pd.h"

/// Create new symbol given its name.
symbol_t * new_symbol (/*cursor_t * location,*/ label_t const * name)
  ATTRIBUTE_MALLOC;

/// Destroy the symbol.  `self' can be NULL.
void delete_symbol (symbol_t * self);

/// Convert void* to symbol, if it is symbol, or return NULL.
symbol_t * symbol (void * ptr)
  ATTRIBUTE_NONNULL(1);

/// Get symbol's label.
label_t const * symbol_label (symbol_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Render symbol into string.  Buf may be NULL, new one would be
/// allocated.  Returns either buf, or newly allocated buffer if buf
/// was NULL.
estring_t * symbol_to_str (symbol_t const * self, estring_t * buf);

/// Set the type of the symbol.  Type must not be NULL.
void symbol_set_type (symbol_t * self, type_t * type)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

/// Get the type of the symbol.
type_t * symbol_type (symbol_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Set the statement associated with the symbol.  Statement must not
/// be NULL.
void symbol_set_stmt (symbol_t * self, statement_t * stmt)
  ATTRIBUTE_NONNULL(1)
  ATTRIBUTE_NONNULL(2);

/// Get the statement associated with the symbol.
statement_t * symbol_stmt (symbol_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Make the symbol hidden / visible.
void symbol_set_hidden (symbol_t * self, int hidden)
  ATTRIBUTE_NONNULL(1);

/// Answer if the symbol is hidden.
int symbol_hidden (symbol_t const * self)
  ATTRIBUTE_NONNULL(1);

/// Assign extra to given symbol.
void symbol_set_extra (symbol_t * self, void * extra)
  ATTRIBUTE_NONNULL(1);

/// Get symbol's extra.
void * symbol_extra (symbol_t const * self)
  ATTRIBUTE_NONNULL(1);

#endif//_AL60L_SYMBOL_H_
