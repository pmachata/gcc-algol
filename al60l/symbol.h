/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef _AL60L_SYMBOL_H_
#define _AL60L_SYMBOL_H_

#include "ast-fwd.h"
#include "type.h"
#include "estring.h"

typedef struct struct_symbol_t { } symbol_t;

/// Name is cloned for use in symtab.
symbol_t * new_symbol (estring_t const* name)
     MALLOC_LIKE;

/// free-semantics.  Passign NULL to this function is ok, and will not
/// do anything.  Assigned type and statement are NOT deleted.
void delete_symbol (symbol_t * symbol);

/// Convert void* to symbol, if it is symbol, or return NULL.
symbol_t * symbol (void * ptr)
     ARG_NONNULL(1);

/// Assign a statement to a symbol.  stmt must be non-NULL.  Once a
/// statement is assigned, it's not possible to assign other
/// statement.
void symbol_assign_stmt (symbol_t * symbol, statement_t * stmt);

/// Assign a type to a symbol.  type must be non-NULL.  It's not
/// possible to reassign once assigned type.
void symbol_assign_type (symbol_t * symbol, type_t * type);

/// Query the symbol's name.
estring_t const* symbol_name (symbol_t * symbol);

/// Query the symbol's type.
type_t const* symbol_type (symbol_t * symbol);

/// Query the symbol's assigned statement.
statement_t const* symbol_stmt (symbol_t * symbol);


#endif//_AL60L_SYMBOL_H_
