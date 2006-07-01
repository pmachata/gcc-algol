/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef _AL60L_SYMBOL_H_
#define _AL60L_SYMBOL_H_

#include "ast-fwd.h"
#include "type.h"
#include "estring.h"

/// Name is cloned for use in symtab.
symbol_t * new_symbol (estring_t const* name)
     MALLOC_LIKE;

void symbol_assign_statement (symbol_t * symbol, statement_t * stmt);
void symbol_assign_type (symbol_t * symbol, type_t * type);


#endif//_AL60L_SYMBOL_H_
