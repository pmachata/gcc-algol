/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef _AL60L_AST_H_
#define _AL60L_AST_H_

#include "estring.h"
#include "type.h"
#include "symbol.h"
#include "ast-fwd.h"

#include <stdio.h>


statement_t * new_stmt_dummy (void);

statement_t * new_stmt_block (void);
void stmt_block_add_statement (statement_t * block, statement_t * stmt);

/// Container is like block, but doesn't dump 'begin' and 'end'.  It's
/// used to wrap all the program, including the procedures and outer
/// labels, into single statement.
statement_t * new_stmt_container (void);

/// Convert void* to statement, if it is statement, or return NULL.
statement_t * statement (void * ptr)
     ARG_NONNULL(1);

void stmt_block_add_decl (statement_t * block, symbol_t const* name);

void delete_stmt (statement_t * stmt);

void stmt_dump (statement_t * stmt, FILE * ofile);
char const* stmt_label (statement_t * stmt);

statement_kind_t stmt_kind (statement_t * stmt);

#endif//_AL60L_AST_H_
