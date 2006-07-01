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

void stmt_block_add_decl (statement_t * block, symbol_t const* name);

void delete_stmt (statement_t * stmt);

void stmt_dump (statement_t * stmt, FILE * ofile);
char const* stmt_label (statement_t * stmt);

statement_kind_t stmt_kind (statement_t * stmt);

#endif//_AL60L_AST_H_
