/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef _AL60L_AST_H_
#define _AL60L_AST_H_

#include "estring.h"
#include <stdio.h>

typedef struct struct_statement_t { } statement_t;

typedef enum enum_statement_kind_t
{
  stmt_dummy,
  stmt_block,
  stmt_goto,
  stmt_assign,
  stmt_cond,
  stmt_for,
  stmt_call,
  statement_kind_t_count
} statement_kind_t;

typedef struct struct_for_elmt_t { } for_elmt_t;

typedef enum enum_for_elmt_kind_t
{
  for_elmt_until,
  for_elmt_while,
  for_elmt_expr,
} for_elmt_kind_t;

typedef struct struct_symtab_e_t { } symtab_e_t;




typedef struct struct_expression_t { } expression_t;

typedef enum enum_expression_kind_t
{
  expr_id,
  expr_value,
  expr_unary,
  expr_binary,
  expr_ternary,
  expr_call,
} expression_kind_t;

typedef enum enum_unary_op_t
{
  op_un_add,
  op_un_sub
} unary_op_t;

typedef enum enum_binary_op_t
{
  op_bin_aadd,
  op_bin_asub,
  op_bin_amul,
  op_bin_ardiv,
  op_bin_aidiv,
  op_bin_apow,
  op_bin_rlt,
  op_bin_rgt,
  op_bin_rlte,
  op_bin_rgte,
  op_bin_req,
  op_bin_rneq,
  op_bin_leq,
  op_bin_limp,
  op_bin_lor,
  op_bin_land,
  op_bin_lnot,
} binary_op_t;

typedef enum enum_ternary_op_t
{
  op_ter_cond,
} ternary_op_t;



typedef struct struct_type_t { } type_t;

typedef enum enum_type_kind_t
{
  type_unknown,
  type_any,
  type_int,
  type_real,
  type_string,
  type_bool,
  type_procedure,
  type_label,
  type_array,
} type_kind_t;


statement_t * new_stmt_dummy (void);

statement_t * new_stmt_block (void);
void stmt_block_add_statement (statement_t * block, statement_t * stmt);

/// Name is cloned for use in symtab.
symtab_e_t * stmt_block_add_decl (statement_t * block, estring_t const* name, type_t * type);

void delete_stmt (statement_t * stmt);

void stmt_dump (statement_t * stmt, FILE * ofile);
char const* stmt_label (statement_t * stmt);

statement_kind_t stmt_kind (statement_t * stmt);

#endif//_AL60L_AST_H_
