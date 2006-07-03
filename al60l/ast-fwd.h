/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef _AL60L_AST_FWD_H_
#define _AL60L_AST_FWD_H_

/// \file This file collects typedefs from several AST
/// modules--symbols, ast itself, and types.

typedef struct struct_statement_t { } statement_t;

typedef enum enum_statement_kind_t
{
  stmt_dummy,
  stmt_container,
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

typedef struct struct_symbol_t { } symbol_t;

#endif//_AL60L_AST_FWD_H_
