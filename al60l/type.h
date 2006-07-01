#ifndef _AL60L_TYPE_H_
#define _AL60L_TYPE_H_

#include "symbol.h"

/// In this module, all type_<kind> procedures returning const* data
/// yield shared copies of data structures.  This means that asking
/// twice for array of ints will yield the same datum each time.


/// Kind may be one of basic types, i.e. int, real, string, bool,
/// label.
type_t const* type_basic (type_kind_t kind);

/// Create a type representing array of given type.
type_t const* type_array (type_t const* host);

int types_same (type_t const* lhs, type_t const* rhs);

type_t * type_proc (type_t const* return_type);
void type_proc_add_param (type_t * type, symbol_t const* param);

#endif//_AL60L_TYPE_H_
