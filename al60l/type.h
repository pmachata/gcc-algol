#ifndef _AL60L_TYPE_H_
#define _AL60L_TYPE_H_

#include "pd.h"

/// \file
/// In this module, all type_<kind> procedures yield shared copies of
/// data structures.  This means that asking twice for array of ints
/// will yield the same datum each time.

typedef struct struct_type_t { } type_t;

/// Return shared rep of unknown type.  Unknown types may be used as a
/// fallback type in situation where the type has to be determined,
/// but is yet unknown.
type_t const* type_unknown (void);

/// Return shared rep of any type.  Any types may be used as a
/// fallback types in situations, where exact type doesn't matter.
type_t const* type_any (void);

/// Return shared rep of int type.
type_t const* type_int (void);

/// Return shared rep of real type.
type_t const* type_real (void);

/// Return shared rep of string type.
type_t const* type_string (void);

/// Return shared rep of bool type.
type_t const* type_bool (void);

/// Return shared rep of label type.
type_t const* type_label (void);

/// Create a type representing array of given type.
type_t const* type_array (type_t const* host);

/// @@@FIXME.  This will be able to build procedure type from return
/// type and types and names of parameters.  For the time being, there
/// is a mere '...' where the real parameters will eventually go.
type_t const* type_proc (type_t const* return_type, ...);

/// Convert void* to type, if it is type, or return NULL.
type_t const* type (void const* ptr)
     ARG_NONNULL(1);


/// Answers 1 or 0, depending on whether the two types are the same.
/// Note that you can simply compare the pointers obtained by type
/// constructors, and achieve the same effect, so this function is
/// only really usefull internally.
int types_same (type_t const* lhs, type_t const* rhs);

/// Answers 1 or 0, depending on whether one type matches the other.
/// This is the same behavior as types_same exhibits, with following
/// exception.  t_any matches any other type, including t_any.  And
/// t_unknown doesn't match any other type, with the exception of
/// t_any, but including t_unknown.  Thus:
///
///  any     ~~ any
///  any     ~~ unknown
///  unknown !~ unknown
///
/// The relation 'matches' is commutative.  It's not transitive:
/// unknown~~any, any~~any, any~~unknown, but unknown!~unknown.
int types_match (type_t const* lhs, type_t const* rhs);

#endif//_AL60L_TYPE_H_
