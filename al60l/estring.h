/*
 * Copyright (c) 2003 Petr Machata, Jiøí Moskovèák, Kry¹tof Oczadlý
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */
#ifndef _AL60L_ESTRING_H_
#define _AL60L_ESTRING_H_

#include <stdlib.h>
#include "pd.h"

///
///
/// \file estring.h
///
/// `endless' strings library.  Estrings are basically the same
/// strings that you use when working with char*. Major difference is,
/// that you don't have to know how long the string will eventually be
/// - estrings are `smart', and grow as necessary as you add new
/// characters to them.
///


typedef struct struct_estring_t { } estring_t;


/// Debugging info for estring.
void estr_stats (estring_t * dest)
     ARG_NONNULL(1);

/// Create empty new string. Please note, that such a string has to be
/// released with estr_delete(). Returns NULL if error is something ill
/// happens.
estring_t * new_estring (void)
     MALLOC_LIKE;

/// Create new string by copying contents of 'src'. 'src' has to be
/// null-terminated string.
estring_t * new_estring_from (char const* src)
     ARG_NONNULL(1)
     MALLOC_LIKE;

/// Create a string as 1:1 copy of another string. Returns NULL if
/// something fails.
estring_t * clone_estring (estring_t const* src)
     ARG_NONNULL(1)
     MALLOC_LIKE;

/// Release memory allocated for string.
void delete_estring (estring_t * str);

/// Assign a new value to given string. Returns 0 on success or -1 if
/// something failed. In such a case, original string is left intact.
/// This clones body of string, thus 'src' is usable after this call.
int estr_assign_cstr (estring_t * dest, char const* src)
     ARG_NONNULL(1)
     ARG_NONNULL(2);

/// Like estr_assign_cstr, but for estring.
int estr_assign (estring_t * dest, estring_t * src)
     ARG_NONNULL(1)
     ARG_NONNULL(2);

/// Clear a string - won't release memory, just truncates the string
/// body.
void estr_clear (estring_t * dest)
     ARG_NONNULL(1);

/// Convert endless string to classic c-style char* string (for example
/// to make it possible to printf() it).
///
/// Warning: pointer is guaranteed to be valid *only* until you call
/// another Str_* functions. Some string handling functions may require
/// the string to grow, and reallocating memory needed for string data
/// may invalidate value returned by this function.
char const* estr_cstr (estring_t * str)
     ARG_NONNULL(1);

/// Convert estring to lower case.
void estr_tolcase (estring_t * str)
     ARG_NONNULL(1);

/// Give back numerical value of string, with base 10.
long estr_tonumber (estring_t * str)
     ARG_NONNULL(1);

/// Like tonumber, but returns a double.
double estr_tofloat(estring_t * str);

/// Append contents of 'src' to the end of 'dest'.  Return 0 if all
/// goes fine, or -1 if something went wrong.
int estr_append_cstr(estring_t * dest, char const* src);

/// Like estr_append_cstr, but append estring instead of raw c string.
int estr_append (estring_t * dest, estring_t const* src);

/// Similar to estr_append_cstr, but contents of 'src' is prepended
/// before contents of 'dest'.  Returns 0 if OK, otherwise -1.
int estr_prepend_cstr (estring_t * dest, char const* src);

/// Like estr_prepend_cstr, but prepend estring instead of raw c
/// string.
int estr_prepend (estring_t * dest, estring_t const* src);

/// Append a single character to the end of estring.  Returns 0 if all
/// goes well, or -1 if something fails (such as we cannot grow more).
int estr_push (estring_t * dest, char what);

/// Pop a single character off the end of estring.  Returns EOF if the
/// string has no more poppable chars.
int estr_pop (estring_t * dest);

/// Compare two estrings.  It returns an integer less than, equal to, or
/// greater than zero if src1 is found, respectively, to be less than,
/// to match, or be greater than src2.
int estr_compare (estring_t * src1, estring_t * src2);

/// Compare estring with cstr.
int estr_compare_cstr (estring_t * src1, char const * src2);

/// Return length of string 'src'.  Note that this is a constant-time
/// operation, as estrings remember their length.
size_t estr_length (estring_t * src);

/// Give back char at 'position'-th position of string 'src'.  Return
/// EOF for characters off the boundaries.
char estr_at (estring_t * src, int position);

/// Write a char to given location.
void estr_write (estring_t * src, char c, int position);

#endif//_AL60L_ESTRING_H_
