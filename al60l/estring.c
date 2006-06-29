/*
 * Copyright (c) 2003 Petr Machata, Jiøí Moskovèák, Kry¹tof Oczadlý
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef SELF_TEST

#include <stdlib.h>
#include <assert.h>

#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "estring.h"

typedef struct struct_estring_rep_t
{
  /// actual length of string - number of used chars, not including
  /// terminating \0
  size_t length;

  /// how much the string can grow before it needs a
  /// realloc. length<limit always holds
  size_t limit;

  /// underlying character string - actual body of the string
  char*  body;
} estring_rep_t;


/// Grow string length to 'length' chars. Returns 0 if all goes well,
/// or -1 if something fails.
static int
private_estr_realloc (estring_rep_t * dest, size_t limit)
{
  assert(limit > 0);

  if (dest->limit >= limit)
    return 0;

  // align limit
  size_t real_limit = (1 + limit / 64) * 64;

  char * new_body = (char*)realloc (dest->body, real_limit);
  if (new_body == NULL)
    return -1;

  dest->body = new_body;
  char *old_end = dest->body + dest->limit;
  dest->limit = real_limit;

  // clean tail
  memset (old_end, 0, real_limit - (old_end - dest->body)); ///@TODO:check!

  return 0;
}

/// Grow string length by 128 chars. Returns 0 if all goes well, or -1
/// if something fails.
static int
private_estr_grow (estring_rep_t * dest)
{
  return private_estr_realloc (dest, dest->limit + 128);
}

void
estr_stats (estring_t * _dest)
{
  estring_rep_t * dest = (void*)_dest;
  printf("estr_stats(%p)\n", (void*)dest);
  printf("  body:\t%p\n",   dest->body);
  printf("  length:\t%i\n", dest->length);
  printf("  limit:\t%i\n",  dest->limit);
}

estring_t *
new_estring (void)
{
  estring_rep_t * ret = malloc (sizeof (estring_rep_t));
  if (ret == NULL)
    return NULL;

  ret->body = NULL;
  ret->length = 0;
  ret->limit = 0;

  if (private_estr_realloc (ret, 1))
    return NULL;
  *ret->body  = '\0';

  return (void*)ret;
}

estring_t *
new_estring_from (char const* src)
{
  assert (src != NULL);

  estring_t * ret = new_estring ();
  estr_append_cstr(ret, src);
  return ret;
}

estring_t *
clone_estring (estring_t const* src)
{
  assert (src != NULL);

  estring_t * ret = new_estring ();
  if (ret == NULL)
    return NULL;

  estr_append (ret, src);

  return ret;
}

void
delete_estring (estring_t * _dest)
{
  assert (_dest != NULL);
  estring_rep_t * dest = (void*)_dest;
  assert (dest->body != NULL);

  free (dest->body);
  free (dest);
}

int
estr_assign_cstr (estring_t * _dest, char const* src)
{
  assert (src != NULL);
  assert (_dest != NULL);

  estring_rep_t * dest = (void*)_dest;
  size_t src_len = strlen (src);

  if (private_estr_realloc (dest, src_len + 1))
    return -1;

  strcpy (dest->body, src);
  dest->length = src_len;

  return 0;
}

int
estr_assign (estring_t * dest, estring_t * _src)
{
  assert (_src != NULL);
  assert (dest != NULL);

  estring_rep_t * src = (void*)_src;
  return estr_assign_cstr (dest, src->body);
}

void
estr_clear (estring_t * _dest)
{
  assert (_dest != NULL);
  estring_rep_t * dest = (void*)_dest;

  memset (dest->body, 0, dest->length);
  dest->length = 0;
}

char const*
estr_cstr (estring_t * _dest)
{
  assert (_dest != NULL);
  estring_rep_t * dest = (void*)_dest;
  return dest->body;
}

void
estr_tolcase (estring_t * _dest)
{
  assert (_dest != 0);
  estring_rep_t * dest = (void*)_dest;

  char * body = dest->body;
  char * bend = body + dest->length -1;

  while (body <= bend)
    {
      *body = tolower (*body);
      ++body;
    }
}

long
estr_tonumber (estring_t * _dest)
{
  assert (_dest != 0);
  estring_rep_t * dest = (void*)_dest;

  return atol (dest->body);
}

double
estr_tofloat (estring_t * _dest)
{
  assert (_dest != 0);
  estring_rep_t * dest = (void*)_dest;
  return atof (dest->body);
}

/// Append given count of characters from source char* string to
/// destination estring. Used internally by estr_append functions.
static int
private_estr_nappend (estring_rep_t * dest, char const* src, size_t src_length)
{
  assert (dest != NULL);
  size_t future_length = dest->length + src_length;

  // if src==dest, we will have to actualize src after the
  // realoocation of dest
  int reloc = dest->body == src;
  if (private_estr_realloc (dest, future_length + 1))
    return -1;
  if (reloc)
    src = dest->body;

  char * body_end = dest->body + dest->length;
  strncpy (body_end, src, src_length);

  dest->length = future_length;

  return 0;
}

int
estr_append_cstr (estring_t * _dest, char const* src)
{
  assert (src != NULL);
  assert (_dest != NULL);
  estring_rep_t * dest = (void*)_dest;

  return private_estr_nappend (dest, src, strlen (src));
}

int
estr_append (estring_t * _dest, estring_t const* _src)
{
  assert (_src != NULL);
  assert (_dest != NULL);
  estring_rep_t * dest = (void*)_dest;
  estring_rep_t const* src = (void*)_src;

  return private_estr_nappend (dest, src->body, src->length);
}

/// Prepend given count of characters from source char* string to
/// destination estring. Used internally by estr_prepend functions.
int
private_estr_nprepend (estring_rep_t * dest, char const* src, size_t src_length)
{
  size_t future_length = dest->length + src_length;

  // if src==dest, we will have to actualize src after the
  // reallocation of dest
  int reloc = src == dest->body;
  if (private_estr_realloc (dest, future_length + 1))
    return -1;
  if (reloc)
    src = dest->body;

  char * fut_body_begin = dest->body + src_length;
  memmove (fut_body_begin, dest->body, dest->length);
  memmove (dest->body, src, src_length);
  dest->length = future_length;

  return 0;
}

int
estr_prepend_cstr (estring_t * _dest, char const* src)
{
  assert (src != NULL);
  assert (_dest != NULL);
  estring_rep_t * dest = (void*)_dest;

  return private_estr_nprepend (dest, src, strlen (src));
}

int
estr_prepend (estring_t * _dest, estring_t const* _src)
{
  assert (_src != NULL);
  assert (_dest != NULL);
  estring_rep_t * dest = (void*)_dest;
  estring_rep_t const* src = (void*)_src;

  return private_estr_nprepend (dest, src->body, src->length);
}

int
estr_push (estring_t * _dest, char what)
{
  assert (_dest != NULL);
  estring_rep_t * dest = (void*)_dest;

  if (dest->length+1 >= dest->limit)
    if (private_estr_grow(dest))
      return -1;

  char * body_end = dest->body + dest->length;
  *body_end = what;
  // unused chars are always zero, so no extra care is to be taken

  dest->length++;

  return 0;
}

int
estr_pop (estring_t * _dest)
{
  assert (_dest != NULL);
  estring_rep_t * dest = (void*)_dest;

  if (dest->length > 0)
    {
      --dest->length;
      char * body_end = dest->body + dest->length;
      char ret = *body_end;
      *body_end = '\0';

      return ret;
    }
  else
    return EOF;
}

int
estr_compare (estring_t * _src1, estring_t * _src2)
{
  assert (_src2 != NULL);
  estring_rep_t * src2 = (void*)_src2;

  return estr_compare_cstr (_src1, src2->body);
}

int
estr_compare_cstr (estring_t * _src1, char const * src2)
{
  assert (_src1 != NULL);
  assert (src2 != NULL);
  estring_rep_t * src1 = (void*)_src1;

  return strcmp (src1->body, src2);
}

size_t
estr_length (estring_t * _dest)
{
  assert (_dest != NULL);
  estring_rep_t * dest = (void*)_dest;
  return dest->length;
}

char
estr_at (estring_t * _dest, int position)
{
  assert (_dest != NULL);
  estring_rep_t * dest = (void*)_dest;

  if (position < 0
      || position >= dest->length)
    return EOF;
  else
    return dest->body[position];
}

void
estr_write (estring_t * _dest, char c, int position)
{
  assert (_dest != NULL);
  estring_rep_t * dest = (void*)_dest;

  dest->body[position] = c;
}

#else /* SELF_TEST */

#include "estring.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>

int
main (void)
{
  printf ("new/clone test.\n");
  estring_t * str1 = new_estring_from ("ahoj svete");
  assert (estr_compare (str1, str1) == 0);

  estring_t * str2 = new_estring_from ("ahoj svete");
  assert (estr_length (str1) == strlen ("ahoj svete"));
  assert (estr_compare (str1, str2) == 0);

  estring_t * str3 = clone_estring (str1);
  delete_estring (str1);
  assert (estr_compare (str2, str3) == 0);
  estr_append (str2, str3);
  estr_append (str3, str3);
  assert (estr_compare (str2, str3) == 0);

  printf ("push/pop test.\n");
  estring_t * str4 = new_estring ();
  int c;
  while ((c = estr_pop (str3)) != EOF)
    estr_push (str4, c);
  assert (estr_compare_cstr (str4, "etevs johaetevs joha") == 0);

  delete_estring (str2);
  delete_estring (str3);
  delete_estring (str4);

  printf ("append/prepend:\n");
  int (*functions[])(estring_t *, estring_t const*) = {estr_append, estr_prepend, NULL};

  for (int (**it)(estring_t *, estring_t const*) = &functions[0];
       *it != NULL;
       it++)
    {
      printf (" + test...\n");
      estring_t * str5 = new_estring_from ("ab");
      assert (estr_length (str5) == 2);
      for (int i = 0; i < 20; ++i)
	{
	  size_t s = estr_length (str5);
	  (*it) (str5, str5);
	  assert (estr_length (str5) == s * 2);
	}
      for (int i = 1; i < estr_length (str5); i += 2)
	{
	  assert (estr_at (str5, i-1) == 'a');
	  assert (estr_at (str5, i) == 'b');
	  assert (estr_at (str5, -i) == EOF);
	  assert (estr_at (str5, -i-1) == EOF);
	}
      assert (estr_at (str5, estr_length (str5)) == EOF);
      delete_estring (str5);
    }

  printf ("All passed.\n");
  return 0;
}

#endif
