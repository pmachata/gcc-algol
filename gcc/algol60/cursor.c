/*
 * Copyright (c) 2005,2006 Petr Machata
 * All rights reserved.
 */

#ifndef SELF_TEST

#include "cursor.h"
#include "util.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static char const* private_cursor_signature = "cursor";

typedef struct struct_cursor_rep_t
{
  char const* signature;

  int offset;
  int line;
  int column;
  char * filename;
  int fnlen;
} cursor_rep_t;

cursor_t *
new_cursor (char const* filename)
{
  cursor_rep_t * ret = malloc (sizeof (cursor_rep_t));

  ret->signature = private_cursor_signature;
  ret->offset = ret->line = ret->column = 0;

  ret->fnlen = strlen (filename);
  ret->filename = a_strdup (filename);

  return (void*)ret;
}

cursor_t *
clone_cursor (cursor_t * _cursor)
{
  cursor_rep_t * cursor = (void*)_cursor;
  cursor_rep_t * ret = (void*) new_cursor (cursor->filename);

  ret->offset = cursor->offset;
  ret->line = cursor->line;
  ret->column = cursor->column;

  return (void*)ret;
}

void
delete_cursor (cursor_t * _cursor)
{
  if (_cursor != NULL)
    {
      cursor_rep_t * cursor = (void*)_cursor;
      free (cursor->filename);
      free (cursor);
    }
}

cursor_t *
cursor (void * ptr)
{
  if (((cursor_rep_t*)ptr)->signature == private_cursor_signature)
    return ptr;
  else
    return NULL;
}

void
cursor_move (cursor_t * _cursor, int offset)
{
  cursor_rep_t * cursor = (void*)_cursor;
  cursor->offset += offset;
  cursor->column += offset;
}

void
cursor_tab (cursor_t * _cursor, int tabsize)
{
  cursor_rep_t * cursor = (void*)_cursor;
  cursor->offset++;
  cursor->column = ((cursor->column + 1) / tabsize + 1) * tabsize;
}

void
cursor_nl (cursor_t * _cursor)
{
  cursor_rep_t * cursor = (void*)_cursor;
  cursor->offset++;
  cursor->line++;
  cursor->column = 0;
}

int
cursor_offset (cursor_t * _cursor)
{
  cursor_rep_t * cursor = (void*)_cursor;
  return cursor->offset;
}

int
cursor_line (cursor_t * _cursor)
{
  cursor_rep_t * cursor = (void*)_cursor;
  return cursor->line;
}

int
cursor_column (cursor_t * _cursor)
{
  cursor_rep_t * cursor = (void*)_cursor;
  return cursor->column;
}

char const*
cursor_file (cursor_t * _cursor)
{
  cursor_rep_t * cursor = (void*)_cursor;
  return cursor->filename;
}

char const*
cursor_to_str (cursor_t * _cursor)
{
  cursor_rep_t * cursor = (void*)_cursor;
  static char * buf = NULL;
  static int bufsize = 0;

  if (bufsize < cursor->fnlen + 64
      || buf == NULL)
    {
      bufsize = cursor->fnlen + 64;
      buf = realloc (buf, bufsize);
    }

  if (cursor->fnlen == 0)
    // don't display empty filename
    sprintf (buf, "%d:%d", cursor->line+1, cursor->column+1);
  else
    sprintf (buf, "%s:%d:%d", cursor->filename, cursor->line+1, cursor->column+1);

  return buf;
}

#else /* SELF_TEST */

#include "cursor.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>

int
main (void)
{
  cursor_t * c = new_cursor ("<stdin>");
  assert (cursor (c));
  cursor_t * c1 = NULL;
  int line = 0;
  static char * str = "0123456701234\nhallo\tworld\nhi\tworld\nhallo   world\n";

  for (char * it = str; *it; ++it)
    {
      assert (str [cursor_offset (c)] == *it);
      assert (cursor_line (c) == line);
      if (*it == '\n')
	{
	  printf ("nl:  %s (%d)\n", cursor_to_str (c), cursor_offset (c));

	  // all lines have the same length
	  if (c1 == NULL)
	    {
	      c1 = new_cursor_copy (c);
	      assert (cursor (c1));
	    }
	  else
	    assert (cursor_column (c) == cursor_column (c1));

	  cursor_nl (c);
	  ++line;
	}
      else if (*it == '\t')
	{
	  printf ("tab: %s (%d) -> ", cursor_to_str (c), cursor_offset (c));
	  cursor_tab (c, 8);
	  printf ("%s (%d)\n", cursor_to_str (c), cursor_offset (c));
	  assert (cursor_column (c) % 8 == 0);
	}
      else
	cursor_move (c, 1);
    }

  int i = cursor_column (c);
  int j = cursor_offset (c);
  int k = cursor_line (c);
  cursor_move (c, 5);
  assert (cursor_column (c) == i+5);
  assert (cursor_offset (c) == j+5);
  assert (cursor_line (c) == k);

  assert (strcmp (cursor_file (c), "<stdin>") == 0);
  assert (strcmp (cursor_file (c1), "<stdin>") == 0);

  delete_cursor (c);
  assert (strcmp (cursor_file (c1), "<stdin>") == 0);

  delete_cursor (c1);

  printf ("All passed.\n");
  return 0;
}

#endif
