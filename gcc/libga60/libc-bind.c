/* define puts and exit as libga60 internals, before some more general
   mechanism for including other libraries, such as libc, is
   invented. */

#include "config.h"

#ifdef HAVE_STDIO_H
# include <stdio.h>
int __a60__puts_PisQ (char const * s);

int
__a60__puts_PisQ (char const * s)
{
  return puts (s);
}
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
void __a60__exit_PviQ (int status);

void
__a60__exit_PviQ (int status)
{
  exit (status);
}
#endif
