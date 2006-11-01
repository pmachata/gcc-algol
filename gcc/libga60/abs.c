/*
\item[abs (E)] for the modulus (absolute value) of the value of the
        expression E
*/

#include "config.h"

#ifdef HAVE_STDLIB_H
# include <stdlib.h>

int __a60__abs_PiiQ (int value);

int
__a60__abs_PiiQ (int value)
{
  return abs (value);
}

#endif


#ifdef HAVE_MATH_H
# include <math.h>

double __a60__abs_PrrQ (double value);

double
__a60__abs_PrrQ (double value)
{
  return fabs (value);
}

#endif
