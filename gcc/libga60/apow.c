/* Very straightforward definition for to-be algol library.  This
   should be rewritten to use more optimal computation methods,
   e.g. see how gfortran does it. */

int
_a60_pow_i_i (int a, int i)
{
  if (i > 0)
    {
      int result = a;
      while (--i)
	result *= a;
      return result;
    }
  else if (i == 0)
    {
      // Algol spec leaves it undefined what happens if you do `0**0'.
      if (a != 0)
	return 1;
      else
	abort ();
    }
  else /* i < 0 */
    {
      if (a == 0)
	abort ();
      else if (a == 1)
	return 1;
      else if (a == -1)
	return -1;
      else
	return 0;
    }
}

double
_a60_pow_r_i (double a, int i)
{
  if (i > 0)
    {
      double result = a;
      while (--i)
	result *= a;
      return result;
    }
  else if (i == 0)
    {
      // Algol spec leaves it undefined what happens if you do `0**0'.
      if (a != 0)
	return 1;
      else
	abort ();
    }
  else /* i < 0 */
    {
      if (a == 0)
	abort ();
      else
	return (double)1.0 / _a60_pow_r_i (a, -i);
    }
}

#include <math.h>

double
_a60_pow_r_r (double a, double r)
{
  if (a > 0)
    return exp (r * log (a));
  else if (a == 0)
    {
      if (r > 0)
	return 0.0;
      else
	abort ();
    }
  else /* a < 0 */
    abort ();
}

double
_a60_pow_i_r (int a, double r)
{
  return _a60_pow_r_r ((double)a, r);
}
