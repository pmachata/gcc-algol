/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef _AL60L_PD_H_
#define _AL60L_PD_H_

#ifdef __GNUC__
# define PRINTF_LIKE(X,Y) __attribute__ ((format (printf, X, Y)))
# define MALLOC_LIKE __attribute__ ((malloc))
# define ARG_NONNULL(X) __attribute__ ((nonnull(X)))
# define ARG_UNUSED __attribute__ ((unused))
#else
# define PRINTF_LIKE(X,Y) /*printf*/
# define MALLOC_LIKE /*malloc*/
# define ARG_NONNULL(X) /*non-null*/
# define ARG_UNUSED /*unused*/
#endif

#endif//_AL60L_PD_H_
