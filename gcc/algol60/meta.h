#ifndef AL60L_META_H
#define AL60L_META_H

#define A60_USER_TO_REP(TYPE, VAR, CVQ)	\
  assert (_##VAR != NULL);			\
  TYPE##_t CVQ VAR = (void CVQ) _##VAR

#define A60_CHECKED_CONVERSION(TYPE, VAR)				\
  if (((TYPE##_t*)VAR)->signature == private_##TYPE##_signature)	\
    return VAR;								\
  else									\
    return NULL

#endif//AL60L_META_H
