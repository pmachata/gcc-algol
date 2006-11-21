//-*-c-*-
#ifndef AL60L_FOR_ELMT_I_GUARD
#define AL60L_FOR_ELMT_I_GUARD

typedef struct struct_for_elmt_t for_elmt_t;

typedef enum enum_for_elmt_kind_t
{
  fek_expr,
  fek_until,
  fek_while
}
for_elmt_kind_t;

#endif//AL60L_FOR_ELMT_I_GUARD
