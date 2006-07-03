/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef _AL60L_LABEL_H_
#define _AL60L_LABEL_H_

#include "pd.h"

typedef struct struct_label_t { } label_t;

/// Create new label.
label_t * new_label (void)
     MALLOC_LIKE;

/// Destroy the label.  Has free-like semantics, passing NULL for
/// `label' is safe.
void delete_label (label_t * label);

/// Convert void* to label, if it is label, or return NULL.
label_t * label (void * ptr)
     ARG_NONNULL(1);

#endif//_AL60L_LABEL_H_
