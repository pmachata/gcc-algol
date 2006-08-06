#include "config.h"
#include "system.h"
#include "gcc.h"

void
lang_specific_driver (int *in_argc, const char *const **in_argv,
		      int *in_added_libraries ATTRIBUTE_UNUSED)
{
  int argc = *in_argc;
  int i;
  char const* const* argv = *in_argv;

  for (i = 1; i < argc; ++i)
    printf ("opt %d: `%s'\n", i, argv[i]);
}

/* Called before linking.  Returns 0 on success and -1 on failure.  */
int
lang_specific_pre_link (void)
{
  return 0;
}

/* Number of extra output files that lang_specific_pre_link may
   generate.  */
int lang_specific_extra_outfiles = 0;
