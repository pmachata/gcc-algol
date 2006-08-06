bool algol60_init (void);
void algol60_finish (void);
unsigned int algol60_init_options (unsigned int, const char **);
int algol60_handle_option (size_t scode, const char *arg, int value);
bool algol60_post_options (const char **pfilename);
void algol60_parse_file (int debug);
bool algol60_mark_addressable (tree exp);
tree algol60_lang_signed_type (tree type_node);
tree algol60_lang_unsigned_type (tree type_node);
tree algol60_lang_signed_or_unsigned_type (int unsignedp, tree type_node);
tree algol60_lang_type_for_mode (enum machine_mode mode, int unsignedp);
tree algol60_lang_type_for_size (unsigned precision, int unsignedp);

tree pushdecl (tree);
tree getdecls (void);
int global_bindings_p (void);
void insert_block (tree);
tree builtin_function (const char *, tree, int, enum built_in_class, const char *, tree);
