#define __GNU_SOURCE

#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <sys/ioctl.h>

#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

// taken from ocaml/otherlibs/unix/unixsupport.h
#define DIR_Val(v) *((DIR **) &Field(v, 0))

CAMLprim value get_terminal_size()
{
    CAMLparam0();

    CAMLlocal1(tup);
    tup = caml_alloc_tuple(2);

    struct winsize w = {};
    int ret = ioctl(1, TIOCGWINSZ, &w);
    if (ret != -1) {
        Store_field(tup, 0, Val_int(w.ws_row));
        Store_field(tup, 1, Val_int(w.ws_col));
    }

    CAMLreturn(tup);
}

CAMLprim value string_compare_fast(value v1, value v1_offset, value v2, value v2_offset)
{
    CAMLparam4(v1, v1_offset, v2, v2_offset);

    char *s1;
    char *s2;
    s1 = String_val(v1) + Int_val(v1_offset);
    s2 = String_val(v2) + Int_val(v2_offset);
    CAMLreturn(Val_int(strcmp(s1, s2)));
}

CAMLprim value string_starts_with(value prefix, value candidate)
{
    CAMLparam2(prefix, candidate);

    char *s1;
    char *s2;
    s1 = String_val(prefix);
    s2 = String_val(candidate);

    for (;;) {
        if (*s1 == '\0') CAMLreturn(Val_true);
        if (*s2 == '\0') CAMLreturn(Val_false);
        if (*s1 != *s2) CAMLreturn(Val_false);
        s1++;
        s2++;
    }
}

CAMLprim value string_is_substring(value ignore_case, value fragment, value text)
{
    CAMLparam3(ignore_case, fragment, text);
    char *f;
    char *t;
    f = String_val(fragment);
    t = String_val(text);
    CAMLreturn(Val_bool(t && f && Bool_val(ignore_case) ?  strcasestr(t, f) : strstr(t, f)));
}

enum simple_d_type
{
    SIMPLE_DT_BLK,
    SIMPLE_DT_CHR,
    SIMPLE_DT_DIR,
    SIMPLE_DT_FIFO,
    SIMPLE_DT_LNK,
    SIMPLE_DT_REG,
    SIMPLE_DT_SOCK,
    SIMPLE_DT_UNKNOWN,
};

// Conversion function remapping the DR_* values which do not consitute a continuous enum
static enum simple_d_type convert_dtype(unsigned char raw_d_type)
{
    switch (raw_d_type) {
    case DT_FIFO:   return SIMPLE_DT_FIFO;
    case DT_CHR:    return SIMPLE_DT_CHR;
    case DT_DIR:    return SIMPLE_DT_DIR;
    case DT_BLK:    return SIMPLE_DT_BLK;
    case DT_REG:    return SIMPLE_DT_REG;
    case DT_LNK:    return SIMPLE_DT_LNK;
    case DT_SOCK:   return SIMPLE_DT_SOCK;
    case DT_WHT:
    default:	    return SIMPLE_DT_UNKNOWN;
    }
}

// Returns a (name:string, type:int) tuple, or ("", 0) if the end of dir has been reached.
CAMLprim value readdir_t(value dirhandle)
{
    CAMLparam1(dirhandle);
    DIR* d;
    d = DIR_Val(dirhandle);

    struct dirent *entry = readdir(d);

    CAMLlocal1(tup);
    tup = caml_alloc_tuple(2);
    Store_field(tup, 0, entry ? caml_copy_string(entry->d_name) : caml_alloc_string(0));
    Store_field(tup, 1, Val_int(convert_dtype(entry ? entry->d_type : DT_UNKNOWN)));
    CAMLreturn(tup);
}

//#include <stdio.h>
//int main() {
//    struct winsize w = {};
//    ioctl(1, TIOCGWINSZ, &w);
//    printf("rows: %d, columns: %d\n", w.ws_row, w.ws_col);
//}
