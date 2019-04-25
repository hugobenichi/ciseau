#include <string.h>
#include <sys/ioctl.h>

#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

CAMLprim value get_terminal_size()
{
    CAMLparam0();

    CAMLlocal1(tup);
    tup = caml_alloc(2, 0);

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
    char *s1;
    char *s2;
    s1 = String_val(v1) + Int_val(v1_offset);
    s2 = String_val(v2) + Int_val(v2_offset);
    return Val_int(strcmp(s1, s2));
}

CAMLprim value string_starts_with(value prefix, value candidate)
{
    char *s1;
    char *s2;
    s1 = String_val(prefix);
    s2 = String_val(candidate);

    for (;;) {
        if (*s1 == '\0') return Val_true;
        if (*s2 == '\0') return Val_false;
        if (*s1 != *s2) return Val_false;
        s1++;
        s2++;
    }
}


//#include <stdio.h>
//int main() {
//    struct winsize w = {};
//    ioctl(1, TIOCGWINSZ, &w);
//    printf("rows: %d, columns: %d\n", w.ws_row, w.ws_col);
//}
