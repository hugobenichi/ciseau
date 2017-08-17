#include <sys/ioctl.h>

#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

CAMLprim value get_terminal_size() {
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

//#include <stdio.h>
//int main() {
//    struct winsize w = {};
//    ioctl(1, TIOCGWINSZ, &w);
//    printf("rows: %d, columns: %d\n", w.ws_row, w.ws_col);
//}
