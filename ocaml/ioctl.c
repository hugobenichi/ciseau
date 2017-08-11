#include <sys/ioctl.h>

#include "caml/mlvalues.h"

CAMLprim value get_size_for_ocaml() {
    struct winsize w;

    int ret = ioctl(1, TIOCGWINSZ, &w);
    if (ret == -1 || w.ws_col == 0) {
        return -1;
    }

    return 0;
}

int get_size(int *rows, int *cols) {
    struct winsize w;

    int ret = ioctl(1, TIOCGWINSZ, &w);
    if (ret == -1 || w.ws_col == 0) {
        return -1;
    }

    *cols = w.ws_col;
    *rows = w.ws_row;
    return 0;
}

#include <stdio.h>
int main() {
    int rows;
    int columns;
    get_size(&rows, &columns);
    printf("rows: %d, columns: %d\n", rows, columns);
}
