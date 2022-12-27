#include <ncnn/c_api.h>
#include "pretty_print.h"

int main() {
    ncnn_mat_t mat;

    float data[4] = {114.0, 514.0, 1919.0, 810.0};

    mat = ncnn_mat_create_external_2d(2, 2, data, NULL);

    c_pretty_print(mat);

    ncnn_mat_destroy(mat);

    c_pretty_print(mat);

    return 0;

}