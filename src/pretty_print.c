#include "pretty_print.h"
#include <stdio.h>

void c_pretty_print(const ncnn_mat_t mat)
{
    printf("w: %d, h: %d, d: %d, c: %d\n", ncnn_mat_get_w(mat), ncnn_mat_get_h(mat), ncnn_mat_get_d(mat), ncnn_mat_get_c(mat));
    for (int q=0; q<ncnn_mat_get_c(mat); q++)
    {

        const float* ptr = (const float *)ncnn_mat_get_channel_data(mat, q);

        for (int z=0; z<ncnn_mat_get_d(mat); z++)
        {
            for (int y=0; y<ncnn_mat_get_h(mat); y++)
            {
                for (int x=0; x<ncnn_mat_get_w(mat); x++)
                {
                    printf("%f ", ptr[x]);
                }
                ptr += ncnn_mat_get_w(mat);
                printf("\n");
            }
            printf("\n");
        }
        printf("------------------------\n");
    }
}
