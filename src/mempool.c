/*
  mempool.c
*/

#include "julia.h"


JL_DLLEXPORT jl_array_t *jl_array_inspect(jl_array_t *ary, void *new_data)
{
    printf("flags.how = %d\n", ary->flags.how);
    printf("flags.ndims = %d\n", ary->flags.ndims);
    printf("flags.pooled = %d\n", ary->flags.pooled);
    printf("flags.ptrarray = %d\n", ary->flags.ptrarray);
    printf("flags.hasptr = %d\n", ary->flags.hasptr);
    printf("flags.isshared = %d\n", ary->flags.isshared);
    printf("flags.isaligned = %d\n", ary->flags.isaligned);
    printf("data ptr = %p\n", ary->data);

    if (new_data) {
        ary->data = new_data;
    }
    return ary;
}
