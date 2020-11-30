/*
  mempool.c
*/

#include "julia.h"

// --- functions for debugging
JL_DLLEXPORT void jl_obj_inspect(jl_value_t *obj)
{
    int i = 0;
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *t = (jl_task_t*)ptls->current_task;
    char *stackbase = (char*)ptls->stackbase;

    printf("stacksize        = %ld, %ld\n", ptls->stacksize, t->bufsz);
    printf("offset of i      = %ld\n", (char*)(&i) - stackbase);
    printf("offset of i      = %ld\n", (char*)(&i) - (char*)t->stkbuf);
    printf("offset of obj    = %ld\n", (char*)obj - stackbase);
    printf("offset of obj    = %ld\n", (char*)obj - (char*)t->stkbuf);
    if(jl_is_int64(obj)) {
        printf("int value        = %ld\n", *(long*)obj);
    }
    printf("-------------\n");
}


JL_DLLEXPORT jl_array_t *jl_array_inspect(jl_array_t *ary, void *new_data)
{

    jl_ptls_t ptls = jl_get_ptls_states();
    //char *frame_addr = (char*)((uintptr_t)jl_get_frame_addr() & ~15);
    char *stackbase = (char*)ptls->stackbase;

    printf("flags.how = %d\n", ary->flags.how);
    printf("flags.ndims = %d\n", ary->flags.ndims);
    printf("flags.pooled = %d\n", ary->flags.pooled);
    printf("flags.ptrarray = %d\n", ary->flags.ptrarray);
    printf("flags.hasptr = %d\n", ary->flags.hasptr);
    printf("flags.isshared = %d\n", ary->flags.isshared);
    printf("flags.isaligned = %d\n", ary->flags.isaligned);
    printf("array ptr = %p\n", ary);
    printf("data ptr = %p\n", ary->data);

    printf("offset = %ld\n", stackbase - (char*)ary);
    if (new_data) {
        ary->data = new_data;
    }
    printf("-------------\n");
    return ary;
}

// --- functions for stack-allocated objects
JL_DLLEXPORT jl_value_t * lt_stack_alloc(jl_value_t *fun, size_t buf_size)
{
    void *buf = alloca(buf_size);
    jl_function_t* func = (jl_function_t*)fun;
    jl_value_t *argument = jl_box_int64((int64_t)buf);
    jl_value_t *ret = jl_call1(func, argument);
    return ret;
}

JL_DLLEXPORT jl_array_t* lt_copy_to_stack_array(jl_array_t *ary, void *dest)
{
    memcpy(dest, (void*)jl_astaggedvalue(ary),
        sizeof(jl_taggedvalue_t) + sizeof(jl_array_t));
    jl_array_t *ret = (jl_array_t*)(jl_valueof(dest));
    return ret;
}
