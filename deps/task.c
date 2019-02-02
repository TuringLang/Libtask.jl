/*
 task.c
 task copying (aka continuation) for lightweight processes (symmetric coroutines)
 */

#include "julia.h"

jl_task_t *jl_clone_task(jl_task_t *t)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    //jl_task_t *newt = (jl_task_t*)jl_gc_alloc(ptls, sizeof(jl_task_t),
    //                                       jl_task_type);   // jl_gc_alloc is not exported.
    jl_task_t *newt = (jl_task_t*)jl_gc_allocobj(sizeof(jl_task_t)); //  More efficient
    //jl_task_t *newt = (jl_task_t*)jl_new_task(t->start, t->ssize); //  Less efficient
    memset(newt, 0, sizeof(jl_task_t));
    jl_set_typeof(newt, jl_task_type);
    newt->stkbuf = NULL;
    newt->gcstack = NULL;
    JL_GC_PUSH1(&newt);
#if JULIA_VERSION_MAJOR == 1 && JULIA_VERSION_MINOR >= 1
    newt->copy_stack = 1;
#else
    newt->parent = ptls->current_task;
    newt->current_module = t->current_module;
#endif

    newt->state = t->state;
    newt->start = t->start;
    newt->tls = jl_nothing;
    newt->logstate = ptls->current_task->logstate;
    newt->result = jl_nothing;
    newt->donenotify = jl_nothing;
    newt->exception = jl_nothing;
    newt->backtrace = jl_nothing;
    newt->eh = t->eh;
    newt->gcstack = t->gcstack;
    newt->tid = t->tid;          // TODO: need testing
    newt->started = t->started;  // TODO: need testing


#if JULIA_VERSION_MAJOR == 1 && JULIA_VERSION_MINOR >= 1
    memcpy(&newt->ctx, &t->ctx, sizeof(jl_ucontext_t));
#else
    memcpy((void*)newt->ctx, (void*)t->ctx, sizeof(jl_jmp_buf));
#endif
//#ifdef COPY_STACKS
    if (t->stkbuf){
#if !(JULIA_VERSION_MAJOR == 1 && JULIA_VERSION_MINOR >= 1)
        newt->ssize = t->ssize;  // size of saved piece
#endif
        // newt->stkbuf = allocb(t->bufsz);
        // newt->bufsz = t->bufsz;
        // memcpy(newt->stkbuf, t->stkbuf, t->bufsz);
        // workaround, newt and t will get new stkbuf when savestack is called.
        t->bufsz    = 0;
        newt->bufsz = 0;
        newt->stkbuf = t->stkbuf;
    }else{
#if !(JULIA_VERSION_MAJOR == 1 && JULIA_VERSION_MINOR >= 1)
        newt->ssize = 0;
#endif
        newt->bufsz = 0;
        newt->stkbuf = NULL;
    }
//#else
//#error Task copying mechanism other than stack copying is not supported yet.
//#endif
    JL_GC_POP();
    jl_gc_wb_back(newt);

    return newt;
}
