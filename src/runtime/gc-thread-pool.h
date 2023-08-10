#ifndef GC_THREAD_POOL_H
#define GC_THREAD_POOL_H

extern void thread_pool_init();
extern void run_on_thread_pool(void (*act)(void));

#endif
