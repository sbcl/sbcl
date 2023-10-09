/*
 * heap.h
 * @copyright (c) 2015, Tohoku University.
 * @author UENO Katsuhiro
 */
#ifndef SMLSHARP__HEAP_H__
#define SMLSHARP__HEAP_H__

union sml_alloc;

/*
 * Initialize the SML# heap.
 * min_size : hint of minimum and initial heap size in bytes.
 * max_size : hint of maximum heap size in bytes.
 * This must allocate min_size bytes in total for SML# heap.
 * The size of SML# heap may grow up to max_size during program execution.
 */
void sml_heap_init(size_t min_size, size_t max_size);

/*
 * Stop collector's thread.
 */
void sml_heap_stop(void);

/*
 * Finalize the SML# heap.
 */
void sml_heap_destroy(void);

/*
 * Initalize thread-local heap of the current worker thread.
 * It returns worker-specific information of the current worker thread.
 */
union sml_alloc *sml_heap_worker_init(void);

/*
 * Finalize thread-local heap of current worker thread.
 * info : worker-specific information of the current worker thread
 */
void sml_heap_worker_destroy(union sml_alloc *info);

/*
 * Called when a mutator switches to SYNC2.
 * control : control block of the mutator thread
 * Note that control may be different from those of the current thread.
 */
void sml_heap_user_sync2(struct sml_user *user);

/*
 * Called when a worker switches to SYNC2.
 * info : worker-specific information of the thread
 * Note that info may be the one of other threads.
 */
void sml_heap_worker_sync2(union sml_alloc *info);

/*
 * Called when all mutators has switched to SYNC1.
 */
void sml_heap_collector_sync1(void);

/*
 * Called when the collector has switched to SYNC2.
 */
void sml_heap_collector_sync2(void);

/*
 * Called when the collector has switched to MARK.
 * At this time, all all mutators has switched to SYNC2.
 */
void sml_heap_collector_mark(void);

/*
 * Called when the collector has finished MARK and switched to ASYNC.
 */
void sml_heap_collector_async(void);

/*
 * Allocate an SML# object with objsize-byte payload.
 * objsize : the number of bytes to be allocated
 */
SML_PRIMITIVE void *sml_alloc(unsigned int objsize);

/*
 * Memory update with write barrier.
 * obj : the object to be altered
 * writeaddr : the address in obj to be updated with new_value
 * new_value : the value to be stored in writeaddr
 * This must perform appropriate write barrier and store new_value
 * to writeaddr.
 */
SML_PRIMITIVE void sml_write(void *obj, void **writeaddr, void *new_value);

/*
 * Check the liveness of the given object.
 * slot : pointer to a pointer to be checked
 * If *slot has been marked as live in sml_heap_collector_mark,
 * this returns true and update obj with its forwarded pointer.
 * This is called after sml_heap_collector_mark and before
 * sml_heap_collector_async.
 */
int sml_heap_check_alive(void **slot);

#endif /* SMLSHARP__HEAP_H__ */
