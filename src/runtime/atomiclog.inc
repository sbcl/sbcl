/* -*- Mode: C -*- */

#ifndef ATOMIC_LOGGING
#define event0(fmt)
#define event1(fmt,a)
#define event2(fmt,a,b)
#define event3(fmt,a,b,c)
#define event4(fmt,a,b,c,d)
#define event5(fmt,a,b,c,d,e)
#define event6(fmt,a,b,c,d,e,f)
#else

#define EVENTBUFMAX 400000
extern uword_t *eventdata;
extern int n_logevents;

/// eventN = record event with N parameters

/// NOTE 1: The buffer is oversized by enough to ensure that i_+7 does not
/// overrun the buffer. So we don't need to adjust the comparison of 'i_ <'
/// by the number of additional arguments.

/// NOTE 2: Assume that pthread_self() can be cast to 'uword_t', which is
/// pretty much true everywhere, and that the low 3 bits are 0
/// (which may not be true for 32-bit, but almost surely is for 64-bit).
/// So we can stuff the low 3 bits with something.

#define event0(fmt) \
    { int i_ = __sync_fetch_and_add(&n_logevents, 2); if (i_ < EVENTBUFMAX) { \
        eventdata[i_  ] = (uword_t)pthread_self(); \
        eventdata[i_+1] = (uword_t)fmt; } }

#define event1(fmt, arg1) \
    { int i_ = __sync_fetch_and_add(&n_logevents, 3); if (i_ < EVENTBUFMAX) { \
        eventdata[i_  ] = 1|(uword_t)pthread_self(); \
        eventdata[i_+1] = (uword_t)fmt; \
        eventdata[i_+2] = (uword_t)arg1; } }

#define event2(fmt, arg1, arg2) \
    { int i_ = __sync_fetch_and_add(&n_logevents, 4); if (i_+3 < EVENTBUFMAX) { \
        eventdata[i_  ] = 2|(uword_t)pthread_self(); \
        eventdata[i_+1] = (uword_t)fmt; \
        eventdata[i_+2] = (uword_t)arg1; \
        eventdata[i_+3] = (uword_t)arg2; } }

#define event3(fmt, arg1, arg2, arg3) \
    { int i_ = __sync_fetch_and_add(&n_logevents, 5); if (i_ < EVENTBUFMAX) { \
        eventdata[i_  ] = 3|(uword_t)pthread_self(); \
        eventdata[i_+1] = (uword_t)fmt; \
        eventdata[i_+2] = (uword_t)arg1; \
        eventdata[i_+3] = (uword_t)arg2; \
        eventdata[i_+4] = (uword_t)arg3; } }

#define event4(fmt, arg1, arg2, arg3, arg4) \
    { int i_ = __sync_fetch_and_add(&n_logevents, 6); if (i_ < EVENTBUFMAX) { \
        eventdata[i_  ] = 4|(uword_t)pthread_self(); \
        eventdata[i_+1] = (uword_t)fmt; \
        eventdata[i_+2] = (uword_t)arg1; \
        eventdata[i_+3] = (uword_t)arg2; \
        eventdata[i_+4] = (uword_t)arg3; \
        eventdata[i_+5] = (uword_t)arg4; } }

#define event5(fmt, arg1, arg2, arg3, arg4, arg5) \
    { int i_ = __sync_fetch_and_add(&n_logevents, 7); if (i_ < EVENTBUFMAX) { \
        eventdata[i_  ] = 5|(uword_t)pthread_self(); \
        eventdata[i_+1] = (uword_t)fmt; \
        eventdata[i_+2] = (uword_t)arg1; \
        eventdata[i_+3] = (uword_t)arg2; \
        eventdata[i_+4] = (uword_t)arg3; \
        eventdata[i_+5] = (uword_t)arg4; \
        eventdata[i_+6] = (uword_t)arg5; } }

#define event6(fmt, arg1, arg2, arg3, arg4, arg5, arg6) \
    { int i_ = __sync_fetch_and_add(&n_logevents, 8); if (i_ < EVENTBUFMAX) { \
        eventdata[i_  ] = 6|(uword_t)pthread_self(); \
        eventdata[i_+1] = (uword_t)fmt; \
        eventdata[i_+2] = (uword_t)arg1; \
        eventdata[i_+3] = (uword_t)arg2; \
        eventdata[i_+4] = (uword_t)arg3; \
        eventdata[i_+5] = (uword_t)arg4; \
        eventdata[i_+6] = (uword_t)arg5; \
        eventdata[i_+7] = (uword_t)arg6; } }

#endif
