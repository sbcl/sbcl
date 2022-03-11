/* -*- Mode: C -*- */

#define EVENTBUFMAX 10000
extern uword_t *eventdata;
extern int n_logevents;

/// eventN = record event with N parameters
#define event0(fmt) \
    { int i_ = __sync_fetch_and_add(&n_logevents, 2); if (i_ < EVENTBUFMAX) { \
        eventdata[i_  ] = 2; \
        eventdata[i_+1] = (uword_t)fmt; } }

#define event1(fmt, arg1) \
    { int i_ = __sync_fetch_and_add(&n_logevents, 3); if (i_ < EVENTBUFMAX) { \
        eventdata[i_  ] = 3; \
        eventdata[i_+1] = (uword_t)fmt; \
        eventdata[i_+2] = (uword_t)arg1; } }

#define event2(fmt, arg1, arg2) \
    { int i_ = __sync_fetch_and_add(&n_logevents, 4); if (i_ < EVENTBUFMAX) { \
        eventdata[i_  ] = 4; \
        eventdata[i_+1] = (uword_t)fmt; \
        eventdata[i_+2] = (uword_t)arg1; \
        eventdata[i_+3] = (uword_t)arg2; } }

#define event3(fmt, arg1, arg2, arg3) \
    { int i_ = __sync_fetch_and_add(&n_logevents, 5); if (i_ < EVENTBUFMAX) { \
        eventdata[i_  ] = 5; \
        eventdata[i_+1] = (uword_t)fmt; \
        eventdata[i_+2] = (uword_t)arg1; \
        eventdata[i_+3] = (uword_t)arg2; \
        eventdata[i_+4] = (uword_t)arg3; } }

#define event4(fmt, arg1, arg2, arg3, arg4) \
    { int i_ = __sync_fetch_and_add(&n_logevents, 6); if (i_ < EVENTBUFMAX) { \
        eventdata[i_  ] = 6; \
        eventdata[i_+1] = (uword_t)fmt; \
        eventdata[i_+2] = (uword_t)arg1; \
        eventdata[i_+3] = (uword_t)arg2; \
        eventdata[i_+4] = (uword_t)arg3; \
        eventdata[i_+5] = (uword_t)arg4; } }

#define event5(fmt, arg1, arg2, arg3, arg4, arg5) \
    { int i_ = __sync_fetch_and_add(&n_logevents, 7); if (i_ < EVENTBUFMAX) { \
        eventdata[i_  ] = 7; \
        eventdata[i_+1] = (uword_t)fmt; \
        eventdata[i_+2] = (uword_t)arg1; \
        eventdata[i_+3] = (uword_t)arg2; \
        eventdata[i_+4] = (uword_t)arg3; \
        eventdata[i_+5] = (uword_t)arg4; \
        eventdata[i_+6] = (uword_t)arg5; } }
