#ifndef _WIN32_THREAD_PRIVATE_EVENTS_H_
#define _WIN32_THREAD_PRIVATE_EVENTS_H_

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>

struct private_events {
  HANDLE events[2];
};

#endif /*  _WIN32_THREAD_PRIVATE_EVENTS_H_ */
