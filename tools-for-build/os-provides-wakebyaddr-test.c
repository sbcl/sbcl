#include <windows.h>

int mutex_word = 0;
int expect = 9;

DWORD waiter(void* arg) {
    mutex_word = expect = 2;
    return WaitOnAddress(&mutex_word, &expect, 4, 100); // .1 sec max
}
DWORD waker(void* arg) {
    mutex_word = 0;
    WakeByAddressSingle(&mutex_word);
    return 0;
}

int main()
{
    // Verify that WaitOnAddress returns right away if the mutex word has the wrong value.
    int result = WaitOnAddress(&mutex_word, &expect, 4, 500); // max = .5 sec
    if (!result) return 0; // what? shouldn't be an error to mismatch

    // Try really waiting
    mutex_word = 9;
    result = WaitOnAddress(&mutex_word, &expect, 4, 20); // wait 20 millisec
    // expect a timeout
    if (!(result == 0 && GetLastError()==ERROR_TIMEOUT)) return 0;

    // Simulate a lisp mutex being woken
    HANDLE hWaiter, hWaker;
    hWaiter = CreateThread(NULL, 0,
                           waiter, 0, /* function and argument */
                           0,  /* flags */
                           0); /* id */

    hWaker = CreateThread(NULL, 0,
                          waker, 0, /* function and argument */
                          CREATE_SUSPENDED,  /* flags */
                          0); /* id */

    for (;;) {
        // wait for the waiter to place itself into a wait state on the mutex
        if (mutex_word == 2) break;
        Sleep(10); // 10 millisec
    }
    // Give them some time to rendezvous
    ResumeThread(hWaker);
    WaitForSingleObject(hWaiter, 200); // .2 sec max
    if (mutex_word == 0) return 104;
    return 1;
}
