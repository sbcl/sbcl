/* create a .o file with undefined references to all the C stuff we need
 * that cmucl hasn't already fouind for us.  Not needed on Linux/i386
 * because it has dynamic loading anyway
 */

void likewecare() {
    getprotobyname();
}

