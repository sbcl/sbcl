{
    printf("(in-package \"SB-WIN32\")\n\n");

    defconstant ("input-record-size", sizeof (INPUT_RECORD));

    defconstant ("MAX_PATH", MAX_PATH);

    printf(";;; CSIDL\n");

    defconstant ("CSIDL_DESKTOP", CSIDL_DESKTOP);
    defconstant ("CSIDL_INTERNET", CSIDL_INTERNET);
    defconstant ("CSIDL_PROGRAMS", CSIDL_PROGRAMS);
    defconstant ("CSIDL_CONTROLS", CSIDL_CONTROLS);
    defconstant ("CSIDL_PRINTERS", CSIDL_PRINTERS);
    defconstant ("CSIDL_PERSONAL", CSIDL_PERSONAL);
    defconstant ("CSIDL_FAVORITES", CSIDL_FAVORITES);
    defconstant ("CSIDL_STARTUP", CSIDL_STARTUP);
    defconstant ("CSIDL_RECENT", CSIDL_RECENT);
    defconstant ("CSIDL_SENDTO", CSIDL_SENDTO);
    defconstant ("CSIDL_BITBUCKET", CSIDL_BITBUCKET);
    defconstant ("CSIDL_STARTMENU", CSIDL_STARTMENU);
    defconstant ("CSIDL_DESKTOPDIRECTORY", CSIDL_DESKTOPDIRECTORY);
    defconstant ("CSIDL_DRIVES", CSIDL_DRIVES);
    defconstant ("CSIDL_NETWORK", CSIDL_NETWORK);
    defconstant ("CSIDL_NETHOOD", CSIDL_NETHOOD);
    defconstant ("CSIDL_FONTS", CSIDL_FONTS);
    defconstant ("CSIDL_TEMPLATES", CSIDL_TEMPLATES);
    defconstant ("CSIDL_COMMON_STARTMENU", CSIDL_COMMON_STARTMENU);
    defconstant ("CSIDL_COMMON_PROGRAMS", CSIDL_COMMON_PROGRAMS);
    defconstant ("CSIDL_COMMON_STARTUP", CSIDL_COMMON_STARTUP);
    defconstant ("CSIDL_COMMON_DESKTOPDIRECTORY", CSIDL_COMMON_DESKTOPDIRECTORY);
    defconstant ("CSIDL_APPDATA", CSIDL_APPDATA);
    defconstant ("CSIDL_PRINTHOOD", CSIDL_PRINTHOOD);
    defconstant ("CSIDL_LOCAL_APPDATA", CSIDL_LOCAL_APPDATA);
    defconstant ("CSIDL_ALTSTARTUP", CSIDL_ALTSTARTUP);
    defconstant ("CSIDL_COMMON_ALTSTARTUP", CSIDL_COMMON_ALTSTARTUP);
    defconstant ("CSIDL_COMMON_FAVORITES", CSIDL_COMMON_FAVORITES);
    defconstant ("CSIDL_INTERNET_CACHE", CSIDL_INTERNET_CACHE);
    defconstant ("CSIDL_COOKIES", CSIDL_COOKIES);
    defconstant ("CSIDL_HISTORY", CSIDL_HISTORY);
    defconstant ("CSIDL_COMMON_APPDATA", CSIDL_COMMON_APPDATA);
    defconstant ("CSIDL_WINDOWS", CSIDL_WINDOWS);
    defconstant ("CSIDL_SYSTEM", CSIDL_SYSTEM);
    defconstant ("CSIDL_PROGRAM_FILES", CSIDL_PROGRAM_FILES);
    defconstant ("CSIDL_MYPICTURES", CSIDL_MYPICTURES);
    defconstant ("CSIDL_PROFILE", CSIDL_PROFILE);
    defconstant ("CSIDL_SYSTEMX86", CSIDL_SYSTEMX86);
    defconstant ("CSIDL_PROGRAM_FILESX86", CSIDL_PROGRAM_FILESX86);
    defconstant ("CSIDL_PROGRAM_FILES_COMMON", CSIDL_PROGRAM_FILES_COMMON);
    defconstant ("CSIDL_PROGRAM_FILES_COMMONX86", CSIDL_PROGRAM_FILES_COMMONX86);
    defconstant ("CSIDL_COMMON_TEMPLATES", CSIDL_COMMON_TEMPLATES);
    defconstant ("CSIDL_COMMON_DOCUMENTS", CSIDL_COMMON_DOCUMENTS);
    defconstant ("CSIDL_COMMON_ADMINTOOLS", CSIDL_COMMON_ADMINTOOLS);
    defconstant ("CSIDL_ADMINTOOLS", CSIDL_ADMINTOOLS);
    defconstant ("CSIDL_CONNECTIONS", CSIDL_CONNECTIONS);
    defconstant ("CSIDL_COMMON_MUSIC", CSIDL_COMMON_MUSIC);
    defconstant ("CSIDL_COMMON_PICTURES", CSIDL_COMMON_PICTURES);
    defconstant ("CSIDL_COMMON_VIDEO", CSIDL_COMMON_VIDEO);
    defconstant ("CSIDL_RESOURCES", CSIDL_RESOURCES);
    defconstant ("CSIDL_RESOURCES_LOCALIZED", CSIDL_RESOURCES_LOCALIZED);
    defconstant ("CSIDL_COMMON_OEM_LINKS", CSIDL_COMMON_OEM_LINKS);
    defconstant ("CSIDL_CDBURN_AREA", CSIDL_CDBURN_AREA);
    defconstant ("CSIDL_COMPUTERSNEARME", CSIDL_COMPUTERSNEARME);
    defconstant ("CSIDL_FLAG_DONT_VERIFY", CSIDL_FLAG_DONT_VERIFY);
    defconstant ("CSIDL_FLAG_CREATE", CSIDL_FLAG_CREATE);
    defconstant ("CSIDL_FLAG_MASK", CSIDL_FLAG_MASK);

    printf(";;; Exceptions\n");
    defconstant("+exception-access-violation+", EXCEPTION_ACCESS_VIOLATION);
    defconstant("+exception-array-bounds-exceeded+", EXCEPTION_ARRAY_BOUNDS_EXCEEDED);
    defconstant("+exception-breakpoint+", EXCEPTION_BREAKPOINT);
    defconstant("+exception-datatype-misalignment+", EXCEPTION_DATATYPE_MISALIGNMENT);
    defconstant("+exception-flt-denormal-operand+", EXCEPTION_FLT_DENORMAL_OPERAND);
    defconstant("+exception-flt-divide-by-zero+", EXCEPTION_FLT_DIVIDE_BY_ZERO);
    defconstant("+exception-flt-inexact-result+", EXCEPTION_FLT_INEXACT_RESULT);
    defconstant("+exception-flt-invalid-operation+", EXCEPTION_FLT_INVALID_OPERATION);
    defconstant("+exception-flt-overflow+", EXCEPTION_FLT_OVERFLOW);
    defconstant("+exception-flt-stack-check+", EXCEPTION_FLT_STACK_CHECK);
    defconstant("+exception-flt-underflow+", EXCEPTION_FLT_UNDERFLOW);
    defconstant("+exception-heap-corruption+", STATUS_HEAP_CORRUPTION);
    defconstant("+exception-illegal-instruction+", EXCEPTION_ILLEGAL_INSTRUCTION);
    defconstant("+exception-in-page-error+", EXCEPTION_IN_PAGE_ERROR);
    defconstant("+exception-int-divide-by-zero+", EXCEPTION_INT_DIVIDE_BY_ZERO);
    defconstant("+exception-int-overflow+", EXCEPTION_INT_OVERFLOW);
    defconstant("+exception-invalid-disposition+", EXCEPTION_INVALID_DISPOSITION);
    defconstant("+exception-noncontinuable-exception+", EXCEPTION_NONCONTINUABLE_EXCEPTION);
    defconstant("+exception-priv-instruction+", EXCEPTION_PRIV_INSTRUCTION);
    defconstant("+exception-single-step+", EXCEPTION_SINGLE_STEP);
    defconstant("+exception-stack-overflow+", EXCEPTION_STACK_OVERFLOW);
    defconstant("+dbg-printexception-c+", DBG_PRINTEXCEPTION_C);
#ifdef DBG_PRINTEXCEPTION_WIDE_C
    defconstant("+dbg-printexception-wide-c+", DBG_PRINTEXCEPTION_WIDE_C);
#else
    defconstant("+dbg-printexception-wide-c+", 0x4001000a);
#endif

    defconstant("+exception-maximum-parameters+", EXCEPTION_MAXIMUM_PARAMETERS);

    printf(";;; FormatMessage\n");

    defconstant("format-message-allocate-buffer", FORMAT_MESSAGE_ALLOCATE_BUFFER);
    defconstant("format-message-from-system", FORMAT_MESSAGE_FROM_SYSTEM);
    defconstant("format-message-max-width-mask", FORMAT_MESSAGE_MAX_WIDTH_MASK);
    defconstant("format-message-ignore-inserts", FORMAT_MESSAGE_IGNORE_INSERTS);

    printf(";;; Errors\n");

    printf(";;; Errors\n");

    defconstant("ERROR_ENVVAR_NOT_FOUND", ERROR_ENVVAR_NOT_FOUND);
    defconstant("ERROR_ALREADY_EXISTS", ERROR_ALREADY_EXISTS);
    defconstant("ERROR_FILE_EXISTS", ERROR_FILE_EXISTS);
    defconstant("ERROR_FILE_NOT_FOUND", ERROR_FILE_NOT_FOUND);
    defconstant("ERROR_ACCESS_DENIED", ERROR_ACCESS_DENIED);

    defconstant("error-io-pending", ERROR_IO_PENDING);
    defconstant("error-broken-pipe", ERROR_BROKEN_PIPE);
    defconstant("error-no-data", ERROR_NO_DATA);
    defconstant("error-handle-eof", ERROR_HANDLE_EOF);


    printf(";;; GetComputerName\n");

    defconstant ("MAX_COMPUTERNAME_LENGTH", MAX_COMPUTERNAME_LENGTH);
    defconstant ("ERROR_BUFFER_OVERFLOW", ERROR_BUFFER_OVERFLOW);

    printf(";;; Windows Types\n");
    DEFTYPE("int-ptr", INT_PTR);
    DEFTYPE("dword",   DWORD);
    DEFTYPE("bool",    BOOL);
    DEFTYPE("uint",    UINT);
    DEFTYPE("ulong",   ULONG);

    printf(";;; File Desired Access\n");
    defconstant ("FILE_GENERIC_READ", FILE_GENERIC_READ);
    defconstant ("FILE_GENERIC_WRITE", FILE_GENERIC_WRITE);
    defconstant ("FILE_GENERIC_EXECUTE", FILE_GENERIC_EXECUTE);
    defconstant ("FILE_SHARE_READ", FILE_SHARE_READ);
    defconstant ("FILE_SHARE_WRITE", FILE_SHARE_WRITE);
    defconstant ("FILE_SHARE_DELETE", FILE_SHARE_DELETE);

    printf(";;; File Creation Dispositions\n");
    defconstant("CREATE_NEW", CREATE_NEW);
    defconstant("CREATE_ALWAYS", CREATE_ALWAYS);
    defconstant("OPEN_EXISTING", OPEN_EXISTING);
    defconstant("OPEN_ALWAYS", OPEN_ALWAYS);
    defconstant("TRUNCATE_EXISTING", TRUNCATE_EXISTING);

    printf(";;; Desired Access\n");
    defconstant("ACCESS_GENERIC_READ", GENERIC_READ);
    defconstant("ACCESS_GENERIC_WRITE", GENERIC_WRITE);
    defconstant("ACCESS_GENERIC_EXECUTE", GENERIC_EXECUTE);
    defconstant("ACCESS_GENERIC_ALL", GENERIC_ALL);
    defconstant("ACCESS_FILE_APPEND_DATA", FILE_APPEND_DATA);
    defconstant("ACCESS_DELETE", DELETE);

    printf(";;; Handle Information Flags\n");
    defconstant("HANDLE_FLAG_INHERIT", HANDLE_FLAG_INHERIT);
    defconstant("HANDLE_FLAG_PROTECT_FROM_CLOSE", HANDLE_FLAG_PROTECT_FROM_CLOSE);

    printf(";;; Standard Handle Keys\n");
    defconstant("STD_INPUT_HANDLE", STD_INPUT_HANDLE);
    defconstant("STD_OUTPUT_HANDLE", STD_OUTPUT_HANDLE);
    defconstant("STD_ERROR_HANDLE", STD_ERROR_HANDLE);

    printf(";;; WinCrypt\n");
    defconstant("crypt-verifycontext", CRYPT_VERIFYCONTEXT);
    defconstant("crypt-silent", CRYPT_SILENT);
    defconstant("prov-rsa-full", PROV_RSA_FULL);

    defconstant("pipe-access-duplex", PIPE_ACCESS_DUPLEX);
    defconstant("pipe-access-inbound", PIPE_ACCESS_INBOUND);
    defconstant("pipe-access-outbound", PIPE_ACCESS_OUTBOUND);
    defconstant("pipe-type-byte", PIPE_TYPE_BYTE);

    defconstant("wait-abandoned", WAIT_ABANDONED);
    defconstant("wait-object-0", WAIT_OBJECT_0);
    defconstant("wait-timeout", WAIT_TIMEOUT);
    defconstant("wait-failed", WAIT_FAILED);

    defconstant("still-active", STILL_ACTIVE);

    /* FIXME: SB-UNIX and SB-WIN32 really need to be untangled. */
    printf("(in-package \"SB-UNIX\")\n\n");
    printf(";;; Unix-like constants and types on Windows\n");
    defconstant("o_rdonly", _O_RDONLY);
    defconstant("o_wronly", _O_WRONLY);
    defconstant("o_rdwr",   _O_RDWR);
    defconstant("o_creat",  _O_CREAT);
    defconstant("o_trunc",  _O_TRUNC);
    defconstant("o_append", _O_APPEND);
    defconstant("o_excl",   _O_EXCL);
    defconstant("o_binary", _O_BINARY);
    defconstant("o_noinherit", _O_NOINHERIT);

    defconstant("enoent", ENOENT);
    defconstant("eexist", EEXIST);
    defconstant("eintr", EINTR);
    defconstant("eagain", EAGAIN);
    defconstant("ebadf", EBADF);

    defconstant("s-ifmt",  S_IFMT);
    defconstant("s-ifdir", S_IFDIR);
    defconstant("s-ifreg", S_IFREG);

    DEFTYPE("ino-t",  ino_t);
    DEFTYPE("time-t", time_t);
    DEFTYPE("off-t",  off_t);
    DEFTYPE("size-t", size_t);
    DEFTYPE("ssize-t", ssize_t);
    DEFTYPE("mode-t", mode_t);

    DEFTYPE("wst-dev-t", wst_dev_t);
    DEFTYPE("wst-ino-t", wst_ino_t);
    DEFTYPE("wst-off-t", wst_off_t);
    DEFTYPE("wst-blksize-t", wst_blksize_t);
    DEFTYPE("wst-blkcnt-t", wst_blkcnt_t);
    DEFTYPE("wst-nlink-t", wst_nlink_t);
    DEFTYPE("wst-uid-t", wst_uid_t);
    DEFTYPE("wst-gid-t", wst_gid_t);

    /* KLUDGE */
    defconstant("fd-setsize", 1024);
    printf("\n");

    defconstant("sizeof-sigset_t", sizeof (sigset_t));
    defconstant("sig_block", SIG_BLOCK);
    defconstant("sig_unblock", SIG_UNBLOCK);
    defconstant("sig_setmask", SIG_SETMASK);

}
