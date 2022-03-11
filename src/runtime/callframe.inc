/* -*- Mode: C -*- */

struct call_frame {
        struct call_frame *old_cont;
        lispobj saved_lra;
        lispobj code;
        lispobj other_state[5];
};

struct call_info {
    struct call_frame *frame;
    int interrupted;
    struct code *code;
    lispobj lra;
    // Byte offset from 'code' base address to program counter,
    // unless 'code' is 0, then an absolute PC.
    uword_t pc;
};

int lisp_frame_previous(struct thread*, struct call_info *info);
