void gcbarrier_patch_code_range(uword_t start, void* limit)
{
    int mask = (1<<gc_card_table_nbits)-1;
    struct varint_unpacker unpacker;
    struct code* code;
    lispobj *where = (lispobj*)start;
    while (where < (lispobj*)limit) {
        if (widetag_of(where) == CODE_HEADER_WIDETAG) {
            code = (struct code*)where;
            varint_unpacker_init(&unpacker, code->fixups);
            // There are two other data streams preceding the one we want
            skip_data_stream(&unpacker);
            skip_data_stream(&unpacker);
            char* instructions = code_text_start(code);
            int prev_loc = 0, loc;
            while (varint_unpack(&unpacker, &loc) && loc != 0) {
                loc += prev_loc;
                prev_loc = loc;
                void* patch_where = instructions + loc;
                UNALIGNED_STORE32(patch_where, mask);
            }
        }
        where += OBJECT_SIZE(*where, where);
    }
}
