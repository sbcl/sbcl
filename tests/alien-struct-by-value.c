struct tiny_align_8 {
  long long m0;
};
long long tiny_align_8_get_m0(struct tiny_align_8 m) { return m.m0; }
/** On ARM64, m gets passed on X1.*/
long long tiny_align_8_get_m0_1(long long _i0, struct tiny_align_8 m) {
  (void) _i0;
  return m.m0;
}
/** On ARM64, m gets passed on X1.*/
long long tiny_align_8_get_m0_2(int _i0, struct tiny_align_8 m) {
  (void) _i0;
  return m.m0;
}
/** On ARM64, m gets passed on stack.*/
long long tiny_align_8_get_m0_3(long long _i0, long long _i1, long long _i2, long long _i3,
                                long long _i4, long long _i5, long long _i6, long long _i7,
                                struct tiny_align_8 m) {
  (void) _i0; (void) _i1; (void) _i2; (void) _i3;
  (void) _i4; (void) _i5; (void) _i6; (void) _i7;
  return m.m0;
}
/** On ARM64, m gets passed on stack.*/
long long tiny_align_8_get_m0_4(int _i0, int _i1, int _i2, int _i3,
                                int _i4, int _i5, int _i6, int _i7,
                                struct tiny_align_8 m) {
  (void) _i0; (void) _i1; (void) _i2; (void) _i3;
  (void) _i4; (void) _i5; (void) _i6; (void) _i7;
  return m.m0;
}
/** On ARM64, m gets passed on stack.*/
long long tiny_align_8_get_m0_5(long long _i0, long long _i1, long long _i2, long long _i3,
                                long long _i4, long long _i5, long long _i6, long long _i7,
                                long long _i8, struct tiny_align_8 m) {
  (void) _i0; (void) _i1; (void) _i2; (void) _i3;
  (void) _i4; (void) _i5; (void) _i6; (void) _i7;
  (void) _i8;
  return m.m0;
}
/** On ARM64, m gets passed on stack.*/
long long tiny_align_8_get_m0_6(long long _i0, long long _i1, long long _i2, long long _i3,
                                long long _i4, long long _i5, long long _i6, long long _i7,
                                int _i8, struct tiny_align_8 m) {
  (void) _i0; (void) _i1; (void) _i2; (void) _i3;
  (void) _i4; (void) _i5; (void) _i6; (void) _i7;
  (void) _i8;
  return m.m0;
}
/** Nothing should change.*/
void tiny_align_8_mutate(volatile struct tiny_align_8 m) {
  m.m0++;
}

/** A small structure with 8-byte alignment.
    SysV x86-64 and AAPCS64 will pass this by register.
*/
struct small_align_8 {
  long long m0, m1;
};
long long small_align_8_get_m0(struct small_align_8 m) { return m.m0; }
long long small_align_8_get_m1(struct small_align_8 m) { return m.m1; }
/** On ARM64, m is passed on X2 and X3. */
long long small_align_8_get_m0_1(long long _i0, long long _i1, struct small_align_8 m) {
  (void) _i0; (void) _i1;
  return m.m0;
}
/** On ARM64, m is passed on X2 and X3 if not Darwin, otherwise X1 and X2.*/
long long small_align_8_get_m0_2(long long _i0, struct small_align_8 m) {
  (void) _i0;
  return m.m0;
}
/** On ARM64, m is passed on stack.*/
long long small_align_8_get_m0_3(long long _i0, long long _i1, long long _i2, long long _i3,
                                 long long _i4, long long _i5, long long _i6, long long _i7,
                                 struct small_align_8 m) {
  (void) _i0; (void) _i1; (void) _i2; (void) _i3;
  (void) _i4; (void) _i5; (void) _i6; (void) _i7;
  return m.m0;
}
/** On ARM64, m is passed on stack, even on Darwin.*/
long long small_align_8_get_m0_4(long long _i0, long long _i1, long long _i2, long long _i3,
                                 long long _i4, long long _i5, long long _i6,
                                 struct small_align_8 m) {
  (void) _i0; (void) _i1; (void) _i2; (void) _i3;
  (void) _i4; (void) _i5; (void) _i6;
  return m.m0;
}
void small_align_8_mutate(volatile struct small_align_8 m) {
  m.m0++;
  m.m1++;
}

/** A large structure with 8-byte alignment.
    This should be too big for any architecture to pass by registers.
*/
struct large_align_8 {
  long long m0, m4, m8, m12;
  long long m1, m5, m9, m13;
  long long m2, m6, m10, m14;
  long long m3, m7, m11, m15;
};
#define large_align_8_get(member)                                \
  long long large_align_8_get_##member(struct large_align_8 m) { \
    return m.member;                                              \
  }
large_align_8_get(m0);
large_align_8_get(m1);
large_align_8_get(m2);
large_align_8_get(m3);
large_align_8_get(m4);
large_align_8_get(m5);
large_align_8_get(m6);
large_align_8_get(m7);
large_align_8_get(m8);
large_align_8_get(m9);
large_align_8_get(m10);
large_align_8_get(m11);
large_align_8_get(m12);
large_align_8_get(m13);
large_align_8_get(m14);
large_align_8_get(m15);

/** On ARM64, m is passed as pointer on X1.*/
long long large_align_8_get_m0_1(long long _i0, struct large_align_8 m) {
  (void) _i0;
  return m.m0;
}

/** On ARM64, m is passed as pointer on stack.*/
long long large_align_8_get_m0_2(long long _i0, long long _i1, long long _i2, long long _i3,
                                 long long _i4, long long _i5, long long _i6, long long _i7,
                                 struct large_align_8 m) {
  (void) _i0; (void) _i1; (void) _i2; (void) _i3;
  (void) _i4; (void) _i5; (void) _i6; (void) _i7;
  return m.m0;
}

/** On ARM64, m is passed as pointer on stack.*/
long long large_align_8_get_m0_3(long long _i0, long long _i1, long long _i2, long long _i3,
                                 long long _i4, long long _i5, long long _i6, long long _i7,
                                 int _i8, struct large_align_8 m) {
  (void) _i0; (void) _i1; (void) _i2; (void) _i3;
  (void) _i4; (void) _i5; (void) _i6; (void) _i7;
  (void) _i8;
  return m.m0;
}

/** Mutates the input struct. Volatile to avoid compiler optimizing away the mutation.*/
void large_align_8_mutate(volatile struct large_align_8 m) {
  m.m0++;
  m.m1++;
  m.m2++;
  m.m3++;
  m.m4++;
  m.m5++;
  m.m6++;
  m.m7++;
  m.m8++;
  m.m9++;
  m.m10++;
  m.m11++;
  m.m12++;
  m.m13++;
  m.m14++;
  m.m15++;
}
