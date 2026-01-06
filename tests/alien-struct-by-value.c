struct tiny_align_8 {
  long long m0;
};
long long tiny_align_8_get_m0(struct tiny_align_8 m) { return m.m0; }
void tiny_align_8_mutate(volatile struct tiny_align_8 m) {
  m.m0++;
}
struct tiny_align_8 tiny_align_8_return(long long val) {
  struct tiny_align_8 result;
  result.m0 = val;
  return result;
}
struct tiny_align_8 tiny_align_8_identity(struct tiny_align_8 m) {
  return m;
}

/** A small structure with 8-byte alignment.
    SysV x86-64 and AAPCS64 will pass this by register.
*/
struct small_align_8 {
  long long m0, m1;
};
long long small_align_8_get_m0(struct small_align_8 m) { return m.m0; }
long long small_align_8_get_m1(struct small_align_8 m) { return m.m1; }
void small_align_8_mutate(volatile struct small_align_8 m) {
  m.m0++;
  m.m1++;
}
struct small_align_8 small_align_8_return(long long v0, long long v1) {
  struct small_align_8 result;
  result.m0 = v0;
  result.m1 = v1;
  return result;
}
struct small_align_8 small_align_8_identity(struct small_align_8 m) {
  return m;
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
struct large_align_8 large_align_8_return(long long v0, long long v1) {
  struct large_align_8 result = {0};
  result.m0 = v0;
  result.m1 = v1;
  return result;
}
struct large_align_8 large_align_8_identity(struct large_align_8 m) {
  return m;
}

/** Structs with floating point members for SSE register testing */
struct two_doubles {
  double d0, d1;
};
struct two_doubles two_doubles_return(double d0, double d1) {
  struct two_doubles result;
  result.d0 = d0;
  result.d1 = d1;
  return result;
}
double two_doubles_sum(struct two_doubles m) {
  return m.d0 + m.d1;
}
struct two_doubles two_doubles_identity(struct two_doubles m) {
  return m;
}

struct two_floats {
  float f0, f1;
};
struct two_floats two_floats_return(float f0, float f1) {
  struct two_floats result;
  result.f0 = f0;
  result.f1 = f1;
  return result;
}
float two_floats_sum(struct two_floats m) {
  return m.f0 + m.f1;
}
struct two_floats two_floats_identity(struct two_floats m) {
  return m;
}

/** Mixed int and float struct - tests split register handling on x86-64 */
struct int_double {
  long long i;
  double d;
};
struct int_double int_double_return(long long i, double d) {
  struct int_double result;
  result.i = i;
  result.d = d;
  return result;
}
long long int_double_get_int(struct int_double m) { return m.i; }
double int_double_get_double(struct int_double m) { return m.d; }
struct int_double int_double_identity(struct int_double m) {
  return m;
}

/** Medium struct (24 bytes) - too large for ARM64 registers, tests boundary */
struct medium_align_8 {
  long long m0, m1, m2;
};
struct medium_align_8 medium_align_8_return(long long v0, long long v1, long long v2) {
  struct medium_align_8 result;
  result.m0 = v0;
  result.m1 = v1;
  result.m2 = v2;
  return result;
}
long long medium_align_8_get_m0(struct medium_align_8 m) { return m.m0; }
long long medium_align_8_get_m1(struct medium_align_8 m) { return m.m1; }
long long medium_align_8_get_m2(struct medium_align_8 m) { return m.m2; }
struct medium_align_8 medium_align_8_identity(struct medium_align_8 m) {
  return m;
}

/** Four floats struct - tests HFA (Homogeneous Floating-point Aggregate) on ARM64 */
struct four_floats {
  float f0, f1, f2, f3;
};
struct four_floats four_floats_return(float f0, float f1, float f2, float f3) {
  struct four_floats result;
  result.f0 = f0;
  result.f1 = f1;
  result.f2 = f2;
  result.f3 = f3;
  return result;
}
float four_floats_sum(struct four_floats m) {
  return m.f0 + m.f1 + m.f2 + m.f3;
}
struct four_floats four_floats_identity(struct four_floats m) {
  return m;
}

/** Three doubles struct - tests HFA boundary (3 doubles = 24 bytes) */
struct three_doubles {
  double d0, d1, d2;
};
struct three_doubles three_doubles_return(double d0, double d1, double d2) {
  struct three_doubles result;
  result.d0 = d0;
  result.d1 = d1;
  result.d2 = d2;
  return result;
}
double three_doubles_sum(struct three_doubles m) {
  return m.d0 + m.d1 + m.d2;
}
struct three_doubles three_doubles_identity(struct three_doubles m) {
  return m;
}

/** HFA with array of 4 floats - tests array-based HFA detection on ARM64 */
struct float_array_4 {
  float arr[4];
};
struct float_array_4 float_array_4_return(float f0, float f1, float f2, float f3) {
  struct float_array_4 result;
  result.arr[0] = f0;
  result.arr[1] = f1;
  result.arr[2] = f2;
  result.arr[3] = f3;
  return result;
}
float float_array_4_sum(struct float_array_4 m) {
  return m.arr[0] + m.arr[1] + m.arr[2] + m.arr[3];
}
float float_array_4_get(struct float_array_4 m, int index) {
  return m.arr[index];
}
struct float_array_4 float_array_4_identity(struct float_array_4 m) {
  return m;
}

/** HFA with array of 2 doubles - tests array-based HFA with doubles */
struct double_array_2 {
  double arr[2];
};
struct double_array_2 double_array_2_return(double d0, double d1) {
  struct double_array_2 result;
  result.arr[0] = d0;
  result.arr[1] = d1;
  return result;
}
double double_array_2_sum(struct double_array_2 m) {
  return m.arr[0] + m.arr[1];
}
double double_array_2_get(struct double_array_2 m, int index) {
  return m.arr[index];
}
struct double_array_2 double_array_2_identity(struct double_array_2 m) {
  return m;
}

/** HFA with array of 3 floats - tests odd-sized array HFA */
struct float_array_3 {
  float arr[3];
};
struct float_array_3 float_array_3_return(float f0, float f1, float f2) {
  struct float_array_3 result;
  result.arr[0] = f0;
  result.arr[1] = f1;
  result.arr[2] = f2;
  return result;
}
float float_array_3_sum(struct float_array_3 m) {
  return m.arr[0] + m.arr[1] + m.arr[2];
}
struct float_array_3 float_array_3_identity(struct float_array_3 m) {
  return m;
}
