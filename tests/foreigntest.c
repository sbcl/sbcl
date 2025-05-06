int summish(int x, int y) { return 1 + x + y; }

int numberish = 42;

int nummish(int x) { return numberish + x; }

short negative_short() { return -1; }
int negative_int()     { return -2; }
long negative_long()   { return -3; }

long long powish(unsigned int x, unsigned int y) {
  long long acc = 1;
  long long xx = (long long) x;
  for(; y != 1; y /= 2) {
    if (y & 1) {
      acc *= xx;
      y -= 1;
    }
    xx *= xx;
  }
  return xx*acc;
}

float return9th(float f1, float f2, float f3, float f4, float f5,
                float f6, float f7, float f8, float f9, float f10,
                float f11, float f12) {
    return f9;
}

double return9thd(double f1, double f2, double f3, double f4, double f5,
                  double f6, double f7, double f8, double f9, double f10,
                  double f11, double f12) {
    return f9;
}

int long_test8(int a1, int a2, int a3, int a4, int a5,
               int a6, int a7, long long l1) {
    return (l1 == powish(2,34));
}

int long_test9(int a1, int a2, int a3, int a4, int a5,
               int a6, int a7, long long l1, int a8) {
    return (l1 == powish(2,35));
}

int long_test2(int i1, int i2, int i3, int i4, int i5, int i6,
               int i7, int i8, int i9, long long l1, long long l2) {
    return (l1 == (1 + powish(2,37)));
}

int long_sap_test1(int *p1, long long l1) {
    return (l1 == (3 + powish(2,*p1)));
}

int long_sap_test2(int *p1, int i1, long long l1) {
    return (l1 == (3 + powish(2,*p1)));
}

long long return_long_long() {
    return powish(2,33);
}

signed char return_schar_test(signed char *p) {
    return *p;
}

unsigned char return_uchar_test(unsigned char *p) {
    return *p;
}

short return_short_test(short *p) {
    return *p;
}

unsigned short return_ushort_test(unsigned short *p) {
    return *p;
}

int return_int_test(int *p) {
    return *p;
}

unsigned int return_uint_test(unsigned int *p) {
    return *p;
}
