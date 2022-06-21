int main () {
  __builtin_cpu_init();
  return __builtin_cpu_supports("avx2") != 0;
}
