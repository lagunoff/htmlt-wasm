#include <stdio.h>
#include <Rts.h>

STATIC_INLINE void hs_init_with_rtsopts_(char *argv[]) {
  int argc;
  for (argc = 0; argv[argc] != NULL; ++argc) {
  }
  hs_init_with_rtsopts(&argc, &argv);
}

// Reactor modules tend to randomly crash after several GC sweeps,
// setting up RTS options here to make the app live longer at the cost
// of high memory consumption
void init_greadymem() {
  char *argv[] = {"uknown.wasm", "+RTS", "--nonmoving-gc", "-H2G", "-DS", "-RTS", NULL};
  hs_init_with_rtsopts_(argv);
}

// Enable sanity checks to gather data for crash reports
void init_debug() {
  char *argv[] = {"uknown.wasm", "+RTS", "-DS", "-RTS", NULL};
  hs_init_with_rtsopts_(argv);
}
