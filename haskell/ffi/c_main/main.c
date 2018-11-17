#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "fib_stub.h"
#endif
#include <stdio.h>

int main(int argc,char** argv)
{
    hs_init(&argc, &argv);

    // TODO report bug, ghc expects
    // void (*)(void)
    // instead of
    // HsInt (*)(HsInt)
    printFib((void (*)(void))&fib);

    hs_exit();

	return 0;		
}

