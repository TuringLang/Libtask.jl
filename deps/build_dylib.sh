#!/bin/bash

if [ $target = "arm-linux-gnueabihf" ]; then
    cd $WORKSPACE/srcdir
    wget "https://julialang-s3.julialang.org/bin/linux/armv7l/1.0/julia-1.0.0-linux-armv7l.tar.gz"
    tar xzvf julia-1.0.0-linux-armv7l.tar.gz
    rm *.tar.gz
    mv julia* julia
    LIBS="`pwd`/julia/lib"
    LIBSJL="`pwd`/julia/lib/julia"
    INCLUDES="`pwd`/julia/include/julia"
    cd Libtask.jl/deps
    gcc -O2 -shared -std=gnu99 -I$INCLUDES -DJULIA_ENABLE_THREADING=1 -fPIC -L$LIBSJL -L$LIBS -Wl,--export-dynamic -ljulia task.c -o libtask.so
    mkdir $prefix/lib
    mv libtask.so $prefix/lib
    exit
fi

if [ $target = "i686-linux-gnu" ]; then
    cd $WORKSPACE/srcdir
    wget "https://julialang-s3.julialang.org/bin/linux/x86/1.0/julia-1.0.0-linux-i686.tar.gz"
    tar xzvf julia-1.0.0-linux-i686.tar.gz
    rm *.tar.gz
    mv julia* julia
    LIBS="`pwd`/julia/lib"
    LIBSJL="`pwd`/julia/lib/julia"
    INCLUDES="`pwd`/julia/include/julia"
    cd Libtask.jl/deps
    gcc -march=pentium4 -O2 -shared -std=gnu99 -I$INCLUDES -DJULIA_ENABLE_THREADING=1 -fPIC -L$LIBSJL -L$LIBS -Wl,--export-dynamic -ljulia task.c -o libtask.so
    mkdir $prefix/lib
    mv libtask.so $prefix/lib
    exit
fi

if [ $target = "x86_64-linux-gnu" ]; then
    cd $WORKSPACE/srcdir
    wget "https://julialang-s3.julialang.org/bin/linux/x64/1.0/julia-1.0.0-linux-x86_64.tar.gz"
    tar xzvf julia-1.0.0-linux-x86_64.tar.gz
    rm *.tar.gz
    mv julia* julia
    LIBS="`pwd`/julia/lib"
    LIBSJL="`pwd`/julia/lib/julia"
    INCLUDES="`pwd`/julia/include/julia"
    cd Libtask.jl/deps
    gcc -O2 -shared -std=gnu99 -I$INCLUDES -DJULIA_ENABLE_THREADING=1 -fPIC -L$LIBSJL -L$LIBS -Wl,--export-dynamic -ljulia task.c -o libtask.so
    mkdir $prefix/lib
    mv libtask.so $prefix/lib
    exit
fi

if [ $target = "x86_64-w64-mingw32" ]; then
    cd $WORKSPACE/srcdir
    wget http://mlg.eng.cam.ac.uk/hong/julia/julia-1.0.0-win64.tar.gz
    tar xzvf julia-1.0.0-win64.tar.gz
    rm *.tar.gz
    mv julia* julia
    LIBS="`pwd`/julia/bin"
    INCLUDES="`pwd`/julia/include/julia"
    cd Libtask.jl/deps
    gcc -O2 -shared -std=gnu99 -I$INCLUDES -DJULIA_ENABLE_THREADING=1 -fPIC -L$LIBS -Wl,--export-all-symbols -ljulia -lopenlibm task.c -o libtask.dll
    mkdir $prefix/bin
    mv libtask.dll $prefix/bin
    exit
fi

if [ $target = "i686-w64-mingw32" ]; then
    cd $WORKSPACE/srcdir
    wget http://mlg.eng.cam.ac.uk/hong/julia/julia-1.0.0-win32.tar.gz
    tar xzvf julia-1.0.0-win32.tar.gz
    rm *.tar.gz
    mv julia* julia
    LIBS="`pwd`/julia/bin"
    INCLUDES="`pwd`/julia/include/julia"
    cd Libtask.jl/deps
    gcc -march=pentium4 -O2 -shared -std=gnu99 -I$INCLUDES -DJULIA_ENABLE_THREADING=1 -fPIC -L$LIBS -Wl,--export-all-symbols -ljulia -lopenlibm task.c -o libtask.dll
    mkdir $prefix/bin
    mv libtask.dll $prefix/bin
    exit
fi

if [ $target = "x86_64-apple-darwin14" ]; then
    cd $WORKSPACE/srcdir
    wget http://mlg.eng.cam.ac.uk/hong/julia/julia-1.0.0-mac64.tar.gz
    tar xzvf julia-1.0.0-mac64.tar.gz
    rm *.tar.gz
    mv julia* julia
    LIBS="`pwd`/julia/Contents/Resources/julia/lib"
    LIBSJL="`pwd`/julia/Contents/Resources/julia/lib/julia"
    INCLUDES="`pwd`/julia/Contents/Resources/julia/include/julia"
    cd Libtask.jl/deps
    gcc -O2 -shared -std=gnu99 -I$INCLUDES -DJULIA_ENABLE_THREADING=1 -fPIC -L$LIBSJL -L$LIBS -ljulia task.c -o libtask.dylib
    mkdir $prefix/lib
    mv libtask.dylib $prefix/lib
    exit
fi
