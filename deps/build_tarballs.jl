# Note that this script can accept some limited command-line arguments, run
# `julia build_tarballs.jl --help` to see a usage message.
using BinaryBuilder

name = "LibtaskDylib"
version_str = read(joinpath(@__DIR__, "../VERSION"), String) |> strip |> (x) -> lstrip(x, ['v'])
version = VersionNumber(version_str)

# Collection of sources required to build Libtask
sources = [
    "https://github.com/TuringLang/Libtask.jl.git" => readlines(`git rev-list -n 1 v$(version_str)`)[1],
]

# Bash recipe for building across all platforms
script = read(joinpath(dirname(@__FILE__), "build_dylib.sh"), String)

# These are the platforms we will build for by default, unless further
# platforms are passed in on the command line
platforms = [
    Linux(:i686, :glibc),
    Linux(:x86_64, :glibc),
    Linux(:armv7l, :glibc, :eabihf),
    MacOS(:x86_64),
    Windows(:i686),
    Windows(:x86_64)
]

# The products that we will ensure are always built
products(prefix) = [
    LibraryProduct(prefix, "libtask", :libtask)
]

# Dependencies that must be installed before this package can be built
dependencies = [

]

# Build the tarballs, and possibly a `build.jl` as well.
build_tarballs(ARGS, name, version, sources, script, platforms, products, dependencies)
