# Note that this script can accept some limited command-line arguments, run
# `julia build_tarballs.jl --help` to see a usage message.
using Pkg
using BinaryBuilder

name = "LibtaskDylib"
version_str = Pkg.TOML.parsefile(joinpath(@__DIR__, "../Project.toml"))["version"] |> strip |> (x) -> lstrip(x, ['v'])
version = VersionNumber(version_str)

# Collection of sources required to build Libtask
function get_commit_id()
    is_pr = get(ENV, "TRAVIS_PULL_REQUEST", "false")
    if is_pr != "false"
        return get(ENV, "TRAVIS_PULL_REQUEST_SHA", "")
    end
    return readlines(`git rev-parse HEAD`)[1]
end

sources = [
    "https://github.com/TuringLang/Libtask.jl.git" => get_commit_id(),
]


# Bash recipe for building across all platforms
script = read(joinpath(dirname(@__FILE__), "build_dylib.sh"), String)

# These are the platforms we will build for by default, unless further
# platforms are passed in on the command line
platforms = [
    Linux(:i686, :glibc),
    Linux(:x86_64, :glibc),
    Linux(:armv7l, :glibc, :eabihf),
    Linux(:aarch64),
    MacOS(:x86_64),
    Windows(:i686),
    Windows(:x86_64)
]

# The products that we will ensure are always built
products(prefix) = [
    LibraryProduct(prefix, "libtask_v1_0", :libtask_v1_0)
    LibraryProduct(prefix, "libtask_v1_1", :libtask_v1_1)
    LibraryProduct(prefix, "libtask_v1_2", :libtask_v1_2)
    LibraryProduct(prefix, "libtask_v1_3", :libtask_v1_3)
]

# Dependencies that must be installed before this package can be built
dependencies = [

]

# Build the tarballs, and possibly a `build.jl` as well.
# build_file = "products/build_$(name).v$(version_str).jl"
build_tarballs(ARGS, name, version, sources, script, platforms, products, dependencies)
