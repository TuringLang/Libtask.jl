const ARCH = @static if string(Sys.ARCH)[1] == 'i'
    "x86"
else
    string(Sys.ARCH)
end

const PLATFORM = @static if Sys.islinux()
    "linux-" * ARCH
elseif Sys.iswindows()
    "windows-" * ARCH
elseif Sys.isapple()
    "darwin-" * ARCH
end

OUTPUT = "$(PLATFORM)-v$(VERSION.major)_$(VERSION.minor)_$(VERSION.patch).jl"
OUTPUT_DEST = joinpath("src/memlayout", OUTPUT)

isfile(OUTPUT_DEST) && exit(0)

const PROJECT_DIR = (@__DIR__) |> dirname |> dirname |> dirname
const INCLUDE = joinpath(dirname(Sys.BINDIR), "include/julia")
const OPTIONS = if string(Sys.ARCH)[1] == 'i'
    Sys.islinux() && run(`sudo apt-get install g++-multilib -y`)
    ["-std=c++11","-march=pentium4", "-m32", "-static-libgcc", "-static-libstdc++"]
else
    ["-std=c++11"]
end

run(`g++ $(OPTIONS) -I$(INCLUDE) $(PROJECT_DIR)/.github/workflows/tasklayout/memlayout.cpp -o memlayout-gen.exe`)
run(`./memlayout-gen.exe`)

isfile(OUTPUT) && Base.Filesystem.mv(OUTPUT, OUTPUT_DEST)
