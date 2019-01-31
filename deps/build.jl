@static if Sys.isapple()
    println("BinaryBuilder has a problem of its macOS support, so we build the dylib by ourselves.")
    using Libdl
    julia_root = Libdl.dlpath("libjulia") |> dirname |> dirname |>abspath
    LIBS = "$(julia_root)/lib"
    LIBSJL = "$(julia_root)/lib/julia"
    INCLUDES = "$(julia_root)/include/julia"
    THIS_DIR = dirname(@__FILE__)
    OUTPUT_DIR = joinpath(THIS_DIR, "usr/lib")
    run(`mkdir -p $OUTPUT_DIR`)
    OUTPUT = joinpath(OUTPUT_DIR, "libtask.dylib")
    run(`gcc -O2 -shared -std=gnu99 -I$INCLUDES -DJULIA_ENABLE_THREADING=1 -fPIC -L$LIBSJL -L$LIBS -ljulia $THIS_DIR/task.c -o $OUTPUT`)

    depsjl = joinpath(THIS_DIR, "deps.jl")
    open(depsjl, "w") do io
        write(io, "const libtask = \"$OUTPUT\";\n")
        write(io, "function check_deps() end\n")
    end;
else
    version_str = read(joinpath(@__DIR__, "../VERSION"), String) |> strip |> (x) -> lstrip(x, ['v'])
    # TODO CHANGE URL
    # https://github.com/KDr2/Libtask.jl/releases/download/T001/build_LibtaskDylib.v1.0.0.jl
    build_script_url = "https://github.com/TuringLang/Libtask.jl/releases/download/v$(version_str)/build_LibtaskDylib.v$(version_str).jl"
    build_script = joinpath(@__DIR__, "tmp-build.jl")
    download(build_script_url, build_script)
    include(build_script)
end
