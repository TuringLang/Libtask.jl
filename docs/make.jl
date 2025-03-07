using Documenter, Libtask

DocMeta.setdocmeta!(
    Libtask,
    :DocTestSetup,
    quote
        using Libtask
    end;
    recursive=true,
)

makedocs(;
    sitename="Libtask", doctest=true, pages=["index.md", "internals.md"], modules=[Libtask]
)

deploydocs(; repo="github.com/TuringLang/Libtask.jl.git", push_preview=true)
