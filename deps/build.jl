function find_prev_tag(tag)
    project_root = (@__DIR__) |> dirname |> abspath
    tags = readlines(`git -C $project_root tag`)
    sort!(tags)
    idx = indexin([tag], tags)[1]
    if idx == nothing return "NO-PREV-TAG" end
    return get(tags, idx - 1, "NO-PREV-TAG")
end

function include_build_script(version_str, try_prev=false)
    build_script_url = "https://github.com/TuringLang/Libtask.jl/releases/download/v$(version_str)/build_LibtaskDylib.v$(version_str).jl"
    build_script = joinpath(@__DIR__, "tmp-build.jl")
    build_script = try download(build_script_url, build_script) catch end
    if build_script == nothing && try_prev # no such file
        version_str = find_prev_tag("v$version_str") |> strip |> (x) -> lstrip(x, ['v'])
        return include_build_script(version_str, false)
    end
    include(build_script)
end

function get_version_str()
    path = joinpath(@__DIR__, "../Project.toml")
    version_reg = r"version\s*=\s*\"(.*)\""
    open(path) do file
        lines = readlines(file)
        for line in lines
            m = match(version_reg, line)
            if isa(m, RegexMatch) return m.captures[1] end
        end
    end
end

version_str = get_version_str() |> strip |> (x) -> lstrip(x, ['v'])
include_build_script(version_str, true)
