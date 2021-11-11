
for fn = readdir("downloaded")
    infile = "downloaded/$fn"
    m = match(r"^(.*?)\.opus", fn)
    if m !== nothing
        outname = "$(m.captures[1]).ogg"
    else
        outname = fn
    end
    outfile = "for-phone/$outname"
    if !ispath(outfile)
        # Waiting on https://github.com/JuliaLang/julia/pull/41639
        run(`ln $infile $outfile`)
    end
end
