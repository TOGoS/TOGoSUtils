open("video-urls.lst") do list_stream
    while !eof(list_stream)
        line = readline(list_stream)

        if match(r"^\s*(?:#|$)", line) != nothing
            continue
        end

        url = line
        # print(line * "\n")
        try
            print("# Attempting to download $(url)...\n")
            cd(() -> run(`youtube-dl --extract-audio $(url)`), "downloaded")
        catch err
            print("# Failed to download $(url)\n")
        end
    end
end
