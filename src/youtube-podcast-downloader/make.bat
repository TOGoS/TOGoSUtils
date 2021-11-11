@echo off

setlocal

set "self_name=%~nx0"

julia -e "print(\"hello\")"
if errorlevel 1 goto nojulia

julia download.jl
julia rename-for-phone.jl
goto eof

:nojulia
echo %self_name%: Error: Julia not installed? 2>&1
echo %self_name%: Maybe `choco install julia` to fix that. 2>&1
exit /B 1


:eof
echo %self_name%: Done!  Enjoy your audio!
