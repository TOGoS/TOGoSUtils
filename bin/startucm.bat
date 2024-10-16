@echo off

setlocal

set "unison_exe=C:\apps\unison-0.5.27\ucm.cmd"
if exist "%unison_exe%" goto unison_exe_found

set "unison_exe=C:\apps\unison\ucm.exe"
if exist "%unison_exe%" goto unison_exe_found

echo Couldn't find ucm.exe or ucm.cmd or whatever>&2
pause
exit /B 1


:unison_exe_found

start alacritty --working-directory C:\Users\TOGoS\stuff\proj\Scratch38-S0010 --title UCM --command "%unison_exe%"
