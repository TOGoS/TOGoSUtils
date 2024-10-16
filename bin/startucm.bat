@echo off

setlocal

if defined UCM_EXE goto ucm_exe_found

set "UCM_EXE=C:\apps\unison-0.5.27\ucm.cmd"
if exist "%UCM_EXE%" goto ucm_exe_found

set "UCM_EXE=C:\apps\unison\ucm.cmd"
if exist "%UCM_EXE%" goto ucm_exe_found

set "UCM_EXE=C:\apps\unison\ucm.exe"
if exist "%UCM_EXE%" goto ucm_exe_found

echo Couldn't find ucm.exe or ucm.cmd or whatever>&2
pause
exit /B 1


:ucm_exe_found

start alacritty --working-directory C:\Users\TOGoS\stuff\proj\Scratch38-S0010 --title UCM --command "%UCM_EXE%"
