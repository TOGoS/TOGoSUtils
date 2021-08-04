@echo off

if "%~1" == "--help" goto show_help
if "%~1" == "--prepend" (shift & goto prepend)
if "%~1" == "--list" goto list

if "%~1" == "--format" goto format
if "%~1" == "--format-set" goto format
if "%~1" == "--set" goto format
if "%~1" == "" goto list

echo %~nx0: Invalid argument: %~1 >&2
exit /b 1


goto list

:show_help

echo winpath.bat, a script to help manage the Path variable.
echo:
echo Output modes:
echo   winpath [--list]      ; list path, one directory per line
echo   winpath --help        ; show this text
echo   winpath --prepend ... ; list path with the specified items prepended
echo Input modes (read paths one-per-line from stdin):
echo   winpath --format      ; format paths as Path value
echo   winpath --set         ; set new path value
echo:
echo To parse the path and then set it, two invocations are required.
echo One 'output' run, and one 'input' run.
echo:
echo   e.g. "winpath --prepend C:\blah\bin | uniq | winpath --format-set >.setpath.bat"
echo:
echo Note that --set uses a temp file, and doesn't work if used in a pipeline.
echo Better to use --format-set, pipe that output to a temporary batch script,
echo and then run that batch script.

goto eof


:prepend
if "%~1" == "" goto list
echo %~1
shift
goto prepend



:list

rem List directories from the path, one per line

setlocal
set pathrest=%Path%

:nextvar
for /F "tokens=1* delims=;" %%a in ("%pathrest%") do (
    echo %%a
    set pathrest=%%b
)
if defined pathrest goto nextvar

endlocal
goto eof


:format

rem EnableDelayedExpansion to make !new_path_value! be a thing inside a loop
setlocal EnableDelayedExpansion
set "new_path_value="

rem type CON > .path.txt

rem Q: How to read lines from standard input?
rem A: Use 'more' -- https://stackoverflow.com/questions/6979747/read-stdin-stream-in-a-batch-file

for /f "tokens=*" %%A in ('more') do (
    if "!new_path_value!" == "" (set "new_path_value=%%A") else (set "new_path_value=!new_path_value!;%%A")
)

if "%~1" == "--format" echo %new_path_value%
if "%~1" == "--format-set" echo set "Path=%new_path_value%"
if "%~1" == "--set" (echo @set "Path=%new_path_value%" >%Temp%\.setpath.bat)

endlocal

if "%~1" == "--set" goto finish_set

goto eof


:finish_set
rem echo Created %Temp%\.setpath.bat, now calling it.
%Temp%\.setpath.bat



:eof
