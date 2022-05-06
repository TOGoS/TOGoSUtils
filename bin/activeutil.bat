@echo off

setlocal

set "self_name=%~df0"

if not defined ACTIVEUTIL_BAT set "ACTIVEUTIL_BAT=%USERPROFILE%\stuff\proj\SynthGen2100\p0019\scripts\activeutil.bat"

if not exist %ACTIVEUTIL_BAT% goto dne

rem Since calling another bat without 'call', this should replace
rem the current script, I think?
%ACTIVEUTIL_BAT% %*

echo Oh weird, %ACTIVEUTIL_BAT% returned back to %self_name% >&2
echo Exiting with error status because that's weird. >&2
exit /B 1

goto eof
:dne
echo %ACTIVEUTIL_BAT% does not exist. >&2
exit /B 1

:eof
