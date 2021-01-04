@echo off

echo:Hello!  %~nx0 demonstrates some tricks for use in .bat files.
echo:

setlocal
rem % ~ N removes the quotes from argument N
if "%~1" EQU "-m" set message=%~2
if not defined message set message=A default message!

echo:Message: %message%
echo:You can override the message by passing -m (message) to this script.
echo:

call require-ccouch-env.bat
if errorlevel 1 (echo Your ccouch environment variables are *NOT* set! & goto endcheck)
if errorlevel 0 (echo Your ccouch environment variables are set!  Congrats! & goto endcheck)
:endcheck


:stdbatfooter
goto eof
:fail
@echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
endlocal
