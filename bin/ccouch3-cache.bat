@echo off

rem This exists because ccouch3 can't [as of 3.6.0] take remote args before
rem the command.  Maybe it should, but currently doesn't.
rem Jot that down for new versions.
rem Anyway, set CCOUCH3_REMOTE_ARGS in your environment to make it work good.

setlocal
call require-ccouch-env.bat
if errorlevel 1 goto fail

if not defined CCOUCH3_REMOTE_ARGS set "CCOUCH3_REMOTE_ARGS="

java -jar %CCOUCH3_JAR% cache %CCOUCH3_REMOTE_ARGS% %*
if errorlevel 1 goto fail
goto eof

:fail
echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
