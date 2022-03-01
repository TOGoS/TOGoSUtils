@echo off
setlocal

set self_name=gitpush-eventually
set interval=60

:again
call gitpush
if errorlevel 1 goto failed
goto eof
:failed
echo %self_name%: gitpush failed with errorlevel=%ERRORLEVEL%.
echo %self_name%: Will try again in %interval% seconds.
sleep %interval%
echo %self_name%: Trying again...
goto again

:eof
