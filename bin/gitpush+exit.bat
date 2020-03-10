@echo off
title gitpush %CD%...
call %~dp0\gitpush.bat
if ERRORLEVEL 1 GOTO errored

exit

:errored
echo Ack, failure!
msg %USERNAME% "gitush %CD% failed!  See command window for deetz."
