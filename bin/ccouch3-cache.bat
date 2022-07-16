@echo off

rem This exists because ccouch3 can't [as of 3.6.0] take remote args before
rem the command.  Maybe it should, but currently doesn't.
rem Jot that down for new versions.
rem Anyway, set CCOUCH3_REMOTE_ARGS in your environment to make it work good.

setlocal

if not defined CCOUCH3_REMOTE_ARGS set "CCOUCH3_REMOTE_ARGS="

ccouch3 cache %CCOUCH3_REMOTE_ARGS% %*
