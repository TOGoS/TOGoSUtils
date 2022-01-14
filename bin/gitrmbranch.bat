@echo off

setlocal
if not defined BASH_EXE set "BASH_EXE=bash"

call "%BASH_EXE%" %~dp0gitrmbranch %*
