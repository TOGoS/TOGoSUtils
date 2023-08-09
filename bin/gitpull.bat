@echo off

setlocal

rem If you install WSL, you'll get C:\Windows\system32\bash.exe,
rem which doesn't play so well with Windows paths.
rem Could try bashifying the path?  Or using a specially-configured bash,
rem defaulting to the Git one:

if defined TU_BASH_EXE goto bash_found
if exist "C:\Program Files\Git\usr\bin\bash.exe" set "TU_BASH_EXE=C:\Program Files\Git\usr\bin\bash.exe"
if defined TU_BASH_EXE goto bash_found
set TU_BASH_EXE=bash
:bash_found

"%TU_BASH_EXE%" %~dp0\gitpull %*
