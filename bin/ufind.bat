@echo off

rem The unix 'find', which is usually included with Git,
rem but given a separate name so as not to conflict with Windows\System32\find,
rem whatever that does.

setlocal

set self_name=%~nx0

if defined UNIX_FIND_EXE goto find_found

if exist "C:\Program Files\Git\usr\bin\find.exe" (set "UNIX_FIND_EXE=C:\Program Files\Git\usr\bin\find.exe" & goto find_found)

echo %self_name%: Coiuldn't find find.exe.  Maybe set UNIX_FIND_EXE. >&2
exit /B 1

:find_found

"%UNIX_FIND_EXE%" %*
