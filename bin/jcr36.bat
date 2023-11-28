@echo off

setlocal

call figure-jcr36-cmd.bat || exit /B 1
rem 2023-11-28: Note that "||" seems to work, but if that's an unreliable feature...
rem if errorlevel 1 exit /B 1

%JCR36_CMD% %*
