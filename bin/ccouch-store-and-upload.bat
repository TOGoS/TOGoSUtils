@echo off

setlocal
call require-ccouch-env.bat
if errorlevel 1 goto fail

set "link_args="

:parse_args
if "%~1" EQU "-sector" goto parse_sector
if "%~1" EQU "-link" goto parse_link

if not defined ccouch_store_sector (echo ccouch_store_sector not specified & goto fail)
goto main


:parse_sector
shift
set "ccouch_store_sector=%~1"
shift
goto parse_args

:parse_link
shift
set link_args="-link"
goto parse_args


:main
call ccouch store -sector %ccouch_store_sector% %link_args% %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto fail
call ccouch3-upload-to-marvin -sector %ccouch_store_sector% %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto fail
goto eof

:fail
echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
