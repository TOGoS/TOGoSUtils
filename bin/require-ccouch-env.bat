@echo off

rem This is intended to be called from other .bat scripts
rem to ensure that necessary variables hjave been set.
rem Usage:
rem   call require-ccouch-env.bat
rem   if errorlevel 1 (echo errorlevel 1: %ERRORLEVEL% & goto fail)
rem   ...and include a :fail section, similar to this one

: reset errorlevel
type nul>nul

if not defined ccouch_jar set ccouch_jar=%USERPROFILE%\Apps\ContentCouch.jar
if not defined ccouch3_jar set ccouch3_jar=%USERPROFILE%\stuff\proj\ContentCouch3\CCouch3.jar

if not defined ccouch_repo_dir (echo ccouch_repo_dir not specified >&2 & goto fail)
if not defined ccouch_repo_name (echo ccouch_repo_name not specified >&2 & goto fail)

:stdbatfooter
goto eof
:fail
echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
