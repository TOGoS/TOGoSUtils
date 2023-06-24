@echo off

rem This is intended to be called from other .bat scripts
rem to ensure that necessary variables hjave been set.
rem Usage:
rem   call require-ccouch-env.bat
rem   if errorlevel 1 (echo errorlevel 1: %ERRORLEVEL% & goto fail)
rem   ...and include a :fail section, similar to this one

: reset errorlevel
type nul>nul

if defined CCOUCH_JAR goto find_ccouch_jar_done
if exist %USERPROFILE%\stuff\proj\Contentcouch\ContentCouch.jar (set "CCOUCH_JAR=%USERPROFILE%\stuff\proj\Contentcouch\ContentCouch.jar" & goto find_ccouch_jar_done)
if exist %USERPROFILE%\Apps\ContentCouch.jar (set "CCOUCH_JAR=%USERPROFILE%\Apps\ContentCouch.jar" & goto find_ccouch_jar_done)
echo "Couldn't find ContentCouch.jar!  Maybe set CCOUCH_JAR and try again" >&2
goto fail
:find_ccouch_jar_done

if defined CCOUCH3_JAR goto find_ccouch3_jar_done
if exist %USERPROFILE%\stuff\proj\ContentCouch3\CCouch3.jar (set "CCOUCH3_JAR=%USERPROFILE%\stuff\proj\ContentCouch3\CCouch3.jar" & goto find_ccouch3_jar_done)
echo "Couldn't find ContentCouch3.jar!  Maybe set CCOUCH3_JAR and try again" >&2
goto fail
:find_ccouch3_jar_done

if not defined CCOUCH_REPO_DIR (echo CCOUCH_REPO_DIR not specified >&2 & goto fail)
if not defined CCOUCH_REPO_NAME (echo CCOUCH_REPO_NAME not specified >&2 & goto fail)

if not defined CCOUCH3_REMOTE_ARGS set "CCOUCH3_REMOTE_ARGS="
if not defined CCOUCH3_TOGNET_REMOTE_ARGS set "TOGNET_CCOUCH_REMOTE_ARGS=-remote-repo:togos-fbs external.marvin.nuke24.net:31580 -remote-repo:fs.marvin fs.marvin.nuke24.net"

set "ccouch3_collected_remote_args=%CCOUCH3_REMOTE_ARGS% %TOGNET_CCOUCH_REMOTE_ARGS%"

:stdbatfooter
goto eof
:fail
echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
