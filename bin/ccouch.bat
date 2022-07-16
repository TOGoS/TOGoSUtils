@echo off

setlocal

if defined CCOUCH_JAR goto ccouch_jar_found
if exist "%UserProfile%\stuff\proj\ContentCouch\ContentCouch.jar" (set "CCOUCH_JAR=%UserProfile%\stuff\proj\ContentCouch\ContentCouch.jar" & goto ccouch_jar_found)

echo Couldn't find ContentCouch.jar.  Try setting the CCOUCH_JAR environment variable. >&2
exit /b 1


:ccouch_jar_found

set "ccouch_args="

if not defined CCOUCH_REPO_DIR goto end_build_repo_arg
if not defined CCOUCH_REPO_NAME goto end_build_repo_arg
if not defined CCOUCH_REMOTE_ARGS set "CCOUCH_REMOTE_ARGS="

rem I replace backslash with slash here because OTHERWISE A TRAILING BACKSLASH
rem GETS INTERPRETED AS AN ESCAPE SEQUENCE AND ccouch WILL COMPLAIN THAT 'No command given'.
rem This can also be worked aound by appending a '.' (e.g. `set CCOUCH_REPO_DIR=%~dp0.` in .env.bat)
rem but I figure just switching them to forward slashes is a bit more foolproof.

set "ccouch_args=-repo:%CCOUCH_REPO_NAME% "%CCOUCH_REPO_DIR:\=/%" %CCOUCH_REMOTE_ARGS%"

:end_build_repo_arg

set "cmd=java -jar "%CCOUCH_JAR%" %ccouch_args% %*"

echo %cmd%
%cmd%
