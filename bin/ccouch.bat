@echo off

setlocal

if defined ccouch_jar goto ccouch_jar_found
if exist "%UserProfile%\stuff\proj\ContentCouch\ContentCouch.jar" (set "ccouch_jar=%UserProfile%\stuff\proj\ContentCouch\ContentCouch.jar" & goto ccouch_jar_found)

echo Couldn't find ContentCouch.jar.  Try setting the ccouch_jar environment variable. >&2
exit /b 1


:ccouch_jar_found

set "ccouch_args="

if not defined ccouch_repo_dir goto end_build_repo_arg
if not defined ccouch_repo_name goto end_build_repo_arg

rem I replace backslash with slash here because OTHERWISE A TRAILING BACKSLASH
rem GETS INTERPRETED AS AN ESCAPE SEQUENCE AND ccouch WILL COMPLAIN THAT 'No command given'.
rem This can also be worked aound by appending a '.' (e.g. `set ccouch_repo_dir=%~dp0.` in .env.bat)
rem but I figure just switching them to forward slashes is a bit more foolproof.

set "ccouch_args=-repo:%ccouch_repo_name% "%ccouch_repo_dir:\=/%""

:end_build_repo_arg

set "cmd=java -jar "%ccouch_jar%" %ccouch_args% %*"

rem echo %cmd%
%cmd%
