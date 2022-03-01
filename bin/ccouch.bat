@echo off

setlocal

if defined ccouch_jar goto ccouch_jar_found
if exist %UserProfile%\stuff\proj\ContentCouch\Contentcouch.jar (set "ccouch_jar=%UserProfile%\stuff\proj\ContentCouch\Contentcouch.jar" & goto ccouch_jar_found)

echo Couldn't find ContentCouch.jar.  Try setting the ccouch_jar environment variable. >&2
exit /b 1


:ccouch_jar_found

set "ccouch_args="

if not defined ccouch_repo_dir goto end_build_repo_arg
if not defined ccouch_repo_name goto end_build_repo_arg

set "ccouch_args=-repo:%ccouch_repo_name% "%ccouch_repo_dir%""

:end_build_repo_arg

rem echo args: %ccouch_args%

java -jar "%ccouch_jar%" %*
