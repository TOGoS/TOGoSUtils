@echo off

setlocal

if defined ccouch3_jar goto ccouch3_jar_found
if exist "%UserProfile%\stuff\proj\ContentCouch3\CCouch3.jar" (set "ccouch3_jar=%UserProfile%\stuff\proj\ContentCouch\CCouch3.jar" & goto ccouch3_jar_found)

echo Couldn't find CCouch3.jar.  Try setting the ccouch3_jar environment variable. >&2
exit /b 1

:ccouch3_jar_found

java -jar "%ccouch3_jar%" %*
