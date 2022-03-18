@echo off

setlocal

set "ccouch3_jar_guess1=%UserProfile%\stuff\proj\ContentCouch3\CCouch3.jar"

if defined ccouch3_jar goto ccouch3_jar_found
if exist "%ccouch3_jar_guess1%" (set "ccouch3_jar=%ccouch3_jar_guess1%" & goto ccouch3_jar_found)

echo Couldn't find CCouch3.jar.  Try setting the ccouch3_jar environment variable. >&2
exit /b 1

:ccouch3_jar_found

java -jar "%ccouch3_jar%" %*
