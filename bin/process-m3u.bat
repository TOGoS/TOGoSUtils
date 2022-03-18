@echo off

setlocal

if defined process_m3u_jar goto process_m3u_jar_found

set "process_m3u_jar_guess1=%UserProfile%\stuff\proj\M3UProcessor\M3UProcessor.jar"
if exist "%process_m3u_jar_guess1%" (set "process_m3u_jar=%process_m3u_jar_guess1%" & goto process_m3u_jar_found)

echo Couldn't find M3UProcessor.jar.  Try setting the process_m3u_jar environment variable. >&2
exit /b 1


:process_m3u_jar_found
java -jar "%process_m3u_jar%" %*

