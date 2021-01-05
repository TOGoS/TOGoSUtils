@echo off

call require-ccouch-env.bat

if not defined tog_stuff_dir (echo tog_stuff_dir not defined >&2 & goto fail)

call ccouch cache-heads //fs.marvin/togthoms1/ //togos-fbs/togthoms1/
if errorlevel 1 goto fail
call ccouch3-cache -recurse -sector music x-ccouch-head:togthoms1/tog/music/work/latest
if errorlevel 1 goto fail
call ccouch checkout -link -merge x-rdf-subject:x-ccouch-head:togthoms1/tog/music/work/latest %tog_stuff_dir%/music/work/
if errorlevel 1 goto fail

:stdbatfooter
goto eof
:fail
@echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
