@echo off

call require-ccouch-env.bat
if not defined datastore_root (echo datastore_root not specified >&2 & goto fail)

set image_archives_dir=%datastore_root%\archives\images
set ccouch_store_sector=pictures

call ccouch cache-heads //fs.marvin/togthoms1/
call ccouch3 cache ^
	-repo:%CCOUCH_REPO_NAME% %CCOUCH_REPO_DIR% ^
	-remote-repo fs.marvin.nuke24.net ^
	-recurse -sector pictures ^
	-remember-missing ^
	x-ccouch-head:togthoms1/archives/images/latest ^
	x-ccouch-head:togthoms1/archives/images/TOGoS/latest
call ccouch checkout -link -merge ^
	x-rdf-subject:x-ccouch-head:togthoms1/archives/images/latest ^
	%image_archives_dir%/
rem call ccouch checkout -link -merge ^
rem 	x-rdf-subject:x-ccouch-head:togthoms1/archives/images/TOGoS/latest ^
rem 	%image_archives_dir%/TOGoS/

:stdbatfooter
goto eof
:fail
@echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
