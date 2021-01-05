@echo off

call require-ccouch-env.bat
if errorlevel 1 goto fail

if "%~1" EQU "-sector" set ccouch_store_sector=%~2
if not defined ccouch_store_sector (echo ccouch_store_sector not specified & goto fail)

@echo on
java -jar %ccouch3_jar% upload -repo:%ccouch_repo_name% %ccouch_repo_dir% ^
	-command-server:fs.marvin plink tog@fs.marvin.nuke24.net ccouch3 command-server -sector %ccouch_store_sector% ";" ^
	-command-server:togos-fbs plink -P 31522 tog@external.marvin.nuke24.net ccouch3 command-server -sector %ccouch_store_sector% ";" ^
	%*
@echo off
goto eof

:fail
echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
