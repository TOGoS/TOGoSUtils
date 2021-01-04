@echo off

setlocal

set script_version=2020-01-04
set script_name_and_version=%~nx0 v%script_version%

call require-ccouch-env.bat
if errorlevel 1 goto fail

if "%~1" EQU "-m" set commit_message=%~2
if not defined commit_message set commit_message=Music work on %ccouch_repo_name%
rem TODO: tog_stuff_dir isn't really required if music_work_dir is set...
if not defined tog_stuff_dir (echo tog_stuff_dir not specified >&2 & goto fail)
if not defined music_work_dir set music_work_dir=%tog_stuff_dir%\music\work
if not defined ccouch_store_sector set ccouch_store_sector=music

call clean-music-work
if errorlevel 1 goto fail

@echo on
java -jar %ccouch_jar% -repo:%ccouch_repo_name% %ccouch_repo_dir% store ^
	-link ^
	-sector "%ccouch_store_sector%" ^
	-n tog/music/work ^
	-a "%script_name_and_version%" ^
	-m "%commit_message%" ^
	%music_work_dir%
@echo off

set lhn_tempfile=%music_work_dir%\.last-head-number
find %ccouch_repo_dir%\heads\%ccouch_repo_name%\tog\music\work | sort -V | tail -n 1 >%lhn_tempfile%
set /p latest_head_file= <%lhn_tempfile%
del %lhn_tempfile%

@echo on
rem pscp -P 31522 %latest_head_file% tog@external.marvin.nuke24.net:/home/tog/.ccouch/heads/eng-lap-426-2019/tog/music/work/
pscp %latest_head_file% tog@fs.marvin.nuke24.net:/home/tog/.ccouch/heads/%ccouch_repo_name%/tog/music/work/
pscp -P 31522 %latest_head_file% tog@external.marvin.nuke24.net:/home/tog/.ccouch/heads/%ccouch_repo_name%/tog/music/work/
@echo off

grep -o "urn:.*" %music_work_dir%\.commit-uris > %music_work_dir%\.commit-blob-uris
set /p music_commit_blob_urn= <%music_work_dir%\.commit-blob-uris
del %music_work_dir%\.commit-blob-uris

echo Commit blob URN: %music_commit_blob_urn%

@echo on
java -jar %ccouch3_jar% upload -repo:%ccouch_repo_name% %ccouch_repo_dir% ^
	-command-server:fs.marvin plink tog@fs.marvin.nuke24.net ccouch3 command-server -sector "%ccouch_store_sector%" ";" ^
	-command-server:togos-fbs plink tog@external.marvin.nuke24.net -P 31522 ccouch3 command-server -sector "%ccouch_store_sector%" ";" ^
	-recurse -v ^
	%music_commit_blob_urn%
@echo off

:stdbatfooter
goto eof
:fail
echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
