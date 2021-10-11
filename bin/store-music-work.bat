@echo off

setlocal

set self_name=%~nx0
set script_version=2021-10-11
set script_name_and_version=%~nx0 v%script_version%

call require-ccouch-env.bat
if errorlevel 1 goto fail

if "%~1" EQU "-m" set commit_message=%~2
if not defined commit_message set commit_message=Music work on %ccouch_repo_name%
rem TODO: tog_stuff_dir isn't really required if music_work_dir is set...
rem if not defined tog_stuff_dir (echo tog_stuff_dir not specified >&2 & goto fail)
if defined music_work_dir goto find_music_work_dir_done
if defined tog_stuff_dir (set music_work_dir=%tog_stuff_dir%\music\work & goto find_music_work_dir_done)
echo %self_name%: Error: Neither music_work_dir nor tog_stuff_dir is set >&2 & goto fail)
:find_music_work_dir_done

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
pscp %latest_head_file% tog@fs.marvin.nuke24.net:/home/tog/.ccouch/heads/%ccouch_repo_name%/tog/music/work/
pscp -P 31522 %latest_head_file% tog@external.marvin.nuke24.net:/home/tog/.ccouch/heads/%ccouch_repo_name%/tog/music/work/
@echo off

call ccouch3-upload-to-marvin -recurse x-ccouch-head:%ccouch_repo_name%/tog/music/work/latest

:stdbatfooter
goto eof
:fail
echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
