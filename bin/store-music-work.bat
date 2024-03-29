@echo off

setlocal

set self_name=%~nx0
rem Version history:
rem 2021-10-30: Updated clean-music-worl.bat to quote UNIX_FIND_EXE
set script_version=2021-10-30
set script_name_and_version=%~nx0 v%script_version%

call require-ccouch-env.bat
if errorlevel 1 goto fail

if "%~1" EQU "-m" set commit_message=%~2
if not defined commit_message set commit_message=Music work on %CCOUCH_REPO_NAME%

if defined TOG_MUSIC_WORK_DIR goto find_music_work_dir_done
if defined TOG_STUFF_DIR (set "TOG_MUSIC_WORK_DIR=%TOG_STUFF_DIR%\music\work" & goto find_music_work_dir_done)
echo %self_name%: Error: Neither TOG_MUSIC_WORK_DIR nor TOG_STUFF_DIR is set >&2 & goto fail)
:find_music_work_dir_done

if not defined ccouch_store_sector set ccouch_store_sector=music
if defined UNIX_FIND_EXE goto find_found
find --version
if errorlevel 1 (echo 'find' appears to be the wrong one.  please set UNIX_FIND_EXE.>&2 && goto fail)
set UNIX_FIND_EXE=find
:find_found
if defined UNIX_SORT_EXE goto sort_found
sort --version
if errorlevel 1 (echo 'sort' appears to be the wrong one.  please set UNIX_SORT_EXE.>&2 && goto fail)
set UNIX_SORT_EXE=sort
:sort_found

call clean-music-work
if errorlevel 1 goto fail

@echo on
java -jar %CCOUCH_JAR% -repo:%CCOUCH_REPO_NAME% %CCOUCH_REPO_DIR% store ^
	-link ^
	-sector "%ccouch_store_sector%" ^
	-n tog/music/work ^
	-a "%script_name_and_version%" ^
	-m "%commit_message%" ^
	-use-uri-dot-files -create-uri-dot-files ^
	"%TOG_MUSIC_WORK_DIR%"
@echo off

rem there has GOT to be a better way to do this stuff!
rem (there is; it's rsync; I seem to recall having trouble finding an rsync for windows)
rem does command-server not write heads?
rem Yes, it does.  I could script something to push those using the existing command-server protocol.
rem 
rem I could also add a command to ccouch3 to spit out .bat files or something

set "lhn_tempfile=%TOG_MUSIC_WORK_DIR%\.last-head-number"
"%UNIX_FIND_EXE%" "%CCOUCH_REPO_DIR%\heads\%CCOUCH_REPO_NAME%\tog\music\work" | "%UNIX_SORT_EXE%" -V | tail -n 1 >%lhn_tempfile%
set /p latest_head_file= <%lhn_tempfile%
del %lhn_tempfile%

echo Skipping pscping of heads because I keep them in Git, now
goto ccouch3_upload

if not defined latest_head_file goto latest_head_not_found
@echo on
if defined FS_MARVIN_SSH_PORT pscp -P %FS_MARVIN_SSH_PORT% "%latest_head_file%" tog@%FS_MARVIN_SSH_HOSTNAME%:/home/tog/.ccouch/heads/%CCOUCH_REPO_NAME%/tog/music/work/
if defined TOGOS_FBS_SSH_PORT pscp -P %TOGOS_FBS_SSH_PORT% "%latest_head_file%" tog@%TOGOS_FBS_SSH_HOSTNAME%:/home/tog/.ccouch/heads/%CCOUCH_REPO_NAME%/tog/music/work/
@echo off
goto ccouch3_upload

:latest_head_not_found
echo %self_name%: Failed to find latest head using command: >&2
echo %self_name%:   "%UNIX_FIND_EXE%" "%CCOUCH_REPO_DIR%\heads\%CCOUCH_REPO_NAME%\tog\music\work" >&2
echo %self_name%: which is then piped to: >&2
echo %self_name%:   "%UNIX_SORT_EXE%" -V >&2
echo %self_name%: Maybe check %lhn_tempfile% >&2

:ccouch3_upload

call ccouch3-upload-to-marvin -recurse "x-ccouch-head:%CCOUCH_REPO_NAME%/tog/music/work/latest"

:stdbatfooter
goto eof



:fail
echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
