@echo off
rem Clean up the music/work directory before storing it!

if not defined music_work_dir (echo %~nx0: Error: music_work_dir not defined >&2 & goto fail)

set ableton_backup_recycle_dir=%music_work_dir%\.relocated-ableton-backups

rem Remove Ableton Backup directories
rem Windows md/mkdir requires backslashes...
if not exist "%ableton_backup_recycle_dir:/=\%" mkdir "%ableton_backup_recycle_dir:/=\%"
rem but find likes forward ones, lol
find %music_work_dir% -path "*/Backup/*.als" -exec mv "{}" "%ableton_backup_recycle_dir:\=/%/" ";"
find %music_work_dir% -name "Backup" -type d -delete

rem TODO: Remove Reason Backup dirs, also?

rem Ableton likes to create lots of empty directories, but I don't care for them!
find %music_work_dir%/2019/ableton-live-library -type d -empty -delete


:stdbatfooter
goto eof
:fail
echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
