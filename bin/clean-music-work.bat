@echo off
rem Clean up the music/work directory before storing it!

rem HINT: Fix your path (or set unix_find_exe) to use find.exe that comes with Git!
rem Otherwise you might end up using an old veresion that doesn't support -delete

if not defined music_work_dir (echo %~nx0: Error: music_work_dir not defined >&2 & goto fail)
if not defined unix_find_exe set unix_find_exe=find

set ableton_backup_recycle_dir=%music_work_dir%\.relocated-ableton-backups

rem Remove Ableton Backup directories
rem Windows md/mkdir requires backslashes...
if not exist "%ableton_backup_recycle_dir:/=\%" mkdir "%ableton_backup_recycle_dir:/=\%"
rem but find likes forward ones, lol
%unix_find_exe% %music_work_dir% -path "*/Backup/*.als" -exec mv "{}" "%ableton_backup_recycle_dir:\=/%/" ";"
%unix_find_exe% %music_work_dir% -name "Backup" -type d -delete

rem TODO: Remove Reason Backup dirs, also?

rem Ableton likes to create lots of empty directories, but I don't care for them!
%unix_find_exe% %music_work_dir%/2019/ableton-live-library -type d -empty -delete


:stdbatfooter
goto eof
:fail
echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
