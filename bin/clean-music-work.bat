@echo off

setlocal

rem Clean up the music/work directory before storing it!

rem HINT: Fix your path (or set UNIX_FIND_EXE) to use find.exe that comes with Git!
rem Otherwise you might end up using an old veresion that doesn't support -delete

if not defined music_work_dir (echo %~nx0: Error: music_work_dir not defined >&2 & goto fail)
if not defined UNIX_FIND_EXE set UNIX_FIND_EXE=find

set ableton_backup_recycle_dir=%music_work_dir%\.relocated-ableton-backups

set self_dir=%~dp0
set dotfiled_dir=%self_dir%\..\dotfiles

for /f "eol=# tokens=*" %%i in (%dotfiled_dir%\music\work\.banned-files) do rm -f "%music_work_dir:\=/%/%%i"


rem Remove Ableton Backup directories
rem Windows md/mkdir requires backslashes...
if not exist "%ableton_backup_recycle_dir:/=\%" mkdir "%ableton_backup_recycle_dir:/=\%"
rem but find likes forward ones, lol
rem "%UNIX_FIND_EXE%" %music_work_dir% -path "*/Backup/*.als" -exec mv "{}" "%ableton_backup_recycle_dir:\=/%/" ";"
rem "%UNIX_FIND_EXE%" %music_work_dir% -name "Backup" -type d -delete
rem [2022-01-01] This should all be obsolete now that ccouch (supposedly) respects .ccouchignore files!
"%UNIX_FIND_EXE%" %music_work_dir% -name "Backup" -type d -exec attrib +h "{}" ";"
"%UNIX_FIND_EXE%" %music_work_dir% -name "Ableton Folder Info" -exec attrib +h "{}" ";"

rem TODO: Remove Reason Backup dirs, also?

rem Ableton likes to create lots of empty directories, but I don't care for them!
"%UNIX_FIND_EXE%" %music_work_dir%/2019/ableton-live-library -type d -empty -delete

rem TODO: Implement ccouch purge
rem ccouch purge urn:sha1:CDZEDEXE4QNNQHZNQZPNHQ4YAC3XIUX2  (aka TOGoS-RVT_2001-ab [2020-12-17 215725].wav)

:stdbatfooter
goto eof
:fail
echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
