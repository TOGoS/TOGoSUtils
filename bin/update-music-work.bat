@echo off

call require-ccouch-env.bat

if defined music_work_dir goto find_music_work_dir_done
if defined tog_stuff_dir (set music_work_dir=%tog_stuff_dir%\music\work & goto find_music_work_dir_done)
echo %self_name%: Error: Neither music_work_dir nor tog_stuff_dir is set >&2 & goto fail)
:find_music_work_dir_done

rem The new and improved way to update heads:
git --git-dir=%CCOUCH_REPO_DIR%/.git --work-tree=%CCOUCH_REPO_DIR% pull fs.marvin master

goto cache_heads_done
call ccouch cache-heads ^
	//fs.marvin/togthoms1/tog/music/work/ //togos-fbs/togthoms1/tog/music/work/ ^
	//fs.marvin/wsitem-3306.1/tog/music/work/ //togos-fbs/wsitem-3306.1/tog/music/work/ ^
	//fs.marvin/framey-2021/tog/music/work/ //togos-fbs/framey-2021/tog/music/work/
:cache_heads_done

if errorlevel 1 goto fail
call ccouch3-cache -recurse -sector music x-ccouch-head:togthoms1/tog/music/work/latest x-ccouch-head:wsitem-3306.1/tog/music/work/latest x-ccouch-head:framey-2021/tog/music/work/latest
if errorlevel 1 goto fail
call ccouch checkout -link -merge x-rdf-subject:x-ccouch-head:togthoms1/tog/music/work/latest %music_work_dir%/
call ccouch checkout -link -merge x-rdf-subject:x-ccouch-head:wsitem-3306.1/tog/music/work/latest %music_work_dir%/
call ccouch checkout -link -merge x-rdf-subject:x-ccouch-head:framey-2021/tog/music/work/latest %music_work_dir%/
if errorlevel 1 goto fail

:stdbatfooter
goto eof
:fail
@echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
