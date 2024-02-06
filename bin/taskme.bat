@echo off
setlocal

set "self_name=%~nx0"
set "input_set=all"
set "mode=pick"

:process_arg
if "" == "%1" goto no_more_args
if "--dry-run" == "%~1" (set "mode=dry-run" & goto process_next_arg)
if "--help" == "%~1" (set "mode=help" & goto process_next_arg)
if "--music" == "%~1" (set "input_set=music" & goto process_next_arg)
set "remaining_args=%remaining_args% "%~1""
:process_next_arg
shift
goto process_arg
:no_more_args

set "tt_file=%UserProfile%\stuff\job\EarthIT\timelog\project-tasks.tef"
set "pt_file=%UserProfile%\stuff\docs\ProjectNotes2\2022\Projects.tef"
set "mt_file=%UserProfile%\stuff\music\TOGoSMusicMetadata\tasks.tef"

if "%input_set%" == "all" set "input_files="%tt_file%" "%pt_file%" "%mt_file%""
if "%input_set%" == "music" set "input_files="%mt_file%""

set "pick_cmd=type %input_files% | deno run "https://deno.land/x/listtodo27@v0.1.3/list-todo.ts" -p %remaining_args%"

if "%mode%" == "pick" goto pick
if "%mode%" == "help" goto print_help
if "%mode%" == "dry-run" goto dry_run

:pick
cls
type %input_files% | deno run "https://deno.land/x/listtodo27@v0.1.3/list-todo.ts" -p %remaining_args%
goto eof


:print_help

echo.
echo %self_name%: Pick a task to work on and pretty-print it.
echo.
echo Options:
echo   --dry-run  ; print the command that would be run
echo   --help     ; print this help text and exit
echo   --music    ; pick only from music-related tasks
echo.
echo Any other arguments will be forwarded to list-todo.ts
echo.
echo Currently selected input files:
echo.%input_files%
echo.
echo The command that would be run is:
:dry_run
echo %pick_cmd:|=^|%

echo.
:eof
