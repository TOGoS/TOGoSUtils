@echo off
setlocal
set "self_dir=%~dp0"

cls
cat %UserProfile%\stuff\job\EarthIT\timelog\project-tasks.tef %UserProfile%\stuff\music\TOGoSMusicMetadata\tasks.tef | deno run "%self_dir%list-todo.ts" -p
