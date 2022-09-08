@echo off
setlocal

cls
cat ^
		%UserProfile%\stuff\job\EarthIT\timelog\project-tasks.tef ^
		%UserProfile%\stuff\music\TOGoSMusicMetadata\tasks.tef ^
	| deno run "https://deno.land/x/listtodo27@v0.1.3/list-todo.ts" -p
