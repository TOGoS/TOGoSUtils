#!/usr/bin/env deno

// Simplified replacement for copy-incoming-pix and import-sd-pix.
// Won't look at timestamps!
// Won't do timezone correction!
// At least not for starters, hehe.

import { walk, WalkEntry, WalkOptions } from "https://deno.land/std@0.118.0/fs/walk.ts";

type FilePath = string;

interface CmdOpts {
	imageDests : FilePath[];
	videoDests : FilePath[];
	inputPaths : FilePath[];
}

function parseCmdOpts(args:string[]) : CmdOpts {
	const cmdOpts : CmdOpts = {
		imageDests: [],
		videoDests: [],
		inputPaths: [],
	};
	for( const arg of args ) {
		let m:RegExpExecArray|null;
		if( (m = /^--image-dest=(.*)/.exec(arg)) != null ) {
			cmdOpts.imageDests.push(m[1]);
		} else if( (m = /^--video-dest=(.*)/.exec(arg)) != null ) {
			cmdOpts.videoDests.push(m[1]);
		} else if( arg.startsWith("-" ) ) {
			throw new Error("Unrecognized argument: "+arg);
		} else {
			cmdOpts.inputPaths.push(arg);
		}
	}
	return cmdOpts;
}

async function *walkAll( inputs:FilePath[], opts:WalkOptions ) : AsyncIterable<WalkEntry> {
	for( const input of inputs ) {
		for await( const entry of walk(input) ) {
			yield entry;
		}
	}
}

function findInputFiles(cmdOpts:CmdOpts) : AsyncIterable<WalkEntry> {
	return walkAll(cmdOpts.inputPaths, {includeFiles: true, includeDirs: false, followSymlinks: true});
}

interface LinkFile {
	classRef: "http://ns.nuke24.net/SynthGen2100/FSAction/LinkFile";
	sourcePath: FilePath;
	targetPath: FilePath;
}
interface ReportUnhandleableFile {
	classRef: "http://ns.nuke24.net/TOGoSUtils/OrganizeIncomingDCIM/ReportUnhandleableFile";
	path: FilePath;
	reason: string;
}
type Action = LinkFile|ReportUnhandleableFile;

async function *decideWhatToDoWithInputFiles(inputFiles: AsyncIterable<WalkEntry>, cmdOpts:CmdOpts) : AsyncIterable<Action> {
	let m:RegExpExecArray|null;
	for await( const entry of inputFiles ) {
		if( (m = /^(\d\d\d\d)(\d\d)(\d\d)_(\d\d)(\d\d)(\d\d)\.([^\.]+)$/.exec(entry.name)) ) {
			const year = m[1];
			const month = m[2];
			const dayOfMonth = m[3];
			const filenameExt = m[7];
			let type =
				/^(?:gif|jpe?g|png)$/i.exec(filenameExt) ? 'image' :
				/^(?:mov|mpeg|mp4|avi|mkv)$/i.exec(filenameExt) ? 'video' :
				'unknown';
			const destPathMainSection = `${year}/${month}/${year}_${month}_${dayOfMonth}/${entry.name}`;
			const destList =
				type == 'image' ? cmdOpts.imageDests :
				type == 'video' ? cmdOpts.videoDests :
				[];

			for( const destPath of cmdOpts.imageDests ) {
				yield {
					classRef: "http://ns.nuke24.net/SynthGen2100/FSAction/LinkFile",
					sourcePath: entry.path,
					targetPath: destPath + "/" + destPathMainSection,
				}
			}
		}
	}
}

const cmdOpts = parseCmdOpts(Deno.args);
const inputFiles = findInputFiles(cmdOpts);

if( cmdOpts.inputPaths.length == 0 ) console.warn("No input paths specified");
if( cmdOpts.imageDests.length == 0 ) console.warn("No image destination paths specified");
if( cmdOpts.videoDests.length == 0 ) console.warn("No video destination paths specified");

console.log("@echo off");

function dirname(path:FilePath) {
	let m;
	if( m = /^(.*?)[\\\/][^\\\/]+$/.exec(path) ) {
		return m[1];
	} else {
		return undefined;
	}
}

const mkdirsAlreadyOutput = new Set<FilePath>();
function mkdir(path:FilePath) {
	if( mkdirsAlreadyOutput.has(path) ) return;
	console.log(`IF NOT EXIST "${path}" MKDIR "${path}"`);
	mkdirsAlreadyOutput.add(path);
}

for await( const action of decideWhatToDoWithInputFiles(inputFiles, cmdOpts) ) {
	switch( action.classRef ) {
	case "http://ns.nuke24.net/TOGoSUtils/OrganizeIncomingDCIM/ReportUnhandleableFile":
		console.log(`rem Ignoring ${action.path}: ${action.reason}`);
		break;
	case "http://ns.nuke24.net/SynthGen2100/FSAction/LinkFile":
		{
			const targetDirPath = dirname(action.targetPath);
			if( targetDirPath != undefined ) {
				mkdir(targetDirPath);
			}
			console.log(`IF NOT EXIST "${action.targetPath}" FSUTIL HARDLINK CREATE "${action.targetPath}" "${action.sourcePath}"`);
			break;
		}
	}
}

console.log("rem Ha ha, that's right; for now this program just outputs Windows CMD commands");
console.log("rem I have wondrous technology in some synthgen library about representing FSActions");
console.log("rem and being able to output them as Bash or CMD scripts or just do them.");
console.log("rem Publish that on Deno.land and useit.");
