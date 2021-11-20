import { walk } from "https://deno.land/std@0.115.1/fs/mod.ts";
import { basename } from "https://deno.land/std@0.115.1/path/mod.ts";

let roots:string[] = [];
const selfName = basename(import.meta.url);
let runMode:"do"|"dump"|"cmd"|"sh" = "dump";

for( const arg of Deno.args ) {
	if( arg == "--do" ) {
		runMode = "do";
	} else if( arg == "--dry-run" ) {
		runMode = "sh";
	} else if( arg.startsWith("-") ) {
		console.error(`${selfName}: Unrecognized argument: ${arg}`);
		Deno.exit(1);
	} else {
		roots.push(arg);
	}
}

interface RemoveFile {
	classRef: "https://ns.nuke24.net/SynthGen2100/FSAction/RemoveFileOrDirectory",
	targetPath: string,
}

type FSAction = RemoveFile;

const actions : FSAction[] = [];

if( roots.length == 0 ) {
	console.error(`${selfName}: warning: No roots specified`);
}

for( const root of roots ) {
	for await( const we of walk(root) ) {
		if( we.isFile && we.name.endsWith("~") ) {
			actions.push({
				classRef: "https://ns.nuke24.net/SynthGen2100/FSAction/RemoveFileOrDirectory",
				targetPath: we.path
			});
		}
	}
}

function doAction(act:RemoveFile) {
	Deno.remove(act.targetPath);
}

switch( runMode ) {
case "dump":
	console.dir(actions);
	break;
case "do":
	for( const act of actions ) {
		doAction(act);
	}
	break;
case "sh":
	if( actions.length > 0 ) console.log("rm "+actions.map(a => JSON.stringify(a.targetPath.replaceAll('\\','/'))).join(" "));
	break;
default:
	console.error(`${selfName}: Oops, run mode ${runMode} not yet supported`);
	Deno.exit(1);
}
