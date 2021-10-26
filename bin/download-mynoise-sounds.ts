#!/usr/bin/env deno

function download( source:string, dest:string ) : Promise<unknown> {
	// Might want to use Deno primitives someday.
	// For now just call curl becauze lazyue
	const dlProc = Deno.run({
		cmd: ['curl', source, '-o', dest]
	});
	return dlProc.status();
}

async function downloadSounds( generatorName:string, dest:string=generatorName ) : Promise<void> {
	const rootUrl = `https://cdn.mynoise.net/Data/${generatorName}`;
	await Deno.mkdir(dest, {recursive: true});
	for( const fn of ['0a','1a','2a','3a','4a','5a','6a','7a','8a','9a','0b','1b','2b','3b','4b','5b','6b','7b','8b','9b'] ) {
		await download(`${rootUrl}/${fn}.ogg`, `${dest}/${fn}.ogg`);
	}
}

let generatorName:string|undefined = undefined;

function printHelp( printer:((x:string)=>void) ) {
	printer("Usage: download-mynoise-sounds --generator=SOMEGENERATOR");
	printer("  e.g. DEEPOCEAN");
}

for( const arg of Deno.args ) {
	let m : RegExpExecArray|null = null;
	if( (m = /^--generator=(.*)$/.exec(arg)) !== null ) {
		generatorName = m[1];
	} else if( '--help' == arg ) {
		printHelp(console.log);
	} else {
		console.error(`Error: Unrecognized argument: ${arg}`);
		Deno.exit(1);
	}
}

if( generatorName === undefined ) {
	console.error(`Error: No --generator=... specified`);
	printHelp(console.error);
	Deno.exit(1);
}

await downloadSounds(generatorName, generatorName);
