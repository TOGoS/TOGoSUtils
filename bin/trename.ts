#!/usr/bin/env deno

interface TRenameOptions {	
	regex: RegExp,
	replacement: string,
	mode : "Run"|"DryRun",
	files: string[],
}

function parseOptions(args:string[]):TRenameOptions {
	let regexStr:string|null = null;
	const files:string[] = [];
	let mode : "Run"|"DryRun" = "DryRun";
	for( const arg of args ) {
		if( arg == "--help" ) {
			console.log("Usage: trename 's/ab(c)123/zyx$1987/' <files>");
			console.log("Usage: trename 's/ab(c)123/zyx$1987/' <files>");
		} else if( arg == "--dry-run" ) {
			mode = "DryRun";
		} else if( !arg.startsWith("-") ) {
			if( regexStr === null ) {
				regexStr = arg;
			} else if( arg == '*' ) {
				// Not automatically expanded by Windows cmd
				for( const entry of Deno.readDirSync(".") ) {
					files.push(entry.name);
				}
			} else {
				files.push(arg);
			}
		} else {
			console.error(`Error: Unrecognized argument: ${arg}`)
			Deno.exit(1);
		}
	}

	if( regexStr === null ) {
		console.error(`Error: No regex given`)
		Deno.exit(1);
	}

	const regexRegex = /^s(\W)([^\1]*)\1([^\1]*)\1([g]*)$/;
	const m = regexRegex.exec(regexStr);
	if( m == null ) {
		console.error(`Error: Failed to parse "${regexStr}" as regex`);
		Deno.exit(1);
	}

	const matchRegexStr = m[2];
	const replacement = m[3];
	const flags = m[4];

	const regex = new RegExp(matchRegexStr, flags);

	return {
		files,
		mode,
		regex,
		replacement
	}
}

const opts = parseOptions(Deno.args);

for( const infile of opts.files ) {
	const outfile = infile.replace(opts.regex, opts.replacement);
	if( outfile != infile ) {
		console.log(`ren "${infile}" "${outfile}"`);
	}
}
