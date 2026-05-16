type GitpulschCommand = GitpushCommand|GitpullCommand;

type GitpullCommand = {
	action: "pull",
	ffMode: undefined|true|false,
};

type GitpushCommand = {
	action: "push"
};

function parseCmd(args:string[]) : GitpulschCommand {
	let action : "push"|"pull"|undefined = undefined; 
	let ffMode : boolean|undefined = true;
	for( const arg of args ) {
		if( "--action=pull" == arg ) {
			action = "pull";
		} else if( "--action=pull" == arg ) {
			action = "push";
		} else if( "--no-ff" == arg ) {
			ffMode = false;
		} else if( "--ff-only" == arg ) {
			ffMode = true;
		} else if( "--allow-merge" == arg ) {
			ffMode = undefined;
		}
	}
	if( action == undefined ) {
		throw new Error("Action not indicated");
	}
	return {
		action, ffMode
	}
}

type FSPath = string;

function findGitDir() : Promise<FSPath> {
	// TODO: Check env vars
	return Promise.resolve(".git");
}

function getRemoteNames(gitDir : FSPath) : Promise<string[]> {
	// TODO: Actually read them from .git/config or something
	return Promise.resolve([
		"togos-fbs",
		"fs.marvin",
		"toggh1",
		"github",
		"togos-mini1"
	]);
}

async function getCurrentBranch(gitDir : FSPath) : Promise<string> {
	const headFile = gitDir+"/HEAD";
	const headText = (await
		Deno.readTextFile(headFile).catch(e =>
			Promise.reject(new Error(`Failed to read ${headFile}: ${e}`))
		)
	).trim();
	
	const HEAD_REF_REGEX = /^ref:\s+(.*)/;
	const headMatch = HEAD_REF_REGEX.exec(headText);
	if( headMatch == null ) {
		throw new Error(`Failed to parse branch from '${headText}'`);
	}
	
	const REFS_WHATEVER_REGEX = /^refs\/heads\/(.*)$/;
	const refMatch = REFS_WHATEVER_REGEX.exec(headMatch[1]);
	if( refMatch == null ) {
		throw new Error(`Failed to derive current branch from ref '${headMatch[1]}`);
	}
	
	return refMatch[1];
}

async function doCmd(cmd : GitpulschCommand) : Promise<number> {
	const gitDir = await findGitDir();
	const currentBranch = await getCurrentBranch(gitDir);
	const remoteNames = await getRemoteNames(gitDir);
	for( const rem of remoteNames ) {
		const cmd = new Deno.Command("git", {
			args: ["pull", rem, currentBranch]
		})
		const proc = cmd.spawn();
		await proc.status;
	}
	return 0;
}

const exitCode = await doCmd(parseCmd(Deno.args));
Deno.exit(exitCode);
