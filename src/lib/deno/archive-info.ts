function maybeConcat(a:string|undefined, b:string|undefined) {
	return a && b ? a+b : undefined;
}

function validateDir(p:FilePath|undefined) : Promise<FilePath|undefined> {
	return (p == undefined || p.length == 0) ? Promise.resolve(undefined) : Deno.stat(p).then(
		stat => stat.isDirectory ? p : undefined,
		err => undefined
	);
}
function filterNulls<T>(t:(T|null|undefined)[]) : T[] {
	return t.filter(i => i != null) as T[];
} 

type FilePath = string;

function findDirs(paths:(FilePath|undefined)[]) : Promise<FilePath[]> {
	const deduped = new Set(paths);
	const validated : Promise<FilePath|undefined>[] = [];
	for( const path of deduped ) {
		validated.push(validateDir(path));
	}
	return Promise.all(validated).then(filterNulls);
}

const homeDirs = await findDirs([
	Deno.env.get("HOME"),
	Deno.env.get("USERPROFILE"),
	maybeConcat(Deno.env.get("HOMEDRIVE"), Deno.env.get("HOMEPATH")),
	'D:',
]);
const stuffDirs = await findDirs(homeDirs.flatMap(hd => [hd, hd+"/stuff"]));

export function findRepo(repoName:string) : Promise<FilePath[]> {
	let m;
	let postStuffPath : string;
	if( (m = /^EarthIT\/(.*)/.exec(repoName)) ) {
		postStuffPath = "proj/"+m[1]; // Hack until I rewrite all this code lol
		// (by rewrite all this I mean probably reference a file from ArchiveInfo,
		// see related notes in timelog from 2021-11-11 if curious).
	} else if( (m = /^TOGoS\/(.*)/.exec(repoName)) ) {
		postStuffPath = m[1];
	} else {
		postStuffPath = repoName;
	}
	return findDirs(stuffDirs.map(sd => sd + '/' + postStuffPath));
}
