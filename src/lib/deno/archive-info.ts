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

export function findRepoFile(repoName:string, file:string) : Promise<FilePath> {
	return findRepo(repoName).then( async repoDirs => {
		for( const repoDir of repoDirs ) {
			const fullPath = `${repoDir}/${file}`;
			try {
				await Deno.stat(fullPath);
				return fullPath;
			} catch( _e ) {
				// I guess it wasn't there.
			}
		}
		throw new Error(`Couldn't find ${file} in any of ${repoDirs.length} instances of ${repoName}`);
	})
}

interface RepoMetadata {
	repoTypeName: "git-repo"|"ccouch-repo";
	name: string;
	title?: string;
	preferredCcouchSector?: string;
}

import { parseTefPieces } from 'https://deno.land/x/tef@0.3.3/tef.ts';

async function* denoReaderToAsyncIterable(reader:Deno.Reader&Deno.Closer) : AsyncIterable<Uint8Array> {
	const buffer = new Uint8Array(65536);
	let read;
	try {
		while( (read = await reader.read(buffer)) != null ) {
			yield buffer.subarray(0, read);
		}
	} finally {
		reader.close();
	}
}

async function* fetchRepoMetadata() : AsyncIterable<RepoMetadata> {
	const repoInfoTefPath = await findRepoFile("TOGoS/docs/ArchiveInfo", "info/repos.tef");
	let currentEntry : RepoMetadata|undefined = undefined;
	for await( const tefPiece of parseTefPieces(denoReaderToAsyncIterable(await Deno.open(repoInfoTefPath))) ) {
		if( tefPiece.type == "new-entry" ) {
			if( currentEntry ) {
				yield currentEntry;
			}
			if( tefPiece.typeString == "git-repo" || tefPiece.typeString == "ccouch-repo" ) {
				currentEntry = {
					repoTypeName: tefPiece.typeString,
					name: tefPiece.idString,
				}
			} else {
				currentEntry = undefined;	
			}
		} else if( tefPiece.type == "header" && currentEntry != undefined ) {
			switch( tefPiece.key ) {
			case "title":
				currentEntry.title = tefPiece.value;
				break;
			case "sector":
				currentEntry.preferredCcouchSector = tefPiece.value;
				break;
			}
		}
	}
	if( currentEntry ) {
		yield currentEntry;
	}
}

if( import.meta.main ) {
	for await( const repoInfo of fetchRepoMetadata() ) {
		const repoPaths = await findRepo(repoInfo.name);
		const extendedRepoInfo = {
			...repoInfo,
			paths: repoPaths,
		};
		console.log(extendedRepoInfo);
	}
}
