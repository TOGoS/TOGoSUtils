import { RESOLVED_PROMISE, voidify } from './promises';
import * as tshash from 'tshash';
import * as fsu from './fsutil';

type FilePath = string;

type CCouchHeadURI = string; // x-ccouch-head:...
type CCouchHashURN = string; // urn:bitprint:...

interface LocalCCouchHead {
	file : FilePath;
	headUri : CCouchHeadURI;
	hashUrn : CCouchHashURN;
}

interface FindLocalHeadsOptions {
	headPrefixes : string[];
	recurse : boolean;
	lastN : number;
}

function promiseEach<T>( items:T[], callback:(x:T)=>void ):Promise<void> {
	return items.reduce( (prom,item) => prom.then( () => callback(item) ), RESOLVED_PROMISE );
}

function walkHeadFiles(headsDir:FilePath, headName:string, recurse:boolean, callback:(headUri:CCouchHeadURI, fullPath:FilePath)=>void):Promise<void> {
	let dir = headsDir+"/"+headName;
	//Log.i("list-ccouch-heads", `Scanning ${dir}...`);
	return fsu.stat(dir).then( dirStat => {
		if( !dirStat.isDirectory() ) {
			//Log.i("list-ccouch-heads", `${dir} is not a directory!`);
			return RESOLVED_PROMISE;
		}

		return fsu.readDir(dir).then( files => promiseEach(files, filename => {
			let fullPath = dir + "/" + filename;
			let headUri = "x-ccouch-head:"+headName+"/"+filename;
			return fsu.stat(fullPath).then( stat => {
				if( stat.isDirectory() ) {
					if( recurse ) return walkHeadFiles(headsDir, headName+"/"+filename, recurse, callback);
					return RESOLVED_PROMISE;
				}
				
				return callback(headUri, fullPath);
			})
		}));
	})
}

function headOrder(path:FilePath) {
	let m;
	if( (m = /^(.*)\/(\d+)$/.exec(path)) != undefined ) {
		return [m[1],+m[2]];
	} else if( (m = /^(.*)\/([^\\/]+)$/.exec(path)) != undefined ) {
		return [m[1],m[2]];
	} else {
		return [path, ""];
	}
}

function compareHeadNames(a:FilePath, b:FilePath) {
	let oa = headOrder(a);
	let ob = headOrder(b);
	let compared = (
		oa[0] < ob[0] ? -1 : oa[0] > ob[0] ? +1 :
		oa[1] < ob[1] ? -1 : oa[1] > ob[1] ? +1 :
		0
	);
	//Log.i("head-comparison", JSON.stringify(oa)+" vs "+JSON.stringify(ob)+": "+compared);
	return compared;
}

// Returns heads, sorted by path and 'natural order'.
function findLocalHeads2(headsDir:FilePath, headName:string, recurse:boolean):Promise<LocalCCouchHead[]> {
	let headList:LocalCCouchHead[] = [];
	let walked:Promise<void> = walkHeadFiles(headsDir, headName, recurse, (headUri:CCouchHeadURI, headFile:FilePath) => {
		return fsu.readFileToUint8Array(headFile).then(tshash.sha1Urn).then( (sha1Urn:CCouchHashURN) => {
			headList.push({
				file: headFile,
				headUri: headUri,
				hashUrn: sha1Urn,
			});
		})
	});
	return walked.then( () => {
		return headList.sort( (a:LocalCCouchHead,b:LocalCCouchHead) => compareHeadNames(a.headUri, b.headUri) );
	});
}


export default class CCouchTool {
	fetchLocalCcouchDirs():Promise<FilePath[]> {
		if( process.env.ccouch_repo_dir != undefined ) {
			return Promise.resolve([process.env.ccouch_repo_dir]);
		} else {
			return Promise.reject(new Error("ccouch_repo_dir not specified"));
		}
	}    

	findLocalHeads(options:FindLocalHeadsOptions, callback:(h:LocalCCouchHead)=>Promise<void> ):Promise<void> {
		let headNames:string[] = [];
		for( let a in options.headPrefixes ) {
			headNames.push(options.headPrefixes[a]);
		}
		return this.fetchLocalCcouchDirs().then( (ccouchDirs:FilePath[]) => {
			let repoPromises:Promise<void>[] = [];
			for( let cd in ccouchDirs ) {
				let headsDir = ccouchDirs[cd] + "/heads";
				for( let hn in headNames ) {
					let headName = headNames[hn];
					repoPromises.push(findLocalHeads2(headsDir, headName, options.recurse).then( (heads:LocalCCouchHead[]) => {
						let startAt = Math.max(0, heads.length - options.lastN);
						let cbPromises:Promise<void>[] = [];
						for( let i=startAt; i<heads.length; ++i ) {
							cbPromises.push(callback(heads[i]))
						}
						return Promise.all(cbPromises).then(voidify);
					}));
				}
			}
			return Promise.all(repoPromises).then(voidify);
		});
	}
}
