import { RESOLVED_PROMISE, voidify } from './promises';
import * as tshash from 'tshash';
import * as fsu from './fsutil';

type FilePath = string;

type CCouchHeadURI = string&{isCCouchHeadUri:true}; // x-ccouch-head:...
type CCouchHashURN = string&{isCCouchHashUrn:true}; // urn:bitprint:...
type CCouchHeadPrefix = string&{isCCouchHeadPrefix:true}; // some-repo/archives/images/foo ; doesn't include number
type CCouchResolvedHeadName = CCouchHeadPrefix&{isCCouchResolvedHeadName:true}; // some-repo/archives/images/foo/123 ; full name of specific head with actual number (not 'latest')

// String validation/converstion functions

function headPrefix(prefix:CCouchHeadPrefix, postfix:string):CCouchHeadPrefix {
	return prefix + "/" + postfix as CCouchHeadPrefix;
}
function resolvedHeadName(prefix:CCouchHeadPrefix, postfix:string):CCouchResolvedHeadName {
	return prefix + "/" + postfix as CCouchResolvedHeadName;
}
function isCCouchHashUrn(str:string):str is CCouchHashURN {
	return /^(?:urn:sha1:|urn:bitprint:).*$/.exec(str) != null;
}

function ccouchHashUrn(str:string):CCouchHashURN {
	if (isCCouchHashUrn(str)) return str;
	throw new Error(`"${str}" is not a valid contentcouch hash URN`);
}

function nameToHeadUri(name:CCouchHeadPrefix):CCouchHeadURI {
	return "x-ccouch-head:"+encodeURI(name) as CCouchHeadURI;
}

interface LocalCCouchHead {
	file : FilePath;
	headUri : CCouchHeadURI;
	headName : CCouchResolvedHeadName;
	hashUrn : CCouchHashURN;
}

interface FindLocalHeadsOptions {
	headPrefixes : CCouchHeadPrefix[];
	recurse : boolean;
	lastN : number;
}

function promiseEach<T>( items:T[], callback:(x:T)=>void ):Promise<void> {
	return items.reduce( (prom,item) => prom.then( () => callback(item) ), RESOLVED_PROMISE );
}

function walkHeadFiles(headsDir:FilePath, headPrefix:CCouchHeadPrefix, recurse:boolean, callback:(headName:CCouchResolvedHeadName, fullPath:FilePath)=>void):Promise<void> {
	let dir = headsDir+"/"+headPrefix;
	//Log.i("list-ccouch-heads", `Scanning ${dir}...`);
	return fsu.stat(dir).then( dirStat => {
		if( !dirStat.isDirectory() ) {
			//return callback(headName, fullPath);
			console.warn(`list-ccouch-heads: ${dir} is not a directory!`);
			// Could use everything before last '/' of headPrefix as head name?
			return RESOLVED_PROMISE;
		}

		return fsu.readDir(dir).then( files => promiseEach(files, filename => {
			let fullPath = dir + "/" + filename;
			let headName:CCouchResolvedHeadName = resolvedHeadName(headPrefix, filename);
			return fsu.stat(fullPath).then( stat => {
				if( stat.isDirectory() ) {
					if( recurse ) return walkHeadFiles(headsDir, headName, recurse, callback);
					return RESOLVED_PROMISE;
				}
				
				return callback(headName, fullPath);
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
function findLocalHeads2(headsDir:FilePath, headPrefix:CCouchHeadPrefix, recurse:boolean):Promise<LocalCCouchHead[]> {
	let headList:LocalCCouchHead[] = [];
	let walked:Promise<void> = walkHeadFiles(headsDir, headPrefix, recurse, (headName:CCouchResolvedHeadName, headFile:FilePath) => {
		return fsu.readFileToUint8Array(headFile).then(blob => ccouchHashUrn(tshash.sha1Urn(blob))).then( (sha1Urn:CCouchHashURN) => {
			headList.push({
				file: headFile,
				headUri: nameToHeadUri(headName),
				headName,
				hashUrn: sha1Urn,
			});
		})
	});
	return walked.then( () => {
		return headList.sort( (a:LocalCCouchHead,b:LocalCCouchHead) => compareHeadNames(a.headName, b.headName) );
	});
}


export default class CCouchTool {
	fetchLocalCcouchDirs():Promise<FilePath[]> {
		if( process.env.CCOUCH_REPO_DIR != undefined ) {
			return Promise.resolve([process.env.CCOUCH_REPO_DIR]);
		} else {
			return Promise.reject(new Error("CCOUCH_REPO_DIR not specified"));
		}
	}    

	findLocalHeads(options:FindLocalHeadsOptions, callback:(h:LocalCCouchHead)=>Promise<void> ):Promise<void> {
		let headPrefixes:CCouchHeadPrefix[] = [];
		for( let a in options.headPrefixes ) {
			headPrefixes.push(options.headPrefixes[a]);
		}
		return this.fetchLocalCcouchDirs().then( (ccouchDirs:FilePath[]) => {
			let repoPromises:Promise<void>[] = [];
			for( let cd in ccouchDirs ) {
				let headsDir = ccouchDirs[cd] + "/heads";
				for( let hn in headPrefixes ) {
					let headName = headPrefixes[hn];
					repoPromises.push(findLocalHeads2(headsDir, headName, options.recurse).then( (heads:LocalCCouchHead[]) => {
						const headsByPrefix:{[headPrefix:string]: LocalCCouchHead[]} = {};
						for( let head of heads ) {
							const m = /(.*)\/[^/]+$/.exec(head.headName);
							if( m == null ) {
								console.warn(`Hmm, couldn't determine prefix of head ${head.headName}; weird`);
								continue;
							}
							const headPrefix = m[1];
							if( headsByPrefix[headPrefix] == undefined ) headsByPrefix[headPrefix] = [];
							headsByPrefix[headPrefix].push(head);
						}
						let cbPromises:Promise<void>[] = [];
						for( const headPrefix in headsByPrefix ) {
							const theseHeads = headsByPrefix[headPrefix];
							const startAt = Math.max(0, theseHeads.length - options.lastN);
							for( let i=startAt; i<theseHeads.length; ++i ) {
								cbPromises.push(callback(theseHeads[i]))
							}
						}
						return Promise.all(cbPromises).then(voidify);
					}));
				}
			}
			return Promise.all(repoPromises).then(voidify);
		});
	}
}
