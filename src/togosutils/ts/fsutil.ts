///<reference types="node"/>

// Try to stay compatible with NodeBuildUtil's FSUtil!

import {
	readFile as nodeReadFile,
	writeFile as nodeWriteFile,
	mkdir as nodeMkdir,
	stat as nodeStat,
	readdir as nodeReadDir,
	Stats
} from 'fs';

type FilePath = string;

export function stat( file:FilePath ):Promise<Stats> {
	return new Promise( (resolve,reject) => {
		nodeStat(file, (err,stats) => {
			if( err ) reject(err);
			else resolve(stats);
		})
	});
}

export function readFile( file:FilePath, options:{encoding?:string, flag?:string}={} ):Promise<Buffer|string> {
	return new Promise( (resolve,reject) => {
		nodeReadFile(file, options, (err:Error|null,content:Buffer) => {
			if( err ) reject(err);
			else resolve(content);
		})
	});
}

export function readFileToUint8Array( file:FilePath, options:{flag?:string}={} ):Promise<Uint8Array> {
	if( (<any>options).encoding ) {
		return Promise.reject(new Error("Why you passing 'encoding' to readFileToUint8Array"));
	}
	return readFile(file,options).then( (content) => {
		// Shouldn't happen, since we're not allowing encoding to be specified, but just in case we screw up:
		if( typeof content == 'string' ) return Promise.reject(new Error("File read as a string!"));
		// Supposedly Buffer acts as a Uint8Array, so we can just return it.
		return Promise.resolve(content);
	});
}

export function readStreamToUint8Array( stream:NodeJS.ReadableStream ):Promise<Uint8Array> {
    return new Promise<Uint8Array>( (resolve, reject) => {
        let buffers:Buffer[] = [];
        stream.on('data', (buf:Buffer) => {
            buffers.push(buf);
        })
        stream.on('end', () => {
            resolve(Buffer.concat(buffers));
        })
        stream.on('error', (err:Error) => {
            reject(err);
        })
        stream.resume();
    });
}

export function writeFile( file:FilePath, data:string|Uint8Array ):Promise<FilePath> {
	// TODO: PRobably unlink any existing file first,
	// because I like to think I'm replacing the file, not rewriting it!
	// Or maybe that should be called putFile instead idk.
	return new Promise( (resolve,reject) => {
		nodeWriteFile( file, data, (err:Error|null) => {
			if( err ) reject(err);
			resolve(file);
		});
	});
}

export function readDir( dir:FilePath ):Promise<FilePath[]> {
	return new Promise( (resolve,reject) => {
		nodeReadDir( dir, (err,files) => {
			if( err ) return reject(err);
			return resolve(files);
		});
	});
}

export function mkdir( dir:FilePath ):Promise<FilePath> {
	return new Promise( (resolve,reject) => {
		nodeMkdir( dir, (err) => {
			if( err && err.code !== 'EEXIST' ) {
				reject(err);
			} else {
				resolve(dir);
			}
		});
	});
}

export function mkdirR( dir:FilePath ):Promise<void> {
	if( dir == '' ) return Promise.resolve();
	let prefix = '';
	if( dir[0] == '/' ) {
		dir = dir.substr(1);
		prefix = '/';
	}
	let comps = dir.split('/');
	let prom:Promise<any> = Promise.resolve();
	for( let i=1; i<=comps.length; ++i ) {
		prom = prom.then( () => mkdir(prefix+comps.slice(0,i).join('/')) );
	}
	return prom;
}

export function mkParentDirs( file:FilePath ):Promise<void> {
	let comps = file.split('/');
	return mkdirR( comps.slice(0,comps.length-1).join('/') );
}
