///<reference types="node"/>

// Try to stay compatible with NodeBuildUtil's FSUtil!

import * as fs from 'fs';

type FilePath = string;

export function stat( file:FilePath ):Promise<fs.Stats> {
	return new Promise( (resolve,reject) => {
		fs.stat(file, (err,stats) => {
			if( err ) reject(err);
			else resolve(stats);
		})
	});
}

export function chmod( file:FilePath, mode:number ):Promise<void> {
	return new Promise( (resolve,reject) => {
		fs.chmod( file, mode, (err) => {
			if( err ) reject(err);
			else resolve();
		});
	});
}

export function readFile( file:FilePath, options:{encoding?:string, flag?:string}={} ):Promise<Buffer|string> {
	return new Promise( (resolve,reject) => {
		fs.readFile(file, options, (err:Error|null,content:Buffer) => {
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
		fs.writeFile( file, data, (err:Error|null) => {
			if( err ) reject(err);
			resolve(file);
		});
	});
}

export function readDir( dir:FilePath ):Promise<FilePath[]> {
	return new Promise( (resolve,reject) => {
		fs.readdir( dir, (err,files) => {
			if( err ) return reject(err);
			return resolve(files);
		});
	});
}


export function rmDir( dir:FilePath ):Promise<void> {
	return new Promise<void>( (resolve,reject) => {
		fs.rmdir(dir, (err) => {
			if( err ) reject(err);
			else resolve();
		})
	});
}

export function rename( oldPath:FilePath, newPath:FilePath ):Promise<FilePath> {
	return new Promise( (resolve,reject) => {
		fs.rename(oldPath, newPath, (err) => {
			if( err ) reject(err);
			else resolve(newPath);
		})
	});
};

export function link( oldPath:FilePath, newPath:FilePath ):Promise<FilePath> {
	return new Promise( (resolve,reject) => {
		fs.link(oldPath, newPath, (err) => {
			if( err ) reject(err);
			else resolve(newPath);
		})
	});
};

export function unlink( file:FilePath ):Promise<void> {
	return new Promise<void>( (resolve,reject) => {
		fs.unlink(file, (err) => {
			if( err ) reject(err);
			else resolve();
		})
	});
}

export function rmRf( fileOrDir:FilePath|string[] ):Promise<void> {
	if( typeof fileOrDir == 'object' ) {
		return Promise.all(fileOrDir.map(rmRf)).then( () => {} );
	}
	return stat(fileOrDir).then( (stats) => {
		if( stats.isDirectory() ) {
			return readDir(fileOrDir).then( (files) => {
				let promz:Promise<void>[] = [];
				for( let i in files ) {
					promz.push(rmRf( fileOrDir+"/"+files[i] ));
				}
				return Promise.all(promz);
			}).then( () => rmDir(fileOrDir) );
		} else {
			return unlink(fileOrDir);
		}
	}, (err) => {
		if( err.code === 'ENOENT' ) return;
		else return Promise.reject(err);
	});
}

export function cp( src:FilePath, dest:FilePath ):Promise<FilePath> {
	return new Promise( (resolve,reject) => {
		let rd = fs.createReadStream(src);
		rd.on('error', reject);
		let wr = fs.createWriteStream(dest);
		wr.on('error', reject);
		wr.on('close', () => resolve(dest) );
		rd.pipe(wr);
	});
}

export function mkdir( dir:FilePath ):Promise<FilePath> {
	return new Promise( (resolve,reject) => {
		fs.mkdir( dir, (err) => {
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
		if( i == 1 && /^[a-z]:$/i.exec(comps[0]) ) continue; // Skip things like "F:"
		prom = prom.then( () => mkdir(prefix+comps.slice(0,i).join('/')) );
	}
	return prom;
}

export function mkParentDirs( file:FilePath ):Promise<void> {
	let comps = file.split('/');
	return mkdirR( comps.slice(0,comps.length-1).join('/') );
}
