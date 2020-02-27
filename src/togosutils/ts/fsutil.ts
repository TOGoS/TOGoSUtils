///<reference types="node"/>

// Try to stay compatible with NodeBuildUtil's FSUtil!

import { readFile as nodeReadFile, writeFile as nodeWriteFile } from 'fs';

type FilePath = string;

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
