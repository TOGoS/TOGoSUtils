/// <reference lib="deno.ns"/>

import * as tef from "https://deno.land/x/tef@0.3.3/tef.ts";

type FilePath = string;

interface BlobLike1 {
	/** If this blob is backed by a file, filePath is the path to it */
	filePath? : FilePath;
	/**
	 * Return an [async] iteratable that wil provide the blob's chunks.
	 * 
	 * 3 modes of operation regarding re-use of buffers:
	 * - default (buffer = undefined) :: each chunk is a unique, immutable Uint8Array
	 * - zero-length buffer provided :: iterator may re-use a buffer, but not the one provided
	 * - non-zero-length buffer provided :: iterator may re-use a buffer, and it may be the one provided
	 * 
	 * @param {Uint8Array} [buffer] a buffer into which chunk data MAY be stored.
	 *   If undefined, each Uint8Array from the resulting iterator should be unique and immutable.
	 *   If a buffer is provided, chunks may re-use the same buffer, which MAY
	 *   be the one provided.  If the provided buffer is of length zero,
	 *   (e.g. if ALLOW_BUFFER_REUSE is passed in) then that buffer cannot be used for non-empty
	 *   chunks, and the iterator must provide its own (either unique per chunk).
	 */
	getChunkIterable(buffer?:Uint8Array) : AsyncIterable<Uint8Array>;
}

class FileBlobLike implements BlobLike1 {
	constructor(protected _filePath:FilePath) {}

	public get filePath() { return this._filePath; }

	public getChunkIterable(buffer?:Uint8Array) : AsyncIterable<Uint8Array> {
		const filePath = this._filePath;
		return {
			[Symbol.asyncIterator]: async function*() {
				const bufferReuseAllowed = buffer != undefined;
				if( buffer == undefined || buffer.length == 0 ) {
					buffer = new Uint8Array(65536);
				}
				const bufferSlice = (bufferReuseAllowed ? buffer.subarray : buffer.slice).bind(buffer);

				const reader = await Deno.open(filePath, {read:true});
				try {
					let readCount : number|null;
					while( (readCount = await reader.read(buffer)) != null ) {
						yield( bufferSlice(0, readCount) );
					}
				} finally {
					reader.close();
				}
			}
		}
	}
}

function readerToIterator(reader : Deno.Reader, buffer? : Uint8Array) : AsyncIterable<Uint8Array> {
	const bufferReuseAllowed = buffer != undefined;
	if( buffer == undefined || buffer.length == 0 ) {
		buffer = new Uint8Array(65536);
	}

	const bufferSlice = (bufferReuseAllowed ? buffer.subarray : buffer.slice).bind(buffer);
	const _buffer = buffer;
	return {
		[Symbol.asyncIterator]: async function*() {
			let readCount : number|null;
			while( (readCount = await reader.read(_buffer)) != null ) {
				yield( bufferSlice(0, readCount) );
			}
		}
	}
}

interface TEFHeader {
	key : string;
	value : string;
}

interface TEFEntry {
	typeString : string;
	idString : string;
	headers : TEFHeader[];
	contentChunks : Uint8Array[];
}

function makeBlankTefEntry() : TEFEntry {
	return {
		typeString: "",
		idString: "",
		headers: [],
		contentChunks: [],
	};
}

function tefEntryIsEmpty( entry : TEFEntry ) : boolean {
	if( entry.typeString.length > 0 ) return false;
	if( entry.idString.length > 0 ) return false;
	if( entry.headers.length > 0 ) return false;
	if( entry.contentChunks.length > 0 ) return false;
	return true;
}

async function *tefPiecesToEntries( pieces : AsyncIterable<tef.TEFPiece> ) : AsyncGenerator<TEFEntry> {
	let currentEntry : TEFEntry = makeBlankTefEntry();

	for await( const piece of pieces ) {
		if( piece.type == "new-entry" ) {
			if( !tefEntryIsEmpty(currentEntry) ) {
				yield(currentEntry);
				currentEntry = makeBlankTefEntry();
			}
			currentEntry.typeString = piece.typeString;
			currentEntry.idString = piece.idString;
		} else if( piece.type == "comment" ) {
			// Skip
		} else if( piece.type == "content-chunk" ) {
			currentEntry.contentChunks.push(piece.data);
		} else if( piece.type == "header" ) {
			currentEntry.headers.push(piece);
		}
	}
}

function tefEntryToJson(e : Readonly<TEFEntry> ) : {[k:string]: string} {
	// Might want to drive translation based on some schema.
	// Not yet sure exactly what that would look like.
	const trimIds = true;

	const obj : {[k:string]: string} = {};

	// Parse ID string
	let effectiveIdString : string;
	let effectiveTitleString : string;
	let m;
	if( trimIds && (m = /^(\S+)\s+(?:-\s+|#\s+)?(.*)/.exec(e.idString)) ) {
		effectiveIdString = m[1];
		effectiveTitleString = m[2];
	} else {
		effectiveIdString = e.idString;
		effectiveTitleString = "";
	}

	// Parse type string and headers
	if( effectiveIdString != "" ) obj.id = effectiveIdString;
	if( effectiveTitleString != "" ) obj.title = effectiveTitleString;
	if( e.typeString != "" ) obj.type = e.typeString;
	for( const header of e.headers ) {
		if( header.value == "" ) continue;
		const appendTo = obj[header.key] != undefined ? obj[header.key] + "\n" : "";
		obj[header.key] = appendTo + header.value;
	}
	
	// Stringify content
	let contentLength = 0;
	for( const chunk of e.contentChunks ) {
		contentLength += chunk.length;
	}
	if( contentLength == 0 ) {
		// Don't add to object
	} else if( e.contentChunks.length == 1 ) {
		obj["content"] = new TextDecoder().decode(e.contentChunks[0]);
	} else {
		const megabuf = new Uint8Array(contentLength);
		let len = 0;
		for( const chunk of e.contentChunks ) {
			megabuf.set(chunk, len);
			len += chunk.length;
		}
	
		obj["content"] = new TextDecoder().decode(megabuf);
	}
	return obj;
}


// TODO: Librarify all that boilerplate!

for await( const entry of tefPiecesToEntries(tef.parseTefPieces(readerToIterator(Deno.stdin))) ) {
	console.log(JSON.stringify(tefEntryToJson(entry)));
	//if( entry.typeString == "task" ) {
	//
	//}
}
