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

// TODO: Librarify all that boilerplate!

interface Item {
	idString?: string;
	typeString?: string;
	title?: string;
	subtaskOf?: string;
	content?: string;
	status?: string;
}
interface ItemEtc extends Item {
	[k: string]: string|undefined;
}

function toCamelCase(phrase:string) {
	const words = phrase.split(/[ -_]/g).map( word => word.toLowerCase() );
	for( let i=1; i<words.length; ++i ) {
		words[i] = words[i].charAt(0).toUpperCase() + words[i].substring(1);
	}
	return words.join('');
}

function tefEntryToItem(e : Readonly<TEFEntry> ) : ItemEtc {
	// Might want to drive translation based on some schema.
	// Not yet sure exactly what that would look like.
	const trimIds = true;

	const obj : ItemEtc = {};

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
	if( effectiveIdString != "" ) obj.idString = effectiveIdString;
	if( effectiveTitleString != "" ) obj.title = effectiveTitleString;
	if( e.typeString != "" ) obj.typeString = e.typeString;
	for( const header of e.headers ) {
		const ccKey = toCamelCase(header.key);

		if( header.value == "" ) continue;
		let value = obj[ccKey] ?? "";
		if( value.length > 0 ) value += "\n";
		obj[ccKey] = value + header.value;
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



import * as colors from 'https://deno.land/std@0.154.0/fmt/colors.ts';

const separatorLine = colors.gray("#".repeat(74));

type ItemPrinter = (item:Item) => Promise<void>;

function collectItemAndParentIds(items:Map<string,Item>, itemId:string, into:string[]) {
	const item = items.get(itemId);
	if( item == undefined ) {
		throw new Error(`Referenced item ${itemId} not found in items map!`);
	}
	if( item.subtaskOf != undefined ) {
		const parentIds = item.subtaskOf.split(/\s+/);
		for( const parentId of parentIds ) {
			collectItemAndParentIds(items, parentId, into);
		}
	}
	into.push(itemId);
}

interface ToDoListingOptions {
	selectionMode : "all"|"random-todo-task"|"todo";
	outputFormat : "pretty"|"json";
}

function prettifyKey(k:string) {
	// Seems like I want this to just revert to TEF-format headers,
	// which makes sense as TEF was intended to be human-readable!
	switch(k) {
	case 'subtaskOf': return 'subtask-of';
	default: return k;
	}
}

function prettyPrintItem(item:Item) : Promise<void> {
	console.log("=" +
		colors.rgb24(item.typeString ?? "item", 0xCCAAAA) + " " +
		colors.rgb24(item.idString ?? "", 0xAACCAA) +
		(item.title ? " - " + colors.brightWhite(item.title) : '')
	);
	for( const k in item ) {
		if( k == 'typeString' || k == 'idString' || k == 'title' || k == 'content' ) continue;
		console.log(`${prettifyKey(k)}: ${(item as ItemEtc)[k]?.replaceAll("\n", ", ")}`);
	}
	if( item.content != undefined ) {
		console.log();
		console.log(item.content.trim());
	}
	return Promise.resolve();
}

async function main(options:ToDoListingOptions) {
	const items : Map<string, Item> = new Map();
	for await( const entry of tefPiecesToEntries(tef.parseTefPieces(readerToIterator(Deno.stdin))) ) {
		const item = tefEntryToItem(entry);
		if( item.idString ) {
			items.set(item.idString, item);
		}
	}
	let itemIds : string[] = [];
	for( const [itemId, item] of items ) {
		if( options.selectionMode == "all" ) {
			itemIds.push(itemId);
		} else if( options.selectionMode == "todo" ) {
			if( item.status != "done" ) {
				itemIds.push(itemId);
			}
		} else {
			if( item.typeString == "task" && item.status != "done" ) {
				itemIds.push(itemId);
			}
		}
	}
	if( options.selectionMode == "random-todo-task" ) {
		itemIds.sort( (a,b) => a == b ? 0 : Math.random() < 0.5 ? -1 : 1 );
		const taskId = itemIds[0];
		itemIds = [];
		collectItemAndParentIds(items, taskId, itemIds);
	}
	
	if( options.outputFormat == "json" ) {
		for( const itemId of itemIds ) {
			console.log(JSON.stringify(items.get(itemId)));
		}
	} else {
		console.log();
		console.log(colors.yellow("Welcome to list-todo!"));
		console.log(`selection mode: ${options.selectionMode}`);
		console.log();
		console.log();
		console.log(separatorLine);
		for( const itemId of itemIds ) {
			console.log(separatorLine);
			console.log();
			const item = items.get(itemId)
			if( item ) {
				prettyPrintItem(item);
			} else {
				console.log(`Bad item ID: ${itemId}`);
			}
			console.log();
		}
		console.log(separatorLine);
		console.log(separatorLine);
		console.log();
		console.log();	
	}
}

function parseOptions(args:string[]) : ToDoListingOptions {
	let selectionMode : "all"|"random-todo-task"|"todo" = "all";
	let outputFormat : "pretty"|"json" = "json";

	for( const arg of args ) {
		if( arg == "--output-format=json" ) {
			outputFormat = "json";
		} else if( arg == "--output-format=pretty" ) {
			outputFormat = "pretty";
		} else if( arg == "-p" ) {
			outputFormat = "pretty";
			selectionMode = "random-todo-task";
		} else if( arg == "--select=all" ) {
			selectionMode = "all";
		} else if( arg == "--select=incomplete" ) {
			selectionMode = "todo";
		}
	}

	return {
		selectionMode,
		outputFormat,
	}
}

main(parseOptions(Deno.args));
