/// <reference lib="deno.ns"/>

import * as tef from "https://deno.land/x/tef@0.3.3/tef.ts";
import { readerToIterable } from "https://deno.land/x/tef@0.3.4/util/denostreamutil.ts";

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

//// Phrase translator

interface Phrase {
	english : string;
	camelCase : string;
	dashSeparated : string;
}

class PhraseTranslator {
	phrases : Map<string,Phrase> = new Map();
	addEnglish(english : string) {
		if( this.phrases.has(english) ) return;

		const words = english.split(' ');

		const lcWords = words.map(w => (w == 'e-mail' ? 'email' : w).toLowerCase());

		const ccWords = [lcWords[0]];
		for( let i=1; i<lcWords.length; ++i ) {
			ccWords[i] = lcWords[i].charAt(0).toUpperCase() + lcWords[i].substring(1);
		}
		const camelCase = ccWords.join('');
		const dashSeparated = lcWords.join('-');
		const phrase : Phrase = { english, camelCase, dashSeparated };
		this.phrases.set(english, phrase);
		this.phrases.set(camelCase, phrase);
		this.phrases.set(dashSeparated, phrase);
	}
	addDashed(dashed : string) {
		this.addEnglish(dashed.split('-').join(' '));
	}
	get(phraseStr : string) : Phrase {
		const phrase = this.phrases.get(phraseStr);
		if( phrase == undefined ) throw new Error(`"${phraseStr}" to not in phrase database`);
		return phrase;
	}
	toDashSeparated(phraseStr : string) : string {
		return this.get(phraseStr).dashSeparated;
	}
	toCamelCase(phraseStr : string) : string {
		return this.get(phraseStr).camelCase;
	}
}

import { assertEquals } from "https://deno.land/std@0.154.0/testing/asserts.ts";

Deno.test("phrase translator does what is expected of it", () => {
	const pt = new PhraseTranslator();
	pt.addDashed("foo-bar-baz");
	assertEquals("fooBarBaz", pt.toCamelCase("foo bar baz"));
	assertEquals("fooBarBaz", pt.toCamelCase("foo-bar-baz"));
	assertEquals("fooBarBaz", pt.toCamelCase("fooBarBaz"));
});

const phraseTranslator = new PhraseTranslator();

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
		phraseTranslator.addDashed(header.key);
		const ccKey = phraseTranslator.toCamelCase(header.key);

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
		const parentIds = item.subtaskOf.split(/,?\s+/);
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

function prettyPrintItem(item:Item) : Promise<void> {
	console.log("=" +
		colors.rgb24(item.typeString ?? "item", 0xCCAAAA) + " " +
		colors.rgb24(item.idString ?? "", 0xAACCAA) +
		(item.title ? " - " + colors.brightWhite(item.title) : '')
	);
	for( const k in item ) {
		if( k == 'typeString' || k == 'idString' || k == 'title' || k == 'content' ) continue;
		console.log(`${phraseTranslator.toDashSeparated(k)}: ${(item as ItemEtc)[k]?.replaceAll("\n", ", ")}`);
	}
	if( item.content != undefined ) {
		console.log();
		console.log(item.content.trim());
	}
	return Promise.resolve();
}

function itemIsDone(item:Item) : boolean {
	return item.status?.startsWith("done") || false;
}

async function main(options:ToDoListingOptions) {
	const items : Map<string, Item> = new Map();
	for await( const entry of tefPiecesToEntries(tef.parseTefPieces(readerToIterable(Deno.stdin))) ) {
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
			if( !itemIsDone(item) ) {
				itemIds.push(itemId);
			}
		} else if( options.selectionMode == "random-todo-task" ) {
			if( item.typeString == "task" && !itemIsDone(item) ) {
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
		} else if( arg == "--select=random-todo-task" ) {
			selectionMode = "random-todo-task";
		} else if( arg == "--select=incomplete" ) {
			selectionMode = "todo";
		} else {
			console.error(`Error: unrecognized argument ${arg}`);
			Deno.exit(1);
		}
	}

	return {
		selectionMode,
		outputFormat,
	}
}

main(parseOptions(Deno.args));
