import * as fs from 'fs';
import { readStreamToUint8Array, readFileToUint8Array, writeFile } from './fsutil';
import { parse } from 'querystring';
import { inflate as zlibInflate, deflate as zlibDeflate, deflateRaw as zlibDeflateRaw } from 'zlib';
import { hash, hexEncode } from 'tshash';
import SHA1 from 'tshash/SHA1';
import { resolve } from 'url';

//enum URIBrand { "uri" };
type URIString = string&{isUri:true};
type GitURIString = URIString&{isGitUri:true};

//enum GitHashStringBrand { "git-hash-string" };
type GitHashString = string&{isGitHashString:true};

/*
interface GitAuthoring {
    name : string;
    emailAddress : string;
    dateSeconds : number;
    dateTimezone : string;
}
*/
type GitAuthoring = string;

interface GitCommit {
    parentRefs : GitURIString[];
    targetRef : GitURIString;
    author : GitAuthoring;
    committer : GitAuthoring;
    message : string;
}

type GitObject = GitCommit;

// TODO: Fix this won't work if author's name has spaces!!!!
function parseAuthor(tokens:string[]):GitAuthoring {
    return tokens.join(" ");
    /*
    return {
        name: tokens[0],
        emailAddress: tokens[1],
        dateSeconds: +tokens[2],
        dateTimezone: tokens[3]
    }
    */
}

function gitHashToUriString(hash:GitHashString):GitURIString {
    return "x-git-object:"+hash as GitURIString;
}

function validateGitHashString(hash:string):GitHashString {
    if( /[0-9a-f]{40}/.exec(hash) == null ) {
        throw new Error(`"${hash}" does not appear to be a git hash string!`);
    }
    return hash as GitHashString;
}

function uriToGitHash(uri:URIString):GitHashString {
    let m:RegExpMatchArray|null;
    if( m = /^x-git-object:([0-9a-f]{40})$/.exec(uri) ) {
        return m[1] as GitHashString;
    } else {
        throw new Error(uri+" doesn't look like a git object URI (expecting x-git-object:....)")
    }
}

function parseGitCommit(blob:Uint8Array, offset:number, sourceDesc:string):GitCommit {
    let parentRefs : GitURIString[] = [];
    let targetRef : GitURIString|undefined = undefined;
    let author : GitAuthoring|undefined = undefined;
    let committer : GitAuthoring|undefined = undefined;

    // Not allowing for non-UTF-8 messages, sorry!
    let body = blob.slice(offset).toString();
    let xx = body.split("\n\n", 2);
    let headers = xx[0].split("\n");
    let message = xx[1];
    for( let h in headers ) {
        const tokens:string[] = headers[h].split(" ");
        switch( tokens[0] ) {
        case 'tree':
            targetRef = gitHashToUriString(validateGitHashString(tokens[1]));
            break;
        case 'parent':
            parentRefs.push( gitHashToUriString(validateGitHashString(tokens[1])) );
            break;
        case 'author':
            author = parseAuthor(tokens.slice(1));
            break;
        case 'committer':
            committer = parseAuthor(tokens.slice(1));
            break;
        }
    }

    if(targetRef == undefined) throw new Error("Commit specified no target")
    if(author == undefined) throw new Error("Commit specified no author")
    if(committer == undefined) throw new Error("Commit specified no committer")

    return { parentRefs, targetRef, author, committer, message };

}

export function parseGitObject(blob:Uint8Array, sourceDesc:string):GitObject {
    let maxHeaderLength = Math.min(blob.length, 20);
    let firstZeroAt:number|undefined = undefined;
    for( let i=0; i<maxHeaderLength; ++i ) {
        if( blob[i] == 0 ) firstZeroAt = i;
    }
    if( firstZeroAt == undefined ) throw new Error("Failed to extract git object header from "+sourceDesc);

    let header:string = blob.slice(0,firstZeroAt).toString();
    let parts:string[] = header.split(" ",2);
    if( parts.length < 2 ) throw new Error("Failed to parse header from "+sourceDesc+"; not enough components!")
    const type = parts[0];
    const indicatedLength = +parts[1];
    const actualLength = blob.length - firstZeroAt - 1;
    if( actualLength != indicatedLength ) {
        throw new Error("Length of serialized "+type+" seems wrong; header said "+indicatedLength+" bytes, but "+actualLength+" available");
    }
    switch(type) {
    case "commit": return parseGitCommit(blob, firstZeroAt+1, sourceDesc);
    default:
        throw new Error("Unrecognized git object type: "+type);
    }
}

export function deflate(text:Uint8Array):Promise<Uint8Array> {
    return new Promise( (resolve,reject) => {
        console.error("Attempting to deflate "+text);
        zlibDeflateRaw(Buffer.from(text), (err:Error,deflated:Uint8Array) => {
            if(err) reject(err);
            else resolve(deflated);
        });
    });
}

export function inflate(deflated:Uint8Array):Promise<Uint8Array> {
    return new Promise<Uint8Array>( (resolve,reject) => {
        zlibInflate(Buffer.from(deflated.buffer), (err:Error, inflated:Uint8Array) => {
            if(err) reject(err);
            resolve(inflated);
        });
    });
}

export function fetchGitObjectBlob(name:string):Promise<Uint8Array> {
    if(name == "-") {
        return readStreamToUint8Array(process.stdin).then(inflate);
    } else {
        return readFileToUint8Array(name).then(inflate)
    }
}
export function fetchBlob(name:string):Promise<Uint8Array> {
    if(name == "-") {
        return readStreamToUint8Array(process.stdin);
    } else {
        return readFileToUint8Array(name);
    }
}

export function fetchGitObject(name:string):Promise<GitObject> {
    return fetchGitObjectBlob(name).then(blob => parseGitObject(blob,name));
}

export function printHello():void {
    console.log("Hello, world!  This is GitUtil");
}

function isGitAuthoring(obj:any):boolean {
    return typeof obj == 'string' || (
        typeof obj.name == 'string' &&
        typeof obj.emailAddress == 'string' &&
        typeof obj.dateSeconds == 'number' &&
        typeof obj.dateTimezone == 'string'
    )
}

function isCommit(obj:any):obj is GitCommit {
    return (
        Array.isArray(obj.parentRefs) && // TODO: Validate those
        typeof obj.targetRef == "string" &&
        isGitAuthoring(obj.author) &&
        isGitAuthoring(obj.committer)
    );
}

const textEncoder = new TextEncoder();

function formatGitObject(type:string, payload:Uint8Array) {
    let header = textEncoder.encode(`${type} ${payload.length}`);
    let result = new Uint8Array(header.length+1+payload.length);
    result.set(header);
    result[header.length] = 0;
    result.set(payload, header.length+1);
    return result;
}

function gitRef(data:Uint8Array):GitHashString {
    return hexEncode(hash(data, SHA1)) as GitHashString;
}

function formatAuthoring(authoring:GitAuthoring) {
    return authoring;
}

export function storeCommit(obj:GitCommit):Promise<GitURIString> {
    let lines:string[] = [];
    lines.push("tree "+uriToGitHash(obj.targetRef));
    for( let p in obj.parentRefs ) {
        lines.push("parent "+uriToGitHash(obj.parentRefs[p]));
    }
    lines.push("author "+formatAuthoring(obj.author));
    lines.push("committer "+formatAuthoring(obj.committer));
    lines.push("");
    lines.push(obj.message);
    let ser0:Uint8Array = formatGitObject("commit", textEncoder.encode(lines.join("\n")));
    let ref:GitHashString = gitRef(ser0);
    let path:string = ".git/objects/"+(ref as string).substr(0,2)+"/"+(ref as string).substring(2);
    return deflate(ser0).then(deflated => {
        return writeFile(path,deflated);
    }).then(() => gitHashToUriString(ref));
}

export function storeGitObject(obj:any):Promise<GitURIString> {
    if( isCommit(obj) ) {
        return storeCommit(obj);
    } else {
        return Promise.reject(new Error("Unrecognized object!"));
    }
}
