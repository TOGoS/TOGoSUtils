import * as fs from 'fs';
import { readStreamToUint8Array, readFileToUint8Array } from './fsutil';
import { parse } from 'querystring';
import { inflate as zlibInflate } from 'zlib';

type GitRef = string;

interface GitAuthoring {
    name : string;
    emailAddress : string;
    dateSeconds : number;
    dateTimezone : string;
}

interface GitCommit {
    parents : GitRef[];
    target : GitRef;
    author : GitAuthoring;
    committer : GitAuthoring;
}

type GitObject = GitCommit;

function parseAuthor(tokens:string[]):GitAuthoring {
    return {
        name: tokens[0],
        emailAddress: tokens[1],
        dateSeconds: +tokens[2],
        dateTimezone: tokens[3]
    }
}

function parseGitCommit(blob:Uint8Array, offset:number, sourceDesc:string):GitCommit {
    let parents : GitRef[] = [];
    let target : GitRef|undefined = undefined;
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
            target = "x-git-object:"+tokens[1];
            break;
        case 'parent':
            parents.push( "x-git-object:"+tokens[1] );
            break;
        case 'author':
            author = parseAuthor(tokens.slice(1));
            break;
        case 'committer':
            committer = parseAuthor(tokens.slice(1));
            break;
        }
    }

    if(target == undefined) throw new Error("Commit specified no target")
    if(author == undefined) throw new Error("Commit specified no author")
    if(committer == undefined) throw new Error("Commit specified no committer")

    return { parents, target, author, committer };

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

export function fetchGitObject(name:string):Promise<GitObject> {
    return fetchGitObjectBlob(name).then(blob => parseGitObject(blob,name));
}

export function printHello():void {
    console.log("Hello, world!  This is GitUtil");
}
