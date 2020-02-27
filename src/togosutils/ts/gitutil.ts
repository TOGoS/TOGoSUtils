interface GitRef {
    sha1Hex: string
}

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


export function parseCommit(blob:Uint8Array):GitCommit {
    let parents : GitRef[] = [];
    let target : GitRef|undefined = undefined;
    let author : GitAuthoring|undefined = undefined;
    let committer : GitAuthoring|undefined = undefined;

    if( blob.slice(0,7).toString() != "commit " ) {
        throw new Error("Serialized commit does not start with 'commit '");
    }
    let firstZeroAt = 0;
    for( firstZeroAt=0; firstZeroAt<blob.length && blob[firstZeroAt] != 0; ++firstZeroAt );


    if(target == undefined) throw new Error("Commit specified no target")
    if(author == undefined) throw new Error("Commit specified no author")
    if(committer == undefined) throw new Error("Commit specified no committer")

    return { parents, target, author, committer };
}

export function printHello():void {
    console.log("Hello, world!  This is GitUtil");
}
