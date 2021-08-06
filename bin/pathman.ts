#!/usr/bin/env deno

const pathSepChar = Deno.build.os == "windows" ? ";" : ":";

function getPathsFromEnv() {
   let pathString:string = Deno.env.get("PATH") ?? "";
   if( pathString == "" ) return [];
   let separator:RegExp = new RegExp(pathSepChar, "g");
   return pathString.split(separator);
}

function dedupe(inputs:string[]) {
   let theHash:{[k:string]:string} = {};
   let deduped:string[] = [];
   for( let s of inputs ) {
      if( theHash[s] == undefined ) {
	 deduped.push(s);
	 theHash[s] = s;
      }
   }
   return deduped;
}

let paths:string[] = [];

for( let arg of Deno.args ) {
   if( arg == "--from-env" ) {
      paths = paths.concat(getPathsFromEnv());
   } else if( arg == "--dedup" ) {
      paths = dedupe(paths);
   } else if( arg == "--emit-lines" ) {
      for( let path of paths ) {
	 console.log(path);
      }
   } else if( arg == "--emit-bat" ) {
      console.log("@set \"PATH="+paths.join(pathSepChar)+"\"");
   } else if( arg == "--help" ) {
      console.log("About: a tool to help manage your $PATH variable");
      console.log("");
      console.log("Usage: pathman <op> ...");
      console.log("Ops read/write an internal list of paths which is initially empty, and");
      console.log("are executed in the order they are specified.");
      console.log("");
      console.log("Ops:");
      console.log("  <path>       ; add an arbitrary path to the list");
      console.log("  --from-env   ; load paths from environment (i.e. $PATH or %Path%)");
      console.log("  --dedup      ; remove duplicate path entries");
      console.log("  --emit-lines ; dump paths, one-per-line");
      console.log("  --emit-bat   ; emit a single-line batch file to set the path");
   } else if( /^-.*/.exec(arg) == null ) {
      paths.push(arg);
   } else {
      console.error("Unrecognized option: "+arg);
      Deno.exit(1);
   }
}
