#!/usr/bin/env deno run
// Help you debug your shell argument quoting system
for( const arg of Deno.args ) {
    console.log(arg);
}
