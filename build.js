#!/usr/bin/env node

"use strict";

const _builder = require('./src/lib/node/Builder');
const builder = new _builder.default();

builder.targets = {
    "default": {
        prereqs: ["js-libs"]
    },
    "js-libs": {
        prereqs: ["node_modules/togosutils"]
    },
    "node_modules/typescript": {
        isDir: true,
        prereqs: ["package.json"],
        invoke: ctx => {
            return ctx.builder.npm(["install"]);
        }
    },
    "node_modules/togosutils": {
        isDir: true,
        prereqs: ["node_modules/typescript"],
        invoke: ctx => {
            return ctx.builder.tsc(["-p", "src/togosutils/ts/tsconfig.json","--module","commonjs","--outDir",ctx.targetName]);
        }
    },
    "hello": {
        invoke: ctx => { console.log("Hello, world!"); }
    }
}

builder.globalPrereqs = ["build.js", "src/lib/node/Builder.js"];
builder.processCommandLineAndSetExitCode(process.argv.slice(2));
