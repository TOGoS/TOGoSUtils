// @ts-check

import * as readline from 'node:readline';
import * as process from 'node:process';

/**
 * @typedef {Object} HandleInputOptions
 * @property {NodeJS.ReadStream} input
 * @property {NodeJS.WriteStream} output
 */

/**
 * @typedef {Object} HandleInputStats
 * @property {number} linesReadCount
 * @property {number} insertsReadCount
 * @property {number} insertsEmittedCount
 * @property {number} valuesEmittedCount
 * @property {number} linesPassedThroughCount
 */

/**
 * 
 * @param {HandleInputOptions} opts
 * @returns {Promise<HandleInputStats>}
 */
function handleInput(opts) {
	const output = opts.output;
	return new Promise( (resolve,reject) => {
		/** @var {HandleInputStats} stats */
		const stats = {
			linesReadCount: 0,
			insertsReadCount: 0,
			insertsEmittedCount: 0,
			valuesEmittedCount: 0,
			linesPassedThroughCount: 0,
		};
		
		/** @var {string|undefined} currentColumnList */
		let currentColumnList = undefined;
		/** @var {string|undefined} */
		let currentTableName = undefined;
		/** @var {"newline"|"post-value"} state */ 
		let state = "newline";

		state = "asd";

		currentTableName = 32;

		function flushSection() {
			if( state == "post-value" ) {
				output.write(";\n");
				state = "newline";
			}
		}

		const rl = readline.createInterface({
			...opts,
			terminal: false,
		});
		rl.on('line', (line) => {
			let m;
			if( (m = /^INSERT INTO (\w+) \(([^\)]*)\) VALUES \(([^\)]*)\);$/.exec(line)) != null ) {
				const tableName = m[1];
				const columnList = m[2];
				const value = m[3];
				if( state == "post-value" && currentColumnList == columnList && currentTableName == tableName ) {
					output.write(`,\n(${value})`);
					++stats.valuesEmittedCount;
				} else {
					flushSection();
					output.write(`-- ${columnList} did not match ${currentColumnList}; opening new section`);
					output.write(`INSERT INTO\n${columnList} VALUES\n(${value})`);
					state = "post-value";
					currentColumnList = columnList;
					currentTableName = tableName;
					++stats.insertsEmittedCount;
					++stats.valuesEmittedCount;
				}
			} else {
				flushSection();
				output.write(line + "\n");
				++stats.linesPassedThroughCount;
			}
		});
		rl.on('close', () => {
			flushSection();
			resolve(stats);
		});
		rl.on('SIGINT', () => reject(new Error("SIGINT")));
	});
}

function leftPad(str, len, padding=" ") {
	str = ""+str;
	while( str.length < len ) {
		str = padding+str;
	}
	return str;
}

/**
 * @param {string} url
 * @returns {string}
 */
function basename(url) {
	const m = /([^\/]*)$/.exec(url);
	return m === null ? url : m[1];
}

if( /* import.meta.main */ true /* why isn't this a thing yet: https://github.com/nodejs/node/blob/main/doc/api/modules.md#accessing-the-main-module */ ) {
	let showStats = false;
	let optionsDone = false;
	const filesToProcess = [];
	const selfName = basename(import.meta.url);

	for( const arg of process.argv ) {
		if( optionsDone || !arg.startsWith("-") || arg == "-" ) {
			console.error(`${selfName}: Error: Input files not yet supported`);
			process.exit(1);
			filesToProcess.push(arg);
		} else if( arg == "--show-stats" ) {
			showStats = true;
		} else if( arg == '--' ) {
			optionsDone = true;
		} else {
			console.error(`${selfName}: Error: Unrecognized option: ${arg}`);
			process.exit(1);
		}
	}

	const input = process.stdin;
	const output = process.stdout;
	const stats = await handleInput({
		input,
		output,
	});

	if( showStats ) {
		output.write(`-- bulk-insertify processing stats:\n`);
		output.write(`-- Lines read:           ${leftPad(stats.linesReadCount         , 10)}\n`);
		output.write(`-- Inserts read:         ${leftPad(stats.insertsReadCount       , 10)}\n`);
		output.write(`-- Inserts emitted:      ${leftPad(stats.insertsEmittedCount    , 10)}\n`);
		output.write(`-- Values emitted:       ${leftPad(stats.valuesEmittedCount     , 10)}\n`);
		output.write(`-- Lines passed through: ${leftPad(stats.linesPassedThroughCount, 10)}\n`);
	}
}
