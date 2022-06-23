// @ts-check

import * as readline from 'node:readline';
import * as process from 'node:process';
import * as fs from 'node:fs/promises';

/**
 * @typedef {Object} HandleInputOptions
 * @property {NodeJS.ReadStream} input
 * @property {NodeJS.WriteStream} output
 */

/**
 * @typedef {Object} HandleInputStats
 * @property {number} processedFileCount
 * @property {number} processedLineCount
 * @property {number} processedInsertCount
 * @property {number} emittedInsertCount
 * @property {number} emittedValueCount
 * @property {number} passedLineCount
 */

class BulkInsertificator {
	/**
	 * @param {NodeJS.WritableStream} output
	 */
	constructor( output ) {
		/** @member {NodeJS.WritableStream} output */
		this.output = output;

		/** @member {string|undefined} currentColumnList */
		this.currentColumnList = undefined;
		/** @member {string|undefined} currentTableName */
		this.currentTableName = undefined;
		/** @member {"newline"|"post-value"} state */
		this.state = "newline";

		/** @member {HandleInputStats} stats */
		this.stats = {
			processedFileCount: 0,
			processedLineCount: 0,
			processedInsertCount: 0,
			emittedInsertCount: 0,
			emittedValueCount: 0,
			passedLineCount: 0,
		};
	}

	/**
	 * @param {string} line
	 * @returns Promise<void>
	 */
	#write( line ) {
		this.output.write(line);
		return Promise.resolve();

		return new Promise( (resolve,reject) => {
			if( this.output.write(line, (err) => {
				if( err ) reject(err);
			}) ) {
				resolve(undefined);
			} else {
				this.output.once('drain', () => {
					resolve(undefined);
				});
			}
		});
	}

	async #flushSection() {
		if( this.state == "post-value" ) {
			await this.#write(";\n");
			this.state = "newline";
		}
	}	

	/**
	 * 
	 * @param {string} line
	 * @return {Promise<void>}
	 */
	async processLine( line ) {
		//await this.#write(`-- Processing, state=${this.state}: ${line}\n`);
		++this.stats.processedLineCount;
		let m;
		if( (m = /^INSERT INTO (\w+) \(([^\)]*)\) VALUES \(([^\)]*)\);$/.exec(line)) != null ) {
			++this.stats.processedInsertCount;
			const tableName = m[1];
			const columnList = m[2];
			const value = m[3];
			if( this.state == "post-value" && this.currentColumnList == columnList && this.currentTableName == tableName ) {
				this.output.write(`,\n(${value})`);
				this.state = "post-value";
				++this.stats.emittedValueCount;
			} else {
				await this.#flushSection();
				//await this.#write(`-- ${columnList} did not match ${this.currentColumnList}; opening new section\n`);
				await this.#write(`INSERT INTO ${tableName}\n(${columnList}) VALUES\n(${value})`);
				this.state = "post-value";
				this.currentColumnList = columnList;
				this.currentTableName = tableName;
				++this.stats.emittedInsertCount;
				++this.stats.emittedValueCount;
			}
		} else {
			await this.#flushSection();
			await this.#write(line + "\n");
			++this.stats.passedLineCount;
		}
	}

	close() {
		return this.#flushSection().then( () => this.stats );
	}

	async processFile(filename) {
		++this.stats.processedFileCount;
		/** @var {NodeJS.ReadStream} input */
		let input;
		if( filename == "-" ) {
			input = process.stdin;
		} else {
			const inputFh = await fs.open(filename);
			input = inputFh.createReadStream();
		}
		const rl = readline.createInterface({
			input,
			terminal: false,
		});
		let lineProm = Promise.resolve();
		rl.on('line', (line) => {
			lineProm = lineProm.then(() => this.processLine(line));
		});
		return new Promise( (resolve,reject) => {
			rl.on('close', async () => {
				this.#flushSection();
				this.#write(`-- Finished reading ${filename}\n`)
				input.close();
				rl.close();
				resolve(lineProm);
			});
			rl.on('SIGINT', () => reject(new Error("SIGINT")));
		});
	}
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

	for( let i=2; i<process.argv.length; ++i ) {
		const arg = process.argv[i];
		if( optionsDone || !arg.startsWith("-") || arg == "-" ) {
			filesToProcess.push(arg);
		} else if( arg == "--show-stats" ) {
			showStats = true;
		} else if( arg == '--' ) {
			optionsDone = true;
		} else if( arg == '--help' ) {
			console.log(`${selfName}: Turn mulitiple INSERT INTO sometable (x,y,z) VALUES (1,2,3)`)
			console.log(`into one big insert, passing everything else through unchanged.`);
			console.log();
			console.log(`Usage: ${selfName} [--show-stats] <input file(s)...>`)
			console.log();
			console.log(`Zero or more input files may be indicated.  Use '-' to read from standard input.`);
			process.exit(0);
		} else {
			console.error(`${selfName}: Error: Unrecognized option: ${arg}`);
			process.exit(1);
		}
	}

	if( filesToProcess.length == 0 ) {
		console.warn(`No input files specified; use '-' to indicate standard input`);
	}

	const output = process.stdout;

	const bulkInsertificator = new BulkInsertificator(output);
	for( const inputFilename of filesToProcess ) {
		await bulkInsertificator.processFile(inputFilename);
	}
	const stats = await bulkInsertificator.close();

	if( showStats ) {
		output.write(`-- bulk-insertify processing stats:\n`);
		output.write(`-- Files read:           ${leftPad(stats.processedFileCount         , 10)}\n`);
		output.write(`-- Lines read:           ${leftPad(stats.processedLineCount         , 10)}\n`);
		output.write(`-- Inserts read:         ${leftPad(stats.processedInsertCount       , 10)}\n`);
		output.write(`-- Inserts emitted:      ${leftPad(stats.emittedInsertCount    , 10)}\n`);
		output.write(`-- Values emitted:       ${leftPad(stats.emittedValueCount     , 10)}\n`);
		output.write(`-- Lines passed through: ${leftPad(stats.passedLineCount, 10)}\n`);
	}
}
