"use strict";

module.exports = (function() { 

const child_process = require('child_process');
const fs = require('fs');
const fsutil = require('./FSUtil');
const mtimeR = fsutil.mtimeR;
const rmRf = fsutil.rmRf;

// To support pre-...syntax node
function append(arr1, arr2) {
	for( let i=0; i<arr2.length; ++i ) arr1.push(arr2[i]);
	return arr1;
}

function concat(arr1, arr2) {
	return append(append([], arr1), arr2);
}

/**
 * Escape program arguments to represent as a command
 * that could be run at the shell.
 * For displaying to humans.
 * Don't actually run at the shell because escaping is probably imperfect.
 */
function argsToShellCommand( args ) {
	if( typeof args === 'string' ) return args;
	let escaped = [];
	for( let i in args ) {
		let arg = args[i];
		if( arg.match(/^[a-zA-Z0-9\/\.\+_\-]+$/) ) escaped.push(arg);
		else escaped.push( '"'+arg.replace(/["\$\\]/g,'\\$&')+'"');
	}
	return escaped.join(' ');
}

function toSet( arr, into ) {
	if( into == undefined ) into = {};
	for( let x in arr ) into[arr[x]] = arr[x];
	return into;
}

// Object-Oriented Yeahhhh!!!!

let Builder = function(targets) {
	this.targets = targets || {};
	this.buildPromises = {};
	this.globalPrereqs = [];
};

Builder.prototype.touch = function( fileOrDir ) {
	this.logger.log("Touching "+fileOrDir);
	let curTime = Date.now()/1000;
	return new Promise( (resolve,reject) => {
		fs.utimes(fileOrDir, curTime, curTime, (err) => {
			if( err ) return reject(err);
			else resolve();
		})
	});
};

Builder.prototype.processCmd = function( args ) {
	if( typeof args === 'string' ) {
		return this.figureShellCommand().then( (prefix) => {
			return concat(prefix, [args]);
		});
	} else {
		return Promise.resolve(args);
	}
}

Builder.prototype.doCmd = function( args, opts ) {
	if( !opts ) opts = {};
	let stdio = opts.stdio || 'inherit';
	
	return this.processCmd(args).then( (args) => {
		let argStr = argsToShellCommand(args);
		this.logger.log("+ "+argStr);
		return new Promise( (resolve,reject) => {
			let cproc;
			if( typeof args === 'string' ) {
				cproc = child_process.spawn( args, [], {
					shell: true,
					cwd: opts.cwd,
					stdio
				} );
			} else {
				cproc = child_process.spawn( args[0], args.slice(1), {
					cwd: opts.cwd,
					stdio
				} );
			}
			cproc.on('error', reject);
			cproc.on('close', (exitCode) => {
				if( opts.onNz == 'return' ) resolve(exitCode);
				else if( exitCode == 0 ) resolve(0);
				else reject(new Error("Process exited with code "+exitCode+": "+argStr));
			});
		});
	});
}

Builder.prototype._findWorkingProgram = function(alternatives, testPostfix, start) {
	if( start == undefined ) start = 0;
	if( start >= alternatives.length ) return Promise.reject("Couldn't figure out how to run shell!");
	let testCommand = concat(alternatives[start], testPostfix);
	return this.doCmd(testCommand).then( () => {
		return alternatives[start];
	}, (err) => {
		this.logger.log(argsToShellCommand(testCommand)+" didn't work; will try something else...")
		return this._findWorkingProgram(alternatives, testPostfix, start+1);
	})
}

Builder.prototype.shellCommandPromise = undefined;
Builder.prototype.figureShellCommand = function() {
	if( this.shellCommandPromise ) return this.shellCommandPromise;
	
	let alternatives = [
		['cmd.exe', '/c'], // Windoze!
		['sh', '-c'], // Unix!
	];
	
	return this.shellCommandPromise = this._findWorkingProgram(alternatives, ['exit 0']);
}

Builder.prototype.npmCommandPromise = undefined;
Builder.prototype.figureNpmCommand = function() {
	if( this.npmCommandPromise ) return this.npmCommandPromise;
	
	// Most non-Windows systems will look at the path for us,
	// so 'npm' should be sufficient.
	if( process.platform != 'win32' ) return this.npmCommandPromise = Promise.resolve(['npm']); 
	
	// Not so on windows!
	// We'll look for npm-cli.js by iterating over everything in %Path%
	
	let alternatives = [
		['npm'],
	];
	
	let envPath = process.env.Path;
	let envPaths = (envPath != '' && envPath != undefined) ? envPath.split(';') : [];
	let leftToCheck = envPaths.length;
	let findNpmCliJsPromises = leftToCheck == 0 ? Promise.resolve() : new Promise( (resolve,reject) => {
		for( let p in envPaths ) {
			let npmCliJsPath = envPaths[p]+'/node_modules/npm/bin/npm-cli.js';
			fs.stat(npmCliJsPath, (err,stats) => {
				if( !err ) alternatives.push( ['node', npmCliJsPath] );
				if( --leftToCheck == 0 ) resolve();
			});
		}
	});
	
	return this.npmCommandPromise = findNpmCliJsPromises.then( () => this._findWorkingProgram(alternatives, ['-v']) );
}

Builder.prototype.npm = function( args ) {
	return this.figureNpmCommand().then( (npmCmd) => this.doCmd(concat(npmCmd, args)) );
}

Builder.prototype.tsc = function( args ) {
	return this.doCmd(concat(["node","node_modules/typescript/bin/tsc"], args));
}

Builder.prototype.targets = {};
Builder.prototype.fetchGeneratedTargets = function() { return Promise.resolve({}) };

Builder.prototype.allTargetsPromise = undefined;
Builder.prototype.fetchAllTargets = function() {
	if( this.allTargetsPromise ) return this.allTargetsPromise;
	
	let allTargets = {};
	for( let n in this.targets ) allTargets[n] = this.targets[n];
	return this.allTargetsPromise = this.fetchGeneratedTargets().then( (generatedTargets) => {
		for( let n in generatedTargets ) allTargets[n] = generatedTargets[n];
		return allTargets;
	});
}

Builder.prototype.fetchTarget = function( targetName ) {
	return this.fetchAllTargets().then( (targets) => targets[targetName] );
}

Builder.prototype.getTargetPrereqSet = function( target ) {
	let set = {}
	if( target.prereqs ) toSet(target.prereqs, set);
	if( target.getPrereqs ) toSet(target.getPrereqs(), set);
	toSet(this.globalPrereqs, set);
	return set;
}

Builder.prototype.buildTarget = function( target, targetName, stackTrace ) {
	let targetMtimePromise = mtimeR(targetName);
	let prereqNames = target.prereqs || []; // TODO: should use the same logic as
	if( prereqNames.length == 0 ) {
		this.logger.log(targetName+" has no prerequisites");
	} else {
		this.logger.log(targetName+" has "+prereqNames.length+" prerequisites: "+prereqNames.join(', '));
	}
	let prereqSet = this.getTargetPrereqSet(target);
	let prereqStackTrace = stackTrace.concat( targetName )
	let latestPrereqMtime = undefined;
	let prereqAndMtimePromz = [];
	for( let prereq in prereqSet ) {
		prereqAndMtimePromz.push(this.build( prereq, prereqStackTrace ).then( () => {
			return mtimeR(prereq).then( (mt) => [prereq, mt] );
		}));
	}
	
	return targetMtimePromise.then( (targetMtime) => {
		return Promise.all(prereqAndMtimePromz).then( (prereqsAndMtimes) => {
			let needRebuild;
			if( targetMtime == undefined ) {
				this.logger.log("Mtime of "+targetName+" is undefined; need rebuild!");
				needRebuild = true;
			} else {
				needRebuild = false;
				for( let m in prereqsAndMtimes ) {
					let prereqAndMtime = prereqsAndMtimes[m];
					let prereqName = prereqAndMtime[0];
					let prereqMtime = prereqAndMtime[1];
					if( prereqMtime == undefined || targetMtime == undefined || prereqMtime > targetMtime ) {
						this.logger.log("OUT-OF-DATE: "+prereqName+" is newer than "+targetName+"; need to rebuild ("+prereqMtime+" > "+targetMtime+")");
						needRebuild = true;
					} else {
						this.logger.log(prereqName+" not newer than "+targetName+" ("+prereqMtime+" !> "+targetMtime+")");
					}
				}
			}
			if( needRebuild ) {
				this.logger.log("Building "+targetName+"...");
				if( target.invoke ) {
					let prom = target.invoke({
						builder: this,
						prereqNames,
						targetName,
					}).then( () => {
						this.logger.log("Build "+targetName+" complete!");
						if( target.isDirectory ) {
							return this.touch(targetName);
						}
					}, (err) => {
						console.error("Error trace: "+stackTrace.join(' > ')+" > "+targetName);
						if( !target.keepOnFailure ) {
							console.error("Removing "+targetName);
							return rmRf(targetName);
						}
					});
					return prom;
				} else {
					this.logger.log(targetName+" has no build rule; assuming up-to-date");
				}
			} else {
				this.logger.log(targetName+" is already up-to-date");
				return Promise.resolve();
			}
		});
	}).then( () => {
		if( target.isDirectory || target.isFile ) {
			return fsutil.stat(targetName).then( stat => {
				if( target.isDirectory && !stat.isDirectory() ) {
					throw new Error(targetName+" expected to be directory, but is not!");
				}
				if( !target.isDirectory && stat.isDirectory() ) {
					throw new Error(targetName+" expected to be regular file, but is directory!");
				}
			})
		}
	})
}

Builder.prototype.build = function( targetName, stackTrace ) {
	if( this.buildPromises[targetName] ) return this.buildPromises[targetName];
	
	return this.buildPromises[targetName] = this.fetchTarget(targetName).then( (targ) => {
		if( targ == null ) {
			return new Promise( (resolve,reject) => {
				fs.stat(targetName, (err,stats) => {
					if( err ) {
						reject(new Error(targetName+" does not exist and I don't know how to build it."));
					} else {
						this.logger.log(targetName+" exists but has no build rule; assuming up-to-date");
						resolve();
					}
				});
			});
		} else {
			return this.buildTarget(targ, targetName, stackTrace);
		}
	});
}

Builder.prototype.processCommandLine = function(argv) {
	let buildList = [];
	let operation = 'build';
	let verbosity = 100;
	for( let i=0; i<argv.length; ++i ) {
		let arg = argv[i];
		if( arg == '--list-targets' ) {
			operation = 'list-targets';
		} else if( arg == '-v' ) {
			verbosity = 200;
		} else {
			// Make tab-completing on Windows not screw us all up!
			buildList.push(arg.replace(/\\/,'/'));
		}
	}
	
	if( verbosity >= 200 ) {
		this.logger = {
			log: console.log
		}
	} else {
		this.logger = {
			log: () => {}
		}
	}
	
	if( operation == 'list-targets' ) {
		return this.fetchAllTargets().then( (targets) => {
			for( let n in targets ) console.log(n);
		});
	} else if( operation == 'build' ) {
		if( buildList.length == 0 ) buildList.push('default');
		let buildProms = [];
		for( let i in buildList ) {
			buildProms.push(this.build(buildList[i], ["argv["+i+"]"]));
		}
		return Promise.all(buildProms);
	}
}

Builder.prototype.processCommandLineAndSetExitCode = function(argv) {
	this.processCommandLine(argv).then( () => {
		this.logger.log("Build completed");
	}, (err) => {
		console.error("Error!", err.message, err.stack);
		console.error("Build failed!");
		process.exitCode = 1;
	});
};

// For ES6/CJS compatibility
Builder.default = Builder;
Builder.Builder = Builder;

return Builder;

})();
