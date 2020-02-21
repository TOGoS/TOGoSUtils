"use strict";
var child_process = require("child_process");
var fs = require('fs');
var ExternalProcessRunner = (function () {
	function ExternalProcessRunner() {
		this.logger = { log: function() { } };
	}
	ExternalProcessRunner.prototype.findWorkingProgram = function (alternatives, testPostfix, start, name) {
		var _this = this;
		if (start === void 0) { start = 0; }
		if (name === void 0) { name = "a program"; }
		if (start >= alternatives.length)
			return Promise.reject(new Error("Couldn't figure out how to run " + name + "!"));
		var testCommand = alternatives[start].concat(testPostfix);
		return this.doCmd(testCommand, { silent: true }).then(function () {
			return alternatives[start];
		}, function (err) {
			_this.logger.log(_this.argsToShellCommand(testCommand) + " didn't work; will try something else...");
			return _this.findWorkingProgram(alternatives, testPostfix, start + 1, name);
		});
	};
	ExternalProcessRunner.prototype.argsToShellCommand = function (args) {
		if (typeof args === 'string')
			return args;
		var escaped = [];
		for (var i in args) {
			var arg = args[i];
			if (arg.match(/^[a-zA-Z0-9\/\.\+_\-]+$/))
				escaped.push(arg);
			else
				escaped.push('"' + arg.replace(/["\$\\]/g, '\\$&') + '"');
		}
		return escaped.join(' ');
	};
	ExternalProcessRunner.prototype.processCmd = function (cmd) {
		if (typeof cmd === 'string') {
			return Promise.resolve(['sh', '-c', cmd]);
		}
		return Promise.resolve(cmd);
	};
	ExternalProcessRunner.prototype.doCmd = function (args, opts) {
		var _this = this;
		if (opts === void 0) { opts = {}; }
		if (!opts)
			opts = {};
		var silent = opts.silent == undefined ? true : opts.silent;
		var stdio = opts.stdio || opts.silent ? undefined : 'inherit';
		var onNz = opts.onNz || 'error';
		return this.processCmd(args).then(function (args) {
			var argStr = _this.argsToShellCommand(args);
			_this.logger.log("+ " + argStr);
			return new Promise(function (resolve, reject) {
				var cproc;
				if (typeof args === 'string') {
					cproc = child_process.spawn(args, [], {
						shell: true,
						//silent,
						cwd: opts.cwd,
						stdio: stdio
					});
				}
				else {
					cproc = child_process.spawn(args[0], args.slice(1), {
						cwd: opts.cwd,
						//silent,
						stdio: stdio
					});
				}
				cproc.on('error', reject);
				cproc.on('close', function (exitCode) {
					if (exitCode == 0 || onNz == 'return')
						resolve(exitCode);
					else
						reject(new Error("Process exited with code " + exitCode + ": " + argStr));
				});
			});
		});
	};
	ExternalProcessRunner.prototype.node = function (args, opts) {
		return this.doCmd( ['node'].concat(args), opts );
	}
	ExternalProcessRunner.prototype.figureNpmCommand = function() {
		var _this = this;
		if( this.npmCommandPromise ) return this.npmCommandPromise;
		
		// Most non-Windows systems will look at the path for us,
		// so 'npm' should be sufficient.
		if( process.platform != 'win32' ) return this.npmCommandPromise = Promise.resolve(['npm']); 
		
		// Not so on windows!
		// We'll look for npm-cli.js by iterating over everything in %Path%
		
		var alternatives = [
				['npm'],
		];
		
		var envPath = process.env.Path;
		var envPaths = (envPath != '' && envPath != undefined) ? envPath.split(';') : [];
		var leftToCheck = envPaths.length;
		var findNpmCliJsPromises = leftToCheck == 0 ? Promise.resolve() : new Promise( function(resolve,reject) {
			for( var p in envPaths ) {
				var npmCliJsPath = envPaths[p]+'/node_modules/npm/bin/npm-cli.js';
				fs.stat(npmCliJsPath, function(err,stats) {
					if( !err ) alternatives.push( ['node', npmCliJsPath] );
					if( --leftToCheck == 0 ) resolve();
				});
			}
		});
		return this.npmCommandPromise = findNpmCliJsPromises.then( function() { _this.findWorkingProgram(alternatives, ['-v']) } );
	}
	ExternalProcessRunner.prototype.npm = function (args, opts) {
		var _this = this;
		return this.figureNpmCommand().then( function(npmCommand) {
			return _this.doCmd( npmCommand.concat(args), opts );
		});
	}
	return ExternalProcessRunner;
}());
Object.defineProperty(exports, "__esModule", { value: true });
exports.default = ExternalProcessRunner;
