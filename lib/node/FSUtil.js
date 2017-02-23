"use strict";

module.exports = (function() {

const fs = require('fs');

function stat( file ) {
	return new Promise( (resolve,reject) => {
		fs.stat(file, (err,stats) => {
			if( err ) reject(err);
			else resolve(stats);
		})
	});
}

function readDir( dir ) {
	return new Promise( (resolve,reject) => {
		fs.readdir( dir, (err,files) => {
			if( err ) return reject(err);
			return resolve(files);
		});
	});
}

function rmDir( dir ) {
	return new Promise( (resolve,reject) => {
		fs.rmdir(dir, (err) => {
			if( err ) reject(err);
			else resolve();
		})
	});
}

function unlink( file ) {
	return new Promise( (resolve,reject) => {
		fs.unlink(file, (err) => {
			if( err ) reject(err);
			else resolve();
		})
	});
}

function rmRf( fileOrDir ) {
	if( typeof fileOrDir == 'object' ) {
		return Promise.all(fileOrDir.map(rmRf));
	}
	return stat(fileOrDir).then( (stats) => {
		if( stats.isDirectory() ) {
			return readDir(fileOrDir).then( (files) => {
				let promz = [];
				for( let i in files ) {
					promz.push(rmRf( fileOrDir+"/"+files[i] ));
				}
				return Promise.all(promz);
			}).then( () => rmDir(fileOrDir) );
		} else {
			return unlink(fileOrDir);
		}
	}, (err) => {
		if( err.code === 'ENOENT' ) return;
		else return Promise.reject(err);
	});	
}

function cp( src, dest ) {
	return new Promise( (resolve,reject) => {
		let rd = fs.createReadStream(src);
		rd.on('error', reject);
		let wr = fs.createWriteStream(dest);
		wr.on('error', reject);
		wr.on('close', () => resolve() );
		rd.pipe(wr);
	});
}

function mkdir( dir ) {
	return new Promise( (resolve,reject) => {
		fs.mkdir( dir, (err) => {
			if( err && err.code !== 'EEXIST' ) {
				reject(err);
			} else {
				resolve();
			}
		});
	});
}

function cpR( src, dest ) {
	return stat(src).then( (srcStat) => {
		if( srcStat.isDirectory() ) {
			let cpPromise = mkdir(dest);
			return readDir(src).then( (files) => {
				for( let f in files ) {
					cpPromise = cpPromise.then( () => {
						cpR( src+"/"+files[f], dest+"/"+files[f] );
					});
				};
				return cpPromise;
			});
		} else {
			return cp( src, dest );
		}
	});
}

function cpRReplacing( src, dest ) {
	return rmRf( dest ).then( () => cpR(src,dest) );
}

function mtimeR( fileOrDir ) {
	return stat(fileOrDir).then( (stats) => {
		if( stats.isFile() ) {
			return stats.mtime;
		} else if( stats.isDirectory() ) {
			return readDir(fileOrDir).then( (files) => {
				let mtimePromz = [];
				for( let f in files ) {
					let fullPath = fileOrDir+"/"+files[f];
					mtimePromz.push(mtimeR(fullPath));
				}
				return Promise.all(mtimePromz).then( (mtimes) => {
					let maxMtime = stats.mtime;
					for( let m in mtimes ) {
						if( mtimes[m] != undefined && mtimes[m] > maxMtime ) {
							maxMtime = mtimes[m];
						}
					}
					return maxMtime;
				});
			});
		} else {
			return Promise.reject(new Error(fileOrDir+" is neither a regular file or a directory!"));
		}
	}, (err) => {
		if( err.code == 'ENOENT' ) return undefined;
		return Promise.reject(new Error("Failed to stat "+fileOrDir+": "+JSON.stringify(err)));
	});
}

return {
	stat,
	readDir,
	mtimeR,
	rmDir,
	rmRf,
	cp,
	cpR,
	cpRReplacing,
}

})();
