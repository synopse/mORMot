/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

let {coreModulesPath, runInThisContext} = process.binding('modules'),
    {relToAbs,loadFile} = process.binding('fs'),
    Module;


// process & module function is documented inside _UBCommonGlobals.js
function startup() {
    /**
     * Current working directory
     * @return {string|String}
     */
    process.cwd = function () {
        return process.startupPath;
    };
    /**
     * List of loaded via `require` modules
     * @private
     * @type {Array<string>}
     */
    process.moduleLoadList = [];
    /**
     * The main executable full path (excluding .exe file name)
     * @type {String}
     * @readonly
     */
    process.execPath = process.binPath;

    Module = NativeModule.require('module');
    Module.call(global, ['.']);
    process.mainModule = global;

//noinspection JSUndeclaredVariable
    /**
     * Load a module. Acts like a <a href="http://nodejs.org/api/modules.html">Node JS</a> require, with 4 difference:
     *
     *   - **Core modules** form NODE_CORE_MODULES always loaded from `coreModulesPath\node_modules` folder
     *   - In case of `moduleName` is **RELATIVE** (not start from `./` `../` or `X:\` where X is drive letter) lookup order is:
     *      - `currentModulePath + \modules`
     *      - `currentModulePath + \node_modules`
     *      - process.execPath + 'node_modules'
     *	    - process.startupPath + 'node_modules'
     *   - in case we run in production mode (`!process.isDebug`) and minimized version of main module exists, it will be loaded.
     *     By "minimized version" we mean package.json `main` entry with `.min.js` extension <br>
     *
     *  *In case you need to debug from there module is loaded set OS Environment variable*
     *  `>SET NODE_DEBUG=modules` *and restart server - require will put to debug log all information about how module are loaded.* Do not do this on production, of course :)
     *
     * @global
     * @method
     * @param {String} moduleName
     * @returns {*}
     */
    global.require = Module.prototype.require;
    global.Buffer = NativeModule.require('buffer').Buffer;
    //global.clearTimeout = function() {};

    global.sleep = process.binding('syNode').sleep;

    const WindowTimer =  NativeModule.require('polyfill/WindowTimer');
    global._timerLoop = WindowTimer.makeWindowTimer(global, function (ms) { global.sleep(ms); });
    /**
     * This function is just to be compatible with node.js
     * @param {Function} callback Callback (called immediately in SyNode)
     */
    process.nextTick = function(callback, arg1, arg2, arg3){
		if (typeof callback !== 'function') {
			throw new TypeError('"callback" argument must be a function');
		}
        // on the way out, don't bother. it won't get fired anyway.
        if (process._exiting)
            return;

        var i, args;

		switch (arguments.length) {
		// fast cases
		case 1:
		  break;
		case 2:
		  args = [arg1];
		  break;
		case 3:
		  args = [arg1, arg2];
		  break;
		default:
		  args = [arg1, arg2, arg3];
		  for (i = 4; i < arguments.length; i++)
			args[i - 1] = arguments[i];
		  break;
		}
        global._timerLoop.setTimeoutWithPriority.apply(undefined, [callback, 0, -1].concat(args));
    };

    global.setImmediate = function(callback, arg1, arg2, arg3){
	  if (typeof callback !== 'function') {
		throw new TypeError('"callback" argument must be a function');
	  }

	  var i, args;

	  switch (arguments.length) {
		// fast cases
		case 1:
		  break;
		case 2:
		  args = [arg1];
		  break;
		case 3:
		  args = [arg1, arg2];
		  break;
		default:
		  args = [arg1, arg2, arg3];
		  for (i = 4; i < arguments.length; i++)
			args[i - 1] = arguments[i];
		  break;
	  }
      global._timerLoop.setTimeoutWithPriority.apply(undefined, [callback, 0, 1].concat(args));
    };

}


function NativeModule(id) {
    this.filename = id + '.js';
    this.id = id;
    this.exports = {};
    this.loaded = false;
}

const NODE_CORE_MODULES = ['fs', 'util', 'path', 'assert', 'module', 'console', 'events','vm',
 'net', 'os', 'punycode', 'querystring', 'timers', 'tty', 'url', 'child_process', 
 'buffer', 'string_decoder', 'internal/util', 'stream', '_stream_readable', '_stream_writable', 
 'internal/streams/BufferList', '_stream_duplex', '_stream_transform', '_stream_passthrough',
 'polyfill/WindowTimer']; 

NativeModule._source = {};
NODE_CORE_MODULES.forEach( (module_name) => { 
  NativeModule._source[module_name] = relToAbs(coreModulesPath, `.\\node_modules\\${module_name}.js`) 
});

NativeModule._cache = {};

NativeModule.require = function (id) {
    if (id == 'native_module') {
        return NativeModule;
    }

    var cached = NativeModule.getCached(id);
    if (cached) {
        return cached.exports;
    }

    if (!NativeModule.exists(id)) {
        throw new Error('No such native module ' + id);
    }

    process.moduleLoadList.push('NativeModule ' + id);

    var nativeModule = new NativeModule(id);

    nativeModule.cache();
    nativeModule.compile();

    return nativeModule.exports;
};

NativeModule.getCached = function (id) {
    if (NativeModule._cache.hasOwnProperty(id)) {
        return NativeModule._cache[id]
    } else {
        return null;
    }
};

NativeModule.exists = function (id) {
    return NativeModule._source.hasOwnProperty(id);
};

NativeModule.getSource = function (id) {
    return loadFile(NativeModule._source[id]);
};

NativeModule.wrap = function (script) {
    return NativeModule.wrapper[0] + script + NativeModule.wrapper[1];
};

NativeModule.wrapper = [
    '(function (exports, require, module, __filename, __dirname) { ', '\n});'
];

NativeModule.prototype.compile = function () {
    var source = NativeModule.getSource(this.id);
    source = NativeModule.wrap(source);

    var fn = runInThisContext(source, this.filename, true);
    fn(this.exports, NativeModule.require, this, this.filename);

    this.loaded = true;
};

NativeModule.prototype.cache = function () {
    NativeModule._cache[this.id] = this;
};

startup();
///patch ModuleLoader