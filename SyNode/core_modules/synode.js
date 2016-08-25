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
     *   - `require` know about UnityBase **models**. In **server thread** context, in case `moduleName` start from `models/ModelName` require search for module inside `ModelName.path` folder:
     *
     *          require('models/UBS/public/UBReport');
     *
     *     will search in domain config (ubConfig.json) path for `UBS` model and perform request relative to this folder, i.e. load `D:\projects\UnityBase\models\UBS\public\UBReport.js` in my case.
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
}


function NativeModule(id) {
    this.filename = id + '.js';
    this.id = id;
    this.exports = {};
    this.loaded = false;
}

const NODE_CORE_MODULES = ['fs', 'util', 'path', 'assert', 'module', 'console', 'events',
 'net', 'os', 'punycode', 'querystring', 'timers', 'tty', 'url', 'child_process']; 

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