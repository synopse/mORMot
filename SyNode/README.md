#  The server-side JavaScript execution using the SpiderMonkey library with nodeJS modules support

- win32/64 target (FPC 3.1.1, Delphi), linux 64 (FPC 3.1.1) 
- based on SpiderMonkey52, almost full support of ES6
- a remote debugger protocol, can be debugged remotely using Firefox - see `SyNode\Samples\02 - Bindings`
- CommonJS modules, compatible with NPM modules
- nairve modules can be implemented using Delphi/FPC (as a dll/so) - see `SyNode\Samples\01 - Dll Modules`
- JavaScript prototype definition based on Delphi RTTI (supported both "new" and old)

## SpiderMonkey library 

### SpiderMonkey 52 (recommended)

Precompiled binary can be downloaded here:

  - x32: https://unitybase.info/downloads/mozjs-52x32dlls.zip
  - x64: https://unitybase.info/downloads/mozjs-52x64dlls.zip
  - Linux x64: http://unitybase.info/media/files/libmozjs-52.zip
  - Linux 64 (glibc2.17): http://unitybase.info/media/files/libmozjs-52-glibc2-17.zip 
 
Or compiled from sources as described [in  instructions inside mozjs folder](/mozjs)

### SpiderMonkey 45 (not supported)

Precompiled binary can be downloaded here:

 - x32: https://unitybase.info/downloads/mozjs-45.zip
 - x64: https://unitybase.info/downloads/mozjs-45-x64.zip
  
