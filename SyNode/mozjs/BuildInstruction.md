<!-- Build instruction for SpiderMonkey45 for Windows to use with SyNode -->

###Preparation
* Download and install **MozillaBuild**. See instruction here [MozillaBuild](https://developer.mozilla.org/en-US/docs/Mozilla/Developer_guide/Build_Instructions/Windows_Prerequisites#mozillabuild)

* Get Mozilla Source Code from here https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Releases/45

* Apply patches.(todo: make patch)
 
## Build SpiderMonkey 45
Follow instruction from [Mozilla Build Documentation](https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Build_Documentation)
The valid options for configure is:

	../configure --enable-ctypes --disable-jemalloc

