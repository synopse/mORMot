SynTaskDialog for Lazarus
=========================

Patched version by Ondrej Pokorny (reddwarf), adding Lazarus support to the SynTaskDialog.pas unit.

TaskDialog display would be native on Windows Vista+, emulated on all other platforms - Windows, Linux and OSX tested.

Added Features
--------------

* Emulated display using LCL;
* added external translation function for the emulated dialog (`TaskDialog_Translate`);
* `tdfAllowDialogCancellation` handled in emulated dialog: if not set, Alt+F4 is blocked; if set: Esc is allowed;
* `tdfPositionRelativeToWindow` handled in emulated dialog


Remarks
-------

Platform-independent icons are from www.iconsdb.com:
    Icon license:
      This icon is provided as CC0 1.0 Universal (CC0 1.0) Public Domain
      Dedication.
      You can copy, modify, use, distribute this icon, even for commercial
      purposes, all without asking permission with no attribution required,
      but always appreciated.

Maybe To-Do: High DPI-aware emulated dialog + icons.

