
# Synopse mORMot Packages

We are providing two packages:

- `mormot_base`: Core units needed by mORMot
  - Implement ORM, SOA and MVC features
  - ORM via static-linked SQLite3
  - ORM over external SQL and MongoDB
  - High-level Domain-Driven-Design units
- `mormot_cross`: Stand-alone package, client-side only, but should be running on all FPC targets

## Lazarus

Initially these Packages were designed to compile into Lazarus.

The `mormot_base` package has just one dependency, disabled by default, which is [ZeosLib](https://sourceforge.net/projects/zeoslib/).

If you want to use ZeosLib, you must setup the package before compile it, follwoing instructions below:

1. Open the Package
1. Click on Options
1. In "Compile Options", click on "Custom Options"
1. Click on "Defines" and uncheck `NOSYNDBZEOS`
1. Save and return to Package
1. Compile

If you have compiled without using this option before, follow the steps above, but using "More > Recompile Clean" option to recompile the package.

If `NOSYNDBZEOS` is defined, `SynDBZeos.pas` unit will be just an "empty unit".

## Delphi

We are not using Packages on Delphi.

To install mORMot on it, see the documentation [here](https://synopse.info/files/html/Synopse%20mORMot%20Framework%20SAD%201.18.html#TITLE_677).