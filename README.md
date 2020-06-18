[<img src="cyclone-winds.png" alt="cyclone-winds">](http://github.com/cyclone-scheme/cyclone-winds)

# cyclone-winds
Package manager for [Cyclone Scheme](https://cyclone-scheme.org).

The official list of packages is on the [wiki](https://github.com/cyclone-scheme/cyclone-winds/wiki).

## Introduction

A "package" for `cyclone-winds` is wrapper around libraries and/or programs. They are extensions to the core implemenation of Cyclone Scheme. 

## Installation

    git clone https://github.com/cyclone-scheme/cyclone-winds.git
    cd cyclone-winds
    make
    sudo make install

*Note that `gmake` should be used instead of `make` on FreeBSD 12 (install it with `sudo pkg install gmake`).*

Optionally it is possible to pass `PREFIX` to set another destination directory.

    make 
    sudo make PREFIX=/home/joe/cyclone-winds install
    /home/joe/cyclone-winds/bin/cyclone-winds --help
   
## Usage

```
$ cyclone-winds [-v] COMMAND [PACKAGES]
```
    
    COMMANDS:
  
      COMMON USE:
      help  -  print usage
      retrieve PACKAGE [...]  - downloads and extracts specified PACKAGE(s)
      install PACKAGE [...] - retrieve and install specified PACKAGE(s)
      reinstall PACKAGE [...] - retrieve and reinstall specified PACKAGE(s)
      upgrade [PACKAGE ...] - upgrade all installed packages or specified PACKAGE(s)
      uninstall PACKAGE [...] - remove specified PACKAGE(s)
      search TERM - search for packages whose name (partially) match the specified TERM
      info PACKAGE - list all metadata about specified PACKAGE
      local-status - list all installed packages
      index - pretty-prints cyclone-winds packages index
  
      PACKAGE AUTHORING:
      build-local [DIR] - build local package using package.scm from DIR or \".\"
      test-local [DIR] - test local package using package.scm in DIR or \".\"
      package [DIR] - scaffold DIR layout and a package.scm stub
      package-srfi [DIR] - scaffold DIR layout and a package.scm stub for SRFIs
   
    PACKAGES:
         Name of the package. Note this can be a symbol or a quoted list of 
         two or more symbols, e.g. \"(cyclone iset)\"~%~%"


```
$ cyclone-winds install iset       
```

## Alternative install directories

Libraries and programs can be installed into an alternative location using respectively the env vars `CYCLONE_LIBRARY_PATH` and `CYCLONE_PROGRAM_PATH`. 

Example on zsh:

```
$ env CYCLONE_LIBRARY_PATH=/home/me/cyclone-libs cyclone-winds install iset
```

```
$ env CYCLONE_PROGRAM_PATH=/home/me/local/bin cyclone-winds install hypothetical-program
```
## Authoring packages

### Use of `package` command

Running `$ cyclone-wfinds package [DIR]` inside of a directory with `.sld` file(s) will generate `package.scm` and `README.md` stubs that can be adjusted manually later if needed. The command also structures the directory tree appropriately for most cases (see [below](#package-file-structure)).

*Note: if you are authoring `SRFIs`, use the `package-srfi` command instead.*

### Parameters for the `package.scm` file

#### Mandatory parameters

*Note that each parameter has a specific type:*

- name: a symbol or a double-quoted list of symbols (e.g. `iset` or `"(cyclone hypothetical-lib)"`)
- version: floating point
- license: string
- authors: one or more strings
- maintainers: one or more strings
- description: string
- tags: one or more strings
- docs: string pointing to documentation (at the moment should point to `https://github.com/cyclone-scheme/cyclone-winds/wiki/PACKAGE-NAME.md`)
- test: string pointing to a test file

#### Code parameters

A `package.scm` file needs at least one `library` and/or `program`.

##### Parameters for `(library ...)`

*Note: more than one occurrence is allowed and libraries will be installed in the order they appear.*

- name: list of **two or more** symbols, being the first one `cyclone`. So to install `lib1` from `pkg` the `name` parameter should be `(cyclone pkg lib1)`. The package file structure should reflect this naming convention, and lib definition file should be placed in `cyclone/pkg/lib1.sld`.
- [optional] description: string

##### Parameters for `(program ...)`

*Note: more than one occurrence is allowed and programs will be installed in the order they appear.*

- name: a single symbol. `.scm` will be appended to this symbol in order to find the program source
- [optional] description: string

#### Optional parameters
- dependencies: zero or more *list* of symbols
- test-dependencies: zero or more *list* of symbols
- foreign-dependencies: zero or more *list* of symbols. It points to OS-level dependencies and is only informative.

### package.scm example

```scheme
(package
 ;; Mandatory parameters
 (name example-package)
 (version 0.1)          
 (license "BSD")       
 (authors "Arthur Maciel <email@email.com>" "Justin Ethier <email@email.com>")
 (maintainers "Arthur Maciel <email@email.com>")
 (description "This is an example package only for demonstration purposes.")
 (tags "Misc" "Devel")
 (docs "https://github.com/cyclone-scheme/cyclone-winds/wiki/example-package.md")
 (test "tests.scm")
 
 (library
  (name (cyclone example-package lib1))
  (description "Just an example of how a minimal package.scm file should look like"))

 (library
  (name (cyclone example-package lib2))
  (description "Another library"))

 (program
  (name example-program)
  (description "A useless example program."))
 
 ;; Optional parameters
 (dependencies ()) 
 (test-dependencies ())
 (foreign-dependencies (debian (libck-dev libtommath-dev))
                       (ubuntu (libtommath-dev))
                       (fedora (libtommath-devel))))
```

### Package file structure

Packages should contain code files (except `package.scm`, `README.md` and the test file) inside a `cyclone` directory (see examples bellow). SRFI implementations should be place under a `srfi` directory instead of a `cyclone` one.

#### Example 1

If in `package.scm` we have a `(library (name (cyclone http-client))` then, after installed, it will be used as `(import (cyclone http-client))`.The package file structure should be:

```
package.scm
README.md
tests.scm
cyclone/
|- http-client.sld
```

#### Example 2

If in `package.scm` we have a `(library (name (cyclone crypto md5))` then, after installed, it will be used as (import (cyclone crypto md5)).The package file structure should be:

```
package.scm
README.md
tests.scm
cyclone/
|- crypto/
   |- md5.sld
```

#### Example 3

If in `package.scm` we have a `(program (name start-server))` then, after installed, it will be called by `$ /usr/local/bin/start-server` (base directory here may change according to your OS configuration or `CYCLONE_PROGRAM_PATH`, if `PREFIX` was changed during `cyclone` installation). The package file structure should be:

```
package.scm
README.md
tests.scm
cyclone/
|- start-server.scm
```

#### Example 4 (SRFIs)

If in `package.scm` we have a `(library (name (srfi 26))` then, after installed, it will be used as `(import (srfi 26))`.The package file structure should be:

```
package.scm
README.md
tests.scm
srfi/
|- 26.sld
```

## Limitations

- Packages will always have their latest version installed (there is no way to select older versions)
- No way to digitally sign packages - only tarball sha256sums are verified

## Tips and tricks

On FreeBSD `/tmp` is not writeable by default for non-root users. So set the environment variable `TMP` to a writeable directory. Example:

```
$ env TMP=/home/me/tmp cyclone-winds retrieve iset
```

