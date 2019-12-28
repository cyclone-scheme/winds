[<img src="cyclone-winds.png" alt="cyclone-winds">](http://github.com/cyclone-scheme/cyclone-winds)

# cyclone-winds
Package manager for [Cyclone Scheme](https://cyclone-scheme.org).

The official list of packages is on the [wiki](https://github.com/cyclone-scheme/cyclone-winds/wiki).

## Introduction

A "package" for `cyclone-winds` is wrapper around libraries and/or programs. They are extensions to the core implemenation of Cyclone Scheme. 

## Installation


Note that `gmake` should be used instead of `make` on FreeBSD 12 (install it with `sudo pkg install gmake`).

   git clone https://github.com/cyclone-scheme/cyclone-winds.git
   cd cyclone-winds
   make
   sudo make install

Optionally it is possible to pass `PREFIX` to set another destination directory.

   make 
   sudo make PREFIX=/home/joe/cyclone-winds install
   /home/joe/cyclone-winds/bin/cyclone-winds --help
   
## Usage

```
$ cyclone-winds [OPTIONS [PACKAGES]]
```
OPTIONS:
       COMMON USE:
       help  -  print usage
       retrieve PACKAGE [PACKAGE2 ...]  - downloads and extracts specified package(s)
       install PACKAGE [PACKAGE2 ...] - retrieve and install specified package(s)
       uninstall PACKAGE [PACKAGE2 ...] - remove specified package(s)
       TODO - search WILDCARD - search for packages that partially match the specified wildcard
       info PACKAGE - list all metadata about specified package
       local-status - list all installed packages
       index - pretty-prints cyclone-winds packages index

       PACKAGE AUTHORING:
       build-local [DIRECTORY] - build local package using package.scm from DIRECTORY or \".\"
       test-local [DIRECTORY] - test local package using (test ...) from package.scm in DIRECTORY or \".\"
       TODO - package - scaffold directory layout and a package.scm stub
       
*Attention! PACKAGES name should be a a **quoted** list of two or more symbols, starting with 'cyclone'*. For example:

```
$ cyclone-winds install "(cyclone iset)"       
```

## Alternative install directories

Libraries and programs can be installed into an alternative location using respectively the env vars `CYCLONE_LIBRARY_PATH` and `CYCLONE_PROGRAM_PATH`. 

Example on zsh:

```
$ env CYCLONE_LIBRARY_PATH=/home/me/cyclone-libs cyclone-winds install "(cyclone iset)"
```

```
$ env CYCLONE_PROGRAM_PATH=/home/me/local/bin cyclone-winds install "(cyclone programX)"
```

## `package.scm` parameters

### Mandatory parameters

- name: list of **two or more symbols** (being the first one `cyclone`, i.e. `(cyclone iset)`)
- version: floating point
- license: string
- authors: one or more strings
- maintainers: one or more strings
- description: string
- tags: one or more strings
- docs: string pointing to documentation
- test: string pointing to a test file

### Code parameters

A `package.scm` file needs at least one `library` and/or `program`.

#### 'library' parameters

*Note: more than one occurrence is allowed and libraries will be installed in the order they appear.*

- name: list of **two or more** symbols, being the first one `cyclone`. So to install `pkg lib1` the `libary` parameter should be `(cyclone pkg lib1)`. The package file structure should reflect this and lib definition file should be placed in `cyclone/pkg/lib1.sld`.
- description: string

#### 'program' parameters

*Note: more than one occurrence is allowed and programs will be installed in the order they appear.*

- name: a single symbol. `.scm` will be appended to this symbol in order to find the program source
- description: string

### Optional parameters
- dependencies: zero or more list of symbols
- test-dependencies: zero or more list of symbols
- foreign-dependencies: zero o more list of symbols. It points to OS-level dependencies and is only informative

## package.scm example

```scheme
;; Mandatory parameters
(package
 (name (cyclone example-package))
 (version 0.1)          
 (license "BSD")       
 (authors "Arthur Maciel <email@email.com>" "Justin <email@email.com>")
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
 (dependencies) 
 (test-dependencies)
 (foreign-dependencies (debian (libck-dev libtommath-dev))
                       (ubuntu (libtommath-dev))
                       (fedora (libtommath-devel))))
```

## Package file structure

All packages should contain their files inside a `cyclone` base directory, except `package.scm` and the test file, which should be places on base directory (see examples bellow).

#### Example 1

If in `package.scm` we have a `(library (name (cyclone http-client))` then after installed it will be used as (import (cyclone http-client)).The package file structure should be:

```
package.scm
cyclone/
|- http-client.sld
```

#### Example 2

If in `package.scm` we have a `(library (name (cyclone crypto md5))` then after installed it will be used as (import (cyclone crypto md5)).The package file structure should be:

```
package.scm
cyclone/
|- crypto/
   |- md5.sld
```

#### Example 3

If in `package.scm` we have a `(program (name start-server))` then after installed it will be called by `$ /usr/local/bin/start-server`. The package file structure should be:

```
package.scm
cyclone/
|- start-server.scm
```

## Limitations

- Packages will always have their latest version installed (there is no way to specify older versions)
- No way to digitally sign packages - only tarball sha256sums are verified

## Tips and tricks

On FreeBSD `/tmp` is not writeable by default for non-root users. So set the environment variable `TMP` to a writeable directory. Example:

```
$ env TMP=/home/me/tmp cyclone-winds retrieve "(cyclone iset)"
```

