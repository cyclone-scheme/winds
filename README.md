# cyclone-winds
Package manager for [Cyclone Scheme](https://cyclone-scheme.org).

To find packages, please visit the [wiki](https://github.com/cyclone-scheme/cyclone-winds/wiki).

## Usage

```
$ cyclone-winds [OPTIONS [PACKAGES]]
```
OPTIONS could be one of the following:
- help  -  print usage
- list  -  list the results
- retrieve PACKAGE [PACKAGE2 ...]  - downloads and extracts specified package(s)
- install PACKAGE [PACKAGE2 ...] - retrieve and install specified package(s)
- uninstall PACKAGE [PACKAGE2 ...] - remove specified package(s)
- search WILDCARD - search for packages that partially match the specified wildcard
- info PACKAGE - list all metadata about specified package
- local-status - list all installed packages

*PACKAGES should be a symbol or a string containing a list of symbols - i.e. `http-client` or `"(http-client)"`*

## package.scm parameters

### Mandatory parameters

- name: list of one or more symbols. Note that 'cyclone' will be prepend to it, so `(example-package)` will become `(cyclone example-package)`
- version: floating point
- license: string
- authors: one or more strings
- maintainers: one or more strings
- description: string
- tags: one or more strings
- docs: string pointing to documentation

### 'library' parameters

*Note: more than one occurrence is allowed and libraries will be installed in the order they appear.*

- name: list of one or more symbols. As with package name, note that 'cyclone' will be prepend to it, so `(example-package lib1)` will become `(cyclone example-package lib1)` and will be searched at package file structure as `"cyclone/example-package/lib1.sld"`
- description: string

### 'program' parameters

*Note: more than one occurrence is allowed and programs will be installed in the order they appear.*

- name: a single symbol. `'.scm'` will be appended to this symbol in order to find the program source
- description: string

### Optional parameters
- dependencies: zero or more list of symbols
- test-dependencies: zero or more list of symbols
- foreign-dependencies: zero o more list of symbols. It encompasses OS-level dependencies and is only informative

## package.scm example


```scheme
;; Mandatory parameters
(package
 (name (example-package))
 (version 0.1)          
 (license "BSD")       
 (authors "Arthur Maciel <email@email.com>" "Justin <email@email.com>")
 (maintainers "Arthur Maciel <email@email.com>")
 (description "This is an example package only for demonstration purposes.")
 (tags "Misc" "Devel")
 (docs "https://github.com/cyclone-scheme/cyclone-winds/wiki/example-package.md")
 
 (library
  (name (example-package lib1))
  (description "Just an example of how a minimal package.scm file should look like"))

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

## Package files structure

All packages should contain their file inside a `cyclone` directory (see the examples bellow).

#### Example 1

If in `package.scm` we have a `(library (name (http-client))` then after installed it will be used as (import (cyclone http-client)).The package file structure should be:

```
cyclone/
|- package.scm
|- http-client.sld
```

#### Example 2

If in `package.scm` we have a `(library (name (crypto md5))` then after installed it will be used as (import (cyclone crypto md5)).The package file structure should be:

```
cyclone/
|- package.scm
|- crypto/
   |- md5.sld
```

#### Example 3

If in `package.scm` we have a `(program (name start-server))` then after installed it will be called by `$ /usr/local/bin/start-server`. The package file structure should be:

```
cyclone/
|- package.scm
|- start-server.scm
```
