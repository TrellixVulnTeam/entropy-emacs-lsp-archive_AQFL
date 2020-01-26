
- [eemacs lsp archive project](#org43c4c21)
- [Copyright (C) date  author](#orgaff1b0a)
- [Commentary:](#org57aa790)
  - [Contribute](#org3de46bb)
    - [Make and load prior](#org578be10)
    - [Recipes](#org6f96bac)

<a id="org43c4c21"></a>

# eemacs lsp archive project


<a id="orgaff1b0a"></a>

# Copyright (C) date  author

    Author:        Entropy <bmsac0001@gmail.com>
    Maintainer:    Entropy <bmsac001@gmail.com>
    Package-Version: 0.1.0
    Created:       2020-01-21
    Compatibility: GNU Emacs 25.3;
    Package-Requires: ((emacs "25.3") (cl-lib "0.5"))
    
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


<a id="org57aa790"></a>

# Commentary:

This project archived all [entropy-emacs](https://github.com/c0001/entropy-emacs.git) required Microsoft Language Server implementations, and aimed for build and load all of them for current working platform.

**Make:**

A simple make file stored in the root of this project, run it with option `help` and `all` for quickly build for your machine architecture, `aarch64`, `x86_64` are only supported ones, further more, only operation system included in `Windows` and `gnu/linux` were supported as so.

**load:**

The project was designed as the extension for GNU/emacs above(and include) for 25.3. The init loader [eemacs-lsp-archive-load.el](eemacs-lsp-archive-load.el) can be load by elisp function `load`, that say:

```elisp
(load "path-to-this-project/eemacs-lsp-archive-load.el")
```

Or add to this project root to your `load-path` and `require` it or using config loader management like `use-package` to deferred load the feature `eemacs-lsp-archive-load`.


<a id="org3de46bb"></a>

## Contribute

There's one elisp object `lspa-register`, whose car was its name and the cdr was a plist called `lspa-rgister-tree`, name was a symbol for indicate what it is, and the tree has three slot `:root`, `:prebuilt` and the `:archive`. This project represent the language server archvie as it, the object indicate where to find the archive and how to build and load thus.

For the keys content formal and meaningful see below list table:

-   `:root`
    
    The `:root` slot contained the lsp archive root path, its type of a path string.

-   `:prebuilt`
    
    For cross-platform designation, a folder named `PreBuilt` must be stored under the `:root`, and all the subdir of it are dired as struct of `system-type/architecture` as its elements, so that one `lspa-register` is served for multi-platfom of multi-architecture.
    
    For represent the folder structure of `:prebuilt`, the value of this key was one alist whose car of each elements was the `system-type` e.g. 'windows-nt', 'gnu/linux', 'darwin' and so more recognized by elisp `system-type` const, the cdr was a alist whose car was the architecture, a symbol like 'x86<sub>64</sub>', 'aarch64' etc and rest of it was a plist and now just has one key `:init`, the loader for the specified architecture, its a elisp file.

-   `:archive`
    
    The same structure for `:prebuilt`, but for source installing aiming. Folder `Archive` must be stored in the `:root` as what `:prebuilt` did.

For the alist of `:prebuilt` or `:archive` slot, can have the **all** platform-and-architecture supported element whose car was 'all' and the rest obey the specified architecture rest form. The **all** type element means can be load for whatever platform and architecture.


<a id="org578be10"></a>

### Make and load prior

That see of the `lspa-register` slot `:prebuilt` and `:archive`, we always prefer to find the prebuilt loader if not found then search the archive loader for individual platform and machine architecture, if not for all, warning hint for user and exit.


<a id="org6f96bac"></a>

### Recipes

For more benefit maintaining and developing `lspa-register`, we have the convention for putting each `lspa-register` to a file called `recipe` under the `elements/recipes` folder as what melpa do (if your are a emacs package developer, you may know what [melpa](https://melpa.org/) is). That say that all the `lspa-register` elisp form are the only content of its recipe. The main loader this project will read them automatically by making and loading.

So as for the contributor, put your recipe into the 'elements/recipes' folder after your register folder built up, that's all.


<a id="org163b5dd"></a>

