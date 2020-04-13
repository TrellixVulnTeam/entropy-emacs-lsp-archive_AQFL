Note: this file is auto converted from lisp-toggle-file.[20200413193854].el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!

- [eemacs lsp archive project](#org5606d73)
- [Copyright (C) date  author](#orga23244d)
- [Commentary:](#h-b5013db2-37a9-44de-9327-05b17e760dbc)
  - [Contribute](#h-8c8344df-e341-4183-9e06-cec26bd8bb43)
    - [`lspa-register` sub-folder naming convention](#h-58d05c1a-62a2-44c4-be4f-1de662fe190b)
    - [Make and load prior](#h-7f5311d9-9ff0-4cb4-96f0-8775fd135246)
    - [Recipes](#h-8960a582-196b-44d4-ad49-bbf74cc943d6)
    - [Get APIs](#org9e77ace)


<a id="org5606d73"></a>

# eemacs lsp archive project


<a id="orga23244d"></a>

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


<a id="h-b5013db2-37a9-44de-9327-05b17e760dbc"></a>

# Commentary:

This project archived all [entropy-emacs](https://github.com/c0001/entropy-emacs.git) required Microsoft Language Server implementations, and aimed for build and load all of them for current working platform.

**Make:**

A simple make file stored in the root of this project, run it with option `help` and `all` for quickly build for your machine architecture, `aarch64`, `x86_64` are only supported ones, further more, only operation system included in `Windows` and `gnu/linux` were supported as so.

> **Notice for make on windows platform:**
>
> If you using Cygwin like posix emulator to make this project, please confirm that all the kits used by this project's required are all built by its toolchain, include `python`, `emacs`, `make`, so that we can guarantee the 'make' environment keep consistency.
>
> Or you should using purely windows individual gnumake to build as well. You can get windows version of 'gnumake' by any populate windows packages manager like [chocolate](https://chocolatey.org/), and ensure that the 'make' version equal or larger than 4.3.

**load:**

The project was designed as the extension for GNU/emacs above(and include) for 25.3. The init loader [eemacs-lsp-archive-load.el](eemacs-lsp-archive-load.el) can be load by elisp function `load`, that say:

```elisp
(load "path-to-this-project/eemacs-lsp-archive-load.el")
```

Or add to this project root to your `load-path` and `require` it or using config loader management like `use-package` to deferred load the feature `eemacs-lsp-archive-load`.

**Environment variables:**

We made the make procedure can be specified for particular platform and architecture even for the archive using type, for testing or other special meanings. The following three environment variables were builtin for the sake of thus:

1.  `Eemacs_Lspa_Use_Archive`:

    The environment variable for indicate whether focely using archive type.

    Valid value are: 't' or 'nil'

2.  `Eemacs_Lspa_Use_Platform`:

    The environment variable for indicating which system platform to use, valid values are all of them can be getted by emacs internal variable `system-type`.

3.  `Eemacs_Lspa_Use_Architecture`

    The environment variable for indicating which architecture to use, valid values are 'x86<sub>64</sub>' and 'aarch64'.


<a id="h-8c8344df-e341-4183-9e06-cec26bd8bb43"></a>

## Contribute

There's one elisp object `lspa-register`, whose car was its name and the cdr was a plist called `lspa-rgister-tree`, name was a symbol for indicate what it is, and the tree has three slot `:root`, `:prebuilt` and the `:archive`. This project represent the language server archvie as it, the object indicate where to find the archive and how to build and load thus.

For the keys content formal of `lspa-register-tree` and meaningful see below list table:

1.  `:root`

    The `:root` slot contained the lsp archive root path, its type of a path string.

2.  `:prebuilt`

    For cross-platform designation, a folder named `PreBuilt` must be stored under the `:root`, and all the subdir of it are hosted as struct of `system-type/architecture` as its elements, so that one `lspa-register` is served for multi-platfom of multi-architecture.

    For represent the folder structure, the value of this key was one alist whose car of each elements was the `system-type` symbol e.g. 'windows-nt', 'gnu/linux', 'darwin' and so more recognized by elisp `system-type` const, the cdr of thus was a alist whose each element's car was the architecture, a symbol like 'x86<sub>64</sub>', 'aarch64' etc and rest of it was a plist called `lspa-manager` which has keys for:

    1.  `:init`, the loader for the specified architecture, its a elisp file for both run the building in **non-interactive** i.e. the batch-mode ENV and the loading infrastructure in interactive ENV.

3.  `:archive`

    The same structure for `:prebuilt`, but for source installing aiming. Folder `Archive` must be stored in the `:root` as what `:prebuilt` did.

For the alist of `:prebuilt` or `:archive` slot, can have the **all** platform-and-architecture supported element whose car was 'all' and the rest form as same as the plist hosted in the architecture slot i.e. the `lspa-manager`. The **all** type element means can be load for <span class="underline">whatever platform and architecture</span>.


<a id="h-58d05c1a-62a2-44c4-be4f-1de662fe190b"></a>

### `lspa-register` sub-folder naming convention

The instance of a `lspa-register` was a folder with specific folder structure, the sub-folders' distribution are commonly reflecting with the `lspa-register` data nesting structure.

For building one `lspa-register` instance, the root of the archive should (but no necessary) name as the `lspa-register`'s name. And then, the following three sub-folder are "Prebuilt", "Archive" and "All", as above mentioned that they are optionally built with your specification.

The important naming convention are those platform and architecture sub-folders under those second hierarchy folder:

-   **For platform:** see the alist of `eemacs-lspa/subr-platform-folder-alist`
-   **For architecture:** see the alist of `eemacs-lspa/subr-architecture-folder-alist`

Thus for a expample, we made a python-language-server `lspa-register` instance:

    + python-language-server
      + Prebuilt
        + WindowsNT
          - x86_64
        + GnuLinux
          - x86_64
          - aarch64
      - Archive

For the place hosting each `lspa-register` instance, please see below sections. ([BROKEN LINK: h-8960a582-196b-44d4-ad49-bbf74cc943d6])


<a id="h-7f5311d9-9ff0-4cb4-96f0-8775fd135246"></a>

### Make and load prior

That see of the `lspa-register` slot `:prebuilt` and `:archive`, we always prefer to find the prebuilt loader if not found then search the archive loader for individual platform and machine architecture, if not for all, warning hint for user and exit.


<a id="h-8960a582-196b-44d4-ad49-bbf74cc943d6"></a>

### Recipes

For more benefit maintaining and developing `lspa-register`, we have the convention for putting each `lspa-register` to a file called `recipe` under the `elements/recipes` folder as what melpa do (if your are a emacs package developer, you may know what [melpa](https://melpa.org/) is). That say that all the `lspa-register` elisp form are the only content of its recipe. The main loader this project will read them automatically by making and loading.

So as for the contributor, put your recipe into the `eemacs-lspa/path-lspa-recipes-root` folder after your register folder built up, that's all.

Thus for a expample, we made a python-language-server `lspa-register` recipe instance:

```emacs-lisp
(python-language-server
 :root
 "python-language-server"
 :prebuilt
 ((gnu/linux
   (x86_64 :init "eemacs-lspa-pyls-prebuilt-gnulinux-x86_64-load.el")
   (aarch64 :init "eemacs-lspa-pyls-prebuilt-gnulinux-aarch64-load.el"))
  (windows-nt
   (x86_64 :init "eemacs-lspa-pyls-prebuilt-windowsnt-x86_64-load.el")))
 :archive nil)
```

As see the sample, we use the abbreviation path for the archvie root, which will be automatically expanding with `eemacs-lspa/path-lspa-repos-root`.


<a id="org9e77ace"></a>

### Get APIs

To all the contributors for writting their own recipes, we recommend to use the project built-in APIs for keeping consistency and obeying the project conventions. All the APIs are hosted on `elements/library/*.el`, read their commentrary for details.


<a id="org241da28"></a>
