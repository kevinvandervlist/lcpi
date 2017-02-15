<a name="1.0.4"></a>
# [1.0.4](https://github.com/kevinvandervlist/lcpi/compare/v1.0.2...v1.0.3) (????-??-??)

### Bug Fixes

*  

### Features

* 


<a name="1.0.3"></a>
# [1.0.3](https://github.com/kevinvandervlist/lcpi/compare/v1.0.2...v1.0.3) (2017-02-15)

### Bug Fixes

* [dc5abb2](https://github.com/kevinvandervlist/lcpi/commit/dc5abb2ead2b34375ad5982aaffb1ceb313e84bf) Regression introduced by [d1314ae](https://github.com/kevinvandervlist/lcpi/commit/d1314aea5f53801071b45f697fcc9f0ef52be893): properly reduce repl commands. 

### Features

* [#7](https://github.com/kevinvandervlist/lcpi/pull/7) Jline (GNU readline-esque) REPL support
* [#8](https://github.com/kevinvandervlist/lcpi/pull/8) Minimize the number of parentheses used during stringification

<a name="1.0.2"></a>
# [1.0.2](https://github.com/kevinvandervlist/lcpi/compare/v1.0.1...v1.0.2) (2017-02-14)

### Bug Fixes

* [#4](https://github.com/kevinvandervlist/lcpi/pull/4) `reset` did not revert back to the `empty` state.
* [#5](https://github.com/kevinvandervlist/lcpi/pull/5) Context must be mutable during `reload` commands.

### Features

* [2c5aad9](https://github.com/kevinvandervlist/lcpi/commit/2c5aad93cdaeaaac018f1e0328e886ff4960d4ef) Sort REPL assignments alphabetically and indent them with 10 chars.
* [7c59ffd](https://github.com/kevinvandervlist/lcpi/commit/7c59ffdb4eeb5374408d595b93f0eaef79c7eb0f) Reload all files instead of the last one.
* [#3](https://github.com/kevinvandervlist/lcpi/pull/3) Transitively load additional files if `load` commands are encounterd while reading files.
* [#6](https://github.com/kevinvandervlist/lcpi/pull/6) Add a command to render an expression in De Bruijn Index notation. 

<a name="1.0.1"></a>
# [1.0.1](https://github.com/kevinvandervlist/lcpi/compare/v1.0.0...v1.0.1) (2017-02-08)

### Bug Fixes

* [1702493](https://github.com/kevinvandervlist/lcpi/commit/1702493a6dbb26c472c7fc74c0d58b4517b23d15) REPL assignments must be capitalized.

### Features

* [#1](https://github.com/kevinvandervlist/lcpi/pull/1) The REPL now supports the `load` command to load a file in the current context.
* [#2](https://github.com/kevinvandervlist/lcpi/pull/2) Add support for a `reload` command to reload the last file again.

<a name="initial"></a>
# Initial release (2017-02-08)

Initial release of lcpi.
