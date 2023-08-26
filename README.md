[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]

# Configurable Text Editor In Pure Elm

This is just a fun project. It is a text editor that supports highlighting through provided parser and basic editor actions.
The editor is optimized to handle large texts (hundreds of thousands of characters).
I took inspiration for some performance optimizations from the great [Ace editor](https://ace.c9.io/).

The editor is meant as a package, exporting `init`, `update` and `view` functions and some related types from the `Main.elm` module. 
However, there is the `Playground.elm` file with an example usage linked in the `index.html` to try it out.

# Getting Started

To try the example: 
1. Run `npm i` to install npm dependencies (most notably `elm` itself)
1. Run `npm run make` to compile the example (outputs resulting js file into `generated` directory)
1. Open the `index.html` file and try out the editor

# Next Steps

* Expand configurability of the editor. Especially keyboard handling.
* Add several prepared configuration to have options for a quick start.
* Add support for popovers on cursor or line number.
* Add support for multiple cursors.


[contributors-shield]: https://img.shields.io/github/contributors/JiriLojda/elm-text-editor.svg?style=for-the-badge
[contributors-url]: https://github.com/JiriLojda/elm-text-editor/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/JiriLojda/elm-text-editor.svg?style=for-the-badge
[forks-url]: https://github.com/JiriLojda/elm-text-editor/network/members
[stars-shield]: https://img.shields.io/github/stars/JiriLojda/elm-text-editor.svg?style=for-the-badge
[stars-url]: https://github.com/JiriLojda/elm-text-editor/stargazers
[issues-shield]: https://img.shields.io/github/issues/JiriLojda/elm-text-editor.svg?style=for-the-badge
[issues-url]:https://github.com/JiriLojda/elm-text-editor/issues
[license-shield]: https://img.shields.io/github/license/JiriLojda/elm-text-editor.svg?style=for-the-badge
[license-url]:https://github.com/JiriLojda/elm-text-editor/blob/master/LICENSE.md
