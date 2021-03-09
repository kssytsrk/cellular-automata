# cellular-automata (ca)
### by _kssytsrk <kassy@dismail.de>_

This project simulates a cellular automata environment using the [lispbuilder-sdl](https://github.com/lispbuilder/lispbuilder) graphics library. Rulesets currently implemented are all of the basic elementary/1-dimensional ones, totalistic and neighbour-number schemes, any 2-states Neumann/Moore neighbourhood ones (if you can find the exact ruleset number, that is), Wireworld and Game of Life. 

This will be updated as I read more papers on cellular automata and improve my understanding of it.

## Installation

Clone this repository to `~/common-lisp` with:

```bash
$ git clone https://github.com/kssytsrk/cellular-automata
```

## Usage

First, install [Quicklisp](https://www.quicklisp.org/beta/) (if not yet installed) to fetch all the dependencies. Then open the basic SBCL REPL (`sbcl` in console)/SLIME/SLY and execute the following:

```common-lisp
(ql:quickload :ca)
```
To simulate a cellular automaton, evaluate the `ca:start` function. 

## Examples

To look at some [subjectively] cute cellular automatons you can evaluate these commands:
```common-lisp
;;; This is rule 30 of an 1-dimensional cellular automaton.
;;; The size of the window is 200px x 200px.
(ca:start :h 200 :w 200 :ruleset 30)
```
```common-lisp
;;; This is rule 2049 of an 1-dimensional totalistic 3-state cellular automaton.
(ca:start :ruleset 2049 :tag :totalistic :states 3)
```
```common-lisp
;;; This is rule 452 of a 2-dimensional neighbour-number cellular automaton.
;;; The size of the window is 400px x 400px. The colorscheme used is golly.
(ca:start :w 400 :h 400 :ruleset 452 :neighbourhood :neumann :tag :neighbour-number :colors :golly)
```

Use `Escape` or your window manager's button/keybinding to close the window and stop the simulation.

## `ca:start` keyword arguments
`ca:start` takes the following keyword arguments (all of them optional and have a default value):
- `:h` and `:w` are the height and width of the window where the results of simulation are displayed. Note that the bigger the size of the window, the slower the automaton might be.
- `neighbourhood` should be either `elementary` (for elementary/1-dimensional 3-neighbour cellular automaton), `:neumann` (for 2-dimensional 5-neighbour cellular automaton), or `:moore` (for 2-dimensional 9-neighbour cellular automaton). Support of other neighbourhoods is in progress.
- `ruleset` is the ruleset's number or a keyword `:game-of-life` or `:wireworld`. 
- `tag` is some (optional) additional quality, right now it can be either `:totalistic` or `:neighbour-number`.
- `steps` is how many times should the evolution run (it works sort of the wrong way for 1-dimensional cellular automatons right now, I know), takes an integer. If left unspecified, runs an infinite number of times.
- `colors` specifies the colorscheme of cellular automaton's states. Can be `:golly` (will use the colors from Golly, got them from Wikipedia) or `:grayscale` (colors will be shades of white/gray/black). If left unspecified, will use the `:grayscale` colorscheme.
- `states` is the number of states possible (and colors used). 2 by default.
- `auto` specifies if the automaton should evolve automatically. `t` by default, if specified `nil` you will have to press `Space` for the automaton to evolve.
- `starting-pixels` - WIP.

## Adding your own rulesets
WIP. (should be already possible, the docs are just not yet written)

## Notes
Suggestions and bugreports (I'm sure there are some bugs here I don't know of yet) are welcome in the Issues section.

## License

MIT license, see the [LICENSE](LICENSE) file for more.
