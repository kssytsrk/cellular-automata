# cellular-automata (ca)
### by _kssytsrk <kassy@dismail.de>_
![rule 581, totalistic elementary 3-state cellular automata](/img/581-totalistic.png "rule 581, totalistic elementary 3-state cellular automata")

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

To look at some [subjectively] cute cellular automata you can evaluate these commands:
```common-lisp
;; This is rule 30 of an 1-dimensional cellular automaton.
;; The size of the window is 200px x 200px.
(ca:start :h 200 :w 200 :ruleset 30)
```
```common-lisp
;; This is rule 2049 of an 1-dimensional totalistic 3-state cellular automaton.
(ca:start :ruleset 2049 :tag :totalistic :states 3)
```
```common-lisp
;; This is rule 452 of a 2-dimensional neighbour-number cellular automaton.
;; The size of the window is 400px x 400px. The colorscheme used is golly.
(ca:start :w 400 :h 400 :ruleset 452 :neighbourhood :neumann
          :dimensions 2
          :tag :neighbour-number :colors :golly)
```
```common-lisp
;; Ruleset shown on the demo image above.
;; Rule 581 of an 1-dimensional totalistic 3-state cellular automaton.
(ca:start :ruleset 581 :tag :totalistic :states 3)
```

Use `Escape` or your window manager's button/keybinding to close the window and stop the simulation.

## `ca:start` keyword arguments
`ca:start` takes the following keyword arguments (all of them optional and have a default value):
- `:h` and `:w` are the height and width of the window where the results of simulation are displayed. Note that the bigger the size of the window, the slower the automaton might be.
- `:dimensions` specifies the dimensions of the cellular automaton, should be either 1 or 2 (for now). 1 by default.
- `:neighbourhood` should be either `:elementary` (for elementary 3-neighbour cellular automaton), `:neumann` (for 5-neighbour cellular automaton, typically used with `:dimensions` set to 2), or `:moore` (for 9-neighbour cellular automaton, typically used with `:dimensions` set to 2). Support of the other neighbourhoods is in progress.
- `:ruleset` is the ruleset's number or a keyword `:game-of-life` or `:wireworld`. 
- `:tag` is some (optional) additional quality, right now it can be either `:totalistic` or `:neighbour-number`.
- `:steps` is how many times should the evolution run, takes an integer. If left unspecified, runs an infinite number of times (for 2-dimensional cellular automata) or the height of the window (for 1-dimensional cellular automata).
- `:colors` specifies the colorscheme of cellular automaton's states. Can be `:golly` (will use the colors from Golly, got them from Wikipedia) or `:grayscale` (colors will be shades of white/gray/black). If left unspecified, will use the `:grayscale` colorscheme.
- `:states` is the number of states possible (and colors used). 2 by default.
- `:auto` specifies if the automaton should evolve automatically. `t` by default, if specified `nil` you will have to press `Space` for the automaton to evolve.
- `:starting-state`, coincidentally, specifies what the starting state of the automaton is. Is `'(:center-dot)` by default (this leaves one active/colored dot in the center of the canvas). By default, possible values are `(:empty)` (quite self-explanatory), `(:dot (x1 y1) (x2 y2)...)` (returns a state with active/colored dots at coordinates (x1 y1), (x2 y2) etc, takes any number of arguments), `(:line ((x1 y1) (x2 y2)))` (returns a state with a line from the point at (x1 y1) to the point at (x2 y2), takes any number of arguments that are two-element lists with each element representing a point), `(:random-dots n)` (returns a state with n random active/colored dots).

## Extensibility
To add your own rulesets/neighbourhoods/states into the program, first open the SBCL (or any other Lisp implementation, actually) REPL, quickload :ca and then input:

```common-lisp
(in-package :ca)
```

This is a prerequisite for any action below.

### Adding your own rulesets
Define a ruleset-generating function named `<your-custom-name-here>-ruleset` (without the angle brackets) that takes the number of the ruleset, the neighbourhood, and the number of possible states. Typically, that function returns a hash table mapping a list of cells (a neighbourhood) to some value, but if you don't need such hash table, the function can just return `t`.

An example is this implementation of a "normal" ruleset generating function:

```common-lisp
(defun normal-ruleset (n neighbourhood possible-states)
  (declare (ignore possible-states))
  (let ((max-pwr (case neighbourhood
                   (:elementary 8)
                   (:neumann 32)
                   (:moore 512)
                   (t 0)))
        (ruleset (make-hash-table :test #'equal)))
    (when (and (>= n 0) (< n (expt 2 max-pwr)))
      (let ((states (decimal-to-base-n-list n max-pwr 2))
            (patterns (reverse
                       (loop for i from 0 below max-pwr
                             collect (decimal-to-base-n-list i
                                                             (case neighbourhood
                                                               (:elementary 3)
                                                               (:neumann 5)
                                                               (:moore 9))
							     2)))))
        (mapcar (lambda (pattern state)
		  (setf (gethash pattern ruleset)
                        state))
                patterns states)
        ruleset))))
```

Since the argument `possible-states` is not used here, it is declared as ignored. 

Another example is the game-of-life ruleset-generating function that returns just `t` (since you don't really need a ruleset for game-of-life):

```common-lisp
(defun game-of-life-ruleset (n neighbourhood possible-states)
  (declare (ignore n neighbourhood possible-states)) t)
```

For a custom ruleset, you also need to define a transition-rule function that gets called on the list of cells (a neighbourhood of the cell) and the ruleset. The function has to be named `<your-custom-name-here>-transition-rule`, the custom name being the same as in the ruleset-generating function. 

The simplest example of the transition rule:

```common-lisp
(defun normal-transition-rule (cells ruleset)
  (gethash cells
           ruleset))
```

After defining both of these, you can now apply the ruleset like this:
```common-lisp
(ca:start :w 400 :h 400 :ruleset :custom-name :neighbourhood :neumann)
```
or like this:
```common-lisp
(ca:start :w 400 :h 400 :ruleset 100 :neighbourhood :neumann :tag :custom-name)
```
The particular method that has to be chosen depends on if you need a ruleset number (if it is used in the ruleset-generating function, then you need it).

### Adding your own starting states
After doing a switch to the :ca package, just define a `<custom-name>-state` (without the brackets) closure (function that returns a function) taking any arguments, and returning a function that takes width and height of the state and returns an appropriately-sized array. 

You can use `empty-state` as base. For example, here's the definition of the `dot-state` function:

```common-lisp
(defun dot-state (&rest points)
  "Returns a function returning a state with active dots at DOTS
coordinates."
  (lambda (h w)
    (let ((array (funcall (empty-state) h w)))
      (dolist (point points)
        (setf (elt array (point-to-index point (list w h))) 1))
      array)))
```

After defining the function, you can use it by supplying `:custom-name` to the `:starting-state` keyword argument in `start`, similar to how it is handled in the previous section.

### Adding your own neighbourhoods
This case is very similar to the previous ones, except the fact that you need to define a variable instead of a function.

So, after switching to the :ca package, define a `*<custom-name>-nb*` (without brackets, but with the star characters) variable that is a list of lists with length of the dimension the neighbourhood is meant for, consisting of integers that are the coordinates of cells in relation to the cell whose neighbourhood is being evaluated (its coordinates are '(0 0)).

An example for 2-dimensional cellular automata would be the Neumann neighbourhood's implementation:
```
(defvar *neumann-nb* '((0 0) (1 0) (0 1) (-1 0) (0 -1)))
```

An example for 1-dimensional cellular automata is its "usual" neighbourhood's implementation:
```
(defvar *elementary-nb* '((-1) (0) (1)))
```

After defining the variable, you can use it by supplying `:custom-name` to the `:neighbourhood` keyword argument in `start`, similar to how it is handled in previous sections.

## Notes
Suggestions and bugreports are welcome in the Issues section.

## License

MIT license, see the [LICENSE](LICENSE) file for more.
