# Proof of Concepts with HumbleUI

A soup of unpolished code from my explorations of Clojure's potential to create dynamic and humane computing experiences.

Usage: `clojure -M:dev` or `bb dev`

Info:

- https://twitter.com/LuisThiamNye/status/1509564956643057666
- https://twitter.com/LuisThiamNye/status/1509569953531322381
- https://www.youtube.com/watch?v=25ijoz5ZrB8

Contents:

- Dependency graph viewer
- Error boundaries that show an image of the canvas before the error.
- Stacktrace viewer where you can expand the frames to see the source of the relevant var and the line of the call site
  - Collapses repeating frames of a StackOverflowError
- Basic text input
- Basic vim-like editor -- to be done better
- Clojure var browser
- Basic file browser with delete function
