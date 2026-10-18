# drinkR with Shinylive

This directory adapts `../shiny_app` for Shinylive. The generated `site/` directory runs
Shiny and R in the browser using WebAssembly, without an R server.

The public app is hosted at <https://rasmusab.github.io/drinkr/>. 

The files are:

- `server.R`, `ui.R`, `helper_functions.R`: application source.
- `drink_info.csv`: drink presets.
- `build.R`: regenerates the static app.
- `site/`: the complete exported app and runtime assets.
- `README.md`: build, preview, and hosting instructions.

