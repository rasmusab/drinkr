# drinkR with Shinylive

This directory adapts `../shiny_app` for Shinylive. The generated `site/` directory runs
Shiny and R in the browser using WebAssembly, without an R server.

The files kept here are:

- `server.R`, `ui.R`, `helper_functions.R`: application source.
- `drink_info.csv`: drink presets.
- `build.R`: regenerates the static app.
- `site/`: the complete exported app and runtime assets.
- `README.md`: build, preview, and hosting instructions.

The copied deployment records, scratchpad, and preview screenshot have been
removed. The original deployment records and scratchpad remain in `../shiny_app`.

## Preview the exported app

From the repository root, run:

```sh
python3 -m http.server 8008 --bind 127.0.0.1 --directory shinylive_app/site
```

Open <http://127.0.0.1:8008/>. Stop the preview with Ctrl+C.

The entry point is `site/index.html`. Keep the entire `site/` directory together;
it contains the R runtime, WebAssembly packages, application source, and other
required assets. Serve it over HTTP or HTTPS rather than opening `index.html`
directly as a `file://` URL.

## Rebuild after editing the R source

Install the exporter once in R:

```r
install.packages("shinylive", repos = "https://cloud.r-project.org")
```

Then, from the repository root:

```sh
Rscript shinylive_app/build.R
```

The build uses Shinylive web assets 0.10.12 and includes WebAssembly package
binaries with `wasm_packages = TRUE`. It was built with the shinylive R package 0.5.0.
It exports `server.R`, `ui.R`, `helper_functions.R`, and `drink_info.csv` through
a temporary staging directory, so build scripts, documentation, and previous
output are not included in the app.
Building requires Internet access to obtain uncached Shinylive assets and packages.

## Changes from the original app

The original `../shiny_app` directory is unchanged. In this copy, adding a drink
waits for an actual button click and for the drink inputs to be ready. The time
selector and plot wait for the browser's clock inputs before calculating dates.
The add/remove icons use current Font Awesome names. The BAC calculations and
drink presets are unchanged.

## Static hosting

Upload the contents of `site/` to any static HTTP(S) host that serves JavaScript,
WebAssembly, and binary data files. No Shiny server is required. This is a static
web app consisting of HTML and supporting files, not a single self-contained
HTML file.

## Publish manually to GitHub Pages

The public app is hosted at <https://rasmusab.github.io/drinkr/>. GitHub Pages
publishes the root of the separate `gh-pages` branch. There is no custom
deployment workflow in this repository, and pushing to `master` alone does not
update the published app.

After making changes, run these commands from the repository root:

```sh
Rscript shinylive_app/build.R
git add shinylive_app
git commit -m "Update Shinylive app"
git push origin master
git subtree push --prefix=shinylive_app/site origin gh-pages
```

The last command publishes only the exported site files. GitHub then serves the
updated site after its Pages deployment completes. The `.nojekyll` file tells
Pages to serve the exported files without Jekyll processing.

See the [Shinylive R package](https://github.com/posit-dev/r-shinylive) and
[export documentation](https://posit-dev.github.io/r-shinylive/reference/export.html).
