# Reopt VSCode plugin

Note. This tool is still under active development.  The features
described here are not yet fully working and subject to change.

Reopt's VSCode plugin helps users better understand
the contents of compiled executables and use Reopt's capabilities
to extract LLVM bytecode from files as well as recompile files
into new executables.

## Using the Reopt in VSCode

The Reopt VSCode plugin introduces a new file type, Reopt project
files.  These store information for running Reopt on a specific
binaries including paths to the binaries.  There are operations
for creating a project file from a binary, and a webview for
viewing the analysis results of a VSCode project file.

## Building

This VSCode plugin is built like many others.  After checking it out,
you should ensure that [Node](https://nodejs.org/) is installed along
with NPM.  Once NPM is installed, you can download dependencies
by running `npm install`.  To compile the plugin, you can then type
`npm run compile` to build the application.

To build a bundled VSix extension file, you need [vsce](https://github.com/Microsoft/vscode-vsce) (`npm i -g vsce`), and then run `vsce package`.


If you want to update dependencies ot the latest version, the easiest approach
is via [npm-check-updates](https://www.npmjs.com/package/npm-check-updates) (`npm i -g npm-check-updates`).  Once installed, run `ncu` to update `package.json`.