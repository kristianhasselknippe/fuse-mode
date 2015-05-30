# fuse-emacs
Emacs plugin for Fuse


To run:

1. Load fuse.el
2. Make sure Fuse is running
3. M-x ielm
4. in ielm (create-connection)
5. (fuse-status) to check is connection was sucessful

Current implemented commands:

- (send-recompile)
  - Recompiles the project
- (send-request-code-completion)


I am currently using csharp-mode for uno and nXML-mode for UX.
These modes work quite nice until we get to do proper UNO and UX syntax highlighting, and we might be able
to derive from these.

Code completion currently does nothing with the result. We could make a backend for AC or Company.

## fuse.el: (delegate-command (command-string))
All incoming responses are filtered through this function.
Currently only WirteToConsole and SetCodeSuggestions are filtered.

See https://www.fusetools.com/developers/forums/general/fuse_text_editor_api
for full protocol specs.
