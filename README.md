# oh_webc

This is a fork of [WebC](https://github.com/11ty/webc) library that is written 
(work in progress) in [OCaml](https://ocaml.org/). Ocaml code is tranformed to 
Javascript with [Melange](https://github.com/melange-re/melange).

## Notes

### Test generate code
To run tests (npm run test) in '_build/default/main/' need to add package.json
to '_build/default/main/node_modules/{melange,melange.js}' directories.
Have to find a better way to make it work.

Content of package.json:
```json
{"type":"module"}
```

Or could run these commands in '_build/default/main/' directory
```
$ echo '{"type":"module"}' > node_modules/melange/package.json
$ echo '{"type":"module"}' > node_modules/melange.js/package.json
```
