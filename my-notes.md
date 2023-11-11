Running 'npm run test' in _build/default/main produces error: 
'SyntaxError: Cannot use import statement outside a module'.
For now to fix this I manually added package.json files to 
'<main>/node_modules/{melange,melage.js}' directories. package.json content
'{"type":"module"}'.
