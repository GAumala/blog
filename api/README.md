# api

Execute blog API server:

```
SQLITE_FILE=api.sqlite stack exec api-exe
```

Or call binary directly:

```
stack build --copy-bins --local-bin-path output
SQLITE_FILE=api.sqlite output/api-exe
```
