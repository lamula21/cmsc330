# Use Ocaml extension
```powershell
$ dune build --watch
```

# Test Locally
```powershell
$ dune runtest -f
```

# Test Specific File
```powershell
$ dune runtest test/student
```

# Load Files into Interactive Shell (UTOP)
```powershell
dune utop src
```
## Show Project Requirements
- Load Function definitions in utop
```utop
open Basics;;
```

- Show these definitions
```utop
# show rev_tup;;
```

- Test function by simply passing values
```utop
rev_utop (3,4,5);;

=> int * int = (5,4,3)
```

# Testing
## Compile Test
- It generates folder _build
```powershell
$ dune build test/public/public.exe
```

## List Tests
```powershell
$ _build/default/test/public/public.exe -list-test
```
## Run Specific Tests
```powershell
$ _build/default/test/public/public.exe -only-test public:0
```