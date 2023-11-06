# Formatting in Ocaml (DOESNT WORK)

## 1. Step
- Create a fresh new folder and put some OCaml file into it, let's name it `test.ml` and let's put into it the following code,

```ocaml
let test = [
  "hello"
]
```

## 2. Step
- create a fresh local opam switch, by issuing in the same folder as the `test.ml` file 

```powershell
opam switch create .
```

## 3. Step
- Install the required dependencies.

```powershell
$ opam install ocamlformat ocamlformat-rpc ocaml-lsp-server
```

## 4. Step
- Create the `.ocamlformat` file in the same folder as the `test.ml` file. 
- It will tell ocamlformat that you want to be ocamlformatted and you can use this file to setup your preferences.
- Add:
```ocaml
profile = default
version = 0.24.1
```

## 5. Step
- Install Ocaml extension
- Start vscode and open test.ml. 
- It will ask you to select the sandbox.
- The code syntax should be highlighted and there should be no error messages from vscode. 
- Finally, hit to format file

```
Ctrl-Shift-I
``` 

- Code should appear
```ocaml
let test = [ "hello" ]
```


# Format Code Nicely 
- To set up your project with ocamlformat version, create a configuration file named `.ocamlformat` in `file.ml` folder containing:
```ocaml
(* src/.ocamlformat *)
profile = default
version = 0.24.1
```

- To format run:
```powershell
$ ocamlformat file.ml
```

- Another way:
```powershell
$ dune fmt
```

# Sandbox
- Compiler aka sandbox aka switch

## Delete Sandbox
```powershell
$ opam switch delete <switch-name>
```

## Switch Sandbox
```powershell
$ opam switch <switch-name>
```
