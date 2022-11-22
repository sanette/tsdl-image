# 0.4 2022/11/22 Bug fix (calling ocaml on an *.ml file)

You can now (again) directly run a "toplevel file" with `ocaml`, for
instance

```
#use "topfind";;
#thread;;
#require "tsdl-image";;
Tsdl_image.Image.(init Init.empty) |> ignore
```

# 0.3 Change library name (BREAKING)

Starting from 0.3, the library name is the same as the opam package
name `tsdl-image`. (The library name used to be `tsdl_image`, which was
confusing).

# new maintainer is sanette
https://github.com/sanette/tsdl-image
