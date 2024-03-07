# Pending

* Allow exact location of `libSDL2_image-2.0.so.0` to be set in
  `LIBSDL2_IMAGE_SHLIB` for situations where you have a debug shared
  library or you don't want `pkg-config` invoked.
* (@linsyking) https://github.com/sanette/tsdl-image/pull/10
  add functions from SDL_IMAGE 2.0.2
    IMG_isSVG
    IMG_LoadSVG_RW
    IMG_SaveJPG_RW
    IMG_SaveJPG
  
# 0.6 2023/08/05 hide startup log

* Fix typo environment lookup of `LIBSLD2_PATH` to be correct `LIBSDL2_PATH`
* Locate `SDL2_image.dll` on Windows
* Use SDL "system" category to log "Loading Sdl_image" message (we now
  log only for dev versions).

# 0.5 2022/11/30 trying to autodetect library path

And add workflow for github actions for testing dynamic libraries from
bytecode.

# 0.4 2022/11/22 Bug fix (calling ocaml on an *.ml file)

You can now (again) directly run a "toplevel file" with `ocaml`, for
instance

```
#use "topfind";;
#thread;;
#require "tsdl-image";;
Tsdl_image.Image.(init Init.empty) |> ignore
```
Thanks @anentropic for reporting on MacOS.

# 0.3 Change library name (BREAKING)

Starting from 0.3, the library name is the same as the opam package
name `tsdl-image`. (The library name used to be `tsdl_image`, which was
confusing).

# 2021 new maintainer is sanette
https://github.com/sanette/tsdl-image
