(*---------------------------------------------------------------------------
    Copyright (c) 2016 The tsdl-image programmers. All rights reserved.
  ---------------------------------------------------------------------------*)
open Ctypes
open Foreign
open Tsdl

module Image = struct
  type 'a result = 'a Sdl.result

  (* Set [debug] to true to print the foreign symbols in the CI. *)
  let debug =
    Sys.getenv_opt "OCAMLCI" = Some "true"
    || Sys.getenv_opt "TSDL_DEBUG" = Some "true"

  let pre = if debug then print_endline else ignore
  let error () = Error (`Msg (Sdl.get_error ()))
  let bool = view ~read:(( <> ) 0) ~write:(fun b -> compare b false) int

  module Init = struct
    type t = Unsigned.uint32

    let i = Unsigned.UInt32.of_int
    let ( + ) = Unsigned.UInt32.logor
    let test f m = Unsigned.UInt32.(compare (logand f m) zero <> 0)
    let eq f f' = Unsigned.UInt32.(compare f f' = 0)
    let empty = i 0
    let jpg = i 1
    let png = i 2
    let tif = i 4
    let webp = i 8
    let jxl = i 16
    let avif = i 32
  end

  (* pkg-config --variable=libdir SDL2_image *)
  (* Use Configurator.V1.Pkg_config instead? *)
  let pkg_config () =
    try
      let ic = Unix.open_process_in "pkg-config --variable=libdir SDL2_image" in
      let dir = input_line ic in
      close_in ic;
      Some dir
    with _ -> None

  (* Dynamic loading at runtime solves two issues:
     + On linux if you want to use #require "tsdl-image"
       in the toplevel, see
       https://github.com/ocamllabs/ocaml-ctypes/issues/70
     + On Windows, linking with pkg-config flags raises the issue
       of the wrong flags "-mwindows" "SDLMain", see
       https://github.com/ocaml/flexdll/issues/163#issuecomment-3732220603
  *)
  let perform_search () : Dl.library option =
    (if debug then
       Sdl.(
         log_info Log.category_system "Loading Sdl_image, Target = %s"
           Build_config.system));
    let env = try Sys.getenv "LIBSDL2_PATH" with Not_found -> "" in
    let filename, path =
      (* In principle only the basename is enough because the appropriate PATH
         is used if SDL2_image was installed properly. We provide below more
         hardcoded paths for non-standard installs where PATH is not correctly
         set. *)
      pre ("Build_config.system = " ^ Build_config.system);
      match (Sys.os_type, Build_config.system) with
      | _, "macosx" ->
          ( "libSDL2_image.dylib",
            [ ""; "/opt/homebrew/lib/"; "/opt/local/lib/"; "/usr/local/lib/" ]
          )
      | "Win32", _ | "Cygwin", _ ->
          ( "SDL2_image.dll",
            [
              "";
              "/SDL2/SDL2_image/x86_64-w64-mingw32/bin";
              "/usr/x86_64-w64-mingw32/sys-root/mingw/bin";
              "/usr/i686-w64-mingw32/sys-root/mingw/bin";
              "/clangarm64/bin";
              "/clang64/bin";
              "/clang32/bin";
              "/ucrt64/bin";
              "/mingw64/bin";
              "/mingw32/bin";
            ] )
      | _ ->
          ( "libSDL2_image.so",
            [ ""; "/usr/lib/x86_64-linux-gnu/"; "/usr/local/lib" ] )
    in
    let rec loop = function
      | [] -> None
      | dir :: rest -> (
          let filename =
            if dir = "" then filename else Filename.concat dir filename
          in
          pre ("Trying: " ^ filename);
          try Some Dl.(dlopen ~filename ~flags:[ RTLD_NOW ])
          with _ -> loop rest)
    in
    match loop (env :: path) with
    | Some f -> Some f
    | None -> (
        (* We execute pkg_config only if everything else failed. *)
        match pkg_config () with
        | Some dir -> loop [ dir ]
        | None ->
            print_endline
              ("Cannot find " ^ filename ^ ", please set LIBSDL2_PATH");
            None)

  let from : Dl.library option =
    let shlib = try Sys.getenv "LIBSDL2_IMAGE_SHLIB" with Not_found -> "" in
    if shlib = "" then perform_search ()
    else Some Dl.(dlopen ~filename:shlib ~flags:[ RTLD_NOW ])

  let foreign = foreign ?from

  let init =
    pre "IMG_Init";
    foreign "IMG_Init" (uint32_t @-> returning uint32_t)

  let quit =
    pre "IMG_Quit";
    foreign "IMG_Quit" (void @-> returning void)

  let version = structure "SDL_version"
  let version_major = field version "major" uint8_t
  let version_minor = field version "minor" uint8_t
  let version_patch = field version "patch" uint8_t
  let () = seal version

  let linked_version =
    pre "IMG_Linked_Version";
    foreign "IMG_Linked_Version" (void @-> returning (ptr version))

  let linked_version () =
    let get v f = Unsigned.UInt8.to_int (getf v f) in
    let v = linked_version () in
    let v = !@v in
    (get v version_major, get v version_minor, get v version_patch)

  let version = linked_version ()

  let () =
    if debug then
      let a, b, c = version in
      Sdl.log "SDL_image Version (%u,%u,%u)" a b c

  let surface =
    view ~read:Sdl.unsafe_surface_of_ptr ~write:Sdl.unsafe_ptr_of_surface
      nativeint

  let texture_result =
    let read v =
      if Nativeint.(compare v zero) = 0 then error ()
      else Ok (Sdl.unsafe_texture_of_ptr v)
    and write = function
      | Ok v -> Sdl.unsafe_ptr_of_texture v
      | Error _ -> raw_address_of_ptr null
    in
    view ~read ~write nativeint

  let surface_result =
    let read v =
      if Nativeint.(compare v zero) = 0 then error ()
      else Ok (Sdl.unsafe_surface_of_ptr v)
    and write = function
      | Ok v -> Sdl.unsafe_ptr_of_surface v
      | Error _ -> raw_address_of_ptr null
    in
    view ~read ~write nativeint

  let rw_ops =
    view ~read:Sdl.unsafe_rw_ops_of_ptr ~write:Sdl.unsafe_ptr_of_rw_ops
      nativeint

  let renderer =
    view ~read:Sdl.unsafe_renderer_of_ptr ~write:Sdl.unsafe_ptr_of_renderer
      nativeint

  let load =
    pre "IMG_Load";
    foreign "IMG_Load" (string @-> returning surface_result)

  let load_rw =
    pre "IMG_Load_RW";
    foreign "IMG_Load_RW" (rw_ops @-> bool @-> returning surface_result)

  type format =
    | Avif
    | Cur
    | Bmp
    | Gif
    | Ico
    | Jpg
    | Jxl
    | Lbm
    | Pcx
    | Png
    | Pnm
    | Svg
    | Qoi
    | Tga
    | Tif
    | Xcf
    | Xpm
    | Xv
    | Webp

  let string_of_format = function
    | Avif -> "AVIF"
    | Cur -> "CUR"
    | Bmp -> "BMP"
    | Gif -> "GIF"
    | Ico -> "ICO"
    | Jpg -> "JPG"
    | Jxl -> "JXL"
    | Lbm -> "LBM"
    | Pcx -> "PCX"
    | Png -> "PNG"
    | Pnm -> "PNM"
    | Qoi -> "QOI"
    | Svg -> "SVG"
    | Tga -> "TGA"
    | Tif -> "TIF"
    | Xcf -> "XCF"
    | Xpm -> "XPM"
    | Xv -> "XV"
    | Webp -> "WEBP"

  let load_typed_rw =
    pre "IMG_LoadTyped_RW";
    foreign "IMG_LoadTyped_RW"
      (rw_ops @-> bool @-> string @-> returning surface_result)

  let load_typed_rw r b f = load_typed_rw r b (string_of_format f)

  let load_texture =
    pre "IMG_LoadTexture";
    foreign "IMG_LoadTexture" (renderer @-> string @-> returning texture_result)

  let load_texture_rw =
    pre "IMG_LoadTexture_RW";
    foreign "IMG_LoadTexture_RW"
      (renderer @-> rw_ops @-> bool @-> returning texture_result)

  let load_texture_typed_rw =
    pre "IMG_LoadTextureTyped_RW";
    foreign "IMG_LoadTextureTyped_RW"
      (renderer @-> rw_ops @-> bool @-> string @-> returning texture_result)

  let load_texture_typed_rw r o b f =
    load_texture_typed_rw r o b (string_of_format f)

  let is_avif =
    pre "IMG_isAVIF";
    foreign "IMG_isAVIF" (rw_ops @-> returning bool)

  let is_cur =
    pre "IMG_isCUR";
    foreign "IMG_isCUR" (rw_ops @-> returning bool)

  let is_bmp =
    pre "IMG_isBMP";
    foreign "IMG_isBMP" (rw_ops @-> returning bool)

  let is_gif =
    pre "IMG_isGIF";
    foreign "IMG_isGIF" (rw_ops @-> returning bool)

  let is_ico =
    pre "IMG_isICO";
    foreign "IMG_isICO" (rw_ops @-> returning bool)

  let is_jpg =
    pre "IMG_isJPG";
    foreign "IMG_isJPG" (rw_ops @-> returning bool)

  let is_jxl =
    pre "IMG_isJXL";
    foreign "IMG_isJXL" (rw_ops @-> returning bool)

  let is_lbm =
    pre "IMG_isLBM";
    foreign "IMG_isLBM" (rw_ops @-> returning bool)

  let is_pcx =
    pre "IMG_isPCX";
    foreign "IMG_isPCX" (rw_ops @-> returning bool)

  let is_png =
    pre "IMG_isPNG";
    foreign "IMG_isPNG" (rw_ops @-> returning bool)

  let is_pnm =
    pre "IMG_isPNM";
    foreign "IMG_isPNM" (rw_ops @-> returning bool)

  let is_qoi =
    pre "IMG_isQOI";
    foreign "IMG_isQOI" (rw_ops @-> returning bool)

  let is_svg =
    pre "IMG_isSVG";
    foreign "IMG_isSVG" (rw_ops @-> returning bool)

  let is_tif =
    pre "IMG_isTIF";
    foreign "IMG_isTIF" (rw_ops @-> returning bool)

  let is_xcf =
    pre "IMG_isXCF";
    foreign "IMG_isXCF" (rw_ops @-> returning bool)

  let is_xpm =
    pre "IMG_isXPM";
    foreign "IMG_isXPM" (rw_ops @-> returning bool)

  let is_xv =
    pre "IMG_isXV";
    foreign "IMG_isXV" (rw_ops @-> returning bool)

  let is_webp =
    pre "IMG_isWEBP";
    foreign "IMG_isWEBP" (rw_ops @-> returning bool)

  let is_format fmt =
    match fmt with
    | Avif -> is_avif
    | Cur -> is_cur
    | Bmp -> is_bmp
    | Gif -> is_gif
    | Ico -> is_ico
    | Jpg -> is_jpg
    | Jxl -> is_jxl
    | Lbm -> is_lbm
    | Pcx -> is_pcx
    | Png -> is_png
    | Pnm -> is_pnm
    | Qoi -> is_qoi
    | Svg -> is_svg
    | Tga -> failwith "TGA cannot safely be detected"
    | Tif -> is_tif
    | Xcf -> is_xcf
    | Xpm -> is_xpm
    | Xv -> is_xv
    | Webp -> is_webp

  let load_avif_rw =
    pre "IMG_LoadAVIF_RW";
    foreign "IMG_LoadAVIF_RW" (rw_ops @-> returning surface_result)

  let load_cur_rw =
    pre "IMG_LoadCUR_RW";
    foreign "IMG_LoadCUR_RW" (rw_ops @-> returning surface_result)

  let load_bmp_rw =
    pre "IMG_LoadBMP_RW";
    foreign "IMG_LoadBMP_RW" (rw_ops @-> returning surface_result)

  let load_gif_rw =
    pre "IMG_LoadGIF_RW";
    foreign "IMG_LoadGIF_RW" (rw_ops @-> returning surface_result)

  let load_ico_rw =
    pre "IMG_LoadICO_RW";
    foreign "IMG_LoadICO_RW" (rw_ops @-> returning surface_result)

  let load_jpg_rw =
    pre "IMG_LoadJPG_RW";
    foreign "IMG_LoadJPG_RW" (rw_ops @-> returning surface_result)

  let load_jxl_rw =
    foreign "IMG_LoadJXL_RW" (rw_ops @-> returning surface_result)

  let load_lbm_rw =
    pre "IMG_LoadLBM_RW";
    foreign "IMG_LoadLBM_RW" (rw_ops @-> returning surface_result)

  let load_pcx_rw =
    pre "IMG_LoadPCX_RW";
    foreign "IMG_LoadPCX_RW" (rw_ops @-> returning surface_result)

  let load_png_rw =
    pre "IMG_LoadPNG_RW";
    foreign "IMG_LoadPNG_RW" (rw_ops @-> returning surface_result)

  let load_pnm_rw =
    pre "IMG_LoadPNM_RW";
    foreign "IMG_LoadPNM_RW" (rw_ops @-> returning surface_result)

  let load_qoi_rw =
    foreign "IMG_LoadQOI_RW" (rw_ops @-> returning surface_result)

  let load_tga_rw =
    pre "IMG_LoadTGA_RW";
    foreign "IMG_LoadTGA_RW" (rw_ops @-> returning surface_result)

  let load_svg_rw =
    pre "IMG_LoadSVG_RW";
    foreign "IMG_LoadSVG_RW" (rw_ops @-> returning surface_result)

  let load_tif_rw =
    pre "IMG_LoadTIF_RW";
    foreign "IMG_LoadTIF_RW" (rw_ops @-> returning surface_result)

  let load_xcf_rw =
    pre "IMG_LoadXCF_RW";
    foreign "IMG_LoadXCF_RW" (rw_ops @-> returning surface_result)

  let load_xpm_rw =
    pre "IMG_LoadXPM_RW";
    foreign "IMG_LoadXPM_RW" (rw_ops @-> returning surface_result)

  let load_xv_rw =
    pre "IMG_LoadXV_RW";
    foreign "IMG_LoadXV_RW" (rw_ops @-> returning surface_result)

  let load_webp_rw =
    pre "IMG_LoadWEBP_RW";
    foreign "IMG_LoadWEBP_RW" (rw_ops @-> returning surface_result)

  let load_format_rw = function
    | Avif -> load_avif_rw
    | Cur -> load_cur_rw
    | Bmp -> load_bmp_rw
    | Gif -> load_gif_rw
    | Ico -> load_ico_rw
    | Jpg -> load_jpg_rw
    | Jxl -> load_jxl_rw
    | Lbm -> load_lbm_rw
    | Pcx -> load_pcx_rw
    | Png -> load_png_rw
    | Pnm -> load_pnm_rw
    | Qoi -> load_qoi_rw
    | Svg -> load_svg_rw
    | Tga -> load_tga_rw
    | Tif -> load_tif_rw
    | Xcf -> load_xcf_rw
    | Xpm -> load_xpm_rw
    | Xv -> load_xv_rw
    | Webp -> load_webp_rw

  let load_sized_svg_rw =
    pre "IMG_LoadSizedSVG_RW";
    foreign "IMG_LoadSizedSVG_RW"
      (rw_ops @-> int @-> int @-> returning surface_result)

  let read_xpm_from_array =
    pre "IMG_ReadXPMFromArray";
    foreign "IMG_ReadXPMFromArray" (string @-> returning surface_result)

  let save_png =
    pre "IMG_SavePNG";
    foreign "IMG_SavePNG" (surface @-> string @-> returning int)

  let save_png_rw =
    pre "IMG_SavePNG_RW";
    foreign "IMG_SavePNG_RW" (surface @-> rw_ops @-> bool @-> returning int)

  let save_jpg =
    pre "IMG_SaveJPG";
    foreign "IMG_SaveJPG" (surface @-> string @-> int @-> returning int)

  let save_jpg_rw =
    pre "IMG_SaveJPG_RW";
    foreign "IMG_SaveJPG_RW"
      (surface @-> rw_ops @-> bool @-> int @-> returning int)
end
