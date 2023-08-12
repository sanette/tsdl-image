(*---------------------------------------------------------------------------
   Copyright (c) 2016-2023 The tsdl-image programmers. All rights reserved.
  ---------------------------------------------------------------------------*)

(** SDL2_image bindings for use with Tsdl

    {b References}

    - {{:https://github.com/sanette/tsdl-image} source on github}

    - {{:https://wiki.libsdl.org/SDL_image/} SDL_image API} *)
module Image : sig
  type 'a result = 'a Tsdl.Sdl.result

  (** {1 Initialization} *)

  module Init : sig
    type t

    val ( + ) : t -> t -> t
    val test : t -> t -> bool
    val eq : t -> t -> bool
    val empty : t
    val jpg : t
    val png : t
    val tif : t
    val webp : t
    val jxl : t
    val avif : t
  end

  val init : Init.t -> Init.t
  (** {{:https://wiki.libsdl.org/SDL_image/IMG_Init} IMG_Init} *)

  val quit : unit -> unit
  (** {{:https://wiki.libsdl.org/SDL_image/IMG_Quit} IMG_Quit} *)

  type format =
    | Avif
    | Ico
    | Cur
    | Bmp
    | Gif
    | Jpg
    | Jxl
    | Lbm
    | Pcx
    | Png
    | Pnm
    | Svg
    | Qoi
    | Tif
    | Xcf
    | Xpm
    | Xv
    | Webp
    | Tga

  (** {1 Loading} *)

  val load : string -> Tsdl.Sdl.surface result
  (** {{:https://wiki.libsdl.org/SDL_image/IMG_Load} IMG_Load} *)

  val load_rw : Tsdl.Sdl.rw_ops -> bool -> Tsdl.Sdl.surface result
  (** {{:https://wiki.libsdl.org/SDL_image/IMG_Load_RW} IMG_Load_RW} *)

  val load_typed_rw :
    Tsdl.Sdl.rw_ops -> bool -> format -> Tsdl.Sdl.surface result
  (** {{:https://wiki.libsdl.org/SDL_image/IMG_LoadTyped_RW} IMG_LoadTyped_RW} *)

  val load_texture : Tsdl.Sdl.renderer -> string -> Tsdl.Sdl.texture result
  (** {{:https://wiki.libsdl.org/SDL2_image/IMG_LoadTexture} IMG_LoadTexture} *)

  val load_texture_rw :
    Tsdl.Sdl.renderer -> Tsdl.Sdl.rw_ops -> bool -> Tsdl.Sdl.texture result
  (** {{:https://wiki.libsdl.org/SDL2_image/IMG_LoadTexture_RW}
        IMG_LoadTexture_RW} *)

  val load_texture_typed_rw :
    Tsdl.Sdl.renderer ->
    Tsdl.Sdl.rw_ops ->
    bool ->
    format ->
    Tsdl.Sdl.texture result
  (** {{:https://wiki.libsdl.org/SDL2_image/IMG_LoadTextureTyped_RW}
        IMG_LoadTextureTyped_RW} *)

  val load_format_rw : format -> Tsdl.Sdl.rw_ops -> Tsdl.Sdl.surface result

  val load_sized_svg_rw :
    Tsdl.Sdl.rw_ops -> int -> int -> Tsdl.Sdl.surface result
  (** {{:https://wiki.libsdl.org/SDL2_image/IMG_LoadSizedSVG_RW}
        IMG_LoadSizedSVG_RW} *)

  val read_xpm_from_array : string -> Tsdl.Sdl.surface result
  (** {{:https://wiki.libsdl.org/SDL_image/IMG_ReadXPMFromArray}
        IMG_ReadXPMFromArray} *)

  (** {1 Saving} *)

  val save_png : Tsdl.Sdl.surface -> string -> int
  (** {{:https://wiki.libsdl.org/SDL2_image/IMG_SavePNG} IMG_SavePNG} *)

  val save_png_rw : Tsdl.Sdl.surface -> Tsdl.Sdl.rw_ops -> bool -> int
  (** {{:https://wiki.libsdl.org/SDL2_image/IMG_SavePNG_RW} IMG_SavePNG_RW} *)

  val save_jpg : Tsdl.Sdl.surface -> string -> int -> int
  (** {{:https://wiki.libsdl.org/SDL2_image/IMG_SaveJPG} IMG_SaveJPG} *)

  val save_jpg_rw : Tsdl.Sdl.surface -> Tsdl.Sdl.rw_ops -> bool -> int -> int
  (** {{:https://wiki.libsdl.org/SDL2_image/IMG_SaveJPG_RW} IMG_SaveJPG_RW} *)

  (** {1 Info} *)

  val is_format : format -> Tsdl.Sdl.rw_ops -> bool
  (** {{:https://wiki.libsdl.org/SDL_image/IMG_isAVIF#related_functions}
        IMG_is*}

      Note that, uniquely, [is_format Tga] will throw an exception, as SDL_image
      does not support testing if a file is in Targa format. *)
end
