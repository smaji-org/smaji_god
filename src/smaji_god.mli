(*
 * smaji_god.mli
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_god.
 *)

module Svg= Smaji_glyph_path.Svg
module Glif= Smaji_glyph_path.Glif
module Animate = Animate

(*
val read_all : string -> string
val write_all : string -> string -> unit
*)

type outline_type =
  | Outline_svg
  | Outline_glif
(** The type of outline, svg and glif are currently supported. *)

type frame = { x : float; y : float; width : float; height : float; }
(** Frame described in float *)

val string_of_frame : frame -> string
(** Return the string representation of frame *)

(* The type of transform *)
type transform =
  | NoTransform
  | MirrorHorizontal
  | MirrorVertical
  | Rotate180

val transform_of_string : string -> transform
(** Return the transform from its string representation *)

val string_of_transform : transform -> string
(** Return the transform from its string representation *)

val reduce_transforms : transform list -> transform list
(** Return the reduced list from the transform list, remove all unnecessary transforms *)

type pos = { pos_x : float; pos_y : float; }
(** The type of position *)

type ratio = { ratio_x : float; ratio_y : float; }
(** The type of ratio *)

type pos_ratio = { pos : pos; ratio : ratio; }
(** The type of position and ratio *)

type pos_ratio_transform= {
  pos: pos;
  ratio: ratio;
  transform: transform;
}

val pos_ratio_default : pos_ratio
(** The default value of pos_ratio, that is, pos (0,0) and ratio (1,1) *)

val pos_ratio_adjust : pos_ratio:pos_ratio -> frame -> frame
(** Adjust frame_f with the given pos_ratio *)

type size = { width : float; height : float; }
(** The type of size *)

type code_point = int * int
(** The type of unicode code point of chinese character *)

(** The type of stroke. Currently, there are 63 different stroke types in god. *)
type stroke_type =
  | S_h     (** Horizontal *)
  | S_sh    (** Slanted Horizontal *)
  | S_u     (** Upward horizontal *)
  | S_du    (** Dot – Upward horizontal *)
  | S_v     (** Vertical *)
  | S_sv    (** Slanted Vertical *)
  | S_rsv   (** Right Slanted Vertical *)
  | S_t     (** Throw *)
  | S_ft    (** Flat Throw *)
  | S_wt    (** Wilted Throw *)
  | S_d     (** Dot *)
  | S_ed    (** Extended Dot *)
  | S_ld    (** Left Dot *)
  | S_wd    (** Wilted Dot *)
  | S_p     (** Press *)
  | S_up    (** Upward horizontal – Press *)
  | S_hp    (** Horizontal – Press *)
  | S_fp    (** Flat Press *)
  | S_ufp   (** Upward horizontal – Flat Press *)
  | S_c     (** Clockwise curve *)
  | S_a     (** Anticlockwise curve *)
  | S_o     (** Oval *)
  | S_hj    (** Horizontal – J hook *)
  | S_uj    (** Upward horizontal – J hook *)
  | S_ht    (** Horizontal – Throw *)
  | S_hsv   (** Horizontal – Slanted Vertical *)
  | S_hv    (** Horizontal – Vertical *)
  | S_hvj   (** Horizontal – Vertical – J hook *)
  | S_htj   (** Horizontal – Throw – J hook *)
  | S_utj   (** Upward horizontal – Throw – J hook *)
  | S_hvh   (** Horizontal – Vertical – Horizontal *)
  | S_hvu   (** Horizontal – Vertical – Upward horizontal *)
  | S_ha    (** Horizontal – Anticlockwise curve *)
  | S_haj   (** Horizontal – Anticlockwise curve – J hook *)
  | S_hpj   (** Horizontal – Press – J hook *)
  | S_htaj  (** Horizontal – Throw – Anticlockwise curve – J hook *)
  | S_htc   (** Horizontal – Throw – Clockwise curve *)
  | S_htht  (** Horizontal – Throw – Horizontal – Throw *)
  | S_htcj  (** Horizontal – Throw – Clockwise curve – J hook *)
  | S_hvhv  (** Horizontal – Vertical – Horizontal – Vertical *)
  | S_hthtj (** Horizontal – Throw – Horizontal – Throw – J hook *)
  | S_vu    (** Vertical – Upward horizontal *)
  | S_vh    (** Vertical – Horizontal *)
  | S_va    (** Vertical – Anticlockwise curve *)
  | S_vaj   (** Vertical – Anticlockwise curve – J hook *)
  | S_vhv   (** Vertical – Horizontal – Vertical *)
  | S_vht   (** Vertical – Horizontal – Throw *)
  | S_vhtj  (** Vertical – Horizontal – Throw – J hook *)
  | S_vj    (** Vertical – J hook *)
  | S_vc    (** Vertical – Clockwise curve *)
  | S_vcj   (** Vertical – Clockwise curve – J hook *)
  | S_tu    (** Throw – Upward horizontal *)
  | S_th    (** Throw – Horizontal *)
  | S_td    (** Throw – Dot *)
  | S_wtd   (** Wilted Throw – Dot *)
  | S_tht   (** Throw – Horizontal – Throw *)
  | S_thtj  (** Throw – Horizontal – Throw – J hook *)
  | S_tj    (** Throw – J hook *)
  | S_cj    (** Clockwise curve – J hook *)
  | S_fpj   (** Flat Press – J hook *)
  | S_pj    (** Press – J hook *)
  | S_thtaj (** Throw – Horizontal – Throw – Anticlockwise curve – J hook *)
  | S_tod   (** Throw – Oval – Dot *)

val stroke_type_of_string : string -> stroke_type
(** Return the storke type from its string representation *)

val string_of_stroke_type : stroke_type -> string
(** Return the string representation of the storke type*)

val code_point_of_string : string -> int * int
(** Return the code point from its string representation *)

val string_of_code_point : int * int -> string
(** Return the string representation of the code point *)

val code_point_of_utf8 : string -> int * int
(** Return the code of from the utf8 encoded character  *)

val version_of_string : string -> int * int
(** Return the god version from its string representation *)

type stroke = {
  stroke_type : stroke_type;
  frame : frame;
  transform: transform;
}
(** The type of stroke *)

(** The type of god.
  There are no min_x, min_y, width, height fields in the type definition.
  [god_frame god] returns these information when is needed.
 *)
type god = {
  version_major : int; (** major version *)
  version_minor : int; (** minor version *)
  code_point : code_point; (** unicode code point *)
  transform : transform; (** applied transform *)
  elements : element list; (** consists of the elements *)
  comment : string; (** comment *)
}

(** The type of subgod. *)
and subgod = {
  god : god; (** subgod *)
  frame : frame; (** and its frame where to place it in the current container *)
}

(** The type of element in god. A god can consists of strokes and/or sub gods. *)
and element = Stroke of stroke | SubGod of subgod

val god_bestfit : god -> frame
(** Calculate the bestfit frame of the god. i.e. the frame is minimum to contain all content of the god.
  This function is not used within this library. It's provided for potential user analysis of god data. *)

val god_frame : god -> frame
(** Calculate the frame of the god. same as god_bestfit, except that [min_x] and [min_y] are both zero, or in rare case less than zero if the god is deliberatly constructed as that. *)

val frame_size : frame -> size
(** Calculate the frame size. *)

val string_of_stroke : stroke -> string
(** Return the string representation of the stroke *)

val string_of_element : ?indent:int -> element -> string
(** Return the string representation of the element *)

val string_of_god : ?indent:int -> god -> string
(** Return the string representation of the god *)

val of_string : dir:string -> ?filename:string -> string -> god
(** [load_file ~dir ?filename string] parses [string] and returns a god, the [filename] is "default.xml" if is not specified. Because a god can reference other god as element, so the file hierarchy in [dir] is hierarchically structured. *)

val load_file : dir:string -> ?filename:string -> code_point -> god
(** [load_file ~dir ?filename (core,variation)] loads [dir]/core/variation/[filename] then parses and returns a god, the [filename] is "default.xml" if is not specified. Because a god can reference other god as element, so the file hierarchy in [dir] is hierarchically structured. *)

val god_flatten : ?pos_ratio_transform:pos_ratio_transform -> god -> stroke list
(** [god_flatten ?pos_ratio god] flattens the structured into a list of storkes, transformed by pos_ratio *)

(** Module Map with stroke_type as type key *)
module StrokeMap : Map.S with type key = stroke_type

val load_glyphs : dir:string -> Svg.t StrokeMap.t
(** [load_glyphs ~dir] loads basic stroke glyphs from [dir], and returns [Svg.t StrokeMap.t] *)

val load_animates : dir:string -> Animate.t StrokeMap.t
(** [load_animates ~dir] loads basic stroke gnimations from [dir], and returns [Animate.t StrokeMap.t] *)

val convert_to_glif_glyphs : Svg.t StrokeMap.t -> Glif.t StrokeMap.t

(** Return the svg part of the stroke. NOTE, stroke.transform is ignored for now. *)
val svg_of_stroke :
  stroke_glyph:Svg.t StrokeMap.t -> stroke -> Svg.t

(** Return the paths of the svg part of the stroke *)
val paths_of_stroke :
  stroke_glyph:Svg.t StrokeMap.t ->
  stroke -> Animate.Path.t list

(** Return the animate part of the stroke *)
val animate_of_stroke :
  stroke_animate:Animate.t StrokeMap.t -> stroke -> Animate.t

(** Return the animation masks of the stroke *)
val animations_of_stroke :
  stroke_animate:Animate.t StrokeMap.t -> stroke -> Rect.animation list

(** Return the svg outline of the god. *)
val svg_of_god :
  stroke_glyph:Svg.t StrokeMap.t -> god -> Svg.t

(** Return the svg-formatted outline of the god *)
val outline_svg_of_god :
  stroke_glyph:Svg.t StrokeMap.t -> god -> string

(** Return the svg-formatted animation of the god *)
val animate_svg_of_god :
  stroke_animate:Animate.t StrokeMap.t -> god -> string

type glif_of_god=
  | Glif of Glif.t
  | Wrapped of { wrap: Glif.t; content: Glif.t }

(** Return the glif-formatted outline of the god *)
val outline_glif_of_god :
  stroke_glyph:Glif.t StrokeMap.t -> god -> glif_of_god

