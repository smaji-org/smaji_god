(*
 * animate.mli
 * -----------
 * Copyright : (c) 2023 - 2023, smaji.org
 * Copyright : (c) 2023 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_god.
 *)

module Svg= Smaji_glyph_outline.Svg
module Path= Svg.Path
module ViewBox= Svg.ViewBox

type svg = Svg.t

(** The type of animate *)
type t = {
  svg : svg; (** outline *)
  animations : Rect.animation list; (** animate mask *)
}

val to_string :
  ?id:int -> ?time:float -> ?indent:int -> ?indent_step:int -> t -> string
  (** Return the svg-formatted string from the animate. Initial [id] specifies the initial name, [time] the initial start time, [indent] and [indent_step] the indentation format. *)

val load_file : string -> t option
(** [load_file path] loads the god animate file specified by [path], if succeeded, returns [Some t], otherwise, None is returned. *)

val load_file_exn : string -> t
(** [load_file path] loads the god animate file specified by [path] and return [t], exception Not_found is raised if failed. *)

module Adjust :
  sig
    val reset_viewBox : t -> t
    val fit_frame : t -> t
    val scale : x:float -> y:float -> t -> t
    val translate : dx:float -> dy:float -> t -> t
  end

