(*
 * smaji_god.ml
 * -----------
 * Copyright : (c) 2023 - 2023, smaji.org
 * Copyright : (c) 2023 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_god.
 *)

open Smaji_glyph_outline.Utils

module Animate = Animate
module Svg= Smaji_glyph_outline.Svg
module Glif= Smaji_glyph_outline.Glif

open Printf

(*
let read_all path=
  let chan= In_channel.open_text path in
  let data= In_channel.input_all chan in
  In_channel.close chan;
  data

let write_all path data=
  let chan= Out_channel.open_text path in
  Out_channel.output_string chan data;
  Out_channel.close chan
*)

(*
let prepare ()=
  Sys.readdir "svg"
    |> Array.iter (fun name->
      let name_in= Filename.concat "svg" name
      and name_out= Filename.concat "out" name in
      match Svg.load_file name_in with
      | Some svg-> svg
          |> Svg.Adjust.fit_frame
          |> Svg.to_string
          |> write_all name_out
      | None-> ()
      )
*)

type outline_type=
  | Outline_svg
  | Outline_glif

type frame= {
  x: int;
  y: int;
  width: int;
  height: int;
}

type frame_f= {
  x: float;
  y: float;
  width: float;
  height: float;
}

let frame_f_of_frame (frame:frame)= {
  x= float_of_int frame.x;
  y= float_of_int frame.y;
  width= float_of_int frame.width;
  height= float_of_int frame.height;
}

let frame_of_frame_f frame: frame= {
  x= int_of_float frame.x;
  y= int_of_float frame.y;
  width= int_of_float frame.width;
  height= int_of_float frame.height;
}

let string_of_frame (frame:frame)= sprintf "{x: %d; y: %d; width: %d; height: %d}"
  frame.x
  frame.y
  frame.width
  frame.height

type pos= {
  pos_x: float;
  pos_y: float;
}
type ratio= {
  ratio_x: float;
  ratio_y: float;
}
type pos_ratio= {
  pos: pos;
  ratio: ratio;
}
let pos_ratio_default= {
  pos= {pos_x=0.;pos_y=0.};
  ratio= {ratio_x=1.;ratio_y=1.};
}

let pos_ratio_adjust_f ~pos_ratio frame_f=
  let x= frame_f.x *. pos_ratio.ratio.ratio_x +. pos_ratio.pos.pos_x
  and y= frame_f.y *. pos_ratio.ratio.ratio_y +. pos_ratio.pos.pos_y
  and width= frame_f.width *. pos_ratio.ratio.ratio_x
  and height= frame_f.height *. pos_ratio.ratio.ratio_y in
  { x; y; width; height }

let pos_ratio_adjust ~pos_ratio frame=
  let frame_f= frame_f_of_frame frame in
  let x= frame_f.x *. pos_ratio.ratio.ratio_x +. pos_ratio.pos.pos_x
  and y= frame_f.y *. pos_ratio.ratio.ratio_y +. pos_ratio.pos.pos_y
  and width= frame_f.width *. pos_ratio.ratio.ratio_x
  and height= frame_f.height *. pos_ratio.ratio.ratio_y in
  frame_of_frame_f { x; y; width; height }

type size= { width: int; height: int }
type size_f= { width: float; height: float }

type code_point= int * int

type stroke_type =
  | S_h     (* Horizontal *)
  | S_sh    (* Slanted Horizontal *)
  | S_u     (* Upward horizontal *)
  | S_du    (* Dot – Upward horizontal *)
  | S_v     (* Vertical *)
  | S_sv    (* Slanted Vertical *)
  | S_rsv   (* Right Slanted Vertical *)
  | S_t     (* Throw *)
  | S_ft    (* Flat Throw *)
  | S_wt    (* Wilted Throw *)
  | S_d     (* Dot *)
  | S_ed    (* Extended Dot *)
  | S_ld    (* Left Dot *)
  | S_wd    (* Wilted Dot *)
  | S_p     (* Press *)
  | S_up    (* Upward horizontal – Press *)
  | S_hp    (* Horizontal – Press *)
  | S_fp    (* Flat Press *)
  | S_ufp   (* Upward horizontal – Flat Press *)
  | S_c     (* Clockwise curve *)
  | S_a     (* Anticlockwise curve *)
  | S_o     (* Oval *)
  | S_hj    (* Horizontal – J hook *)
  | S_uj    (* Upward horizontal – J hook *)
  | S_ht    (* Horizontal – Throw *)
  | S_hsv   (* Horizontal – Slanted Vertical *)
  | S_hv    (* Horizontal – Vertical *)
  | S_hvj   (* Horizontal – Vertical – J hook *)
  | S_htj   (* Horizontal – Throw – J hook *)
  | S_utj   (* Upward horizontal – Throw – J hook *)
  | S_hvh   (* Horizontal – Vertical – Horizontal *)
  | S_hvu   (* Horizontal – Vertical – Upward horizontal *)
  | S_ha    (* Horizontal – Anticlockwise curve *)
  | S_haj   (* Horizontal – Anticlockwise curve – J hook *)
  | S_hpj   (* Horizontal – Press – J hook *)
  | S_htaj  (* Horizontal – Throw – Anticlockwise curve – J hook *)
  | S_htc   (* Horizontal – Throw – Clockwise curve *)
  | S_htht  (* Horizontal – Throw – Horizontal – Throw *)
  | S_htcj  (* Horizontal – Throw – Clockwise curve – J hook *)
  | S_hvhv  (* Horizontal – Vertical – Horizontal – Vertical *)
  | S_hthtj (* Horizontal – Throw – Horizontal – Throw – J hook *)
  | S_vu    (* Vertical – Upward horizontal *)
  | S_vh    (* Vertical – Horizontal *)
  | S_va    (* Vertical – Anticlockwise curve *)
  | S_vaj   (* Vertical – Anticlockwise curve – J hook *)
  | S_vhv   (* Vertical – Horizontal – Vertical *)
  | S_vht   (* Vertical – Horizontal – Throw *)
  | S_vhtj  (* Vertical – Horizontal – Throw – J hook *)
  | S_vj    (* Vertical – J hook *)
  | S_vc    (* Vertical – Clockwise curve *)
  | S_vcj   (* Vertical – Clockwise curve – J hook *)
  | S_tu    (* Throw – Upward horizontal *)
  | S_th    (* Throw – Horizontal *)
  | S_td    (* Throw – Dot *)
  | S_wtd   (* Wilted Throw – Dot *)
  | S_tht   (* Throw – Horizontal – Throw *)
  | S_thtj  (* Throw – Horizontal – Throw – J hook *)
  | S_tj    (* Throw – J hook *)
  | S_cj    (* Clockwise curve – J hook *)
  | S_fpj   (* Flat Press – J hook *)
  | S_pj    (* Press – J hook *)
  | S_thtaj (* Throw – Horizontal – Throw – Anticlockwise curve – J hook *)
  | S_tod   (* Throw – Oval – Dot *)

module Stroke = struct
  type t= stroke_type
  let compare= compare
end

let stroke_type_of_string= function
  | "a"-> S_a
  | "cj"-> S_cj
  | "c"-> S_c
  | "d"-> S_d
  | "du"-> S_du
  | "ed"-> S_ed
  | "fpj"-> S_fpj
  | "fp"-> S_fp
  | "ft"-> S_ft
  | "haj"-> S_haj
  | "ha"-> S_ha
  | "hj"-> S_hj
  | "hpj"-> S_hpj
  | "hp"-> S_hp
  | "h"-> S_h
  | "hsv"-> S_hsv
  | "htaj"-> S_htaj
  | "htcj"-> S_htcj
  | "htc"-> S_htc
  | "hthtj"-> S_hthtj
  | "htht"-> S_htht
  | "htj"-> S_htj
  | "ht"-> S_ht
  | "hvh"-> S_hvh
  | "hvhv"-> S_hvhv
  | "hvj"-> S_hvj
  | "hv"-> S_hv
  | "hvu"-> S_hvu
  | "ld"-> S_ld
  | "o"-> S_o
  | "pj"-> S_pj
  | "p"-> S_p
  | "rsv"-> S_rsv
  | "sh"-> S_sh
  | "sv"-> S_sv
  | "td"-> S_td
  | "th"-> S_th
  | "thtaj"-> S_thtaj
  | "thtj"-> S_thtj
  | "tht"-> S_tht
  | "tj"-> S_tj
  | "tod"-> S_tod
  | "t"-> S_t
  | "tu"-> S_tu
  | "ufp"-> S_ufp
  | "uj"-> S_uj
  | "up"-> S_up
  | "u"-> S_u
  | "utj"-> S_utj
  | "vaj"-> S_vaj
  | "va"-> S_va
  | "vcj"-> S_vcj
  | "vc"-> S_vc
  | "vh"-> S_vh
  | "vhtj"-> S_vhtj
  | "vht"-> S_vht
  | "vhv"-> S_vhv
  | "vj"-> S_vj
  | "v"-> S_v
  | "vu"-> S_vu
  | "wd"-> S_wd
  | "wtd"-> S_wtd
  | "wt"-> S_wt
  | _-> failwith "stroke_type_of_string"

let string_of_stroke_type= function
  | S_a->     "a"
  | S_cj->    "cj"
  | S_c->     "c"
  | S_d->     "d"
  | S_du->    "du"
  | S_ed->    "ed"
  | S_fpj->   "fpj"
  | S_fp->    "fp"
  | S_ft->    "ft"
  | S_haj->   "haj"
  | S_ha->    "ha"
  | S_hj->    "hj"
  | S_hpj->   "hpj"
  | S_hp->    "hp"
  | S_h->     "h"
  | S_hsv->   "hsv"
  | S_htaj->  "htaj"
  | S_htcj->  "htcj"
  | S_htc->   "htc"
  | S_hthtj-> "hthtj"
  | S_htht->  "htht"
  | S_htj->   "htj"
  | S_ht->    "ht"
  | S_hvh->   "hvh"
  | S_hvhv->  "hvhv"
  | S_hvj->   "hvj"
  | S_hv->    "hv"
  | S_hvu->   "hvu"
  | S_ld->    "ld"
  | S_o->     "o"
  | S_pj->    "pj"
  | S_p->     "p"
  | S_rsv->   "rsv"
  | S_sh->    "sh"
  | S_sv->    "sv"
  | S_td->    "td"
  | S_th->    "th"
  | S_thtaj-> "thtaj"
  | S_thtj->  "thtj"
  | S_tht->   "tht"
  | S_tj->    "tj"
  | S_tod->   "tod"
  | S_t->     "t"
  | S_tu->    "tu"
  | S_ufp->   "ufp"
  | S_uj->    "uj"
  | S_up->    "up"
  | S_u->     "u"
  | S_utj->   "utj"
  | S_vaj->   "vaj"
  | S_va->    "va"
  | S_vcj->   "vcj"
  | S_vc->    "vc"
  | S_vh->    "vh"
  | S_vhtj->  "vhtj"
  | S_vht->   "vht"
  | S_vhv->   "vhv"
  | S_vj->    "vj"
  | S_v->     "v"
  | S_vu->    "vu"
  | S_wd->    "wd"
  | S_wtd->   "wtd"
  | S_wt->    "wt"

let code_point_of_string str=
  let of_hex str= "0x"^str |> int_of_string in
  let codes= String.split_on_char ',' str in
  match codes with
  | []-> failwith "code_point_of_string"
  | c::e::_-> (of_hex c, of_hex e)
  | c::_->
    try (of_hex c,0) with
    | _->
      let codes= String.split_on_char ':' str in
      match codes with
      | []-> failwith "code_point_of_string"
      | c::e::_-> (of_hex c, of_hex e)
      | c::_-> (of_hex c,0)

let byte str i = Char.code (String.unsafe_get str i)
let fail str pos msg = failwith (Printf.sprintf "at position %d of %s: %s" pos str msg)

let unsafe_extract_next str ofs =
  let ch = String.unsafe_get str ofs in
  match ch with
    | '\x00' .. '\x7f' ->
      (Uchar.of_char ch, ofs + 1)
    | '\xc0' .. '\xdf' ->
      if ofs + 2 > String.length str then
        fail str ofs "unterminated UTF-8 sequence"
      else
        (Uchar.of_int (((Char.code ch land 0x1f) lsl 6) lor (byte str (ofs + 1) land 0x3f)), ofs + 2)
    | '\xe0' .. '\xef' ->
      if ofs + 3 > String.length str then
        fail str ofs "unterminated UTF-8 sequence"
      else
        (Uchar.of_int (
          ((Char.code ch land 0x0f) lsl 12) lor
          ((byte str (ofs + 1) land 0x3f) lsl 6)lor
          (byte str (ofs + 2) land 0x3f))
        , ofs + 3)
    | '\xf0' .. '\xf7' ->
      if ofs + 4 > String.length str then
        fail str ofs "unterminated UTF-8 sequence"
      else
        (Uchar.of_int (
          ((Char.code ch land 0x07) lsl 18) lor
          ((byte str (ofs + 1) land 0x3f) lsl 12) lor
          ((byte str (ofs + 2) land 0x3f) lsl 6) lor
          (byte str (ofs + 3) land 0x3f))
        , ofs + 4)
    | _ ->
      fail str ofs "invalid start of UTF-8 sequence"

let code_point_of_utf8 str=
  let len= String.length str in
  let core, next= unsafe_extract_next str 0 in
  if next >= len then
    (Uchar.to_int core, 0)
  else
    let variation, _= unsafe_extract_next str next in
    (Uchar.to_int core, Uchar.to_int variation)

let string_of_code_point cp=
  let c,e= cp in
  sprintf "(%x,%x)" c e

let version_of_string str=
  let versions= String.split_on_char '.' str in
  match versions with
  | c::e::_-> (int_of_string c, int_of_string e)
  | c::_-> (int_of_string c,0)
  | []-> failwith "version_of_string"

type stroke_f= {
  frame_f: frame_f;
  stroke_type: stroke_type;
}

type stroke= {
  frame: frame;
  stroke_type: stroke_type;
}

let to_stroke_f stroke= {
  frame_f= frame_f_of_frame stroke.frame;
  stroke_type= stroke.stroke_type;
}

type transform=
  | NoTransform
  | MirrorHorizontal
  | MirrorVertical
  | Rotate180

let transform_of_string= function
  | "" | "none"-> NoTransform
  | "mirror_horizontal"-> MirrorHorizontal
  | "mirror_vertical"-> MirrorVertical
  | "rotate180"-> Rotate180
  | _-> failwith "transform_of_string"
let string_of_transform= function
  | NoTransform-> "none"
  | MirrorHorizontal-> "mirror_horizontal"
  | MirrorVertical-> "mirror_vertical"
  | Rotate180-> "rotate180"

let reduce_transforms l=
  let[@tail_mod_cons] rec reduce l=
    match l with
    | []-> []
    | [_]-> l
    | MirrorHorizontal::MirrorHorizontal::tl-> reduce tl
    | MirrorVertical::MirrorVertical::tl-> reduce tl
    | Rotate180::Rotate180::tl-> reduce tl
    | MirrorHorizontal::MirrorVertical::tl->
      Rotate180::tl |> List.sort compare |> reduce
    | MirrorVertical::MirrorHorizontal::tl->
      Rotate180::tl |> List.sort compare |> reduce
    | hd::tl-> hd :: reduce tl
  in
  l |> List.sort compare |> reduce

module Raw = struct
  type ref= {
    code_point: code_point;
    frame: frame;
  }

  type stroke_unknown= {
    stroke_type: stroke_type option;
    x: int option;
    y: int option;
    width: int option;
    height: int option;
  }

  type ref_unknown= {
    code_point: code_point option;
    x: int option;
    y: int option;
    width: int option;
    height: int option;
  }

  type element=
    | Stroke of stroke
    | Ref of ref

  type god= {
    version_major: int;
    version_minor: int;
    code_point: code_point;
    transform: transform;
    elements: element list;
  }

  let get_stroke attrs=
    match ListLabels.fold_left
      attrs
      ~init:{stroke_type=None; x=None; y=None; width=None; height=None}
      ~f:(fun acc attr->
        let ((_ns, name), value)= attr in
        match name with
        | "type"-> { acc with stroke_type= Some (stroke_type_of_string value) }
        | "x"-> { acc with x= Some (int_of_string value) }
        | "y"-> { acc with y= Some (int_of_string value) }
        | "width"-> { acc with width= Some (int_of_string value) }
        | "height"-> { acc with height= Some (int_of_string value) }
        | _-> acc)
    with
    | {
        stroke_type= Some stroke_type;
        x= Some x;
        y= Some y;
        width= Some width;
        height= Some height;
      }->
      let frame:frame= { x; y; width; height; } in
      {
        stroke_type;
        frame;
      }
    | _-> failwith "get_stroke"


  let get_ref attrs=
    match ListLabels.fold_left
      attrs
      ~init:{code_point=None; x=None; y=None; width=None; height=None}
      ~f:(fun (acc:ref_unknown) attr->
        let ((_ns, name), value)= attr in
        match name with
        | "unicode"-> { acc with code_point= Some (code_point_of_string value) }
        | "x"-> { acc with x= Some (int_of_string value) }
        | "y"-> { acc with y= Some (int_of_string value) }
        | "width"-> { acc with width= Some (int_of_string value) }
        | "height"-> { acc with height= Some (int_of_string value) }
        | _-> acc)
    with
    | {
        code_point= Some code_point;
        x= Some x;
        y= Some y;
        width= Some width;
        height= Some height;
      }->
      let frame:frame= { x; y; width; height; } in
      {
        code_point;
        frame;
      }
    | _-> failwith "get_ref"

  let get_character attrs=
    match ListLabels.fold_left
      attrs
      ~init:{code_point=None; x=None; y=None; width=None; height=None}
      ~f:(fun (acc:ref_unknown) attr->
        let ((_ns, name), value)= attr in
        match name with
        | "utf8"-> { acc with code_point= Some (code_point_of_utf8 value) }
        | "x"-> { acc with x= Some (int_of_string value) }
        | "y"-> { acc with y= Some (int_of_string value) }
        | "width"-> { acc with width= Some (int_of_string value) }
        | "height"-> { acc with height= Some (int_of_string value) }
        | _-> acc)
    with
    | {
        code_point= Some code_point;
        x= Some x;
        y= Some y;
        width= Some width;
        height= Some height;
      }->
      let frame:frame= { x; y; width; height; } in
      {
        code_point;
        frame;
      }
    | _-> failwith "get_character"

  let of_xml_nodes nodes=
    let attrs, god= Ezxmlm.member_with_attr "god" nodes in
    let (version_major, version_minor)= attrs |> Ezxmlm.get_attr "version" |> version_of_string in
    let attrs, glyph= Ezxmlm.member_with_attr "glyph" god in
    let code_point= attrs |> Ezxmlm.get_attr "unicode" |> code_point_of_string in
    let transform= (try attrs |> Ezxmlm.get_attr "transform" with Not_found-> "none")
      |> transform_of_string in
    let elements= List.filter_map (fun node->
      match node with
      | `El (((_ns,name), attrs), _nodes)->
        (match name with
        | "stroke"-> Some (Stroke (get_stroke attrs))
        | "ref"-> Some (Ref (get_ref attrs))
        | "character"-> Some (Ref (get_character attrs))
        | _-> None)
      | `Data _-> None)
      glyph
    in
    {
      version_major;
      version_minor;
      code_point;
      transform;
      elements;
    }


  let of_string string=
    let _dtd, nodes= Ezxmlm.from_string string in
    of_xml_nodes nodes

  let load_file path=
    In_channel.with_open_text path @@ fun chan->
    let _dtd, nodes= Ezxmlm.from_channel chan in
    of_xml_nodes nodes

end

type god= {
  version_major: int;
  version_minor: int;
  code_point: code_point;
  transform: transform;
  elements: element list;
}
and subgod= { god: god ; frame: frame }
and element=
  | Stroke of stroke
  | SubGod of subgod

let god_frame god: frame=
  let (nx, ny, px, py)=
    match god.elements with
    | []-> (0,0,0,0)
    | head::elements->
      let init =
        let frame=
          match head with
          | Stroke stroke-> stroke.frame
          | SubGod subgod-> subgod.frame
        in
        (frame.x, frame.y, frame.width, frame.height)
      in
      ListLabels.fold_left elements
        ~init
        ~f:(fun (nx, ny, px, py) element->
          let frame=
            match element with
            | Stroke stroke-> stroke.frame
            | SubGod subgod-> subgod.frame
          in
          (
            min nx frame.x,
            min ny frame.y,
            max px (frame.x+frame.width),
            max py (frame.y+frame.height)
          ))
  in
  let x= nx
  and y= ny
  and width= px - nx
  and height= py - ny in
  let width, height=
    match god.transform with
    | NoTransform
    | MirrorHorizontal
    | MirrorVertical
    | Rotate180-> width, height
  in
  { x; y; width; height }

let calc_size god: size=
  let frame= god_frame god in
  let width= frame.width
  and height= frame.height in
  { width; height }

let calc_size_f god: size_f=
  let size= calc_size god in
  let width= float_of_int size.width
  and height= float_of_int size.height in
  { width; height }

let string_of_stroke stroke=
  let stroke_type= string_of_stroke_type stroke.stroke_type
  and frame= string_of_frame stroke.frame in
  sprintf "{ %s; %s }" stroke_type frame

let rec string_of_god ?(indent=0) god=
  let indent_str= String.make indent ' ' in
  let elements= List.map (string_of_element ~indent:(indent+2)) god.elements |> String.concat "\n" in
  sprintf "%s{ version: %d.%d; unicode: %s; elements:\n%s\n%s}"
    indent_str
    god.version_minor god.version_minor
    (string_of_code_point god.code_point)
    elements
    indent_str
and string_of_element ?(indent=0) elem=
  let indent_str= String.make indent ' ' in
  match elem with
  | Stroke stroke-> indent_str ^ (string_of_stroke stroke)
  | SubGod god->
    let god= string_of_god ~indent:(indent+2) god.god
    and frame= string_of_frame god.frame in
    sprintf "%s{ frame: %s; god:\n%s\n%s}" indent_str frame god indent_str

let rec load_file ~dir ?(filename="default.xml") code_point=
  let code_core, code_variation= code_point in
  let core_dir= sprintf "%x" code_core in
  let variation_dir= sprintf "%x" code_variation in
  let ( / ) = Filename.concat in
  let god_raw= Raw.load_file (dir / core_dir / variation_dir / filename) in
  let elements= god_raw.elements |> List.map (function
    | Raw.Ref ref-> SubGod { god= (load_file ~dir ~filename ref.code_point); frame= ref.frame }
    | Raw.Stroke s-> Stroke s
    )
  in
  {
    version_major= god_raw.version_major;
    version_minor= god_raw.version_minor;
    code_point= god_raw.code_point;
    transform= god_raw.transform;
    elements;
  }

let of_string ~dir ?(filename="default.xml") string=
  let god_raw= Raw.of_string string in
  let elements= god_raw.elements |> List.map (function
    | Raw.Ref ref-> SubGod { god= (load_file ~dir ~filename ref.code_point); frame= ref.frame }
    | Raw.Stroke s-> Stroke s
    )
  in
  {
    version_major= god_raw.version_major;
    version_minor= god_raw.version_minor;
    code_point= god_raw.code_point;
    transform= god_raw.transform;
    elements;
  }

let rec god_flatten ?(pos_ratio=pos_ratio_default) god=
  let elements= ListLabels.map
    god.elements
    ~f:(fun element->
      match element with
      | Stroke stroke->
        let frame= stroke.frame
          |> frame_f_of_frame
          |> pos_ratio_adjust_f ~pos_ratio
          |> frame_of_frame_f
        in
        [ { stroke with frame } ]
      | SubGod subgod->
        let size= calc_size_f subgod.god in
        let ratio= {
          ratio_x= float_of_int subgod.frame.width /. size.width;
          ratio_y= float_of_int subgod.frame.height /. size.height;
        } in
        let ratio_final= {
          ratio_x= ratio.ratio_x *. pos_ratio.ratio.ratio_x;
          ratio_y= ratio.ratio_y *. pos_ratio.ratio.ratio_y;
        } in
        let pos_x=
          float_of_int subgod.frame.x
            *. pos_ratio.ratio.ratio_x
            +. pos_ratio.pos.pos_x
        and pos_y=
          float_of_int subgod.frame.y
            *. pos_ratio.ratio.ratio_y
            +. pos_ratio.pos.pos_y
        in
        let pos_ratio= {
          pos= {pos_x; pos_y};
          ratio= ratio_final;
        } in
        god_flatten ~pos_ratio subgod.god)
  in
  List.concat elements

module StrokeMap= Map.Make(Stroke)

let load_glyphs ~dir= let ( / ) = Filename.concat in [
  (S_a, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "a.svg");
  (S_cj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "cj.svg");
  (S_c, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "c.svg");
  (S_d, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "d.svg");
  (S_du, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "du.svg");
  (S_ed, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "ed.svg");
  (S_fpj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "fpj.svg");
  (S_fp, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "fp.svg");
  (S_ft, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "ft.svg");
  (S_haj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "haj.svg");
  (S_ha, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "ha.svg");
  (S_hj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "hj.svg");
  (S_hpj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "hpj.svg");
  (S_hp, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "hp.svg");
  (S_h, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "h.svg");
  (S_hsv, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "hsv.svg");
  (S_htaj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "htaj.svg");
  (S_htcj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "htcj.svg");
  (S_htc, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "htc.svg");
  (S_hthtj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "hthtj.svg");
  (S_htht, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "htht.svg");
  (S_htj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "htj.svg");
  (S_ht, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "ht.svg");
  (S_hvh, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "hvh.svg");
  (S_hvhv, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "hvhv.svg");
  (S_hvj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "hvj.svg");
  (S_hv, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "hv.svg");
  (S_hvu, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "hvu.svg");
  (S_ld, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "ld.svg");
  (S_o, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "o.svg");
  (S_pj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "pj.svg");
  (S_p, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "p.svg");
  (S_rsv, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "rsv.svg");
  (S_sh, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "sh.svg");
  (S_sv, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "sv.svg");
  (S_td, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "td.svg");
  (S_th, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "th.svg");
  (S_thtaj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "thtaj.svg");
  (S_thtj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "thtj.svg");
  (S_tht, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "tht.svg");
  (S_tj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "tj.svg");
  (S_tod, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "tod.svg");
  (S_t, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "t.svg");
  (S_tu, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "tu.svg");
  (S_ufp, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "ufp.svg");
  (S_uj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "uj.svg");
  (S_up, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "up.svg");
  (S_u, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "u.svg");
  (S_utj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "utj.svg");
  (S_vaj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "vaj.svg");
  (S_va, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "va.svg");
  (S_vcj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "vcj.svg");
  (S_vc, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "vc.svg");
  (S_vh, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "vh.svg");
  (S_vhtj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "vhtj.svg");
  (S_vht, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "vht.svg");
  (S_vhv, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "vhv.svg");
  (S_vj, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "vj.svg");
  (S_v, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "v.svg");
  (S_vu, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "vu.svg");
  (S_wd, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "wd.svg");
  (S_wtd, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "wtd.svg");
  (S_wt, Smaji_glyph_outline.Svg.load_file_exn @@ dir / "wt.svg");
  ]
  |> List.to_seq
  |> StrokeMap.of_seq

let load_animates ~dir= let ( / ) = Filename.concat in [
  (S_a, Animate.load_file_exn @@ dir / "a.svg");
  (S_cj, Animate.load_file_exn @@ dir / "cj.svg");
  (S_c, Animate.load_file_exn @@ dir / "c.svg");
  (S_d, Animate.load_file_exn @@ dir / "d.svg");
  (S_du, Animate.load_file_exn @@ dir / "du.svg");
  (S_ed, Animate.load_file_exn @@ dir / "ed.svg");
  (S_fpj, Animate.load_file_exn @@ dir / "fpj.svg");
  (S_fp, Animate.load_file_exn @@ dir / "fp.svg");
  (S_ft, Animate.load_file_exn @@ dir / "ft.svg");
  (S_haj, Animate.load_file_exn @@ dir / "haj.svg");
  (S_ha, Animate.load_file_exn @@ dir / "ha.svg");
  (S_hj, Animate.load_file_exn @@ dir / "hj.svg");
  (S_hpj, Animate.load_file_exn @@ dir / "hpj.svg");
  (S_hp, Animate.load_file_exn @@ dir / "hp.svg");
  (S_h, Animate.load_file_exn @@ dir / "h.svg");
  (S_hsv, Animate.load_file_exn @@ dir / "hsv.svg");
  (S_htaj, Animate.load_file_exn @@ dir / "htaj.svg");
  (S_htcj, Animate.load_file_exn @@ dir / "htcj.svg");
  (S_htc, Animate.load_file_exn @@ dir / "htc.svg");
  (S_hthtj, Animate.load_file_exn @@ dir / "hthtj.svg");
  (S_htht, Animate.load_file_exn @@ dir / "htht.svg");
  (S_htj, Animate.load_file_exn @@ dir / "htj.svg");
  (S_ht, Animate.load_file_exn @@ dir / "ht.svg");
  (S_hvh, Animate.load_file_exn @@ dir / "hvh.svg");
  (S_hvhv, Animate.load_file_exn @@ dir / "hvhv.svg");
  (S_hvj, Animate.load_file_exn @@ dir / "hvj.svg");
  (S_hv, Animate.load_file_exn @@ dir / "hv.svg");
  (S_hvu, Animate.load_file_exn @@ dir / "hvu.svg");
  (S_ld, Animate.load_file_exn @@ dir / "ld.svg");
  (S_o, Animate.load_file_exn @@ dir / "o.svg");
  (S_pj, Animate.load_file_exn @@ dir / "pj.svg");
  (S_p, Animate.load_file_exn @@ dir / "p.svg");
  (S_rsv, Animate.load_file_exn @@ dir / "rsv.svg");
  (S_sh, Animate.load_file_exn @@ dir / "sh.svg");
  (S_sv, Animate.load_file_exn @@ dir / "sv.svg");
  (S_td, Animate.load_file_exn @@ dir / "td.svg");
  (S_th, Animate.load_file_exn @@ dir / "th.svg");
  (S_thtaj, Animate.load_file_exn @@ dir / "thtaj.svg");
  (S_thtj, Animate.load_file_exn @@ dir / "thtj.svg");
  (S_tht, Animate.load_file_exn @@ dir / "tht.svg");
  (S_tj, Animate.load_file_exn @@ dir / "tj.svg");
  (S_tod, Animate.load_file_exn @@ dir / "tod.svg");
  (S_t, Animate.load_file_exn @@ dir / "t.svg");
  (S_tu, Animate.load_file_exn @@ dir / "tu.svg");
  (S_ufp, Animate.load_file_exn @@ dir / "ufp.svg");
  (S_uj, Animate.load_file_exn @@ dir / "uj.svg");
  (S_up, Animate.load_file_exn @@ dir / "up.svg");
  (S_u, Animate.load_file_exn @@ dir / "u.svg");
  (S_utj, Animate.load_file_exn @@ dir / "utj.svg");
  (S_vaj, Animate.load_file_exn @@ dir / "vaj.svg");
  (S_va, Animate.load_file_exn @@ dir / "va.svg");
  (S_vcj, Animate.load_file_exn @@ dir / "vcj.svg");
  (S_vc, Animate.load_file_exn @@ dir / "vc.svg");
  (S_vh, Animate.load_file_exn @@ dir / "vh.svg");
  (S_vhtj, Animate.load_file_exn @@ dir / "vhtj.svg");
  (S_vht, Animate.load_file_exn @@ dir / "vht.svg");
  (S_vhv, Animate.load_file_exn @@ dir / "vhv.svg");
  (S_vj, Animate.load_file_exn @@ dir / "vj.svg");
  (S_v, Animate.load_file_exn @@ dir / "v.svg");
  (S_vu, Animate.load_file_exn @@ dir / "vu.svg");
  (S_wd, Animate.load_file_exn @@ dir / "wd.svg");
  (S_wtd, Animate.load_file_exn @@ dir / "wtd.svg");
  (S_wt, Animate.load_file_exn @@ dir / "wt.svg");
  ]
  |> List.to_seq
  |> StrokeMap.of_seq

let convert_to_glif_glyphs glyphs=
  glyphs |> StrokeMap.map Smaji_glyph_outline.glif_of_svg

let svg_of_stroke ~stroke_glyph stroke=
  let svg: Svg.t= StrokeMap.find stroke.stroke_type stroke_glyph in
  let x=
    (float_of_int stroke.frame.width) /.
    svg.viewBox.width
  and y=
    (float_of_int stroke.frame.height) /.
    svg.viewBox.height
  and dx= float_of_int stroke.frame.x
  and dy= float_of_int stroke.frame.y in
  svg
    |> Smaji_glyph_outline.Svg.Adjust.scale ~x ~y
    |> Smaji_glyph_outline.Svg.Adjust.translate ~dx ~dy

let paths_of_stroke ~stroke_glyph stroke=
  let svg= svg_of_stroke ~stroke_glyph stroke in
  svg.paths

let animate_of_stroke ~stroke_animate stroke=
  let animate:Animate.t=
    StrokeMap.find stroke.stroke_type stroke_animate in
  let svg= animate.svg in
  let x=
    (float_of_int stroke.frame.width) /.
    svg.viewBox.width
  and y=
    (float_of_int stroke.frame.height) /.
    svg.viewBox.height
  and dx= float_of_int stroke.frame.x
  and dy= float_of_int stroke.frame.y in
  animate
    |> Animate.Adjust.scale ~x ~y
    |> Animate.Adjust.translate ~dx ~dy

let animations_of_stroke ~stroke_animate stroke=
  let animate= animate_of_stroke ~stroke_animate stroke in
  animate.animations

let svg_of_god ~stroke_glyph god=
  let viewBox= Smaji_glyph_outline.Svg.ViewBox.{ min_x= 0.; min_y= 0.; width= 0.; height= 0.; }
  and paths= god
    |> god_flatten
    |> List.map (paths_of_stroke ~stroke_glyph)
    |> List.concat
  in
  let svg= Smaji_glyph_outline.Svg.{ viewBox; paths } in
  Smaji_glyph_outline.Svg.Adjust.viewBox_fitFrame_reset svg

let outline_svg_of_god ~stroke_glyph god=
  let size= calc_size god in
  let rec svg_of_god ?(indent=0) god=
    let indent_str0= String.make indent ' '
    and indent_str1= String.make (indent+2) ' '
    and indent_str2= String.make (indent+4) ' ' in
    let elements= ListLabels.map god.elements
      ~f:(fun element->
        match element with
        | Stroke stroke->
          let godSvg:Svg.t= StrokeMap.find stroke.stroke_type stroke_glyph in
          let dx= float_of_int stroke.frame.x -. godSvg.viewBox.min_x
          and dy= float_of_int stroke.frame.y -. godSvg.viewBox.min_y
          and rx= float_of_int stroke.frame.width /. godSvg.viewBox.width
          and ry= float_of_int stroke.frame.height /. godSvg.viewBox.height in
          let translate=
            if dx <> 0. || dy <> 0.
            then sprintf "translate(%s %s)" (string_of_float dx) (string_of_float dy)
            else ""
          in
          let scale=
            if rx <> 1. || ry <> 1.
            then sprintf "scale(%s %s)" (string_of_float rx) (string_of_float ry)
            else ""
          in
          let transform=
            if translate <> "" || scale <> ""
            then sprintf {|transform="%s"|} (String.concat " " [translate; scale])
            else ""
          in
          let paths=
            let paths_svg= godSvg.paths
              |> List.map (Animate.Svg.Path.to_string_svg ~indent:(indent+6))
              |> String.concat "\n"
            in
            sprintf "%s<path d=\"%s\"\n%s/>" indent_str2 paths_svg indent_str2
          in
          sprintf "%s<g %s>\n%s\n%s</g>"
            indent_str1 transform
            paths
            indent_str1
        | SubGod subgod->
          let size= calc_size subgod.god in
          let dx= float_of_int subgod.frame.x
          and dy= float_of_int subgod.frame.y
          and rx= float_of_int subgod.frame.width /. float_of_int size.width
          and ry= float_of_int subgod.frame.height /. float_of_int size.height in
          let translate=
            sprintf "translate(%s %s)" (string_of_float dx) (string_of_float dy)
          in
          let scale=
            if rx <> 1. || ry <> 1.
            then sprintf "scale(%s %s)" (string_of_float rx) (string_of_float ry)
            else ""
          in
          let transform=
            if translate <> "" || scale <> ""
            then sprintf {|transform="%s"|} (String.concat " " [translate; scale])
            else ""
          in
          let subgod_str= svg_of_god ~indent:(indent+2+2) subgod.god in
          (sprintf "%s<g %s>\n%s\n%s</g>"
            indent_str1 transform
            subgod_str
            indent_str1)
          )
    in
    let elements_str= String.concat "\n" elements in
    let transform=
      let x= sprintf "translate(%d 0)"
        (- size.width )
      and y= sprintf "translate(0 %d)"
        (- size.height)
      in
      (match god.transform with
      | NoTransform-> []
      | MirrorHorizontal-> ["scale(-1 1)"; x]
      | MirrorVertical-> ["scale(1 -1)"; y]
      | Rotate180-> ["scale(-1 -1)"; x; y])
        |> String.concat " "
        |> sprintf {|transform="%s"|}
    in
    sprintf "%s<g %s>\n%s\n%s</g>"
      indent_str0
      transform
      elements_str
      indent_str0
  in
  match god.version_major, god.version_minor with
  | (1, 0) ->
    let asvg= svg_of_god ~indent:2 god in
    sprintf
      "<svg viewBox=\"0,0 %d,%d\" xmlns=\"http://www.w3.org/2000/svg\">\n%s\n</svg>"
      size.width size.height asvg
  | _-> failwith (sprintf "outline_svg_of_god %d %d" god.version_major god.version_minor)

let animate_svg_of_god ~stroke_animate god=
  let size= calc_size god in
  let rec animate_svg_of_god ?(id=0) ?(time=0.) ?(indent=0) god=
    let indent_str0= String.make indent ' '
    and indent_str1= String.make (indent+2) ' ' in
    let (next_id, next_time), elements= ListLabels.fold_left_map god.elements
      ~init:(id,time)
      ~f:(fun (id,time) element->
        match element with
        | Stroke stroke->
          let godAnimate:Animate.t= StrokeMap.find stroke.stroke_type stroke_animate in
          let dx= float_of_int stroke.frame.x -. godAnimate.svg.viewBox.min_x
          and dy= float_of_int stroke.frame.y -. godAnimate.svg.viewBox.min_y
          and rx= float_of_int stroke.frame.width /. godAnimate.svg.viewBox.width
          and ry= float_of_int stroke.frame.height /. godAnimate.svg.viewBox.height in
          let translate=
            if dx <> 0. || dy <> 0.
            then sprintf "translate(%s %s)" (string_of_float dx) (string_of_float dy)
            else ""
          in
          let scale=
            if rx <> 1. || ry <> 1.
            then sprintf "scale(%s %s)" (string_of_float rx) (string_of_float ry)
            else ""
          in
          let transform=
            if translate <> "" || scale <> ""
            then sprintf {|transform="%s"|} (String.concat " " [translate; scale])
            else ""
          in
          let godAnimate_str=
            Animate.to_string ~id ~time ~indent:(indent+4) godAnimate in
          ((id+1, time+.0.5),
          (sprintf "%s<g %s>\n%s\n%s</g>"
            indent_str1 transform
            godAnimate_str
            indent_str1))
        | SubGod subgod->
          let size= calc_size subgod.god in
          let dx= float_of_int subgod.frame.x
          and dy= float_of_int subgod.frame.y
          and rx= float_of_int subgod.frame.width /. float_of_int size.width
          and ry= float_of_int subgod.frame.height /. float_of_int size.height in
          let translate=
            sprintf "translate(%s %s)" (string_of_float dx) (string_of_float dy)
          in
          let scale=
            if rx <> 1. || ry <> 1.
            then sprintf "scale(%s %s)" (string_of_float rx) (string_of_float ry)
            else ""
          in
          let transform=
            if translate <> "" || scale <> ""
            then sprintf {|transform="%s"|} (String.concat " " [translate; scale])
            else ""
          in
          let (id_next,time_next), subgod_str= animate_svg_of_god ~time ~id ~indent:(indent+4) subgod.god in
          ((id_next, time_next),
          (sprintf "%s<g %s>\n%s\n%s</g>"
            indent_str1 transform
            subgod_str
            indent_str1))
          )
    in
    let elements_str= String.concat "\n" elements in
    let transform=
      let x= sprintf "translate(%d 0)"
        (- size.width)
      and y= sprintf "translate(0 %d)"
        (- size.height)
      in
      (match god.transform with
      | NoTransform-> []
      | MirrorHorizontal-> ["scale(-1 1)"; x]
      | MirrorVertical-> ["scale(1 -1)"; y]
      | Rotate180-> ["scale(-1 -1)"; x; y])
        |> String.concat " "
        |> sprintf {|transform="%s"|}
    in
    let content= sprintf "%s<g %s>\n%s\n%s</g>"
      indent_str0
      transform
      elements_str
      indent_str0
    in
    (next_id,next_time), content
  in
  match god.version_major, god.version_minor with
  | (1, 0) ->
    let _, asvg= animate_svg_of_god ~indent:2 god in
    sprintf
      "<svg viewBox=\"0,0 %d,%d\" xmlns=\"http://www.w3.org/2000/svg\">\n%s\n</svg>"
      size.width size.height asvg
  | _-> failwith (sprintf "animate_svg_of_god %d %d" god.version_major god.version_minor)

type glif_of_god=
  | Glif of Glif.t
  | Wrapped of { wrap: Glif.t; content: Glif.t }

let outline_glif_of_god ~stroke_glyph god=
  let size= calc_size god in
  let glif_of_god ?(wrapped=true) god=
    let name=
      let base= string_of_code_point god.code_point in
      if wrapped then
        base ^ "_base"
      else
        base
    in
    let format= 2
    and formatMinor= 0
    and advance= Glif.{ width= float_of_int size.width; height= float_of_int size.height }
    and unicodes= let (core,_)= god.code_point in [core]
    and elements= ListLabels.map god.elements ~f:(fun element->
      match element with
      | Stroke stroke->
        let godGlif:Glif.t= StrokeMap.find stroke.stroke_type stroke_glyph in
        let dx= float_of_int stroke.frame.x
        and dy= float_of_int stroke.frame.y
        and rx= float_of_int stroke.frame.width /. godGlif.advance.width
        and ry= float_of_int stroke.frame.height /. godGlif.advance.height in
          Glif.Component
          Glif.{
            base= Some ("stroke/" ^ string_of_stroke_type stroke.stroke_type);
            xScale= rx;
            xyScale= 0.;
            yxScale= 0.;
            yScale= ry;
            xOffset= dx;
            yOffset= dy;
            identifier= None;
          }
      | SubGod subgod->
        let size= calc_size subgod.god in
        let dx= float_of_int subgod.frame.x
        and dy= float_of_int subgod.frame.y
        and rx= float_of_int subgod.frame.width /. float_of_int size.width
        and ry= float_of_int subgod.frame.height /. float_of_int size.height in
          Glif.Component
          Glif.{
            base= Some ("god/" ^ string_of_code_point subgod.god.code_point);
            xScale= rx;
            xyScale= 0.;
            yxScale= 0.;
            yScale= ry;
            xOffset= dx;
            yOffset= dy;
            identifier= None;
          }
      )
    in
    Glif.{
      name;
      format;
      formatMinor;
      advance;
      unicodes;
      elements;
    }
  in
  let transfrom_wrap god=
    let size= calc_size_f god in
    let code_point= god.code_point in
    let wrap=
      let xScale, yScale, xOffset, yOffset=
        match god.transform with
        | NoTransform-> (1., 1., 0., 0.)
        | MirrorHorizontal-> (-1., 1., -. size.width, 0.)
        | MirrorVertical-> (1., -1., 0., -. size.height)
        | Rotate180-> (-1., -1., -. size.width, -. size.height)
      in
      Glif.Component Glif.{
        base= Some ("glyph/" ^ string_of_code_point code_point ^ "/content");
        xScale;
        xyScale= 0.;
        yxScale= 0.;
        yScale;
        xOffset;
        yOffset;
        identifier= None;
      }
    in
    let (core,_)= code_point in
    Glif.{
      name= string_of_code_point code_point;
      format= 2;
      formatMinor= 0;
      advance= {width= 0.; height= 0.};
      unicodes= [core];
      elements= [wrap];
    }
  in
  let gen_glif ()=
    match god.transform with
    | NoTransform-> Glif (glif_of_god ~wrapped:false god)
    | MirrorHorizontal->
      let content= glif_of_god god in
      let wrap= transfrom_wrap god in
      Wrapped { wrap; content }
      (* ["scale(-1 1)"; x] *)
    | MirrorVertical->
      let content= glif_of_god god in
      let wrap= transfrom_wrap god in
      Wrapped { wrap; content }
      (* ["scale(1 -1)"; y] *)
    | Rotate180->
      let content= glif_of_god god in
      let wrap= transfrom_wrap god in
      Wrapped { wrap; content }
      (* ["scale(-1 -1)"; x; y] *)
  in
  match god.version_major, god.version_minor with
  | (1, 0) ->
    gen_glif ()
  | _-> failwith (sprintf "outline_glif_of_god %d %d" god.version_major god.version_minor)

