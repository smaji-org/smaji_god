open Glyph_outline.Utils

module Animate = Animate

open Printf

let write_all path data=
  let chan= Out_channel.open_text path in
  Out_channel.output_string chan data;
  Out_channel.close chan

let read_all path=
  let chan= In_channel.open_text path in
  let data= In_channel.input_all chan in
  In_channel.close chan;
  data

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

let frame_to_frame_f (frame:frame)= {
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
  let frame_f= frame_to_frame_f frame in
  let x= frame_f.x *. pos_ratio.ratio.ratio_x +. pos_ratio.pos.pos_x
  and y= frame_f.y *. pos_ratio.ratio.ratio_y +. pos_ratio.pos.pos_y
  and width= frame_f.width *. pos_ratio.ratio.ratio_x
  and height= frame_f.height *. pos_ratio.ratio.ratio_y in
  frame_of_frame_f { x; y; width; height }

type size= { width: float; height: float }

type code_point= int * int

type stroke_type=
  | S_a
  | S_cj
  | S_c
  | S_d
  | S_du
  | S_ed
  | S_fpj
  | S_fp
  | S_ft
  | S_haj
  | S_ha
  | S_hj
  | S_hpj
  | S_hp
  | S_h
  | S_hsv
  | S_htaj
  | S_htcj
  | S_htc
  | S_hthtj
  | S_htht
  | S_htj
  | S_ht
  | S_hvh
  | S_hvhv
  | S_hvj
  | S_hv
  | S_hvu
  | S_ld
  | S_o
  | S_pj
  | S_p
  | S_rsv
  | S_sh
  | S_sv
  | S_td
  | S_th
  | S_thtaj
  | S_thtj
  | S_tht
  | S_tj
  | S_tod
  | S_t
  | S_tu
  | S_ufp
  | S_uj
  | S_up
  | S_u
  | S_utj
  | S_vaj
  | S_va
  | S_vcj
  | S_vc
  | S_vh
  | S_vhtj
  | S_vht
  | S_vhv
  | S_vj
  | S_v
  | S_vu
  | S_wd
  | S_wtd
  | S_wt

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
  frame_f= frame_to_frame_f stroke.frame;
  stroke_type= stroke.stroke_type;
}

type transform=
  | None
  | Horizontal_mirror
  | Vertical_mirror
  | Rotate90
let transform_of_string= function
  | "none"-> None
  | "horizontal_mirror"-> Horizontal_mirror
  | "vertical_mirror"-> Vertical_mirror
  | "rotate90"-> Rotate90
  | _-> failwith "transform_of_string"
let transform_to_string= function
  | None-> "none"
  | Horizontal_mirror-> "horizontal_mirror"
  | Vertical_mirror-> "vertical_mirror"
  | Rotate90-> "rotate90"

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



  let load_file path=
    let chan= open_in path in
    let _dtd, nodes= Ezxmlm.from_channel chan in
    close_in chan;
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
end

type subgod= { god: god ; frame: frame }
and element=
  | Stroke of stroke
  | SubGod of subgod
and god= {
  version_major: int;
  version_minor: int;
  code_point: code_point;
  transform: transform;
  elements: element list;
}

let calc_size god=
  ListLabels.fold_left god.elements
    ~init:{width=0.;height=0.}
    ~f:(fun acc element->
      let frame=
        match element with
        | Stroke stroke-> stroke.frame
        | SubGod subgod-> subgod.frame
      in
      { width= max acc.width (float_of_int (frame.x+frame.width));
        height= max acc.height (float_of_int (frame.y+frame.height));
      })

let string_of_stroke stroke=
  let stroke_type= string_of_stroke_type stroke.stroke_type
  and frame= string_of_frame stroke.frame in
  sprintf "{ %s; %s }" stroke_type frame

let rec string_of_element ?(indent=0) elem=
  let indent_str= String.make indent ' ' in
  match elem with
  | Stroke stroke-> indent_str ^ (string_of_stroke stroke)
  | SubGod god->
    let god= string_of_god ~indent:(indent+2) god.god
    and frame= string_of_frame god.frame in
    sprintf "%s{ frame: %s; god:\n%s\n%s}" indent_str frame god indent_str
and string_of_god ?(indent=0) god=
  let indent_str= String.make indent ' ' in
  let elements= List.map (string_of_element ~indent:(indent+2)) god.elements |> String.concat "\n" in
  sprintf "%s{ version: %d.%d; unicode: %s; elements:\n%s\n%s}"
    indent_str
    god.version_minor god.version_minor
    (string_of_code_point god.code_point)
    elements
    indent_str

let rec load_file dir code_point=
  let code_glyph, code_variation= code_point in
  let glyph_dir= sprintf "%x" code_glyph in
  let variation_dir= sprintf "%x" code_variation in
  let ( / ) = Filename.concat in
  let god_raw= Raw.load_file (dir / glyph_dir / variation_dir / "default.xml") in
  let elements= god_raw.elements |> List.map (function
    | Raw.Ref ref-> SubGod { god= (load_file dir ref.code_point); frame= ref.frame }
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
          |> frame_to_frame_f
          |> pos_ratio_adjust_f ~pos_ratio
          |> frame_of_frame_f
        in
        [ { stroke with frame } ]
      | SubGod subgod->
        let size= calc_size subgod.god in
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

let load_glyphs directory= let ( / ) = Filename.concat in [
  (S_a, Glyph_outline.Svg.load_file_exn @@ directory / "a.svg");
  (S_cj, Glyph_outline.Svg.load_file_exn @@ directory / "cj.svg");
  (S_c, Glyph_outline.Svg.load_file_exn @@ directory / "c.svg");
  (S_d, Glyph_outline.Svg.load_file_exn @@ directory / "d.svg");
  (S_du, Glyph_outline.Svg.load_file_exn @@ directory / "du.svg");
  (S_ed, Glyph_outline.Svg.load_file_exn @@ directory / "ed.svg");
  (S_fpj, Glyph_outline.Svg.load_file_exn @@ directory / "fpj.svg");
  (S_fp, Glyph_outline.Svg.load_file_exn @@ directory / "fp.svg");
  (S_ft, Glyph_outline.Svg.load_file_exn @@ directory / "ft.svg");
  (S_haj, Glyph_outline.Svg.load_file_exn @@ directory / "haj.svg");
  (S_ha, Glyph_outline.Svg.load_file_exn @@ directory / "ha.svg");
  (S_hj, Glyph_outline.Svg.load_file_exn @@ directory / "hj.svg");
  (S_hpj, Glyph_outline.Svg.load_file_exn @@ directory / "hpj.svg");
  (S_hp, Glyph_outline.Svg.load_file_exn @@ directory / "hp.svg");
  (S_h, Glyph_outline.Svg.load_file_exn @@ directory / "h.svg");
  (S_hsv, Glyph_outline.Svg.load_file_exn @@ directory / "hsv.svg");
  (S_htaj, Glyph_outline.Svg.load_file_exn @@ directory / "htaj.svg");
  (S_htcj, Glyph_outline.Svg.load_file_exn @@ directory / "htcj.svg");
  (S_htc, Glyph_outline.Svg.load_file_exn @@ directory / "htc.svg");
  (S_hthtj, Glyph_outline.Svg.load_file_exn @@ directory / "hthtj.svg");
  (S_htht, Glyph_outline.Svg.load_file_exn @@ directory / "htht.svg");
  (S_htj, Glyph_outline.Svg.load_file_exn @@ directory / "htj.svg");
  (S_ht, Glyph_outline.Svg.load_file_exn @@ directory / "ht.svg");
  (S_hvh, Glyph_outline.Svg.load_file_exn @@ directory / "hvh.svg");
  (S_hvhv, Glyph_outline.Svg.load_file_exn @@ directory / "hvhv.svg");
  (S_hvj, Glyph_outline.Svg.load_file_exn @@ directory / "hvj.svg");
  (S_hv, Glyph_outline.Svg.load_file_exn @@ directory / "hv.svg");
  (S_hvu, Glyph_outline.Svg.load_file_exn @@ directory / "hvu.svg");
  (S_ld, Glyph_outline.Svg.load_file_exn @@ directory / "ld.svg");
  (S_o, Glyph_outline.Svg.load_file_exn @@ directory / "o.svg");
  (S_pj, Glyph_outline.Svg.load_file_exn @@ directory / "pj.svg");
  (S_p, Glyph_outline.Svg.load_file_exn @@ directory / "p.svg");
  (S_rsv, Glyph_outline.Svg.load_file_exn @@ directory / "rsv.svg");
  (S_sh, Glyph_outline.Svg.load_file_exn @@ directory / "sh.svg");
  (S_sv, Glyph_outline.Svg.load_file_exn @@ directory / "sv.svg");
  (S_td, Glyph_outline.Svg.load_file_exn @@ directory / "td.svg");
  (S_th, Glyph_outline.Svg.load_file_exn @@ directory / "th.svg");
  (S_thtaj, Glyph_outline.Svg.load_file_exn @@ directory / "thtaj.svg");
  (S_thtj, Glyph_outline.Svg.load_file_exn @@ directory / "thtj.svg");
  (S_tht, Glyph_outline.Svg.load_file_exn @@ directory / "tht.svg");
  (S_tj, Glyph_outline.Svg.load_file_exn @@ directory / "tj.svg");
  (S_tod, Glyph_outline.Svg.load_file_exn @@ directory / "tod.svg");
  (S_t, Glyph_outline.Svg.load_file_exn @@ directory / "t.svg");
  (S_tu, Glyph_outline.Svg.load_file_exn @@ directory / "tu.svg");
  (S_ufp, Glyph_outline.Svg.load_file_exn @@ directory / "ufp.svg");
  (S_uj, Glyph_outline.Svg.load_file_exn @@ directory / "uj.svg");
  (S_up, Glyph_outline.Svg.load_file_exn @@ directory / "up.svg");
  (S_u, Glyph_outline.Svg.load_file_exn @@ directory / "u.svg");
  (S_utj, Glyph_outline.Svg.load_file_exn @@ directory / "utj.svg");
  (S_vaj, Glyph_outline.Svg.load_file_exn @@ directory / "vaj.svg");
  (S_va, Glyph_outline.Svg.load_file_exn @@ directory / "va.svg");
  (S_vcj, Glyph_outline.Svg.load_file_exn @@ directory / "vcj.svg");
  (S_vc, Glyph_outline.Svg.load_file_exn @@ directory / "vc.svg");
  (S_vh, Glyph_outline.Svg.load_file_exn @@ directory / "vh.svg");
  (S_vhtj, Glyph_outline.Svg.load_file_exn @@ directory / "vhtj.svg");
  (S_vht, Glyph_outline.Svg.load_file_exn @@ directory / "vht.svg");
  (S_vhv, Glyph_outline.Svg.load_file_exn @@ directory / "vhv.svg");
  (S_vj, Glyph_outline.Svg.load_file_exn @@ directory / "vj.svg");
  (S_v, Glyph_outline.Svg.load_file_exn @@ directory / "v.svg");
  (S_vu, Glyph_outline.Svg.load_file_exn @@ directory / "vu.svg");
  (S_wd, Glyph_outline.Svg.load_file_exn @@ directory / "wd.svg");
  (S_wtd, Glyph_outline.Svg.load_file_exn @@ directory / "wtd.svg");
  (S_wt, Glyph_outline.Svg.load_file_exn @@ directory / "wt.svg");
  ]
  |> List.to_seq
  |> StrokeMap.of_seq

let load_animates directory= let ( / ) = Filename.concat in [
  (S_a, Animate.load_file_exn @@ directory / "a.svg");
  (S_cj, Animate.load_file_exn @@ directory / "cj.svg");
  (S_c, Animate.load_file_exn @@ directory / "c.svg");
  (S_d, Animate.load_file_exn @@ directory / "d.svg");
  (S_du, Animate.load_file_exn @@ directory / "du.svg");
  (S_ed, Animate.load_file_exn @@ directory / "ed.svg");
  (S_fpj, Animate.load_file_exn @@ directory / "fpj.svg");
  (S_fp, Animate.load_file_exn @@ directory / "fp.svg");
  (S_ft, Animate.load_file_exn @@ directory / "ft.svg");
  (S_haj, Animate.load_file_exn @@ directory / "haj.svg");
  (S_ha, Animate.load_file_exn @@ directory / "ha.svg");
  (S_hj, Animate.load_file_exn @@ directory / "hj.svg");
  (S_hpj, Animate.load_file_exn @@ directory / "hpj.svg");
  (S_hp, Animate.load_file_exn @@ directory / "hp.svg");
  (S_h, Animate.load_file_exn @@ directory / "h.svg");
  (S_hsv, Animate.load_file_exn @@ directory / "hsv.svg");
  (S_htaj, Animate.load_file_exn @@ directory / "htaj.svg");
  (S_htcj, Animate.load_file_exn @@ directory / "htcj.svg");
  (S_htc, Animate.load_file_exn @@ directory / "htc.svg");
  (S_hthtj, Animate.load_file_exn @@ directory / "hthtj.svg");
  (S_htht, Animate.load_file_exn @@ directory / "htht.svg");
  (S_htj, Animate.load_file_exn @@ directory / "htj.svg");
  (S_ht, Animate.load_file_exn @@ directory / "ht.svg");
  (S_hvh, Animate.load_file_exn @@ directory / "hvh.svg");
  (S_hvhv, Animate.load_file_exn @@ directory / "hvhv.svg");
  (S_hvj, Animate.load_file_exn @@ directory / "hvj.svg");
  (S_hv, Animate.load_file_exn @@ directory / "hv.svg");
  (S_hvu, Animate.load_file_exn @@ directory / "hvu.svg");
  (S_ld, Animate.load_file_exn @@ directory / "ld.svg");
  (S_o, Animate.load_file_exn @@ directory / "o.svg");
  (S_pj, Animate.load_file_exn @@ directory / "pj.svg");
  (S_p, Animate.load_file_exn @@ directory / "p.svg");
  (S_rsv, Animate.load_file_exn @@ directory / "rsv.svg");
  (S_sh, Animate.load_file_exn @@ directory / "sh.svg");
  (S_sv, Animate.load_file_exn @@ directory / "sv.svg");
  (S_td, Animate.load_file_exn @@ directory / "td.svg");
  (S_th, Animate.load_file_exn @@ directory / "th.svg");
  (S_thtaj, Animate.load_file_exn @@ directory / "thtaj.svg");
  (S_thtj, Animate.load_file_exn @@ directory / "thtj.svg");
  (S_tht, Animate.load_file_exn @@ directory / "tht.svg");
  (S_tj, Animate.load_file_exn @@ directory / "tj.svg");
  (S_tod, Animate.load_file_exn @@ directory / "tod.svg");
  (S_t, Animate.load_file_exn @@ directory / "t.svg");
  (S_tu, Animate.load_file_exn @@ directory / "tu.svg");
  (S_ufp, Animate.load_file_exn @@ directory / "ufp.svg");
  (S_uj, Animate.load_file_exn @@ directory / "uj.svg");
  (S_up, Animate.load_file_exn @@ directory / "up.svg");
  (S_u, Animate.load_file_exn @@ directory / "u.svg");
  (S_utj, Animate.load_file_exn @@ directory / "utj.svg");
  (S_vaj, Animate.load_file_exn @@ directory / "vaj.svg");
  (S_va, Animate.load_file_exn @@ directory / "va.svg");
  (S_vcj, Animate.load_file_exn @@ directory / "vcj.svg");
  (S_vc, Animate.load_file_exn @@ directory / "vc.svg");
  (S_vh, Animate.load_file_exn @@ directory / "vh.svg");
  (S_vhtj, Animate.load_file_exn @@ directory / "vhtj.svg");
  (S_vht, Animate.load_file_exn @@ directory / "vht.svg");
  (S_vhv, Animate.load_file_exn @@ directory / "vhv.svg");
  (S_vj, Animate.load_file_exn @@ directory / "vj.svg");
  (S_v, Animate.load_file_exn @@ directory / "v.svg");
  (S_vu, Animate.load_file_exn @@ directory / "vu.svg");
  (S_wd, Animate.load_file_exn @@ directory / "wd.svg");
  (S_wtd, Animate.load_file_exn @@ directory / "wtd.svg");
  (S_wt, Animate.load_file_exn @@ directory / "wt.svg");
  ]
  |> List.to_seq
  |> StrokeMap.of_seq

let svg_of_stroke ~stroke_glyph stroke=
  let svg: Animate.Svg.t= StrokeMap.find stroke.stroke_type stroke_glyph in
  let x= 
    (float_of_int stroke.frame.width) /.
    svg.viewBox.width
  and y= 
    (float_of_int stroke.frame.height) /.
    svg.viewBox.height
  and dx= float_of_int stroke.frame.x
  and dy= float_of_int stroke.frame.y in
  svg
    |> Glyph_outline.Svg.Adjust.scale ~x ~y
    |> Glyph_outline.Svg.Adjust.translate ~dx ~dy
 
let rect_of_stroke ~stroke_animate stroke=
  let rect:Animate.t= StrokeMap.find stroke.stroke_type stroke_animate in
  let svg= rect.svg in
  let x= 
    (float_of_int stroke.frame.width) /.
    svg.viewBox.width
  and y= 
    (float_of_int stroke.frame.height) /.
    svg.viewBox.height
  and dx= float_of_int stroke.frame.x
  and dy= float_of_int stroke.frame.y in
  rect
    |> Animate.Adjust.scale ~x ~y
    |> Animate.Adjust.translate ~dx ~dy
 
let paths_of_stroke ~stroke_glyph stroke=
  let svg= svg_of_stroke ~stroke_glyph stroke in
  svg.paths
 
let animations_of_stroke ~stroke_animate stroke=
  let svg= rect_of_stroke ~stroke_animate stroke in
  svg.animations
 
let outline_of_god ~stroke_glyph god=
  let viewBox= Glyph_outline.Svg.ViewBox.{ min_x= 0.; min_y= 0.; width= 0.; height= 0.; }
  and paths= god
    |> god_flatten
    |> List.map (paths_of_stroke ~stroke_glyph)
    |> List.concat
  in
  let svg= Glyph_outline.Svg.{ viewBox; paths } in
  Glyph_outline.Svg.Adjust.fit_frame svg

let animate_svg_of_god ~stroke_animate god=
  let size= calc_size god in
  let rec animate_svg_of_god ?(id=0) ?(time=0.) ?(indent=0) god=
    let indent_str0= String.make indent ' ' in
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
            Animate.to_string ~id ~time ~indent:(indent+2) godAnimate in
          ((id+1, time+.0.5),
          (sprintf "%s<g %s>\n%s\n%s</g>"
            indent_str0 transform
            godAnimate_str
            indent_str0))
        | SubGod subgod->
          let size= calc_size subgod.god in
          let dx= float_of_int subgod.frame.x
          and dy= float_of_int subgod.frame.y
          and rx= float_of_int subgod.frame.width /. size.width
          and ry= float_of_int subgod.frame.height /. size.height in
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
          let (id_next,time_next), subgod_str= animate_svg_of_god ~time ~id ~indent:(indent+2) subgod.god in
          ((id_next, time_next),
          (sprintf "%s<g %s>\n%s\n%s</g>"
            indent_str0 transform
            subgod_str
            indent_str0))
          )
    in
    (next_id,next_time), (String.concat "\n" elements)
  in
  match god.version_major, god.version_minor with
  | (1, 0) ->
    let _, asvg= animate_svg_of_god ~indent:2 god in
    let width= string_of_float size.width
    and height= string_of_float size.height in
    sprintf
      "<svg viewBox=\"0,0 %s,%s\" xmlns=\"http://www.w3.org/2000/svg\">\n%s\n</svg>"
      width height asvg
  | _-> failwith (sprintf "animate_svg_of_god %d %d" god.version_major god.version_minor)

