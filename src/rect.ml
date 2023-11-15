(*
 * rect.ml
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

module Utils= Smaji_glyph_outline.Utils
module MiniParsec= Utils.MiniParsec

open Utils
open Printf

type attrName=
  | X
  | Y
  | Width
  | Height

let attrName_of_string= function
  | "x"-> X
  | "y"-> Y
  | "width"-> Width
  | "height"-> Height
  | _-> failwith "attrName_of_string"

let string_of_attrName= function
  | X      -> "x"
  | Y      -> "y"
  | Width  -> "width"
  | Height -> "height"

type animate= {
  attrName: attrName;
  time_begin: float;
  time_dur: float;
  values: float list;
}

type animation= {
  position: Svg.ViewBox.t;
  rotate: float option;
  changes: animate list;
}

let string_of_animate_hum animate=
  sprintf "{ attrName: %s; begin: %s; duration: %s; values: %s}"
    (string_of_attrName animate.attrName)
    (string_of_float animate.time_begin)
    (string_of_float animate.time_dur)
    (animate.values |> List.map string_of_float |> String.concat ";")

let string_of_animate_svg ?(time=0.) ?(indent=0) animate=
  let indent= String.make indent ' ' in
  sprintf {|%s<animate attributeName="%s" begin="%s" dur="%s" values="%s" repeatCount="1" fill="freeze"/>|}
    indent
    (string_of_attrName animate.attrName)
    ((string_of_float (animate.time_begin+.time)) ^ "s")
    ((string_of_float animate.time_dur) ^ "s")
    (animate.values |> List.map string_of_float |> String.concat ";")

let string_of_animation_hum animation=
  let position= ViewBox.to_string_hum animation.position
  and rotate= Option.value ~default:0. animation.rotate |> string_of_float
  and changes= animation.changes
    |> List.map string_of_animate_hum
    |> String.concat "; " in
  sprintf "{ position: %s; rotate: %s; changes: %s}"
    position
    rotate
    changes

let string_of_animation_svg ?(time=0.) ?(indent=0) animation=
  let indent_str= String.make indent ' ' in
  let positon= animation.position in
  let changes= animation.changes
    |> List.map (string_of_animate_svg ~time ~indent:(indent+2))
    |> String.concat "\n" in
  let rect_info= sprintf {|x="%s" y="%s" width="%s" height="%s"|}
    (string_of_float positon.min_x)
    (string_of_float positon.min_y)
    (string_of_float positon.width)
    (string_of_float positon.height)
  in
  let rotate_info= animation.rotate
    |> Option.map (fun rotate-> sprintf {| transform="rotate(%s %s,%s)"|}
      (string_of_float rotate)
      (string_of_float positon.min_x)
      (string_of_float positon.min_y)
      )
    |> Option.value ~default:""
  in
  sprintf "%s<rect %s%s>\n%s\n%s</rect>"
    indent_str
    rect_info
    rotate_info
    changes
    indent_str

type rect= {
    x: float;
    y: float;
    width: float;
    height: float;
    transform: float option;
}


module Parser = struct
  open MiniParsec

  let ( let* )= bind

  let string_of_cl cl= String.concat "" (List.map (String.make 1) cl)

  let space= char ' ' <|> char '\t' <|> (newline |>> (fun _-> '\n'))

  let spaces= many space

  let float1=
    let* neg= option (char '-') in
    let* integer= many1 num_dec << option (char '.') in
    let* fractional= option (many1 num_dec) in
    let neg= Option.is_some neg in
    let integer= integer |> string_of_cl |> float_of_string in
    let fractional= match fractional with
      | None-> 0.
      | Some cl-> "." ^ (string_of_cl cl) |> float_of_string
    in
    let num= integer +. fractional in
    let result= if neg then -. num else num in
    return result

  let number_sep= spaces >> option (char ',') >> spaces

  let transform= string "rotate" >> spaces >> char '(' >> spaces >> float1
end


module Adjust = struct
  let change_position ~dx ~dy change=
    match change.attrName with
    | X-> let values= change.values |> List.map ((+.) dx) in
      { change with values }
    | Y-> let values= change.values |> List.map ((+.) dy) in
      { change with values }
    | Width | Height-> change

  let change_scale ~x ~y change=
    match change.attrName with
    | X-> let values= change.values |> List.map (( *. ) x) in
      { change with values }
    | Y-> let values= change.values |> List.map (( *. ) y) in
      { change with values }
    | Width-> let values= change.values |> List.map (( *. ) x) in
      { change with values }
    | Height-> let values= change.values |> List.map (( *. ) y) in
      { change with values }

  let position ~dx ~dy animation=
    let position= animation.position in
    let position=
      { position with
        min_x= position.min_x +. dx;
        min_y= position.min_y +. dy;
      }
    in
    let changes= animation.changes in
    let changes= List.map (change_position ~dx ~dy) changes in
    { animation with
      position;
      changes;
    }

  let scale ~x ~y animation=
    let position= animation.position in
    let position=
      ViewBox.{
        min_x= position.min_x *. x;
        min_y= position.min_y *. y;
        width= position.width *. x;
        height= position.height *. y;
      }
    in
    let changes= animation.changes in
    let changes= List.map (change_scale ~x ~y) changes in
    { animation with
      position;
      changes;
    }
end

module Raw = struct
  type rect_unknown= {
    x: float option;
    y: float option;
    width: float option;
    height: float option;
    transform: float option;
  }

  type animate_unknown= {
    attrName: attrName option;
    time_begin: float;
    time_dur: float;
    values: float list;
  }

  let get_rect attrs=
    match ListLabels.fold_left
      attrs
      ~init:{x=None; y=None; width=None; height=None; transform= None}
      ~f:(fun acc attr->
        let ((_ns, name), value)= attr in
        match name with
        | "x"-> { acc with x= Some (float_of_string value) }
        | "y"-> { acc with y= Some (float_of_string value) }
        | "width"-> { acc with width= Some (float_of_string value) }
        | "height"-> { acc with height= Some (float_of_string value) }
        | "transform"->
          (match Utils.MiniParsec.parse_string Parser.transform value with
          | Ok (rotate, _)->
            { acc with transform= Some rotate }
          | Error _-> acc)
        | _-> acc)
    with
    | {
        x= Some x;
        y= Some y;
        width= Some width;
        height= Some height;
        transform= transform;
      }->
      ({x; y; width; height; transform}:rect)
    | _-> failwith "get_rect"

  let float_of_timeCount str=
    let len= String.length str in
    match str.[len-1] with
    | 's'->
      if str.[len-2] == 'm' then (* ms *)
        (str
          |> StringLabels.sub ~pos:0 ~len:(len-2)
          |> String.trim |> float_of_string)
          /. 1000.
      else (* s *)
        str
          |> StringLabels.sub ~pos:0 ~len:(len-1)
          |> String.trim |> float_of_string
    | 'n'-> (* min *)
        (str
          |> StringLabels.sub ~pos:0 ~len:(len-3)
          |> String.trim |> float_of_string)
          *. 60.
    | 'h'->  (* h *)
        (str
          |> StringLabels.sub ~pos:0 ~len:(len-1)
          |> String.trim |> float_of_string)
          *. 60. *. 60.
    | _-> str |> String.trim |> float_of_string

  let get_animate attrs=
    match ListLabels.fold_left
      attrs
      ~init:{attrName= None; values=[]; time_begin=0.0; time_dur=0.0}
      ~f:(fun acc attr->
        let ((_ns, name), value)= attr in
        match name with
        | "attributeName"-> { acc with attrName= Some (attrName_of_string value) }
        | "values"-> { acc with values= value |> String.split_on_char ';' |> List.map float_of_string}
        | "begin"-> { acc with time_begin= float_of_timeCount value }
        | "dur"-> { acc with time_dur= float_of_timeCount value }
        | _-> acc)
    with
    | {
        attrName= Some attrName;
        time_begin= time_begin;
        time_dur= time_dur;
        values= values;
      }->
      ({attrName; values; time_begin; time_dur}:animate)
    | _-> failwith "get_animate"

end

let of_xml (attrs, nodes)=
  let rect= Raw.get_rect attrs in
  let animate_list= nodes
    |> Ezxmlm.members_with_attr "animate"
    |> List.map (fun (attrs,_)-> Raw.get_animate attrs)
  in
  let position= ViewBox.{
    min_x= rect.x;
    min_y= rect.y;
    width= rect.width;
    height= rect.height;
  } in
  let rotate= rect.transform in
  let changes= animate_list in
  {
    position;
    rotate;
    changes;
  }

