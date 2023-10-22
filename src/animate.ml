module Svg= Glyph_outline.Svg
module Path= Svg.Path
module ViewBox= Svg.ViewBox

type svg= Svg.t

type t= {
  svg: Svg.t;
  animations: Rect.animation list;
}

let to_string ?(id=0) ?(time=0.) ?(indent=0) ?(index_step=2) (t:t)=
  let open Printf in
  let id= sprintf "clip%d" id in
  let indent_str0= String.make indent ' '
  and indent_str1= String.make (indent+index_step) ' ' in
  let svg= t.svg in
  let paths= svg.paths
    |> List.map (fun path->
      sprintf "%s<clipPath id=\"%s\">\n%s<path d=\"%s\"\n%s/>\n%s</clipPath>"
        indent_str0
        id
        indent_str1
        (Path.to_string_svg ~indent:(indent+index_step*2) path)
        indent_str1
        indent_str0
      )
    |> String.concat "\n"
  in
  let animations= t.animations
    |> List.map (Rect.string_of_animation_svg ~time ~indent:(indent+index_step))
    |> String.concat "\n"
  in
  let g= sprintf "%s<g clip-path=\"url(#%s)\">\n%s\n%s</g>"
    indent_str0
    id
    animations
    indent_str0
  in
  sprintf "%s\n%s"
    paths
    g

let xml_member name nodes=
  try
    Some (Ezxmlm.member name nodes)
  with
    Ezxmlm.Tag_not_found _-> None

let xml_member_with_attr name nodes=
  try
    Some (Ezxmlm.member_with_attr name nodes)
  with
    Ezxmlm.Tag_not_found _-> None

let load_file path=
  let chan= open_in path in
  let _dtd, nodes= Ezxmlm.from_channel chan in
  close_in chan;
  let get_paths nodes=
    Ezxmlm.members_with_attr "path" nodes
  and get_animations nodes=
    Ezxmlm.members_with_attr "rect" nodes in
  let attrs, svg= nodes |> Ezxmlm.member_with_attr "svg" in
  match Ezxmlm.get_attr "viewBox" attrs |> ViewBox.of_string with
  | Some viewBox->
    let paths=
      let clipPath= Ezxmlm.member "clipPath" svg in
      let container= match xml_member "g" clipPath with
        | Some g-> g
        | None-> clipPath
      in
      container
        |> get_paths
        |> List.map (fun (attrs, _)->
          Ezxmlm.get_attr "d" attrs)
        |> List.filter_map Path.of_string
    in
    let animations=
      let container= Ezxmlm.member "g" svg in
      container
        |> get_animations
        |> List.map Rect.of_xml
    in
    let svg= Svg.{viewBox; paths} in
    Some {svg; animations}
  | None-> None

let load_file_exn path=
  path |> load_file |> Option.get

module Adjust = struct
  let reset_viewBox (t:t)=
    let svg= t.svg in
    let animations= t.animations in
    let dx= -. svg.viewBox.min_x
    and dy= -. svg.viewBox.min_y in
    let viewBox= { svg.viewBox with min_x= 0.; min_y= 0. } in
    let paths= svg.paths |> List.map (Path.Adjust.position ~dx ~dy) in
    let animations= animations |> List.map (Rect.Adjust.position ~dx ~dy) in
    let svg= Svg.{ viewBox; paths } in
    { svg; animations }


  let fit_frame t=
    let svg= t.svg in
    match Path.get_frame_paths svg.paths with
    | None-> t
    | Some frame->
      let height= frame.py -. frame.ny
      and width= frame.px -. frame.nx
      and min_x= frame.nx
      and min_y= frame.ny in
      let viewBox= ViewBox.{ min_x; min_y; width; height } in
      let svg= { svg with viewBox } in
      { t with svg } |> reset_viewBox

  let scale ~x ~y t=
    let svg= t.svg in
    let animations= t.animations in
    let viewBox= svg.viewBox
    and paths= svg.Svg.paths in
    let viewBox= { viewBox with
      width= viewBox.width *. x;
      height= viewBox.height *. y;
      }
    and paths= List.map (Path.Adjust.scale ~x ~y) paths in
    let animations= List.map (Rect.Adjust.scale ~x ~y) animations in
    let svg= Svg.{ viewBox; paths } in
    { svg; animations }

  let translate ~dx ~dy t=
    let svg= t.svg in
    let animations= t.animations in
    let paths= List.map (Path.Adjust.position ~dx ~dy) svg.Svg.paths in
    let animations= List.map (Rect.Adjust.position ~dx ~dy) animations in
    let svg= Svg.{ svg with paths } in
    { svg; animations }
end

