open Graphics
open Symb

let start =
  open_graph " 700x700"; set_line_width 2;
  set_window_title "ZSy simulation"

type capt_pos = Below | Left


let plot scale margin max1 max2 z =

  let pos (x, y) =
    scale*x + margin, scale*y + margin
  in

  let draw_line p1 p2 =
    let x1,y1 = pos p1
    and x2, y2 = pos p2 in
    moveto x1 y1;
    lineto x2 y2
  in

  let caption cpos p s =
    let x, y = pos p in
    let t1, t2 = text_size s in
    begin  match cpos with
    | Below -> moveto (x - t1/2) (y - margin/2)
    | Left -> moveto (x - margin/2 + t1) (y - t2/2)
    end;
    draw_string s
  in

  let draw_edge color_lt color_leq p1 p2 r =
    begin match r with
    | LEQ -> set_color color_leq
    | LT -> set_color color_lt
    end;
    draw_line p1 p2
  in

  let draw_edges color_lt color_leq (c,r) =
    let n = Array.length c - 1 in
    let h = c.(0) in
    let point = (List.filter (fun x -> x <> h) (Array.to_list c) = []) in
    if point then
      let x, y = pos h in
      set_color black;
      fill_circle x y (scale/20)
    else
      for i = 0 to n-1 do
        (* if not (frame c.(i) c.(i+1)) then *)
        draw_edge  color_lt color_leq c.(i) c.(i+1) r.(i)
      done
  in

  let draw_contour color_lt color_leq color_fill (c,r) =
    set_color color_fill;
    fill_poly (Array.map pos c);
    draw_edges color_lt color_leq (c,r)
  in


  let draw_grid color max1 max2 =
    set_color color;
    draw_line (0,0) (0, max1);
    draw_line (0,0) (max2, 0);
    for i = 0 to max1 do
      caption Below (i, 0) (string_of_int i);
      draw_line (i, 0) (i, max1);
    done;
    for i = 0 to max2 do
      caption Left (0, i) (string_of_int i);
      draw_line (0,i) (max1, i);
    done
  in

  clear_graph ();
  draw_grid (rgb 200 200 200) max1 max2;
  draw_contour red black (rgb 180 180 180) z;
