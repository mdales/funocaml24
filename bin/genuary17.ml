open Claudius

let generate_poly (x : int) (y : int) (r : int) (sides : int) (a : float) (col : int) =
  let segment_angle = (2. *. Float.pi) /. (Float.of_int sides) in
  let fr = Float.of_int r in
  let rec loop = (fun (i : int) (rest) ->
    let angle = ((Float.of_int i) *. segment_angle) +. a in
    let next : Primitives.point = {
      x = Int.of_float (fr *. sin angle) ;
      y = Int.of_float (fr *. cos angle) ;
    } in
    match i with
    | 0 -> next :: rest
    | _ -> next :: loop (i - 1) rest
  ) in
  let points = loop (sides - 1) [] in
  let shifted = List.map (fun (p : Primitives.point) : Primitives.point ->
    {
      x = p.x + x ;
      y = p.y + y ;
    }
  ) points in
  Primitives.Polygon (shifted, col)

let poly_points poly =
  match poly with
  | Primitives.Polygon (list, _) -> list
  | _ -> []

let generate_star (x : int) (y : int) (r1 : int) (r2 : int) (sides : int) (a : float) (col : int) =
  let s1 = generate_poly x y r1 sides a col |> poly_points in
  let s2 = generate_poly x y r2 sides (a +. ((Float.pi *. 2.) /. ((Float.of_int sides) *. 2.))) col |> poly_points in
  let mixed = List.concat (List.map2 (fun a b -> [b ; a]) s1 s2) in
  Primitives.Polygon (mixed, col)

let generate_cross (x : int) (y : int) (_r1 : int) (_r2 : int) (sides : int) (a : float) (col : int) =
  let s1 = generate_poly x y 132 sides a col |> poly_points in
  let s2 = generate_poly x y 56 sides (a +. ((Float.pi *. 2.) /. ((Float.of_int sides) *. 2.))) col |> poly_points in
  let s1s2 = List.concat (List.map2 (fun a b -> [b ; a]) s1 s2) in
  let s3 = generate_poly x y 102 (sides * 2) (a +. ((Float.pi *. 2.) /. ((Float.of_int sides) *. 4.))) col |> poly_points in
  let mixed = List.concat (List.map2 (fun a b -> [b ; a]) s1s2 s3) in
  Primitives.Polygon (mixed, col)

let generate_filled_star (x : int) (y : int) (r1 : int) (r2 : int) (sides : int) (a : float) (_col : int) =
  let col = 111 in
  let s1 = generate_poly x y r1 sides a col |> poly_points in
  let s2 = generate_poly x y r2 sides (a +. ((Float.pi *. 2.) /. ((Float.of_int sides) *. 2.))) col |> poly_points in
  let mixed = List.concat (List.map2 (fun a b -> [b ; a]) s1 s2) in
  Primitives.FilledPolygon (mixed, col)

let generate_filled_cross (x : int) (y : int) (_r1 : int) (_r2 : int) (sides : int) (a : float) (_col : int) =
  let col = 16 in
  let s1 = generate_poly x y 131 sides a col |> poly_points in
  let s2 = generate_poly x y 56 sides (a +. ((Float.pi *. 2.) /. ((Float.of_int sides) *. 2.))) col |> poly_points in
  let s1s2 = List.concat (List.map2 (fun a b -> [b ; a]) s1 s2) in
  let s3 = generate_poly x y 101 (sides * 2) (a +. ((Float.pi *. 2.) /. ((Float.of_int sides) *. 4.))) col |> poly_points in
  let mixed = List.concat (List.map2 (fun a b -> [b ; a]) s1s2 s3) in
  Primitives.FilledPolygon (mixed, col)

let tick t s fb i =
  let notshow1 = Base.KeyCodeSet.exists (fun x -> x == Key.Num1) i in

  let gen_star, gen_cross = match notshow1 with
  | false -> generate_star, generate_cross
  | true -> generate_filled_star, generate_filled_cross
  in

  let width, height = Screen.dimensions s in
  let ft = Float.of_int t in
  let col = (Palette.size (Screen.palette s)) - 1 in
  let fcol = Float.of_int (col + 1) in
  let inner_radius = 100
  and outer_radius = 131 in
  let tscale = 100. in
  Framebuffer.map_inplace (fun _ ->
    match notshow1 with
  | false -> 0
  | true -> if ((sin (ft /. tscale)) >= 0.) then 16 else 111
  ) fb;

  (match (sin (ft /. tscale)) >= 0. with
  | true -> List.concat (List.init 3 (fun i ->
    List.init 3 (fun j ->
      gen_star
        ((i * outer_radius * 2) + ((width / 2) - (outer_radius * 2)))
        ((j * outer_radius * 2) + ((height / 2) - (outer_radius * 2)))
        inner_radius
        outer_radius
        8
        ((Float.pi *. (1. /. 8.) *. (cos (ft /. tscale))))
        ((Int.of_float ((fcol *. 0.25) *. (cos (ft /. 150.)))) + ((col * 3) / 4))
    )
  ))
  | false -> List.concat (List.init 4 (fun i ->
      List.init 3 (fun j ->
        gen_cross
          ((i * outer_radius * 2) + ((width / 2) - (outer_radius * 2)) - outer_radius)
          ((j * outer_radius * 2) + ((height / 2) - (outer_radius * 2)) + outer_radius)
          outer_radius
          inner_radius
          4
          (((Float.pi *. (1. /. 4.) *. (0.0 +. (cos (ft /. tscale))))) +. (Float.pi /. 4.))
          ((Int.of_float ((fcol *. 0.25) *. (cos (ft /. 150.)))) + ((col * 3) / 4))
      )
  ))) |> Framebuffer.render fb;
  fb

let palette = Palette.of_list (List.rev (Palette.to_list (Palette.generate_mono_palette 128)))

let slide = (palette, None, tick)
