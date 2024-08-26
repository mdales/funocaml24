open Claudius

let vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb7400b86930c35e60ce5390d94ea8de48bfe3"

let palette = Palette.load_tic80_palette vapour_palette

let boot s =
  Framebuffer.init (Screen.dimensions s) (fun _x _y -> 1)

let tick t s _p _i =
  let ft = (float_of_int t)
  and w, h = (Screen.dimensions s)
  and colors = (Palette.size (palette)) in
  let fcolors = Float.of_int colors in
  let buffer = Framebuffer.init (w, h) (fun i j ->
    let x = Float.of_int (i - (w / 2))
    and y = Float.of_int (j - (h / 2)) in
    let d1 = (float_of_int w) /. sqrt ((x *. x) +. (y *. y) +. 1.0)
    and c1 = ((atan2 y x) +. Float.pi) *. (fcolors /. (2.0 *. Float.pi)) in
    let c2 = c1 +. (sin (ft /. 70.0) *. Float.pi *. 2.0)
    and d2 = d1 +. (Float.rem (ft /. 10.0) fcolors) in
    let p = (int_of_float (Float.floor c2)) lxor (int_of_float (Float.floor d2)) in
    let pindex = (p mod colors) in
    if pindex < 0 then (colors + pindex) else  pindex
  ) in
  Framebuffer.filled_circle (w / 2) (h / 2) 15. 1 buffer;
  buffer

  let slide = (palette, boot, tick)