open Claudius
open Bdfparser

let claudius_slide = [
  "Claudius overview";
  "Functional fantasy console library";
  "Flitter meets TIC-80";
  "";
  "Idioms:";
  "* Pixel functional - like shaders";
  "* Screen functional - transforming data to primatives";
  "* Imperative - sometimes it's just what you need";
  "";
  "Challenges:";
  "* What's a useful set of features";
  "* Immutability vs inplace";
  "* Is state a library problem or not";
  "* Where to keep up the facade";
  "";
  "Key types/concepts:";
  "* Screen.t - record for dimensions, palette, etc.";
  "* Palette.t - list of colours used in final render.";
  "* Framebuffer.t - the abstract framebuffer in palette space";
  "* Primatives.t - operations to render to the framebuffer";
  "";
]

let credits = [|
  "fun ocaml";
  "avsm";
  "pferris";
  "lauriej";
  "dave84";
  "catnip";
  "aldroid";
  "enfys";
  "jonathanhogg";
  "genuary.art";
  "lovebyte";
  "field-fx";
|]

let summary_slide = [
  "Summary";
  "Good";
  "* Tiny Code Christmas a great way to learn OCaml";
  "* Mix of imperative and functional makes it easy to transition into";
  "* LoveByte commuity was super supportive";
  "";
  "Less good";
  "* Should not have added imperative interface - encourages me to be lazy";
  "";
  "Next steps";
  "* More odoc (thanks panglesd!)";
  "* Get it into opam";
  "* Get web page up for those interested in TCC24";
  "";
  "Code";
  "* https://github.com/mdales/tcc23";
  "* https://github.com/mdales/claudius";
  "* https://github.com/mdales/claudius-examples";
  "* https://github.com/mdales/bdfparser";
  "* https://github.com/mdales/ocaml-gif";
  "";
  "Related cool stuff";
  "* https://tcc.lovebyte.party";
  "* https://twitch.tv/fieldfxdemo";
  "* https://github.com/jonathanhogg/flitter";
  "";
  "Thanks:";
]

let palette =  Palette.load_tic80_palette "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let _palette =
  Palette.of_list [
    0x00FFFFFF;
    0x001B202B;
    0x00B2552C;
  ]

let draw_char (x : int) (y : int) (f : Bdf.t) (c : char) (col : int) (buffer : Framebuffer.t) : int =
  match Bdf.glyph_of_char f (Uchar.of_char c) with
  | None -> 0
  | Some glyph -> (
    let gw, gh, ox, oy = Bdf.Glyph.dimensions glyph in
    let bmp = Bdf.Glyph.bitmap glyph in
    (* Framebuffer.draw_rect x (y - gh) gw gh (Bytes.length bmp) buffer; *)
    if gh > 0 then (
    let bytes_per_line = (Bytes.length bmp) / gh in
    for h = 0 to (gh - 1) do
      for w = (bytes_per_line - 1) downto 0 do

        let bits_offset = ((bytes_per_line - 1) - w) * 8 in

        let bitcount = if (((bits_offset + 8)) < gw) then 8 else ((gw - bits_offset)) in
        let b = int_of_char (Bytes.get bmp ((h * bytes_per_line) + w)) in
        for bit = 0 to (bitcount - 1) do
          let isbit = (b lsl bit) land 0x80 in
          match isbit with
          | 0 -> ()
          | _ ->
          Framebuffer.pixel_write
            (x + bits_offset + bit + ox)
            ((y - oy) + h - gh)
            col
            buffer
        done
      done
    done); gw
  )

let draw_string (x : int) (y : int) (f : Bdf.t) (s : string) (col : int) (buffer : Framebuffer.t) =
  let sl = List.init (String.length s) (String.get s) in
  let rec loop offset remaining =
    match remaining with
    | [] -> offset
    | c :: remaining -> (
      let width = draw_char (x + offset) y f c col buffer in
      loop (offset + width) remaining
    )
  in loop 0 sl

let string_length (f : Bdf.t) (s : string) : int =
  let sl = List.init (String.length s) (String.get s) in
  List.fold_left (fun acc c ->
    let cwidth = match Bdf.glyph_of_char f (Uchar.of_char c) with
    | None -> 0
    | Some glyph -> (
      let gw, _gh, _ox, _oy = Bdf.Glyph.dimensions glyph in
      gw
    )
    in
    acc + cwidth
  ) 0 sl

let offset = ref 0
let debounce = ref []

let boot s =
  offset := 0;
  Framebuffer.init (Screen.dimensions s) (fun _x _y ->  15)

let tick title_font body_font prose t s _fb i =

  let i_list = Base.KeyCodeSet.to_list i in
  let updated_offset = match !debounce, i_list with
  | [Key.Space], [] -> (!offset) + 1
  | [Key.Down], [] -> (!offset) + 1
  | [Key.Up], [] -> (!offset) - 1
  | _ -> !offset
  in
  offset := updated_offset;
  debounce := i_list;

  let fb = Framebuffer.init (Screen.dimensions s) (fun _x _y ->  0) in
  let w, h = Screen.dimensions s in

  let palsize = Palette.size (Screen.palette s) in



  let inset = 5 in
  List.iteri (fun i s ->

    let font = match i with
    | 0 -> title_font
    | _ -> body_font in

    if ((String.compare s "Thanks:") == 0) then (
      (* let ft = float_of_int (t mod 240) in
      let tscale = 240. in*)
      let s = credits.((t / (7 * 8)) mod (Array.length credits))
      and col = (((t / 7) mod 8) + 0)  in
      ignore(draw_string
        (* (inset + 320 + 60 + (int_of_float (320. *. ((cos (Float.pi *. 1.8 *. (ft /. tscale)))))))*)
        60
        ((i + 1 - !offset) * 17) title_font s col fb)
    );

    ignore(draw_string inset ((i + 1 - !offset) * 17) font s 12 fb)

  ) prose;

  let step = h / palsize in
  for i = 0 to palsize do
    Framebuffer.filled_rect (w - 16) (step * i) 16 step (palsize - (i + 1)) fb
  done;

  fb

let generate_slide prose =
  let body_font = Result.get_ok (Bdf.create "thirdparty/classic-mac-fonts/bdf/Courier-12.bdf")
  and title_font = Result.get_ok (Bdf.create "thirdparty/chicago-bdf/Chicago-12.bdf") in
  (palette, Some boot, tick title_font body_font prose)
