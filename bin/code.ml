open Astring
open Claudius
open Bdfparser

let ocaml_keywords = [
  "and";
  "as";
  "asr";
  "assert";
  "class";
  "constraint";
  "do";
  "done";
  "downto";
  "else";
  "end";
  "exception";
  "external";
  "false";
  "for";
  "fun";
  "function";
  "functor";
  "if";
  "in";
  "include";
  "inherit";
  "initializer";
  "land";
  "lazy";
  "let";
  "lor";
  "lsl";
  "lsr";
  "lxor";
  "match";
  "method";
  "mod";
  "module";
  "mutable";
  "new";
  "nonrec";
  "object";
  "of";
  "open";
  "or";
  "private";
  "rec";
  "ref";
  "sig";
  "struct";
  "then";
  "to";
  "true";
  "try";
  "type";
  "val";
  "virtual";
  "when";
  "while";
  "with";
]

let lua_keywords = [
  "and";
  "break";
  "do";
  "else";
  "elseif";
  "end";
  "false";
  "for";
  "function";
  "if";
  "in";
  "local";
  "nil";
  "not";
  "or";
  "repeat";
  "return";
  "then";
  "true";
  "until";
]

let palette =  Palette.load_tic80_palette "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"

let inset = 5

let offset = ref 0
let debounce = ref []

let boot initial_offset s =
  offset := initial_offset;
  Framebuffer.init (Screen.dimensions s) (fun _x _y ->  0)

type part =
  | Misc of string
  | Comment of string
  | String of string
  | Number of string
  | Keyword of string
  | Seperator of char

let colour_parts p =
  List.map (fun t ->
  match t with
  | Misc s -> (s, 12)
  | Comment s -> (s, 13)
  | String s -> (s, 5)
  | Keyword s -> (s, 3)
  | Number s -> (s, 11)
  | Seperator c -> (Printf.sprintf "%c" c, 12)
) p

let process_atom keywords w =
  match List.exists (fun k -> String.compare k w == 0) keywords with
  | true -> Keyword w
  | false -> (
    match int_of_string_opt w with
    | None -> (
      match float_of_string_opt w with
      | None -> Misc w
      | Some _ -> Number w
    )
    | Some _ -> Number w
  )

let rec ksplitter prose keywords start =
  let n = String.find ~start (fun c ->  match String.find (fun t -> t == c) " ;,()*+-/%=[]"  with None -> false | Some _ -> true) prose in
  match n with
  | None -> [process_atom keywords (String.Sub.to_string (String.sub ~start prose))]
  | Some idx -> (
    match idx with
    | 0 -> Seperator (String.get prose idx) :: ksplitter prose keywords (idx + 1)
    | _ -> (if idx != start then [(
      let w = (String.Sub.to_string (String.sub ~start ~stop:idx prose)) in
      process_atom keywords w
      )] else []) @ [Seperator (String.get prose idx)] @ ksplitter prose keywords (idx + 1)
  )

(* All of this is a hack just for the talk - if I was doing this properly I'd use a lex/tokenizer) *)
let ocaml_splitter line =
  let rec inner x =
    (* extract strings and comments first *)
    let comment_start = String.find_sub ~sub:"(*" x in
    let comment_end = String.find_sub ~start:(match comment_start with None -> 0 | Some x -> (x + 2)) ~sub:"*)" x in

    match comment_start, comment_end with
    | Some x, None -> inner (String.Sub.to_string (String.sub ~stop:(x - 1) line)) @  [Comment (String.Sub.to_string (String.sub ~start:x line))]
    | None, Some x -> [Comment (String.Sub.to_string (String.sub ~stop:x line))] @ inner  (String.Sub.to_string (String.sub ~start:(x + 1) line))
    | Some x, Some y -> inner (String.Sub.to_string (String.sub ~stop:(x - 0) line)) @ [Comment (String.Sub.to_string (String.sub ~start:x ~stop:(y + 2) line))] @ inner (String.Sub.to_string (String.sub ~start:(y + 2) line))
    | None, None -> (

      let string_start = String.find (fun c -> c == '"') x in
      let string_end = String.find ~start:(match string_start with None -> 0 | Some x -> (x + 1)) (fun c -> c == '"') x in

      match string_start, string_end with
      | Some x, None -> inner (String.Sub.to_string (String.sub ~stop:(x - 1) line)) @ [String (String.Sub.to_string (String.sub ~start:x line))]
      | None, Some x -> [String (String.Sub.to_string (String.sub ~stop:x line))] @ inner  (String.Sub.to_string (String.sub ~start:(x + 1) line))
      | Some x, Some y -> inner (String.Sub.to_string (String.sub ~stop:x line)) @ [String (String.Sub.to_string (String.sub ~start:x ~stop:(y + 1) line))] @ inner (String.Sub.to_string (String.sub ~start:(y + 1) line))
      | None, None -> (
          ksplitter x ocaml_keywords 0
      )
    )
  in inner line |> colour_parts

let lua_splitter line =
  let rec inner x =
      (* extract strings and comments first *)
      let comment_start = String.find_sub ~sub:"--" x in

      match comment_start with
      | Some x ->  if (x > 0) then (inner (String.Sub.to_string (String.sub ~stop:(x - 1) line))) else ([]) @  [Comment (String.Sub.to_string (String.sub ~start:x line))]
      | None -> (

        let string_start = String.find (fun c -> c == '"') x in
        let string_end = String.find ~start:(match string_start with None -> 0 | Some x -> (x + 1)) (fun c -> c == '"') x in

        match string_start, string_end with
        | Some x, None -> inner (String.Sub.to_string (String.sub ~stop:(x - 1) line)) @ [String (String.Sub.to_string (String.sub ~start:x line))]
        | None, Some x -> [String (String.Sub.to_string (String.sub ~stop:x line))] @ inner  (String.Sub.to_string (String.sub ~start:(x + 1) line))
        | Some x, Some y -> inner (String.Sub.to_string (String.sub ~stop:x line)) @ [String (String.Sub.to_string (String.sub ~start:x ~stop:(y + 1) line))] @ inner (String.Sub.to_string (String.sub ~start:(y + 1) line))
        | None, None -> (
          ksplitter x lua_keywords 0
        )
      )
    in inner line |> colour_parts

let draw_string x y font s splitter fb =
  splitter s
  |> List.fold_left (fun offset (w, c) ->
    offset + (Textslide.draw_string offset y font w c fb)
  ) x


let tick title_font body_font heading code splitter _t s _fb i =
  let w, h = Screen.dimensions s in

  let i_list = Base.KeyCodeSet.to_list i in
  let shift = List.exists (fun x -> (x == 0x400000e1) || (x == 0x400000e5)) i_list in
  let i_list = List.filter (fun x -> (x != 0x400000e1) && (x != 0x400000e5)) i_list in

  let updated_offset = match !debounce, i_list with
  | [0x00000020], [] -> (!offset) + (if shift then 10 else 1)
  | [0x40000051], [] -> (!offset) + (if shift then 10 else 1)
  | [0x40000052], [] -> (!offset) - (if shift then 10 else 1)
  | _ -> !offset
  in
  offset := if (updated_offset >= 0) then updated_offset else 0;
  debounce := i_list;

  let fb = Framebuffer.init (w, h) (fun _x _y ->  0) in

  let palsize = Palette.size (Screen.palette s) in

  List.iteri (fun i s ->
    ignore(draw_string inset ((i + 2 - !offset) * 17) body_font s splitter fb)
  ) code;

  Framebuffer.filled_rect 0 0 w 18 0 fb;
  ignore(Textslide.draw_string inset 17 title_font (Printf.sprintf "%s : %d" heading !offset) 12 fb);
  let step = h / palsize in
  List.init palsize (fun i ->
    Primitives.FilledRect(
      {x = w - 16 ; y = step * i},
      {x = w - 1; y = (step * (i + 1))},
      (palsize - (i + 1))
    )
  )
  |> Framebuffer.render fb;
  fb

let generate_slide filename initial_offset =
  let code = In_channel.with_open_bin filename In_channel.input_all |>
    Stdlib.String.split_on_char '\n'
  in

  let fname = Fpath.of_string filename |> Result.get_ok in
  let cwd = Fpath.of_string (Sys.getcwd ()) |> Result.get_ok in
  let basename = Fpath.normalize (Fpath.append cwd fname ) |> Fpath.to_string
  and ext = Fpath.get_ext fname in

  let splitter = match ext with
  | ".ml" | ".mli" -> ocaml_splitter
  | ".lua" -> lua_splitter
  | _ -> ocaml_splitter
  in

  let body_font = Result.get_ok (Bdf.create "thirdparty/classic-mac-fonts/bdf/Courier-12.bdf")
  and title_font = Result.get_ok (Bdf.create "thirdparty/chicago-bdf/Chicago-12.bdf") in
  (palette, Some (boot initial_offset), tick title_font body_font basename code splitter)
