structure UriEscape : sig
    val escape:   string -> string
    val unescape: string -> string
end
=
struct

structure Hex :
sig
  val fromHex1: char -> int option
  val fromHex2: char * char -> int option
  val toHex1:   int -> char option
  val toHex2:   int -> (char * char) option
end
=
struct

fun toHex1 i =
  if i >=  0 andalso i <=  9 then SOME (chr (i + 48)) else
  if i >= 10 andalso i <= 15 then SOME (chr (i + 55)) else
  NONE

fun fromHex1 c =
let
  val i = ord c
in
  if i >= 48 andalso i <=  57 then SOME (i - 48) else
  if i >= 97 andalso i <= 102 then SOME (i - 87) else
  if i >= 65 andalso i <=  70 then SOME (i - 55) else
  NONE
end


fun fromHex2 (h2, h1) =
  case fromHex1 h2 of NONE => NONE | SOME d2 =>
  case fromHex1 h1 of NONE => NONE | SOME d1 =>
  SOME (d2 * 16 + d1)

fun toHex2 d =
  if d < 0 orelse d >= 16 * 16 then NONE else
  SOME (valOf (toHex1 (d div 16)), valOf (toHex1 (d mod 16)))

end

open Hex


fun isNotEscapedChar c = (* RFC3986 *)
let
  val i = ord c
in
  if (i >= 65 andalso i <= 90)  (* A-Z *) orelse
     (i >= 97 andalso i <= 122) (* a-z *) orelse
     (i >= 48 andalso i <= 57)  (* 0-9 *) orelse
     c = #"-" orelse
     c = #"." orelse
     c = #"_" orelse
     c = #"~" orelse
     (* reserved *)
     c = #":" orelse
     c = #"/" orelse
     c = #"?" orelse
     c = #"#" orelse
     c = #"[" orelse
     c = #"]" orelse
     c = #"@" orelse
     c = #"!" orelse
     c = #"$" orelse
     c = #"&" orelse
     c = #"'" orelse
     c = #"(" orelse
     c = #")" orelse
     c = #"*" orelse
     c = #"+" orelse
     c = #"," orelse
     c = #";" orelse
     c = #"="
   then true else false
end


fun needEscape text =
let
  fun scan getc strm = case getc strm of NONE => SOME (false, strm) | SOME (c, strm) =>
    if isNotEscapedChar c then scan getc strm else SOME (true, strm)
in
  Option.valOf (StringCvt.scanString scan text)
end


fun escapeChar (c, r) = if isNotEscapedChar c then c::r else
  case toHex2 (ord c) of NONE => r | SOME (h2, h1) => (h1::(h2::(#"%"::r)))


fun escape text =
  if needEscape text
  then String.implode (List.rev (CharVector.foldl escapeChar [] text))
  else text


fun unescape text =
let
  fun scan r getc strm = case getc strm of NONE => SOME (List.rev r, strm) | SOME (c, strm) =>
    if c <> #"%" then scan (c::r) getc strm else
    case getc strm of NONE => SOME (List.rev r, strm) | SOME (c1, strm) =>
    case getc strm of NONE => SOME (List.rev r, strm) | SOME (c2, strm) =>
    case fromHex2 (c1, c2) of NONE => SOME (List.rev r, strm) | SOME n =>
    scan ((chr n)::r) getc strm
in
  if Char.contains text #"%"
  then String.implode (Option.valOf (StringCvt.scanString (scan []) text))
  else text
end


end
