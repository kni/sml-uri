structure UriPunycode : sig
    val encode: word list -> string
    val decode: string -> word list option
end
=
struct

val base = 0w36
val tmin = 0w1
val tmax = 0w26
val skew = 0w38
val damp = 0w700

val initialBias = 0w72
val initialN    = 0w128

val unicodeMax = 0wx10ffff


fun codePoint d =
  if  0w0 <= d andalso d <= 0w25 then d + (Word.fromInt (ord #"a")) else
  if 0w26 <= d andalso d <= 0w36 then d + (Word.fromInt (ord #"0")) - 0w26 else raise Overflow


fun adapt (delta, numpoints, firsttime) =
let
  val delta = if firsttime then delta div damp else delta div 0w2
  val delta = delta + (delta div numpoints)

  fun loop delta k =
    if delta > ((base - tmin) * tmax) div 0w2
    then loop (if delta = 0w0 then base - tmin else delta) (k + base)
    else (k, delta)

  val (k, delta) = loop delta 0w0
in
  k + (((base - tmin + 0w1) * delta) div (delta + skew))
end


local

fun qtLoop q bias k output =
let
  val t = if k <= bias then tmin else
          if k >= bias + tmax then tmax else k - bias
in
  if q < t then (q, output) else
  let
    val cp = codePoint (t + (q - t) mod (base - t))
  in
    qtLoop ((q - t) div (base - t)) bias (k + base) ((chr (Word.toInt cp))::output)
  end
end


fun inputFolder n basic_length (c, (delta, h, bias, output)) =
let
  val delta = if c < n then (delta + 0w1) else delta
in
  if c <> n then (delta, h, bias, output) else
  let
    val (q, output) = qtLoop delta bias base output
    val bias = adapt (delta, h + 0w1, h = basic_length)
  in
    (0w0, h + 0w1, bias, (chr (Word.toInt (codePoint q)))::output)
  end
end



fun hLoop input input_size n delta bias basic_length h output =
let
  val m = List.foldl (fn (a, acc) => if a >= n andalso a < acc then a else acc) unicodeMax input
  val delta = delta + (m - n) * (h + 0w1)
  val n = m
  val (delta, h, bias, output) = List.foldl (inputFolder n basic_length) (delta, h, bias, output) input
in
  if h < input_size
  then hLoop input input_size (n + 0w1) (delta + 0w1) bias basic_length h output
  else output
end


in

fun encode input =
let
  val input_size   = List.length input

  val basic        = List.foldl (fn(a, acc) => if a < initialN then (chr (Word.toInt a))::acc else acc ) [] input
  val basic_length = List.length basic

  val output = if basic_length > 0 andalso basic_length < input_size then #"-"::basic else basic

  val output = hLoop input (Word.fromInt input_size) initialN 0w0 initialBias (Word.fromInt basic_length) (Word.fromInt basic_length) output
in
  String.implode (List.rev output)
end

end


local

fun digitValue code =
  if code >= 0w65 andalso code <=  0w90 (* A-Z *) then SOME (code - Word.fromInt (ord #"A"))        else
  if code >= 0w97 andalso code <= 0w122 (* a-z *) then SOME (code - Word.fromInt (ord #"a"))        else
  if code >= 0w48 andalso code <=  0w57 (* 0-9 *) then SOME (code - Word.fromInt (ord #"0") + 0w26) else
  NONE


fun kLoop k w i bias [] = SOME (w, i, [])
  | kLoop k w i bias (cp::code) =
    case digitValue cp of NONE => NONE (* invalid punycode input *) | SOME digit =>
    let
      val i = i + digit * w
      val t = if k <= bias then tmin else
              if (k >= bias + tmax) then tmax else k - bias
    in
      if digit < t
      then SOME (w, i, code)
      else kLoop (k + base) (w * (base - t)) i bias code
    end

fun listInsert [] 0w0 e = [e]
  | listInsert xs pos e =
let
  fun loop i [] ys =
        if i = pos
        then List.rev (e::ys)
        else List.rev ys
    | loop i (x::xs) ys =
        if i = pos
        then loop (i + 0w1) xs (x::e::ys)
        else loop (i + 0w1) xs (x::ys)
in
  loop 0w0 xs []
end


fun codeLoop output code n i bias = if List.length code <= 0 then SOME (rev output) else
let
  val oldi = i
in
  case kLoop base 0w1 i bias code of NONE => NONE | SOME (w, i, code) =>
  let
    val outputLength = Word.fromInt (List.length output)
    val bias = adapt(i - oldi, outputLength + 0w1, oldi = 0w0)
    val n = n + (i div (outputLength + 0w1))
    val i = i mod (outputLength + 0w1)
    val output = listInsert output i n
  in
    codeLoop output code n (i + 0w1) bias
  end
end


fun scanDelimiter found beforeDelimiter afterDelimiter all getc strm =
  case getc strm of
      NONE => if found then SOME ((rev beforeDelimiter, rev afterDelimiter), strm) else SOME (([], rev all), strm)
    | SOME (c, strm) =>
      let
        val i = Word.fromInt (ord c)
      in
        if i >= initialN then NONE else
        if found
        then scanDelimiter found beforeDelimiter (i::afterDelimiter) (i::all) getc strm
        else
          if c = #"-"
          then scanDelimiter true      beforeDelimiter  afterDelimiter (i::all) getc strm
          else scanDelimiter found (i::beforeDelimiter) afterDelimiter (i::all) getc strm
      end
in

fun decode text =
  case StringCvt.scanString (scanDelimiter false [] [] []) text of NONE => NONE | SOME (output, code) =>
  case codeLoop output code initialN 0w0 initialBias of NONE => NONE | SOME output => SOME (List.rev output)

end

end
