structure Uri :
sig
  eqtype uri
  val uri: string -> uri option

  val scheme:    uri -> string option
  val user:      uri -> string option
  val password:  uri -> string option
  val host:      uri -> string option
  val port:      uri -> string option
  val path:      uri -> string
  val query:     uri -> string option
  val fragment:  uri -> string option

  val toString: uri -> string
  val toIri:    uri -> string
  val abs:      uri -> uri -> uri
end
=
struct

  open Scancom

  datatype uri = URI of {
      scheme    : string option,
      user      : string option,
      password  : string option,
      host      : string option,
      port      : string option,
      path      : string,
      query     : string option,
      fragment  : string option
    }


  val scheme    = takeWhile (fn c => c <> #":" andalso c <> #"/" andalso c <> #"?" andalso c <> #"#") <* takeStr ":"

  val authority = takeStr "//" *> takeWhile (fn c => c <> #"/" andalso c <> #"?" andalso c <> #"#")

  val path      = takeWhile (fn c => c <> #"?" andalso c <> #"#")

  val query     = takeStr "?" *> takeWhile (fn c => c <> #"#")

  val fragment  = takeStr "#" *> takeTail


  (* optionAnd - maybe and must *)
  fun optionAnd (p : ('a, 'cs) Scanner ) (pg : ('a option -> ('b, 'cs) Scanner)) : ('b, 'cs) Scanner = fn getc => fn strm =>
      case p getc strm of
          SOME (r, t) => (pg (SOME r)) getc t
        | NONE        => (pg NONE)     getc strm


  infix 1 ?>
  fun p ?> pg = optionAnd p pg

  local
    fun doSplit f s = case String.fields f s of [f,s] => (SOME f, SOME s) | [f] =>  (SOME f, NONE) | _ => (NONE, NONE)
    fun isColon c = c = #":"
    fun isAt    c = c = #"@"
  in
    fun doAuthority NONE = (NONE, NONE, NONE, NONE)
      | doAuthority (SOME a) =
        case doSplit isAt a of
            (SOME ui, SOME hp) => let val (u, pw) = doSplit isColon ui val (h, p) = doSplit isColon hp in (u, pw, h, p) end
          | (SOME hp, NONE   ) => let                                  val (h, p) = doSplit isColon hp in (NONE, NONE, h, p) end
          | _                  => (NONE, NONE, NONE, NONE)
  end

  fun toLower NONE     = NONE
    | toLower (SOME s) = SOME (String.map Char.toLower s)


  fun onlyAscii text =
  let
    fun scan getc strm = case getc strm of NONE => SOME (true, strm) | SOME (c, strm) =>
      if Char.isAscii c then scan getc strm else SOME (false, strm)
  in
    Option.valOf (StringCvt.scanString scan text)
  end

  fun filterUnicode 0wx202b = false (* RIGHT-TO-LEFT EMBEDDING *)
    | filterUnicode 0wx202c = false (* POP DIRECTIONAL FORMATTING *)
    | filterUnicode _       = true

  fun encodeHostPart p = if onlyAscii p then p else
    "xn--" ^ (UriPunycode.encode (List.filter filterUnicode (Unicode.toCaseFolded (Utf8.decode p))))

  fun encodeHost h = if onlyAscii h then h else
    String.concatWith "." (List.map encodeHostPart (String.tokens (fn c => c = #".") h))


  fun decodeHostPart h = if not (String.isPrefix "xn--" h) then h else
    case UriPunycode.decode (String.extract (h, 4, NONE)) of NONE => h | SOME u => Utf8.encode u

  fun decodeHost h = if not (String.isSubstring "xn--" h) then h else
    String.concatWith "." (List.map decodeHostPart (String.tokens (fn c => c = #".") h))


  fun normalize p =
    if String.isSubstring "/." p orelse String.isSubstring "./" p orelse String.isSubstring "//" p
    then
      let
        val isAbs = String.isPrefix "/" p
        val isDot = String.isPrefix "." p
        val isDir = String.isSuffix "/" p orelse String.isSuffix "/." p orelse String.isSuffix "/.." p
        val ps = String.tokens (fn c => c = #"/") p

        fun doit []         y         = List.rev (if isDir then (""::y) else y)
          | doit ("."::xs)  y         = doit xs y
          | doit (".."::xs) (z::y)    = if isAbs orelse z <> ".." then doit xs y else doit xs (".."::z::y)
          | doit (".."::xs) []        = if isAbs then doit xs [] else doit xs [".."]
          | doit (x::xs)    y         = doit xs (x::y)

        val n = doit ps []
        val r = String.concatWith "/" (if isAbs then (""::n) else n)
      in
        if r = "" andalso isDot then "." else r
      end
    else p

  val scanURI =
      scheme    ?> (fn s =>
      authority ?> (fn a =>
      path      ?> (fn p =>
      query     ?> (fn q =>
      fragment  ?> (fn f =>
        let
          val s = toLower s
          val (u, pw, h, port) = doAuthority a
          val h = case h of NONE => NONE | SOME h => SOME (encodeHost h)
          val p = case p of NONE => ""   | SOME p =>      (UriEscape.escape (normalize p))
          val q = case q of NONE => NONE | SOME q => SOME (UriEscape.escape q)
        in
          pure (URI { scheme = s, user = u, password = pw, host = h, port = port, path = p, query = q, fragment = f })
        end
      )))))



  val uri = StringCvt.scanString scanURI

  fun scheme    (URI {scheme    = scheme,    ...}) = scheme
  fun user      (URI {user      = user,      ...}) = user
  fun password  (URI {password  = password,  ...}) = password
  fun host      (URI {host      = host,      ...}) = host
  fun port      (URI {port      = port,      ...}) = port
  fun path      (URI {path      = path,      ...}) = path
  fun query     (URI {query     = query,     ...}) = query
  fun fragment  (URI {fragment  = fragment,  ...}) = fragment


  fun toString' iri (URI { scheme = scheme, user = user, password = password, host = host, port = port, path = path, query = query, fragment = fragment }) = String.concat [
      ( case scheme    of NONE => "" | SOME s => (s ^ ":") ),
      ( case host      of NONE => "" | SOME _ => ("//") ),
      ( case user      of NONE => "" | SOME u => case password of NONE => (u ^ "@") | SOME pw => (u ^ ":" ^ pw ^ "@") ),
      ( case host      of NONE => "" | SOME h => (if iri then decodeHost h else h) ),
      ( case port      of NONE => "" | SOME p => (":" ^ p) ),
      ( if iri then UriEscape.unescape path else path ),
      ( case query     of NONE => "" | SOME q => ("?" ^ (if iri then UriEscape.unescape  q else q)) ),
      ( case fragment  of NONE => "" | SOME f => ("#" ^ f) )
    ]

  fun toString u = toString' false u
  fun toIri    u = toString' true  u

  fun abs (URI (uri as { scheme = SOME _, ... })) _ = URI uri
    | abs (URI (uri as { host = SOME _, ... })) (URI base) =
      URI {
        scheme    = #scheme     base,
        user      = #user       uri,
        password  = #password   uri,
        host      = #host       uri,
        port      = #port       uri,
        path      = #path       uri,
        query     = #query      uri,
        fragment  = #fragment   uri
      }
    | abs (URI uri) (URI base) =
    let

      fun beforeMerge p =
        let
          val isAbs = String.size p > 0 andalso #"/" = String.sub (p, 0)
          val ps = String.tokens (fn c => c = #"/") p
          val l = List.length ps
          val n = if l > 0 then List.take (ps, l - 1) else []
        in
          String.concatWith "/" (if isAbs then (""::n) else n)
        end

      val (new_path, new_query) = case ((#path uri), (#path base)) of
          ("", _) => (#path base, (case #query uri of SOME q => SOME q | _ => #query base))
        | (p,  b) => (( if #"/" <> String.sub (p, 0) then normalize (String.concat [beforeMerge(b), "/", p]) else p), #query uri)

    in
      URI {
        scheme    = #scheme     base,
        user      = #user       base,
        password  = #password   base,
        host      = #host       base,
        port      = #port       base,
        path      = new_path,
        query     = new_query,
        fragment  = #fragment   uri
      }
    end

end
