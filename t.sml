fun showUri uri = String.concat [
    "scheme    : ", ( case Uri.scheme    uri of NONE => "" | SOME s => s ), "\n",
    "user      : ", ( case Uri.user      uri of NONE => "" | SOME u => u ), "\n",
    "password  : ", ( case Uri.password  uri of NONE => "" | SOME p => p ), "\n",
    "host      : ", ( case Uri.host      uri of NONE => "" | SOME h => h ), "\n",
    "port      : ", ( case Uri.port      uri of NONE => "" | SOME p => p ), "\n",
    "path      : ", (      Uri.path      uri                             ), "\n",
    "query     : ", ( case Uri.query     uri of NONE => "" | SOME q => q ), "\n",
    "fragment  : ", ( case Uri.fragment  uri of NONE => "" | SOME f => f ), "\n"
  ]


fun showResult u = case u of SOME uri => print ("SOME " ^ (Uri.toString uri) ^ "\n") | NONE => print ("NONE\n")


fun showUriForTest uri =
  let
    val u = Uri.uri uri
  in
    print ("     " ^ uri ^ "\n");
    showResult u;
    case u of SOME uri => print("\n" ^ showUri uri) | NONE => print "\n";
    print "\n"
  end



(*
val _ = showUriForTest "http://www.foo.com:80/baz/foo.html?a=b&c=d"
val _ = showUriForTest "http://u:p@www.foo.com/baz/bar.html?a=b#too"
val _ = showUriForTest "urn:example:animal:ferret:nose"
*)


val testNumber = ref 0
fun incTestNumber () = (testNumber := 1 + !testNumber; Int.toString (!testNumber))
fun endTests () = print ("1.." ^ (Int.toString (!testNumber)) ^ "\n")


fun isSomeAndCont x name f =
  let
    val n = incTestNumber ()
  in
    case x of
        NONE => print ("not ok " ^ n ^ " - " ^ name ^ "\n")
      | SOME x => (print ("ok " ^ n ^ " - " ^ name ^ "\n"); f x)
  end


fun is (got, expected, name) =
  let
    val n = incTestNumber ()
  in
    if got = expected
    then print ("ok " ^ n ^ " - " ^ name ^ "\n")
    else print ("not ok " ^ n ^ " - " ^ name ^ "\n")
  end



val u = Uri.uri "http://www.Foo.com:80/baz/foo.html?a=b&c=d"
val _ = isSomeAndCont u "new" (fn u => (
  is (Uri.scheme    u, SOME "http",           "scheme");
  is (Uri.user      u, NONE,                  "user");
  is (Uri.password  u, NONE,                  "password");
  is (Uri.host      u, SOME "www.foo.com",    "host");
  is (Uri.port      u, SOME "80",             "port");
  is (Uri.path      u, "/baz/foo.html",       "path");
  is (Uri.query     u, SOME "a=b&c=d",        "query");
  is (Uri.fragment  u, NONE,                  "fragment");
  is (Uri.toString  u, "http://www.foo.com:80/baz/foo.html?a=b&c=d", "toString");
() ) )


val u = Uri.uri "http:////u:p@www.foo.com/baz/bar.html?a=b#too"
val _ = isSomeAndCont u "new" (fn u => (
  is (Uri.scheme    u, SOME "http",           "scheme");
  is (Uri.user      u, SOME "u",              "user");
  is (Uri.password  u, SOME "p",              "password");
  is (Uri.host      u, SOME "www.foo.com",    "host");
  is (Uri.port      u, NONE,                  "port");
  is (Uri.path      u, "/baz/bar.html",       "path");
  is (Uri.query     u, SOME "a=b",            "query");
  is (Uri.fragment  u, SOME "too",            "fragment");
  is (Uri.toString  u, "http://u:p@www.foo.com/baz/bar.html?a=b#too", "toString");
() ) )


val u = Uri.uri "urn:a:b:c:d"
val _ = isSomeAndCont u "new" (fn u => (
  is (Uri.scheme    u, SOME "urn",     "scheme");
  is (Uri.user      u, NONE,           "user");
  is (Uri.password  u, NONE,           "password");
  is (Uri.host      u, NONE,           "host");
  is (Uri.port      u, NONE,           "port");
  is (Uri.path      u, "a:b:c:d",      "path");
  is (Uri.query     u, NONE,           "query");
  is (Uri.fragment  u, NONE,           "fragment");
  is (Uri.toString  u, "urn:a:b:c:d", "toString");
() ) )


val _ =
let
  val base = "http://a/b/c/d;p?q"
  val t = [
    ("http://www.baz.com/baz.html", "http://www.foo.com/baz/bar.html", "http://www.baz.com/baz.html"),
    ("foo.html?a=b&c=d",            "http://www.foo.com/baz/bar.html", "http://www.foo.com/baz/foo.html?a=b&c=d"),
    ("foo.html?a=b&c=d",            "http://www.foo.com/",             "http://www.foo.com/foo.html?a=b&c=d"),
    ("foo.html?a=b&c=d",            "http://www.foo.com",              "http://www.foo.com/foo.html?a=b&c=d"),
    ("//g",                         base, "http://g"),
    ("//g",                         base, "http://g"),
    ("g:h",                         base, "g:h"),
    ("g",                           base, "http://a/b/c/g"),
    ("./g",                         base, "http://a/b/c/g"),
    ("g/",                          base, "http://a/b/c/g/"),
    ("/g",                          base, "http://a/g"),
    ("//g",                         base, "http://g"),
    ("?y",                          base, "http://a/b/c/d;p?y"),
    ("g?y",                         base, "http://a/b/c/g?y"),
    ("#s",                          base, "http://a/b/c/d;p?q#s"),
    ("g#s",                         base, "http://a/b/c/g#s"),
    ("g?y#s",                       base, "http://a/b/c/g?y#s"),
    (";x",                          base, "http://a/b/c/;x"),
    ("g;x",                         base, "http://a/b/c/g;x"),
    ("g;x?y#s",                     base, "http://a/b/c/g;x?y#s"),
    ("",                            base, "http://a/b/c/d;p?q"),
    (".",                           base, "http://a/b/c/"),
    ("./",                          base, "http://a/b/c/"),
    ("..",                          base, "http://a/b/"),
    ("../",                         base, "http://a/b/"),
    ("../g",                        base, "http://a/b/g"),
    ("../..",                       base, "http://a/"),
    ("../../",                      base, "http://a/"),
    ("../../g",                     base, "http://a/g")
  ]

  fun loop [] i = ()
    | loop ((url, base, expected)::xs) i =
      let
        val u = Option.valOf (Uri.uri url)
        val b = Option.valOf (Uri.uri base)
        val full = Uri.abs u b
        val name = "abs " ^ (Int.toString i)
      in
        is (Uri.toString full, expected, name);
        loop xs (i + 1)
      end
in
  print "\nTests for abs:\n";
  loop t 1
end


val _ =
let
  val t = [
	("/", "/"),
	(".", "."),
	("//", ""), (* Тут - host, а не path *)
	("//a", ""), (* Тут a - host, а не path *)
	("/a/", "/a/"),
    ("/a/b/.", "/a/b/"),
	("/a//b/./c/d", "/a/b/c/d"),
	("a//b/./c/d", "a/b/c/d"),
	("/a/b/../d", "/a/d"),
	("/a/../c/d", "/c/d"),
	("/a/b/../../c", "/c"),
	("/a/../../b", "/b"),
	("a/../../b", "../b"),
	("a/../../../b", "../../b"),
    ("/b/c/..", "/b/"),
    ("a1/a2/a3/a4/a5/../../b", "a1/a2/a3/b"),
    ("a1/a2/a3/a4/a5/../../../b", "a1/a2/b"),
    ("a1/a2/a3/a4/a5/../../../../b", "a1/b"),
    ("a1/a2/a3/a4/a5/../../../../../b", "b"),
    ("a1/a2/a3/a4/a5/../../../../../../b", "../b")
  ]

  fun loop [] i = ()
    | loop ((url, expected)::xs) i =
      let
        val u = Option.valOf (Uri.uri url)
        val name = "normalize " ^ (Int.toString i)
      in
        is (Uri.path u, expected, name);
        loop xs (i + 1)
      end
in
  print "\nTests for normalization:\n";
  loop t 1
end


val _ =
let
  (* "http://www.foo.com/Головна/сторінка?a=Вітаю&b=До побачення" *)
  val s = "http://www.foo.com/\208\147\208\190\208\187\208\190\208\178\208\189\208\176/\209\129\209\130\208\190\209\128\209\150\208\189\208\186\208\176?a=\208\146\209\150\209\130\208\176\209\142&b=\208\148\208\190 \208\191\208\190\208\177\208\176\209\135\208\181\208\189\208\189\209\143"

  val e = "http://www.foo.com/%D0%93%D0%BE%D0%BB%D0%BE%D0%B2%D0%BD%D0%B0/%D1%81%D1%82%D0%BE%D1%80%D1%96%D0%BD%D0%BA%D0%B0?a=%D0%92%D1%96%D1%82%D0%B0%D1%8E&b=%D0%94%D0%BE%20%D0%BF%D0%BE%D0%B1%D0%B0%D1%87%D0%B5%D0%BD%D0%BD%D1%8F"
  val u = Option.valOf (Uri.uri s)
in
  is (Uri.toString u, e, "escape");
  is (Uri.toIri    u, s, "unescape")
end


val _ =
let
  val s = "https://localhost/%d0%92%d1%96%d1%82%d0%b0%d1%8e2/"
  val e = "https://localhost/%D0%92%D1%96%D1%82%D0%B0%D1%8E2/"
  val u = Option.valOf (Uri.uri s)
in
  is (Uri.toString u, e, "reescape")
end


val _ =
let
  (* perl -E 'say join "", map { $_ < 127 ? chr($_) : sprintf "\\%03u", $_ } unpack "C*", "Вітаю, hello. ☺"' *)

  (* The host must be lowercased! lowercase function missed for unicode now. *)

  (* "http://Інтернаціоналізовані.Доменні.Імена/Головна/сторінка?a=Вітаю&b=До побачення" *)
  val s = "http://\208\134\208\189\209\130\208\181\209\128\208\189\208\176\209\134\209\150\208\190\208\189\208\176\208\187\209\150\208\183\208\190\208\178\208\176\208\189\209\150.\208\148\208\190\208\188\208\181\208\189\208\189\209\150.\208\134\208\188\208\181\208\189\208\176/\208\147\208\190\208\187\208\190\208\178\208\189\208\176/\209\129\209\130\208\190\209\128\209\150\208\189\208\186\208\176?a=\208\146\209\150\209\130\208\176\209\142&b=\208\148\208\190 \208\191\208\190\208\177\208\176\209\135\208\181\208\189\208\189\209\143"

  (* "http://інтернаціоналізовані.доменні.імена/Головна/сторінка?a=Вітаю&b=До побачення" *)
  val l = "http://\209\150\208\189\209\130\208\181\209\128\208\189\208\176\209\134\209\150\208\190\208\189\208\176\208\187\209\150\208\183\208\190\208\178\208\176\208\189\209\150.\208\180\208\190\208\188\208\181\208\189\208\189\209\150.\209\150\208\188\208\181\208\189\208\176/\208\147\208\190\208\187\208\190\208\178\208\189\208\176/\209\129\209\130\208\190\209\128\209\150\208\189\208\186\208\176?a=\208\146\209\150\209\130\208\176\209\142&b=\208\148\208\190 \208\191\208\190\208\177\208\176\209\135\208\181\208\189\208\189\209\143"

  val e = "http://xn--80aaahmo1ambbffeu2a2e1ohef.xn--d1acufac6o.xn--80ajuf6j/%D0%93%D0%BE%D0%BB%D0%BE%D0%B2%D0%BD%D0%B0/%D1%81%D1%82%D0%BE%D1%80%D1%96%D0%BD%D0%BA%D0%B0?a=%D0%92%D1%96%D1%82%D0%B0%D1%8E&b=%D0%94%D0%BE%20%D0%BF%D0%BE%D0%B1%D0%B0%D1%87%D0%B5%D0%BD%D0%BD%D1%8F"
  val u = Option.valOf (Uri.uri s)

in
  is (Uri.toString u, e, "IDN");
  is (Uri.toIri u, l, "IRI")
end


val _ =
let
  (* "› NASA Advisory Council Commercial Space Committee" *)
  val t = "\226\128\186 NASA Advisory Council Commercial Space Committee"
  val e = " NASA Advisory Council Commercial Space Committee-4r54a"
in
  is ((UriPunycode.encode o Utf8.decode) t, e, "Punycode")
end


val _ = endTests ()
