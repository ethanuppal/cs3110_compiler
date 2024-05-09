open Alcotest
open X86ISTMB

let test_merge_paths () =
  (check string) "An empty list yields the empty path." "" (Util.merge_paths []);
  (check string) "A singleton list of an empty string yields the empty path." ""
    (Util.merge_paths [ "" ]);
  (check string) "A list of empty strings yields the empty path." ""
    (Util.merge_paths [ ""; ""; "" ]);
  (check string) "Slashes are preserved at the front." "/"
    (Util.merge_paths [ "/" ]);
  (check string) "Slashes are preserved at the front." "/a"
    (Util.merge_paths [ "/a" ]);
  (check string) "Slashes are preserved at the front." "/a/b"
    (Util.merge_paths [ "/a"; "b" ]);
  (check string)
    "Path components should be trimmed of internal slashes and a single slash \
     inserted between"
    "a/b/c/d/e/f"
    (Util.merge_paths [ "a"; "/b"; "c/"; "/d/"; "e"; "f" ])

let test_basename () =
  (check string) "Empty path is preserved" "" (Util.basename "");
  (check string) "Filenames are preserved" "foo" (Util.basename "foo");
  (check string) "Last component is extracted" "foo" (Util.basename "/foo");
  (check string) "Last component is extracted" "foo" (Util.basename "bar/foo");
  (check string) "Last component is extracted" "foo.baz"
    (Util.basename "bar/foo.baz");
  (check string) "Last component is extracted" "foo.baz"
    (Util.basename "bop/bong/birp/bar/foo.baz")

let test_suite =
  ( "lib/util.ml",
    [
      test_case "Util.merge_paths" `Quick test_merge_paths;
      test_case "Util.test_basename" `Quick test_basename;
    ] )
