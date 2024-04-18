let test_suite =
  let open Alcotest in
  let open X86ISTMB in
  let test_merge_paths =
    let test () =
      (check string) "An empty list yields the empty path." ""
        (Util.merge_paths []);
      (check string)
        "A singleton list of an empty string yields the empty path." ""
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
        "Path components should be trimmed of internal slashes and a single \
         slash inserted between"
        "a/b/c/d/e/f"
        (Util.merge_paths [ "a"; "/b"; "c/"; "/d/"; "e"; "f" ])
    in
    test_case "Util.merge_paths" `Quick test
  in
  ("lib/util.ml", [ test_merge_paths ])
