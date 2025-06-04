(lang dune 2.8)

(name tezla)

(generate_opam_files true)

(source
 (uri "git+https://github.com/joaosreis/tezla.git"))

(bug_reports "https://github.com/joaosreis/tezla/issues")

(homepage "https://github.com/joaosreis/tezla")

(license MIT)

(authors "João Santos Reis <joaosreis@outlook.pt>")

(maintainers "João Santos Reis <joaosreis@outlook.pt>")

(package
 (name tezla-converter)
 (synopsis "An intermediate representation of the Michelson language")
 (depends
  (dune :build)
  (bisect_ppx :with-test)
  ppx_inline_test
  (alcotest :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)
  (containers
   (and
    (>= 3.0)
    (< 4.0)))
  (zarith
   (and
    (>= 1.0)
    (< 2.0)))
  michelson-adt
  michelson-parser
  tezla-adt
  logs
  ppx_here))

(package
 (name tezla-adt)
 (synopsis "An intermediate representation of the Michelson language")
 (depends
  (dune :build)
  (bisect_ppx :with-test)
  ppx_inline_test
  (alcotest :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)
  (containers
   (and
    (>= 3.0)
    (< 4.0)))
  (zarith
   (and
    (>= 1.0)
    (< 2.0)))
  michelson-adt
  ppx_string))
