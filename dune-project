(lang dune 2.8)

(using coq 0.3)

(name tezla)

(generate_opam_files true)

(source
 (uri "git+https://github.com/joaosreis/tezla.git"))

(bug_reports "https://github.com/joaosreis/tezla/issues")

(homepage "https://github.com/joaosreis/tezla")

(license MIT)

(authors "Jo\195\163o Santos Reis <joaosreis@outlook.pt>")

(maintainers "Jo\195\163o Santos Reis <joaosreis@outlook.pt>")

(package
 (name tezla)
 (synopsis "An intermediate representation of the Michelson language")
 (depends
  (dune :build)
  (bisect_ppx :with-test)
  ppx_inline_test
  (alcotest :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)
  (core_unix
   (and
    :with-test
    (>= v0.15)))
  (core
   (>= v0.15))
  bignum
  michelson-adt
  michelson-parser))
