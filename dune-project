(lang dune 1.11)
(name michelscil)

(generate_opam_files true)

(source (uri "git+https://gitlab.com/releaselab/fresco/michelscil.git"))
(bug_reports "https://gitlab.com/releaselab/fresco/michelscil/issues")
(homepage "https://gitlab.com/releaselab/fresco/michelscil")
(license MIT)
(authors "João Santos Reis <joaosreis@outlook.pt>")
(maintainers "João Santos Reis <joaosreis@outlook.pt>")

(package
 (name michelscil)
 (synopsis "An instantiation of SCIL for the Michelson language")
 (depends
  (dune :build)
  (zarith (> 1.0))))
