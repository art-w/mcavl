(lang dune 3.3)

(name mcset)

(generate_opam_files true)

(source (github art-w/mcset))

(authors "Arthur Wendling")

(maintainers "Arthur Wendling")

(license MIT)

(package
 (name mcset)
 (synopsis "Lock-free Sets for OCaml multicore")
 (depends
   (ocaml (>= 5.0.0))
   dune
   (domainslib :with-test)))
