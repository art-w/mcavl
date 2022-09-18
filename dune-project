(lang dune 3.3)

(name mcavl)

(generate_opam_files true)

(source (github art-w/mcavl))

(authors "Arthur Wendling")

(maintainers "art.wendling@gmail.com")

(documentation "https://art-w.github.io/mcavl/mcavl/Mcavl")

(license MIT)

(package
 (name mcavl)
 (synopsis "Lock-free Sets for OCaml multicore")
 (depends
   (ocaml (>= 5.0.0))
   dune
   (alcotest :with-test)
   (domainslib :with-test)
   (multicoretests :with-test)
   (multicorecheck :with-test)
   ))
