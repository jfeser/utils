(lang dune 1.11)
(generate_opam_files true)

(name utils)
(license MIT)
(authors "Jack Feser")

(package
 (name utils)
 (synopsis "Utility library")
 (depends
  (core (>= v0.12))
  (logs (>= 0.7))
  (gen (>= 0.5))))
