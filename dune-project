(lang dune 3.0)
(name prefix_tree_dict)

(generate_opam_files true)

(package
 (name prefix_tree_dict)
 (synopsis "Prefix Tree Dictionary implementation in OCaml")
 (description "A polymorphic prefix tree (trie) based dictionary with dict interface")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (ounit2 (>= 2.2))
  (qcheck (>= 0.21))))
