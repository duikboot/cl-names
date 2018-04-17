;;;; names.asd

(asdf:defsystem #:names
  :description "Handy utility to cherrypick the best name from a database of names"
  :author "Arjen Dijkstra <dijkstra.arjen@gmail.com>"
  :license "MIT"
  :depends-on (#:uiop #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "names")))

