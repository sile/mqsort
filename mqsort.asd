(in-package :asdf)

(defsystem mqsort
  :name "mqsort"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "Multikey Quicksort"

  :serial t
  :components ((:file "package")
               (:file "mqsort")))
  