(asdf:defsystem "robots-txt"
  :description "Parse robots.txt and query against it."
  :author "Louis Brauer <louis@brauer.family>"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :depends-on ("uiop")
  :components ((:file "robots-txt")))
