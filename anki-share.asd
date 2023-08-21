(asdf:defsystem #:anki-share
  :description "A platform to share Anki cards and decks"
  :author "Timothee Chauvin"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:access      ; unified access to dictionary-like data structures
               #:alexandria  ; various utilities
               #:cl-dbi      ; database-independent interface to databases
               #:cl-fad      ; file operations
               #:cl-pass       ; password hashing
               #:cl-ppcre      ; regular expressions
               #:easy-routes   ; route handling utility on top of hunchentoot
               #:hunchentoot   ; web server
               #:lass          ; CSS generation
               #:mito          ; ORM for saving our objects to a database
               #:osicat        ; OS interface (for creating links)
               #:py4cl         ; to interact with HTML sanitization Python lib
               #:quri          ; URL encoding and decoding
               #:slynk         ; server for sly
               #:spinneret     ; HTML5 generation
               #:split-sequence
               #:st-json
               #:str            ; string manipulation
               #:sxql           ; a DSL for constructing SQL queries
               #:zip            ; a library for ZIP files (here, for apkg files)
               )
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "util")
     (:file "parameters")
     (:file "db")
     (:file "media")
     (:file "note-vote")
     (:file "user")
     (:file "anki-model")
     (:file "anki-note")
     (:file "card")
     (:file "deck")
     (:file "deck-note")
     (:file "db-triggers")
     (:file "html")
     (:file "server")
     (:file "main")
     ))))
