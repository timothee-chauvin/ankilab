;;; Parameters

(in-package :anki-share)

(defparameter *project-fdn* (namestring (asdf:system-source-directory :anki-share)))
(defparameter *db-fdn* (str:concat *project-fdn* "db/")
  "The database directory, also containing the media")
(defparameter *db-ffn* (str:concat *db-fdn* "anki.db"))
(defparameter *static-fdn* (str:concat *project-fdn* "static/")
  "This website's static assets")
(defparameter *media-dn* "media/")
(defparameter *media-fdn* (str:concat *static-fdn* *media-dn*)
  "Where the user-uploaded media files are stored")
(defparameter *decks-fdn* (str:concat *project-fdn* "decks/")
  "Where the decks to be downloaded are stored")
(defparameter *blog-posts-fdn* (str:concat *project-fdn* "blog/")
  "Where the blog posts are stored")
(defparameter *fn-length* 8
  "The length of the generated filenames (doesn't include the extension)")
(defparameter *notes-per-page* 20
  "The number of notes to send per HTML page")
(defparameter *lock* (bt:make-lock)
  "A lock for critical sections in the context of multi-threading")
(alexandria:define-constant +zero-width-space+ "&#8203;"
  :test #'string=)
