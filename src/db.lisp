;;; Generic database functions and macros

(in-package :anki-share)


(defun db-connect ()
  "Connect to our database"
  (mito:connect-toplevel :sqlite3 :database-name *db-ffn*))

(defun db-disconnect ()
  (mito:disconnect-toplevel))

(defun ensure-tables ()
  "Ensure the tables exist in our database (create them if not)"
  (mapcar #'mito:ensure-table-exists (list 'anki-model
                                           'anki-note
                                           'card
                                           'user
                                           'note+vote
                                           'deck
                                           'deck+note)))

(defun fetch-db (fn query)
  "Fetch the results from executing QUERY on the sqlite3 filename FN"
  (let ((connection (dbi:connect :sqlite3
                                 :database-name fn)))
    (dbi:fetch-all
     (dbi:execute
      (dbi:prepare connection query)))))

(defmacro execute-sxql-queries (conn &body sxql-queries)
  `(progn
     ,@(mapcar #'(lambda (query)
                   `(apply #'dbi:do-sql ,conn
                           (multiple-value-list
                            (sxql:yield
                             ,query))))
               sxql-queries)))

(defmacro locked (f-name &rest args)
  "Run function `f-name' with `args' holding the global lock, `*lock*'"
  `(bt:with-lock-held (*lock*)
     (,f-name ,@args)))

(defmacro locked-sexpr (&body body)
  "Run `body' holding the global lock, `*lock*'"
  `(bt:with-lock-held (*lock*)
     ,@body))

(defmacro common-db-functions (class-symbol &key get-single-by get-multiple-by)
  "Common functions on classes as database tables.
Example usage: (common-db-functions anki-note
                                    :get-single-by (id)
                                    :get-multiple-by (author-id))
Will create:
- get-anki-notes ()
- get-anki-note-by-id (id)
- anki-note-exists-by-id (id)
- get-anki-notes-by-author-id (author-id)"
  `(progn
     (defun ,(build-symbol ("GET-" class-symbol "S")) ()
       ;;Get all the elements of the class
       (locked mito:retrieve-dao ',class-symbol))
     ,@(mapcar #'(lambda (field)
                   `(progn
                      (defun ,(build-symbol ("GET-" class-symbol "-BY-" field)) (,field)
                        ;; Get the first instance of the class matching field
                        (locked mito:find-dao ',class-symbol ,(build-keyword-symbol (field)) ,field))
                      (defun ,(build-symbol (class-symbol "-EXISTS-BY-" field)) (,field)
                        ;; Return an instance if there is one matching field
                        (,(build-symbol ("GET-" class-symbol "-BY-" field)) ,field))))
               get-single-by)
     ,@(mapcar #'(lambda (field)
                   `(defun ,(build-symbol ("GET-" class-symbol "S-BY-" field)) (,field)
                      ;; Get all elements of the class matching field
                      (locked mito:retrieve-dao ',class-symbol ,(build-keyword-symbol (field)) ,field)))
               get-multiple-by)))
