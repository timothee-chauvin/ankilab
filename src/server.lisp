(in-package :anki-share)

(defvar *server* nil)

(defun start-server ()
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                :port 8002
                                :document-root *static-fdn*))
  (hunchentoot:start *server*))

(defun stop-server ()
  (hunchentoot:stop *server*))

(defun restart-server ()
  (stop-server)
  (start-server))

(defun start-all ()
  "Get the website ready to serve"
  (ensure-directories-exist *blog-posts-fdn*)
  (ensure-directories-exist *media-fdn*)
  ;; cookies only, no URL rewriting
  (setf hunchentoot:*rewrite-for-session-urls* nil)
  ;; TODO not for production
  ;; invoke the debugger instead of only logging the error
  ;; (setf hunchentoot:*catch-errors-p* nil)
  ;; avoid unfortunate Spinneret issues with newlines inside HTML
  (setf *html-style* :tree)
  (setf *print-pretty* nil)
  (setf *suppress-inserted-spaces* t)
  (db-connect)
  (ensure-tables)
  (start-server))

(defun stop-all ()
  (stop-server)
  (db-disconnect))

(defun restart-all ()
  (stop-all)
  (start-all))

(defmacro with-json (items)
  "Return a JSON string corresponding to `items'"
  ;; NOTE only works for a depth of 1 in the JSON due to st-json
  `(progn
     (setf (hunchentoot:content-type*) "application/json")
     (st-json:write-json-to-string
      (st-json:jso ,@items))))

(defmacro with-authorization (redirect-to &body body)
  "Execute `body' if the user is authorized, otherwise send a login page that
will redirect to the URL the access of which was being attempted, or
`redirect-to' if it is not the symbol 'here."
  `(if (logged-in-p)
       (progn
         ,@body)
       (r/login :err "This action is only available to signed in users."
                :code hunchentoot:+http-authorization-required+
                :redirect-to ,(if (eql redirect-to 'here)
                                  '(hunchentoot:request-uri*)
                                  redirect-to))))

(defmacro when-or-404 (expr &body body)
  `(if ,expr
       (progn ,@body)
       (r/generic-404)))

(defmacro with-server-side-check (expr &body body)
  "Execute `body' if expr evaluates to true, otherwise send a 400 Bad Request
HTTP response"
  `(if ,expr
       (progn ,@body)
       (progn
         (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
         (400-html))))

(defmacro with-post-only (&body body)
  `(when-or-404 (eql :POST (hunchentoot:request-method*))
     (progn ,@body)))

(defun check-credentials (email password)
  "verify that email is in our database with a matching password and is
confirmed"
  (let ((user (get-user-by-email email)))
    (cond ((not user) 'no-such-user)
          ((not (cl-pass:check-password password (user-hpassword user)))
           'invalid-password)
          ((not (user-confirmed user)) 'not-confirmed)
          (t 'OK))))

(defun redirect-with-args (target &key (code 303) args)
  "`args' is an alist"
  (hunchentoot:redirect (funcall #'url-with-params target args)
                        :code code
                        :add-session-id nil))

(defun redirect (target &optional &key (code 303))
  (if code
      (redirect-with-args target :code code)
      (redirect-with-args target)))

(defun blog-post-exists (slug)
  (and (probe-file (str:concat *blog-posts-fdn* slug ".html"))
       (probe-file (str:concat *blog-posts-fdn* slug ".title"))))

(defun which-cache-busted-file (request)
  "Return the name of the file referred to by the script name of this `request',
or `nil' if the script name doesn't correspond to a cache-busted file."
  (let ((script-name (hunchentoot:script-name* request)))
    (cond ((ppcre:scan "^/main-.*\.css$" script-name) "main.css")
          ((ppcre:scan "^/main-.*\.js$" script-name) "main.js"))))

(hunchentoot:define-easy-handler (r/serve-cache-busted-file
                                  :uri #'which-cache-busted-file) ()
  (hunchentoot:handle-static-file (str:concat *static-fdn*
                                              (which-cache-busted-file
                                               hunchentoot:*request*))))

(hunchentoot:define-easy-handler (r/file-upload :uri "/file-upload") ()
  (with-authorization here
    (file-upload-page-html)))

(hunchentoot:define-easy-handler (r/main-page :uri "/") ()
  (main-page-html))

(hunchentoot:define-easy-handler (r/signup :uri "/signup") (err)
  (signup-html err))

(hunchentoot:define-easy-handler (r/signup-received :uri "/signup-received"
                                                    :default-request-type :POST)
    (email username password1 password2)
  (with-post-only
    (flet ((signup-error (err) (redirect-with-args "/signup" :args `(("err" . ,err)))))
      (cond
        ((not (string= password1 password2))
         (signup-error "passwords don't match"))
        ((user-exists-by-email email)
         (signup-error "this email is already used"))
        ((user-exists-by-username username)
         (signup-error "this username is already used"))
        (t (progn
             (add-new-user email username password1)
             (r/login-received :email email
                               :password password1)
             ))))))

(hunchentoot:define-easy-handler (r/login :uri "/login")
    (err
     (code :init-form hunchentoot:+http-ok+)
     redirect-to)
  (setf (hunchentoot:return-code*) code)
  (login-html err redirect-to))

(hunchentoot:define-easy-handler (r/logout :uri "/logout"
                                           :default-request-type :POST) ()
  (when (and hunchentoot:*session* (eql :POST (hunchentoot:request-method*)))
    (hunchentoot:remove-session hunchentoot:*session*))
  (redirect "/"))

(hunchentoot:define-easy-handler (r/login-received :uri "/login-received"
                                                   :default-request-type :POST)
    (email password (redirect-to :init-form "/"))
  (ecase (check-credentials email password)
    ((no-such-user invalid-password)
     (redirect-with-args "/login"
                         :args '(("err" . "This email or password is invalid."))))
    (not-confirmed
     (redirect-with-args "/login"
                         :args '(("err" . "This user is pending confirmation.
I don't get notifications so please ping me.
An email-based flow will be set up later."))))
    (OK
      (let ((cookie (hunchentoot:regenerate-session-cookie-value
                     (hunchentoot:start-session))))
        (setf (hunchentoot:cookie-secure cookie) t)
        (setf (hunchentoot:cookie-http-only cookie) t)
        ;; the maximum inactive time for this session, in seconds
        (setf (hunchentoot:session-max-time hunchentoot:*session*) 3600)
        (setf (current-user)
              (get-user-by-email email))
        (redirect redirect-to)))))

(defun extract-from-deck (deck-fdn user
                          &key keep-tags add-tags make-deck title description)
  (let* ((anki2-ffn (str:concat deck-fdn "collection.anki2"))
         (new-name-mapping (save-media-files deck-fdn))
         (pending-notes (extract-pending-notes-from-anki2
                         anki2-ffn user :keep-tags keep-tags :add-tags add-tags)))
    (extract-models-from-anki2 anki2-ffn)
    (anki-notes-media-orig-to-used-update pending-notes new-name-mapping)
    (replace-images-in-notes pending-notes new-name-mapping)
    (save-notes pending-notes)
    (extract-cards-from-anki2 anki2-ffn)
    (when make-deck
      (deck-create title description user pending-notes))))

(hunchentoot:define-easy-handler (r/file-upload-received :uri "/file-upload-received"
                                                         :default-request-type :POST)
    ;; TODO rename
    (keep-tags make-deck (title :init-form "Title not supplied")
               (description :init-form "Description not supplied")
               (add-tags :init-form ""))
  ;; keep-tags is an extended boolean, will be NIL if the checkbox is unchecked
  ;; TODO enforce max size
  ;; TODO input validation: zip file, contents, etc
  (with-authorization "/file-upload"
    (let* ((ffn (namestring (first (hunchentoot:post-parameter "file"))))
           (unzipped-dir-fdn (str:concat ffn "-unzipped/"))
           (user (get-current-user)))
      ;; TODO way to do it without unzipping on disk?
      (zip:unzip ffn unzipped-dir-fdn)
      (extract-from-deck unzipped-dir-fdn user
                         :keep-tags keep-tags
                         :add-tags add-tags
                         :make-deck make-deck
                         :title title
                         :description description)
      (uiop:delete-directory-tree (parse-namestring unzipped-dir-fdn) :validate t)
      (redirect "/"))))

(hunchentoot:define-easy-handler (r/note-vote :uri "/note-vote"
                                              :default-request-type :POST)
    ((note-id :parameter-type 'integer)
     (vote-value :parameter-type 'integer))
  "To be used via the fetch API. Returns a JSON array of the form: {'up': (0|1),
'down': (0|1)}"
  (with-post-only
    (with-authorization "/notes"
      (with-server-side-check (and (or (= 1 vote-value) (= -1 vote-value))
                                   (anki-note-exists-by-id note-id))
        (let* ((user-id (user-id (get-current-user)))
               (note (get-anki-note-by-id note-id)))
          (note+vote-create-or-update
           :user-id user-id
           :note-id note-id
           :value vote-value)
          (with-json
              ("up" (anki-note-upvotes note) "down" (anki-note-downvotes note))))))))

(hunchentoot:define-easy-handler (r/edit-received :uri "/notes/edit-received"
                                                  :default-request-type :POST)
    ((note-id :parameter-type 'integer)
     tags)
  (with-post-only
    (with-authorization "/notes"
      (with-server-side-check (anki-note-exists-by-id note-id)
        (let ((note (get-anki-note-by-id note-id)))
          (setf (anki-note-tags note) (tags-string-to-list tags))
          (locked mito:save-dao note)
          (redirect (format nil "/notes/~a" note-id)))))))

(defun 404-return-code ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))

(hunchentoot:define-easy-handler (r/generic-404 :uri "/404") ()
  (404-return-code)
  (404-html))

(hunchentoot:define-easy-handler (r/notes :uri "/notes")
    ((tags :parameter-type '(list string))
     (page :parameter-type 'integer :init-form 1)
     (user-id :parameter-type 'integer))
  (notes-page-html tags :page page :user-id user-id))

(hunchentoot:define-easy-handler (r/blog :uri "/blog") ()
  (blog-html))

(easy-routes:defroute r/blog-post ("/blog/:slug") ()
  ;; TODO use Common Lisp restarts to avoid duplicating code...
  ;; try producing the HTML, if it fails because file doesn't exist, return 404
  (if (string= "" slug)
      (redirect "/blog")
      (when-or-404 (blog-post-exists slug)
        (blog-post-html slug))))

(easy-routes:defroute r/anki-note ("/notes/:note-id") ()
  (when-or-404 (anki-note-exists-by-id note-id)
    (anki-note-page-html note-id)))

(easy-routes:defroute r/anki-note-edit ("/notes/:note-id/edit") ()
  (when-or-404 (anki-note-exists-by-id note-id)
    (with-authorization here
      (anki-note-page-html note-id :editing t))))

(easy-routes:defroute r/anki-note-delete ("/notes/:note-id/delete") ()
  (when-or-404 (anki-note-exists-by-id note-id)
    (with-authorization here
      (anki-note-delete-page-html note-id))))

(easy-routes:defroute r/anki-note-delete-action ("/notes/:note-id/delete/action"
                                                 :method :POST)
    ()
  (with-authorization (format nil "/notes/~a/delete" note-id)
    (let ((note (get-anki-note-by-id note-id)))
      (with-server-side-check
          (and note
               (= (user-id (get-current-user))
                  (anki-note-author-id note)))
        (locked mito:delete-dao note)
        (redirect "/notes")))))

(hunchentoot:define-easy-handler (r/decks :uri "/decks") ()
  (decks-page-html))

(easy-routes:defroute r/deck ("/decks/id/:deck-id")
    ((page :parameter-type 'integer :init-form 1))
  (when-or-404 (deck-exists-by-id deck-id)
    (deck-page-html deck-id :page page)))

(hunchentoot:define-easy-handler (r/new-deck :uri "/decks/new")
    () ; TODO first note ID
  (with-authorization here
    (new-deck-page-html)))

(hunchentoot:define-easy-handler (r/new-deck-action :uri "/decks/new/action")
    ;; TODO factorize with file-upload-received
    ((title :init-form "Title not supplied")
     (description :init-form "Description not supplied"))
  (with-authorization "/decks/new"
    (with-post-only
      (let* ((user (get-current-user))
             (deck (deck-create-without-notes title description user)))
        (redirect (format nil "/decks/id/~a" (deck-id deck)))))))

(hunchentoot:define-easy-handler (r/deck-add-note :uri "/decks/add-note")
    ((deck-id :parameter-type 'integer)
     (note-id :parameter-type 'integer))
  "To be used via the fetch API. In case of success, returns a JSON array
  containing the new HTML of the note: {'html': '<the HTML>'}"
  (with-authorization "/"
    (with-post-only
      (let ((user (get-current-user))
            (deck (get-deck-by-id deck-id))
            (note (get-anki-note-by-id note-id)))
        (with-server-side-check
            (and (not (null deck))
                 (not (null note))
                 ;; TODO this check could result in a 403 forbidden
                 (= (deck-author-id deck) (user-id user)))
          (deck+note-create deck note)
          (with-json
              ("html" (anki-note-html note))))))))

(easy-routes:defroute r/deck-download ("/decks/id/:deck-id/download")
    (&path (deck-id 'integer))
  ;; TODO cache and have a flag to tell whether the deck needs to be re-created
  (with-server-side-check (deck-exists-by-id deck-id)
    (let* ((deck (get-deck-by-id deck-id))
           (notes (get-deck-notes deck))
           (used-media-fns (get-used-media-by-notes notes))
           (tmp-deck-fdn (ensure-directories-exist (format nil "~a~a/"
                                                           *decks-fdn* deck-id)))
           (collection-ffn (str:concat tmp-deck-fdn "collection.anki2"))
           (media-ffn (str:concat tmp-deck-fdn "media"))
           (apkg-fn (format nil "~a.apkg" deck-id))
           (apkg-ffn (format nil "~a/~a" *decks-fdn* apkg-fn)))
      (dbi:with-connection (deck-connection :sqlite3
                                            :database-name collection-ffn)
        (execute-sxql-queries deck-connection
          (sxql:create-table :notes
              ((id :type 'integer :primary-key t)
               (guid  :type 'text    :not-null t)
               (mid   :type 'integer :not-null t)
               (mod   :type 'integer :not-null t)
               (usn   :type 'integer :not-null t)
               (tags  :type 'text    :not-null t)
               (flds  :type 'text    :not-null t)
               (sfld  :type 'integer :not-null t)
               (csum  :type 'integer :not-null t)
               (flags :type 'integer :not-null t)
               (data  :type 'text    :not-null t)))
          (sxql:create-table :cards
              ((id :type 'integer :primary-key t)
               (nid    :type 'integer :not-null t)
               (did    :type 'integer :not-null t)
               (ord    :type 'integer :not-null t)
               (mod    :type 'integer :not-null t)
               (usn    :type 'integer :not-null t)
               (type   :type 'integer :not-null t)
               (queue  :type 'integer :not-null t)
               (due    :type 'integer :not-null t)
               (ivl    :type 'integer :not-null t)
               (factor :type 'integer :not-null t)
               (reps   :type 'integer :not-null t)
               (lapses :type 'integer :not-null t)
               (left   :type 'integer :not-null t)
               (odue   :type 'integer :not-null t)
               (odid   :type 'integer :not-null t)
               (flags  :type 'integer :not-null t)
               (data   :type 'text    :not-null t)))
          (sxql:create-table :col
              ((id :type 'integer :primary-key t)
               ;; creation time
               (crt    :type 'integer :not-null t)
               ;; modification timestamp
               (mod    :type 'integer :not-null t)
               ;; time when "schema" was last updated
               (scm    :type 'integer :not-null t)
               ;; version
               (ver    :type 'integer :not-null t)
               ;; dirty (unused, set to 0)
               (dty    :type 'integer :not-null t)
               ;; update sequence number
               (usn    :type 'integer :not-null t)
               ;; last sync time
               (ls     :type 'integer :not-null t)
               ;; some configuration
               (conf   :type 'text    :not-null t)
               ;; data of each model in this collection
               (models :type 'text    :not-null t)
               ;; info about the decks in this collection (only 1 in our case)
               (decks  :type 'text    :not-null t)
               (dconf  :type 'text    :not-null t)
               ;; a cache of all tags, but not used for exporting
               (tags   :type 'text    :not-null t)))
          ;; Seems like Anki accepts this table not to exist, but not playing with fire
          (sxql:create-table :graves
              ((usn  :type 'integer :not-null t)
               (oid  :type 'integer :not-null t)
               (type :type 'integer :not-null t)))
          (sxql:create-table :revlog
              ((id :type 'integer :primary-key t)
               (cid     :type 'integer :not-null t)
               (usn     :type 'integer :not-null t)
               (ease    :type 'integer :not-null t)
               (ivl     :type 'integer :not-null t)
               (lastIvl :type 'integer :not-null t)
               (factor  :type 'integer :not-null t)
               (time    :type 'integer :not-null t)
               (type    :type 'integer :not-null t)))
          (sxql:insert-into :col
            (sxql:set=
             :id 1
             :crt (get-universal-time)  ; we're creating the collection now
             :mod (* 1000 (get-universal-time)) ; this one's in milliseconds
             :scm 0
             :ver 11
             :dty 0
             :usn 0                     ; TODO not sure
             :ls 0
             :conf "{}"
             :models (st-json:write-json-to-string
                      (apply #'st-json:jso
                             (loop :for model :in (get-models-by-notes notes)
                                   :collect (write-to-string (anki-model-id model))
                                   :collect (anki-model-data model))))
             :decks (st-json:write-json-to-string
                     (st-json:jso
                      (write-to-string deck-id)
                      (st-json:jso
                       "id" deck-id
                       "mod" (loop :for note :in notes
                                   :maximize (anki-note-mod note))
                       "name" (deck-title deck)
                       "usn" 0          ; TODO not sure
                       "newToday" '(0 0)
                       "revToday" '(0 0)
                       "lrnToday" '(0 0)
                       "timeToday" '(0 0)
                       "conf" 1         ; belongs to the Default config group
                       "desc" (deck-description deck)
                       "dyn" 0          ; this is not a filtered deck
                       "collapsed" :false)))
             :dconf "{}"
             :tags "{}"
             )))
        (dolist (note notes)
          (execute-sxql-queries deck-connection
            (sxql:insert-into :notes
              (sxql:set=
               :id (anki-note-id note)
               :guid (anki-note-guid note)
               :mid (anki-note-model-id note)
               :mod (anki-note-mod note)
               :usn -1               ; TODO not sure
               :tags (tags-list-to-string (anki-note-tags note))
               :flds (fields-list-to-string (anki-note-orig-fields note))
               :sfld (elt (anki-note-orig-fields note)
                          (st-json:getjso "sortf"
                                          (anki-model-data
                                           (get-anki-model-by-id
                                            (anki-note-model-id note)))))
               :csum (anki-note-csum note)
               :flags 0                 ; unused by Anki
               :data ""                 ; unused by Anki
               ))))
        (dolist (card (get-cards-by-notes notes))
          (execute-sxql-queries deck-connection
            (sxql:insert-into :cards
              (sxql:set=
               :id (card-id card)
               :nid (card-note-id card)
               :did deck-id
               :ord (card-ord card)
               :mod (card-mod card)
               :usn -1
               :type 0
               :queue 0
               :due (card-note-id card)
               :ivl 0
               :factor 0
               :reps 0
               :lapses 0
               :left 0
               :odue 0
               :odid 0
               :flags 0
               :data "")))))
      ;; Include the used media
      (with-open-file (media media-ffn :direction :output :if-exists :supersede)
        (st-json:write-json
         (apply #'st-json:jso
          (loop :for (orig-media-fn . used-media-fn) :in used-media-fns
                :for used-media-ffn = (str:concat *media-fdn* used-media-fn)
                :for i :upfrom 0
                :do (osicat:make-link (format nil "~a~a" tmp-deck-fdn i)
                                      :target used-media-ffn
                                      :hard t)
                :collect (write-to-string i)
                :collect orig-media-fn))
         media))
      (zip:zip apkg-ffn tmp-deck-fdn :if-exists :supersede)
      ;; Now the directory isn't useful anymore
      (uiop:delete-directory-tree (truename tmp-deck-fdn) :validate t)
      (hunchentoot:handle-static-file
       apkg-ffn nil
       ;; callback: to execute before sending the file
       #'(lambda (pathname content-type)
           (declare (ignore pathname content-type))
           (setf (hunchentoot:header-out "Content-Disposition")
                 (format nil "attachment; filename=~a" apkg-fn)))))))

(easy-routes:defroute r/deck-delete ("/decks/id/:deck-id/delete") ()
  (when-or-404 (deck-exists-by-id deck-id)
    (with-authorization here
      (deck-delete-page-html deck-id))))

(easy-routes:defroute r/deck-delete-action ("/decks/id/:deck-id/delete/action"
                                            :method :POST)
    ()
  (with-authorization (format nil "/decks/id/~a/delete" deck-id)
    (let ((deck (get-deck-by-id deck-id)))
      (with-server-side-check
          (and deck
               (= (user-id (get-current-user))
                  (deck-author-id deck)))
        (locked mito:delete-dao deck)
        (redirect "/decks")))))

(easy-routes:defroute r/tag ("/tags/:tag") ()
  (let ((tag (quri:url-decode tag)))
    (when-or-404 (tag-exists tag)
      (tag-page-html tag))))

(easy-routes:defroute r/user ("/users/:user-id") ()
  (when-or-404 (user-exists-by-id user-id)
    (user-page-html user-id)))

(easy-routes:defroute r/user-decks ("/users/:user-id/decks") ()
  (when-or-404 (user-exists-by-id user-id)
    (user-decks-page-html user-id)))
