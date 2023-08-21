;;; HTML snippets

(in-package :anki-share)

(defun url-with-params (target &optional args-alist)
  "`target' should not be an empty string, should not already have URL args,
and should not need to be URL encoded."
  (when (string= "" target) (error "target should not be the empty string"))
  (if (null args-alist)
      target
      (format nil "~a?~a" target (quri:url-encode-params
                                  args-alist))))

(defun url-with-params-replace (target parameters-alist replace-key new-value)
  "Build an URL with `target' and `parameters-alist' as query parameters,
but after replacing `key'=something with `key'=`new-value'"
  (funcall #'url-with-params target
         (loop :with found = nil
               :for (key . value) :in parameters-alist
               :if (string= key replace-key)
                 :do (setf found t)
                 :and :collect (cons key new-value) :into args
               :else
                 :collect (cons key value) :into args
               :finally (if (not found)
                            (return (push (cons replace-key new-value) args))
                            (return args)))))

(defun url-with-tag-params (target tags)
  "Return a query string of the form `target'?tags=...&tags=...
with all tags in `tags'"
  (funcall #'url-with-params target
         (mapcar #'(lambda (tag) `("tags" . ,tag)) tags)))

(defun logged-in-as-p (user)
  "Is the user logged in as `user'?"
  (and (logged-in-p)
       (= (user-id (hunchentoot:session-value :user))
          (user-id user))))

(defun get-current-user ()
  "Return the current user, or nil if not logged in."
  (let ((user (hunchentoot:session-value :user)))
    ;; This test to consider freshly deleted users as logged out
    (when (and user
               (user-exists-by-id (user-id user)))
      user)))

(defun logged-in-p ()
  "Is the user logged in?"
  (get-current-user))

(defun (setf current-user) (new-user)
  (setf (hunchentoot:session-value :user) new-user))

(defun unselect-tag-link (tag current-tags)
  "Return the link destination for selected tag TAG given that CURRENT-TAGS
are all the selected tags. If this tag is clicked, it is selected out."
  (url-with-tag-params (hunchentoot:script-name*) (str-set-difference current-tags (list tag))))

(defun select-tag-link (tag current-tags)
  "A link to add TAG to CURRENT-TAGS"
  (url-with-tag-params (hunchentoot:script-name*) (str-union current-tags (list tag))))

(defmacro with-page-string ((&key title) &body body)
  "return the html string for a standard webpage with `title' and `body'"
  `(progn
     (setf (hunchentoot:header-out "content-security-policy")
           "default-src 'none'; script-src 'self'; connect-src 'self'; font-src 'self' fonts.gstatic.com;\
img-src 'self'; style-src 'self' fonts.googleapis.com; base-uri 'self'; form-action 'self';")
     (setf (hunchentoot:header-out "Strict-Transport-Security")
           "max-age=63072000; includeSubDomains")
     (with-html-string
       (:doctype)
       (:html
        (:head
         (:title (format nil "~a - AnkiLab" ,title))
         (:meta :name "viewport" :content "width=device-width, initial-scale=1")
         (:meta :name "description" :content "AnkiLab is an unofficial platform for sharing Anki cards and decks, exploring different features than Ankiweb shared decks.")
         (:link :rel "stylesheet" :type "text/css"
                :href (format nil "/main-~a.css"
                              (get-file-hash (str:concat *static-fdn* "main.css"))))
         ;; load the Tinos font from Google web fonts
         (:link :rel "stylesheet"
                :href "https://fonts.googleapis.com/css?family=Tinos:400|Tinos:400&display=swap")
         (:script :src (format nil "/main-~a.js"
                              (get-file-hash (str:concat *static-fdn* "main.js")))
                  :async))
        (:body
         (:raw (header-html))
         (:div#body
          ,@body))))))

(defun tag-html (tag &key deletable href)
  (with-html-string
    (:a.tag.nolinkstyle :href href
            (if deletable
                (:span tag (:img :src "/img/close_icon.svg"))
                tag))))

(defun user-link-html (user)
  (with-html-string
    (:a.nolinkstyle :href (format nil "/users/~a" (user-id user))
                    (user-username user))))

(defun header-html ()
  (let ((user (get-current-user)))
    (with-html-string
      (:header
       (:div#header-title (:a.nolinkstyle :href "/" "AnkiLab"))
       (:div.navlink
        (:a.nolinkstyle :href "/notes" "Cards"))
       (:div.navlink
        (:a.nolinkstyle :href "/decks" "Decks"))
       (:div.navlink
        (:a.nolinkstyle :href "/file-upload" "Submit"))
       (:div.navlink
        (:a.nolinkstyle :href "/blog" "Blog"))
       (:div.navlink
        (:a.nolinkstyle :href "/blog/about" "About"))
       (:div#header-user
        (if (logged-in-p)
            (progn
              (:div (:a.nolinkstyle.small :href (format nil "/users/~a" (user-id user))
                                          (format nil "~a~:[ (confirmation pending)~;~]"
                                                  (user-username user) (user-confirmed user))))
              (:form :action "/logout" :method "post"
                     (:button#logout :type "submit" "Log out")))
            (progn
              (:a.btn :href "/login" "Sign in"))))))))

(defun tag-selection-html (current-tags)
  "Return the HTML code for the tag selection object, given that CURRENT-TAGS
are already selected"
  (let ((possible-tags (possible-tags current-tags))
        (*html-style* :tree)
        (*print-pretty* nil))
    (with-html-string
      (:div#tag-selection
       (:div#tags-selected
        (loop :for tag :in current-tags
              :collect (:raw (tag-html tag :href (unselect-tag-link tag current-tags)
                                           :deletable t)))
        (when possible-tags
          (:span.tag#tag-filter-btn (if current-tags
                                        "add more tags..."
                                        "filter by tags...")
                                    (:img :src "/img/plus.svg"))))
       (:div#tag-filter-list
        (loop
          :for tag in (str-sort possible-tags)
          :collect (:raw (tag-html tag :href (select-tag-link tag current-tags)))))))))

(defun anki-field-html (field-name used-field)
  "Render a field preceded by its name"
  (with-html-string
    (:div.anki-field (:div.anki-field-name field-name)
                     (:div.anki-field-field
                      (if (string= "" used-field)
                          (:raw +zero-width-space+) ; to avoid shrinking the div
                          (:raw used-field))))))

(defun anki-note-tag-list-html (note &key href-fn)
  (let ((tags (anki-note-tags note)))
    (with-html-string
      (:div.tag-list
       (loop
         :for tag in (str-sort tags)
         :collect (:raw (tag-html tag :href (funcall href-fn tag))))))))

(defun anki-note-editing-tag-list-html (note)
  (let ((tags (anki-note-tags note)))
    (with-html-string
      (:div.editing-tag-list
       (:span "tags: ")
       (:input :type "text" :name "tags"
               :value (tags-list-to-string (str-sort tags)))))))

(defun deck-in-note-html (deck)
  (with-html-string
    (:a.deck-in-note.nolinkstyle :href (format nil "/decks/id/~a" (deck-id deck))
                                 (deck-title deck))))

(defun add-deck-list (note)
  (let* ((note-id (anki-note-id note))
         (current-decks (get-decks-by-note note))
         (current-decks-ids (mapcar #'deck-id current-decks))
         (user (get-current-user))
         (user-decks (get-decks-by-author-id (user-id user)))
         (user-decks-ids (mapcar #'deck-id user-decks))
         (potential-decks-ids (set-difference user-decks-ids current-decks-ids)))
    (with-html-string
      (:div.deck-add-list
       :data-note-id note-id
       (loop :for deck-id :in potential-decks-ids
             :collect (:button.deck-in-note.rectangle.light.add-deck-btn
                       :data-deck-id deck-id :data-note-id note-id
                       (deck-title (get-deck-by-id deck-id))))
       ;; TODO pass note ID
       (:a.deck-in-note.toggle-add-deck-list-btn :href "/decks/new"
                                                 "create new deck..."
                                                 (:img :src "/img/plus.svg"))))))

(defun anki-note-deck-list-html (note)
  (let ((decks (get-decks-by-note note)))
    (with-html-string
      (:div.deck-list
       (loop
         :for deck :in decks
         :collect (:raw (deck-in-note-html deck)))
       (when (logged-in-p)
         (:span.deck-in-note.toggle-add-deck-list-btn :data-note-id (anki-note-id note)
                                          "add to deck..."
                                          (:img :src "/img/plus.svg"))
         (:raw (add-deck-list note)))))))

(defun anki-note-fields-html (note)
  (let* ((model (get-anki-model-by-id (anki-note-model-id note)))
         (model-fields-data (st-json:getjso "flds" (anki-model-data model)))
         (field-names (mapcar #'(lambda (field) (st-json:getjso "name" field)) model-fields-data)))
    (with-html-string
      (:div.anki-note-fields
       (loop :for field-name :in field-names
             :for used-field :in (anki-note-used-fields note)
             :collect (:raw (anki-field-html field-name used-field)))))))

(defun anki-note-footer-html (note)
  "Only rendered when not editing"
  (with-accessors ((id anki-note-id)
                   (author-id anki-note-author-id)) note
    (with-html-string
      (:div.anki-note-footer
       (:button.vote-btn.upvote-btn :data-note-id id
                                    (:img.arrow.upvote :src "/img/arrow.svg")
                                    (:div.vote-value
                                     (anki-note-upvotes note)))
       (:button.vote-btn.downvote-btn :data-note-id id
                                      (:img.arrow.downvote :src "/img/arrow.svg")
                                      (:div.vote-value
                                       (anki-note-downvotes note)))
       (:span "by "
              (:raw (user-link-html (get-user-by-id author-id)))
              " "
              (:a.nolinkstyle :href (format nil "/notes/~a" id) "details")
              " "
              (:a.nolinkstyle :href (format nil "/notes/~a/edit" id) "edit"))))))

(defun anki-note-editing-buttons (note)
  "The Cancel and Save buttons when editing a note"
  (let ((user (get-current-user)))
    (with-html-string
      (when (and user
                 (= (user-id user) (anki-note-author-id note)))
        (:a.btn.danger :href (format nil "/notes/~a/delete" (anki-note-id note))
                       "Delete"))
      (:a.btn :href (format nil "/notes/~a" (anki-note-id note)) "Cancel")
      (:button :type "submit" "Save"))))

(defun anki-note-html (note &key editing)
  (with-html-string
    (:div.anki-note
     (:raw (anki-note-fields-html note))
     (when editing
       (:form :action "/notes/edit-received" :method "post"
              (:input :name "note-id" :type "hidden" :value (anki-note-id note))
              (:div#anki-note-editing-footer
               (:raw (anki-note-editing-tag-list-html note))
               (:raw (anki-note-editing-buttons note)))))
     (when (not editing)
       (:raw (anki-note-tag-list-html note
                                      :href-fn #'(lambda (tag) (format nil "/tags/~a" (quri:url-encode tag)))))
       (:raw (anki-note-deck-list-html note))
       (:raw (anki-note-footer-html note))))))

(defun paging-nav-html (page max-page)
  "Return links to first, previous, next and last page, with some potentially omitted
depending on the value of `page' relative to 1 and `max-page'"
  (with-html-string
    (:div.paging-nav
     (when (> page 1)
       ;; TODO macro
       (:a.paging-nav :href (url-with-params-replace
                             (hunchentoot:script-name*)
                             (hunchentoot:get-parameters*)
                             "page" 1)
                      (:img.arrow.arrow-left :src "/img/arrow_left_double.svg"))
       (:a.paging-nav :href (url-with-params-replace
                             (hunchentoot:script-name*)
                             (hunchentoot:get-parameters*)
                             "page" (1- page))
                      (:img.arrow.arrow-left :src "/img/arrow_left.svg")))
     (when (< page max-page)
       (:a.paging-nav :href (url-with-params-replace
                             (hunchentoot:script-name*)
                             (hunchentoot:get-parameters*)
                             "page" (1+ page))
                      (:img.arrow.arrow-right :src "/img/arrow_left.svg"))
       (:a.paging-nav :href (url-with-params-replace
                             (hunchentoot:script-name*)
                             (hunchentoot:get-parameters*)
                             "page" max-page)
                      (:img.arrow.arrow-right :src "/img/arrow_left_double.svg"))))))

(defun notes-paging-info-html (start end length page max-page)
  "A banner with info about the page we are in, and navigation links to other pages"
  (with-html-string
    (:div.paging-info
     ;; for displaying, we count notes starting from 1
     (:div.paging-info-text
      (format nil "Showing cards ~a-~a of ~a" (1+ start) end length))
     (:raw (paging-nav-html page max-page)))))

(defun anki-notes-html (notes &key (page 1))
  (let* ((length (length notes))
         (start (* (- page 1) *notes-per-page*))
         (end (min (* page *notes-per-page*) (length notes)))
         (max-page (ceiling (length notes) *notes-per-page*)))
    (if (not (<= 0 start (- length 1)))
        ""
        (with-html-string
          (:raw (notes-paging-info-html start end length page max-page))
          (dolist (note (subseq notes start end))
            (:raw (anki-note-html note)))
          (:raw (notes-paging-info-html start end length page max-page))))))

(defun notes-page-html (tags &key page user-id)
  (let* ((notes-matching (get-filtered-notes :tags tags :user-id user-id)))
    (with-page-string (:title "Anki cards")
      (:p "Filter by tags:"
          (:raw (tag-selection-html tags)))
      (:h2 (format nil "~a card~:p ~a" (length notes-matching)
                   (if (or tags user-id) "matching" "in collection")))
      (:raw (anki-notes-html notes-matching :page page)))))

(defun deck-html (deck &key page)
  (with-html-string
    (:h1 (deck-title deck))
    (:p (deck-description deck))
    (:raw (anki-notes-html (get-deck-notes deck)
                           :page page))))

(defun deck-page-html (deck-id &key page)
  (let ((deck (get-deck-by-id deck-id))
        (user (get-current-user)))
    (with-page-string (:title (deck-title deck))
      (when (and user
                 (= (user-id user) (deck-author-id deck)))
        (:a.btn.danger :href (format nil "/decks/id/~a/delete" deck-id)
                       "Delete"))
      (:a.btn :href (format nil "/decks/id/~a/download" deck-id) "Download")
      (:raw (deck-html deck :page page)))))

(defun decks-list-html (decks &key (show-author t))
  (with-html-string
    (dolist (deck decks)
      (:p (:a :href (format nil "/decks/id/~a" (deck-id deck))
              (deck-title deck))
          (when show-author
            (:raw (str:concat
                   " by "
                   (user-link-html (get-user-by-id (deck-author-id deck))))))))))

(defun decks-page-html ()
  (with-page-string (:title "Decks")
    (:raw (decks-list-html (get-decks)))))

(defun user-decks-page-html (user-id)
  (let* ((user (get-user-by-id user-id))
         (username (user-username user))
         (decks-matching (get-decks-by-author-id user-id)))
    (with-page-string (:title (format nil "Decks from ~a" username))
      (:h1 (format nil "All decks from ~a" username))
      (:raw (decks-list-html decks-matching :show-author nil)))))

(defun main-page-html ()
  (with-page-string (:title "Welcome")
    (:p.main-page-intro "AnkiLab is an unofficial platform for sharing Anki cards. It differs most notably from Ankiweb shared decks by displaying all Anki cards directly in the browser, allowing to browse cards without having to download an entire deck into one's collection. "
        (:a :href "/notes" "Explore cards")
        " to get a demonstration. You can contribute by "
        (:a :href "/file-upload" "submitting some of your Anki cards")
        ". Learn more on the "
        (:a :href "/blog/about" "About page")
        " or read a more comprehensive "
        (:a :href "/blog/comparison-to-ankiweb" "comparison to Ankiweb shared decks")
        "."
    )))

(defun file-upload-page-html ()
  (with-page-string (:title "Submit Anki cards")
    (:h1 "Submit Anki cards as a .apkg file:")
    (:p.main-page-intro (:b "N.B. ") "To create a .apkg file, you can either export an entire
    deck by clicking on "
        (:i "Export")
        " from a deck's options, or you can export any subset of your cards
    by selecting some cards in Anki's card browser, right-clicking and
    clicking on "
        (:i "Export Notes")
        ".")
    (:form :action "/file-upload-received"
           :enctype "multipart/form-data"
           :method "post"
           (:div
            (:input :name "file" :type "file"))
           (:div
            (:input :type "checkbox" :name "keep-tags" :id "keep-tags")
            (:label :for "keep-tags" "Keep tags"))
           (:div
            ;; TODO better than a space for spacing
            (:label :for "add-tags" "Add tags to every note ")
            (:input :type "text" :name "add-tags" :id "add-tags"
                    :placeholder "tag1 tag2..."))
           (:div
            (:input :type "checkbox" :name "make-deck" :id "make-deck")
            (:label :for "make-deck" "Make it a deck here")
            (:div#make-deck-details
             (:div
              (:label :for "title" "Title: *")
              (:input :type "text" :name "title" :id "title"
                      :placeholder "Title"))
             (:div
              (:label :for "description" "Description: *")
              (:textarea :name "description" :id "description"
                         :placeholder "Description" :rows 5 :cols 40))))

           (:div
            (:input :type "submit" :value "Submit")))))

(defun signup-html (err)
  (with-page-string (:title "Sign up")
    ;; TODO cleaner way to handle this
    (when err (:p err))
    (:div.signup-form
     (:form.signup-form :action "/signup-received" :method "post"
                        (:input :type "text" :name "email" :placeholder "email address")
                        (:input :type "text" :name "username" :placeholder "username")
                        (:input :type "password" :name "password1" :placeholder "password")
                        (:input :type "password" :name "password2" :placeholder "repeat password")
                        (:button "Sign Up")))
    (:p "N.B. As an early user, you won't be asked to verify your email (you'll
    be signed in straight away) as I haven't set up email validation yet. Your
    account will be validated later, though.")
    (:p "Already have an account? "
        (:a :href "/login" "Sign in"))))

(defun login-html (err redirect-to)
  (with-page-string (:title "Log in")
    ;; TODO prevent brute force attacks
    (when err (:p err))
    (:div.login-form
     (:form.login-form :action "/login-received" :method "post"
                       (:input :type "text" :name "email" :placeholder "email address")
                       (:input :type "password" :name "password" :placeholder "password")
                       (when redirect-to
                         (:input :name "redirect-to" :type "hidden" :value redirect-to))
                       (:button "Log in")))
    (:p "Don't have an account? "
        (:a :href "/signup" "Sign up"))))

(defun 404-html ()
  (with-page-string (:title "Page not found")
    (:p "Page not found!")))

(defun 400-html ()
  (with-page-string (:title "Invalid request")
    (:p "The submitted request was invalid.")))

(defun anki-note-page-html (note-id &key editing)
  (with-page-string (:title (format nil "Note ~a" note-id))
    (:raw (anki-note-html (get-anki-note-by-id note-id) :editing editing))))

(defun tag-page-html (tag)
  (with-page-string (:title (format nil "'~a' tag" tag))
    (:h1 (format nil "All notes matching '~a':" tag))
    (:raw (anki-notes-html (get-anki-notes-by-tag tag)))))

(defun user-page-html (user-id)
  (let* ((user (get-user-by-id user-id))
         (username (user-username user)))
    (with-page-string (:title username)
      (:h1 username)
      (:p (:a :href (url-with-params "/notes" `(("user-id" . ,user-id)))
              (format nil "~a note~:p"
                      (length (get-anki-notes-by-author-id user-id)))))
      (:p (:a :href (format nil "/users/~a/decks" user-id)
              (format nil "~a deck~:p"
                      (length (get-decks-by-author-id user-id)))))
      (when (logged-in-as-p user)
        (:p (:a :href "/decks/new" "Create new deck"))))))

(defun new-deck-page-html ()
  (with-page-string (:title "New deck")
    (:h1 "Create a new deck")
    (:form :action "/decks/new/action" :method "post"
           (:div
            (:label :for "title" "Title: *")
            (:input :type "text" :name "title"
                    :placeholder "Title" :required t))
           (:div
            (:label :for "description" "Description: *")
            (:textarea :name "description" :rows 5 :cols 40
                       :placeholder "Description" :required t))
           (:div
            (:input :type "submit" :value "Submit")))))

(defun anki-note-delete-page-html (note-id)
  (with-page-string (:title (format nil "Delete note ~a" note-id))
    (:p "Are you sure you want to delete this note?")
    (:form.inline :action (str:concat (hunchentoot:script-name*) "/action")
                  :method "post"
           (:input :type "submit" :value "Yes"))
    (:a.btn :href (format nil "/notes/~a" note-id) "No")
    (:raw (anki-note-html (get-anki-note-by-id note-id)))))

(defun deck-delete-page-html (deck-id)
  (with-page-string (:title (format nil "Delete deck ~a" deck-id))
    (:p "Are you sure you want to delete this deck? Its notes won't be affected.")
    (:form.inline :action (str:concat (hunchentoot:script-name*) "/action")
                  :method "post"
           (:input :type "submit" :value "Yes"))
    (:a.btn :href (format nil "/decks/id/~a" deck-id) "No")
    (:raw (deck-html (get-deck-by-id deck-id)
                     :page 1))))

(defun blog-html ()
  (with-page-string (:title "Blog")
    (:div.blog-post-container
     (:h1 "Blog")
     (:a :href "/blog/comparison-to-ankiweb" "Comparison to Ankiweb shared decks"))))

(defun blog-post-html (slug)
  "The existence of the file has already been verified. TODO see r/blog-post"
  (let ((title (uiop:read-file-string (str:concat *blog-posts-fdn* slug ".title")))
        (html (uiop:read-file-string (str:concat *blog-posts-fdn* slug ".html"))))
    (with-page-string (:title title)
      (:div.blog-post-container
       (:raw html)))))
