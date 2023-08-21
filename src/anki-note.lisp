;;; This website's own representation of Anki cards,
;;; which can be converted from and into actual Anki cards

(in-package :anki-share)

(defconstant +anki-field-sep+ #\Us
  "The field separator for Anki notes")

(defparameter *allowed-html-tags*
  '("b" "blockquote" "br" "code" "div" "em" "i" "img" "li" "ol" "strong" "ul")
  "The list of tags that don't have to be escaped when rendering HTML")

(defparameter *allowed-html-attrs*
  (alexandria:alist-hash-table
   '(("a" . "href")
     ("img" . "src")))
  "The list of allowed attributes, as a hash table because bleach expects a
Python dictionary and py4cl only translates hash tables into Python dicts")

(defun fields-list-to-string (fields)
  (list-to-string +anki-field-sep+ fields))

(defun fields-string-to-list (fields-str)
  (split-sequence +anki-field-sep+ fields-str))

(defun tags-string-to-list (tags-str)
  (str-remove-duplicates
   (split-sequence #\Space tags-str :remove-empty-subseqs t)))

(defun tags-list-to-string (tags)
  (list-to-string #\Space tags))

(mito:deftable anki-note ()
  ((id :col-type :integer
       :primary-key t
       :documentation "kept when importing")
   (guid :col-type :text
         :documentation "A random base 91 string, kept when importing")
   (mod :col-type :integer
        :documentation "The note's last modification timestamp, as seconds since
Epoch. Kept when importing, since we don't modify notes (except tags) at the
moment")
   (orig-fields :col-type :text
           :inflate #'fields-string-to-list
           :deflate #'fields-list-to-string
           :documentation "The note's original fields, as a list of strings.
These fields aren't changed during the upload-download cycle.")
   (used-fields :col-type :text
                :inflate #'fields-string-to-list
                :deflate #'fields-list-to-string
                :documentation "The note's used fields, for rendering. These
fields are sanitized HTML-wise and use media filenames corresponding to how
media are stored on the server (according to the `media-orig-to-used' mapping).
They could be recomputed every time but this is currently too slow with bleach
and the Python FFI.")
   (media-orig-to-used :col-type (or :text :null)
                       :inflate #'read-from-string
                       :deflate #'write-to-string
                       :documentation "A mapping from the originally used media
filenames and the ones used on the server. Implementation: a-list.")
   (tags :col-type (or :text :null)
         :inflate #'tags-string-to-list
         :deflate #'tags-list-to-string
         :initform '()
         :documentation "The note's tags as a list of strings")
   (csum :col-type :integer
         :documentation "The checksum of the card for finding duplicates. Taken
from Anki and not modified, because it's not trivial to compute: it's the
integer representation of the first 4 bytes of the SHA1 of the first field,
stripped of its HTML")
   (model-id :col-type :integer
             :documentation "kept when importing")
   (author-id :col-type :integer)))

(common-db-functions anki-note
                     :get-single-by (id)
                     :get-multiple-by (author-id model-id))

(defun sanitize (html)
  (py4cl:python-exec "import bleach")
  (py4cl:python-call "bleach.clean" html
                     :tags *allowed-html-tags*
                     :attributes *allowed-html-attrs*
                     :strip_comments t))

;; TODO maybe macrofy this in common-db-functions
(defun anki-note-add-tag (tag note-id)
  "Add TAG to the note with ID NOTE-ID"
  ;; TODO don't add a tag that already exists (check here?)
  (let ((note (get-anki-note-by-id note-id)))
    (when note  ; fail silently if note isn't found
      (push tag (anki-note-tags note))
      (locked mito:save-dao note))))

(defun possible-tags (tags)
  "Return a list of tags that are compatible with TAGS (i.e. share at least
one note)"
  (str-set-difference
   (str-multi-union (mapcar #'(lambda (note)
                                (when (has-tags note tags)
                                  (anki-note-tags note)))
                            (get-anki-notes)))
   tags))

(defun anki-note-sanitize-used-fields (note)
  (setf (anki-note-used-fields note)
        (mapcar #'sanitize (anki-note-used-fields note))))

;; TODO this could be macrofied with also add-anki-model
(defun add-or-update-anki-note (note)
  "If this note isn't in the collection, add it, otherwise update it"
  ;; TODO multiple cards may have been created at the same millisecond
  (anki-note-sanitize-used-fields note)
  (let ((existing (get-anki-note-by-id (anki-note-id note))))
    (if existing                        ; TODO may have changed model ID
        (progn
          (setf (anki-note-media-orig-to-used existing)
                (anki-note-media-orig-to-used note))
          (setf (anki-note-orig-fields existing)
                (anki-note-orig-fields note))
          (setf (anki-note-used-fields existing)
                (anki-note-used-fields note))
          (setf (anki-note-tags existing) (str-remove-duplicates
                                           (str-union (anki-note-tags existing)
                                                      (anki-note-tags note))))
          (locked mito:save-dao existing))
        (locked mito:insert-dao note))))

(defun extract-pending-notes-from-anki2 (fn user &key keep-tags add-tags)
  "Extract all notes from filename `fn', owned by `user', but don't add them
to the collection yet, just return them in a list"
  (loop :for row :in (fetch-db fn "select id, guid, mid, mod, flds, tags, csum from notes")
        :for fields = (fields-string-to-list (access row :|flds|))
        :for tags = (str-union (tags-string-to-list add-tags)
                               (when keep-tags
                                 (tags-string-to-list (access row :|tags|))))
        :collect (make-instance 'anki-note
                                :id (access row :|id|)
                                :guid (access row :|guid|)
                                :mod (access row :|mod|)
                                :orig-fields fields
                                :used-fields fields     ; for now
                                :media-orig-to-used nil ; for now
                                :tags tags
                                :csum (access row :|csum|)
                                :model-id (access row :|mid|)
                                :author-id (user-id user))))

(defun anki-note-media-orig-to-used-update (note old-name new-name)
  "Update the `media-orig-to-used' field of `note' with a mapping from
  `old-name' to `new-name' if one doesn't already exist and if `note' uses
  `old-name' in one way or another."
  (let ((old-img-tag (format nil "<img src=\"~a\">" old-name))
        (old-at-import (format nil "@import url(\"~a\")" old-name)))
    (unless (str-access (anki-note-media-orig-to-used note)
                        old-name)       ; already included
      (when (or
             ;; Is it imported in the `css' field of the model data?
             (search old-at-import
                     (st-json:getjso "css"
                                     (anki-model-data
                                      (get-anki-model-by-id
                                       (anki-note-model-id note)))))
             ;; Is it used as an image tag in a field?
             (plusp
              (loop :for used-field :in (anki-note-used-fields note)
                    :count (search old-img-tag used-field))))
        (push (cons old-name new-name)
              (anki-note-media-orig-to-used note))))))

(defun anki-notes-media-orig-to-used-update (notes name-mapping)
  "Update the `media-orig-to-used' fields of each note in `notes' by determining
  whether each note actually uses the elements of `name-mapping'."
  (loop :for note :in notes
        :do (loop :for (old-name . new-name) :in name-mapping
                  :do (anki-note-media-orig-to-used-update note old-name new-name))))

(defun replace-image-in-note (note old-name new-name)
  "Replace all image references to `old-name' (not necessarily used) in
`orig-fields' into references to `new-name' in `used-fields', update
`media-orig-to-used' accordingly."
  (let* ((old-tag (format nil "<img src=\"~a\">" old-name))
         (new-tag (format nil "<img src=\"/media/~a\">" new-name)))
    (setf (anki-note-used-fields note)
          (loop :for used-field :in (anki-note-used-fields note)
                :collect (str:replace-all
                          old-tag new-tag used-field)))))

(defun replace-images-in-notes (pending-notes new-name-mapping)
  "Return the modified list of notes with fields updated as per
`new-name-mapping'. NOTE: must be called before sanitizing used-fields."
  (loop :for note :in pending-notes
        :do (loop :for (old-name . new-name) :in new-name-mapping
                  :do (replace-image-in-note note old-name new-name))))

(defun save-notes (notes)
  (dolist (note notes)
    (add-or-update-anki-note note)))

(defun has-tags (note tags)
  "Whether NOTE has all of TAGS in its tags"
  (let ((note-tags (anki-note-tags note)))
    (= (length tags)
       (length (str-intersection tags note-tags)))))

(defun get-filtered-notes (&key tags user-id)
  "Return a list of notes matching all the tags in `tags' and from `user-id'"
  (loop :for note :in (get-anki-notes)
        :when (and (has-tags note tags)
                   (or (eql nil user-id)
                       (= (anki-note-author-id note) user-id)))
          :collect note))

;; TODO maybe good candidate for macrofication
(defun get-tags ()
  "Return a list of all tags in the collection"
  (str-remove-duplicates
   (str-multi-union
    (mapcar #'anki-note-tags (get-anki-notes)))))

;; TODO the same
(defun tag-exists (tag)
  "Is TAG in the collection?"
  (str-find tag (get-tags)))

;; TODO the same
(defun get-anki-notes-by-tag (tag)
  "Return all notes having TAG in their list of tags"
  (loop :for note :in (get-anki-notes)
        :when (str-find tag (anki-note-tags note))
          :collect note))

(defun get-models-by-notes (notes)
  "Return a list of all the models used in `notes'"
  (mapcar #'get-anki-model-by-id
          (multi-union
           (mapcar #'(lambda (note) (list (anki-note-model-id note)))
                   notes))))

(defun get-used-media-by-notes (notes)
  "Return a list of the filenames of the media used in `notes', as an a-list
from original filenames to used filenames."
  (multi-union
   (loop :for note :in notes
         :collect (anki-note-media-orig-to-used note))
   :test #'equal))
