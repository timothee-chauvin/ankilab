;;; Functions and macros related to user-uploaded media
;;; (like images that can be included in notes)

(in-package :anki-share)

(defun get-media-ffns ()
  "Return a list of the full filenames of everything in *media-fdn*"
  (mapcar #'namestring (uiop:directory-files *media-fdn*)))

(defun get-media-fns ()
  "Return a list of the filenames of everything in *media-fdn*"
  (mapcar #'file-namestring (get-media-ffns)))

(defun fn-exists (fn)
  "Whether there already is a file named FN in *media-fdn*"
  (str-find fn (get-media-fns)))

(defun same-files (ffn1 ffn2)
  "Check whether 2 files have the same content"
  (with-open-file (f1 ffn1 :element-type '(unsigned-byte 8))
    (with-open-file (f2 ffn2 :element-type '(unsigned-byte 8))
      (loop :for b1 = (read-byte f1 nil 'eof)
            :for b2 = (read-byte f2 nil 'eof)
            :when (not (eql b1 b2))
              :do (return nil)
            :when (eql 'eof b1)
              :do (return t)))))

(defun file-exists (ffn)
  "Whether the file (full filename FFN) already exists (same bytes)
in our collection. If it exists, return the corresponding filename"
  (loop :for other-ffn :in (get-media-ffns)
        :when (same-files ffn other-ffn)
          :do (return (file-namestring other-ffn))
        :finally (return nil)))

(defun random-name (current-fn)
  "Return a random name (string) for CURRENT-FN (which is
used in order to preserve its extension)"
  (let* ((ext (pathname-type current-fn))
         (ext-str (if ext (str:concat "." ext) "")))
    (str:concat (random-string *fn-length*)
     ext-str)))

(defun new-random-name (current-fn)
  "Return a random name (string) for CURRENT-FN which is guaranteed to
be new (i.e. not already in the collection)"
  (loop :for name = (random-name current-fn)
        :when (not (fn-exists name))
          :do (return name)))

(defun get-deck-name-mapping (media-ffn)
  "Return an a-list (fn->fn) corresponding to the JSON in MEDIA-FFN"
  (loop :for (given-fn . associated-fn)
          :in (with-open-file (f (pathname media-ffn))
                (st-json::jso-alist (st-json:read-json f)))
        :collect (cons (string given-fn)
                       (string associated-fn))))

(defun save-files-with-new-names (given-name-mapping deck-fdn)
  "Return an a-list (string->string) of the new name mapping after checking for
existing files in deck-pathname, creation of new names for the new files, and
saving the files. The car of each cons cell is the original name as uploaded,
and the cdr is the last part of the name as used in the HTML to retrieve our
stored image (e.g. xyzxyz.png, without the directories)"
    (loop :with new-fn = nil
          ;; `number-fn': 0, 1, 2... `orig-fn': the name originally used in notes
          :for (number-fn . orig-fn) :in given-name-mapping
          :for number-ffn = (str:concat deck-fdn number-fn)
          :for existing-fn = (file-exists number-ffn)
          :if existing-fn
            :collect (cons orig-fn existing-fn)
          :else
            :do (setf new-fn (new-random-name orig-fn))
            :and :do (cl-fad:copy-file number-ffn
                                       (str:concat *media-fdn* new-fn))
            :and :collect (cons orig-fn new-fn)))

(defun save-media-files (deck-fdn)
  "Save the files indicated in the file named \"media\" in `deck-fdn'
and return the new name mapping"
  (let* ((media-ffn (str:concat deck-fdn "media"))
         (given-name-mapping (get-deck-name-mapping media-ffn)))
    ;; TODO filter: keep only the files actually used in the notes
    (save-files-with-new-names given-name-mapping deck-fdn)))
