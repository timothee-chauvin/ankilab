;;; an intermediate table with deck + note pairs

(in-package :anki-share)

(mito:deftable deck+note ()
  ((deck-id :col-type :integer)
   (note-id :col-type :integer))
  (:primary-key deck-id note-id))

(common-db-functions deck+note
                     :get-multiple-by (deck-id note-id))

(defun deck+note-create (deck note)
  (let ((deck-id (deck-id deck))
        (note-id (anki-note-id note)))
    (locked-sexpr
      (unless (mito:find-dao 'deck+note :deck-id deck-id :note-id note-id)
        (mito:insert-dao (make-instance 'deck+note
                                        :deck-id deck-id
                                        :note-id note-id))))))

(defun deck-create (title description author notes)
  (let ((deck (deck-create-without-notes title description author)))
    (dolist (note notes)
      (deck+note-create deck note))))

(defun get-deck-notes (deck)
  "Return a list of all the notes in `deck'"
  (mapcar #'(lambda (deck+note)
              (get-anki-note-by-id (deck+note-note-id deck+note)))
          (get-deck+notes-by-deck-id (deck-id deck))))

(defun get-decks-by-note (note)
  "Return a list of all the decks containing `note'"
  (mapcar #'(lambda (deck+note) (get-deck-by-id (deck+note-deck-id deck+note)))
          (get-deck+notes-by-note-id (anki-note-id note))))

;; TODO note-level authorization
(defun deck-delete-with-notes (deck)
  "Delete `deck' and all its notes"
  (dolist (note (get-deck-notes deck))
    (locked mito:delete-dao note))
  (locked mito:delete-dao deck))
