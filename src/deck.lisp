;;; decks as lists of notes

(in-package :anki-share)

(mito:deftable deck ()
  ((title :col-type :text)
   (description :col-type (or :text :null))
   (author-id :col-type :integer)))

(common-db-functions deck
                     :get-single-by (id)
                     :get-multiple-by (author-id))

;; TODO in common-db-functions? the same whenever ID is implicit
(defmethod deck-id ((deck deck))
  (mito:object-id deck))

(defun deck-create-without-notes (title description author)
  "Create a deck with everything except its notes, which are stored in the
deck+note table, so the full deck-create function is there, not here.
Return the new deck object"
  (locked mito:insert-dao (make-instance 'deck
                                         :title title
                                         :description description
                                         :author-id (user-id author))))
