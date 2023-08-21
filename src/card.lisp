(in-package :anki-share)

(mito:deftable card ()
  ((id :col-type :integer
       :primary-key t
       :documentation "kept when importing")
   (note-id :col-type :integer
            :documentation "kept when importing")
   (ord :col-type :integer
        :documentation "Which of the note's cards it is. Kept when importing")
   (mod :col-type :integer
        :documentation "Modification timestamp. Kept when importing")))

(common-db-functions card
                     :get-single-by (id)
                     :get-multiple-by (note-id))

(defun get-cards-by-notes (notes)
  (loop :for note :in notes
        :for note-id = (anki-note-id note)
        :with cards = '()
        :do (setf cards
                  (append (get-cards-by-note-id note-id) cards))
        :finally (return cards)))

(defun add-or-update-card (card)
  "If the card isn't in the collection, add it, otherwise update it"
  (let ((existing (get-card-by-id (card-id card))))
    (if existing
        (progn
          (setf (card-ord existing) (card-ord card))
          (setf (card-mod existing) (card-mod card))
          ;; the note ID isn't supposed to change
          (locked mito:save-dao existing))
        (locked mito:insert-dao card))))

(defun extract-cards-from-anki2 (fn)
  (loop :for row :in (fetch-db fn "select id, nid, ord, mod from cards")
        :do (add-or-update-card (make-instance 'card
                                               :id (access row :|id|)
                                               :note-id (access row :|nid|)
                                               :ord (access row :|ord|)
                                               :mod (access row :|mod|)))))
