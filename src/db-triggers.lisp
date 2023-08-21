(in-package :anki-share)

(defmethod mito:delete-dao :before ((note anki-note))
  (let ((id (anki-note-id note))
        (model-id (anki-note-model-id note)))
    ;; TODO would be nice to have, but not very important and complicates
    ;; the use of locking
    (declare (ignore model-id))
    ;; maybe delete the note's model if it's now unused
    ;; (when (= 1 (length (get-anki-notes-by-model-id model-id)))
    ;;   (mito:delete-by-values 'anki-model :id model-id))))
    (mito:delete-by-values 'deck+note :note-id id)
    (mito:delete-by-values 'note+vote :note-id id)))

(defmethod mito:delete-dao :before ((deck deck))
  (let ((id (deck-id deck)))
    (mito:delete-by-values 'deck+note :deck-id id)))

(defmethod mito:delete-dao :before ((user user))
  "Delete the `user''s decks, notes and votes"
  (let ((id (user-id user)))
    (mito:delete-by-values 'note+vote :note-id id)
    (mito:delete-by-values 'anki-note :author-id id)
    (mito:delete-by-values 'deck :author-id id)))
