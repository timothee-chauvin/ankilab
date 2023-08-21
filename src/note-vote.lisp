;;; An intermediate table with note + vote pairs

(in-package :anki-share)

(mito:deftable note+vote ()
  ((user-id :col-type :integer)
   (note-id :col-type :integer)
   (value :col-type :integer
          :documentation "The value of the vote, +1 or -1"))
  (:primary-key user-id note-id))

(common-db-functions note+vote
                     :get-multiple-by (user-id note-id))

;; TODO will be computed twice for each note at the moment if using the
;; functions below
(defun anki-note-votes (note)
  "Return a p-list of the form (:upvotes ... :downvotes ...)
downvotes is a positive integer."
  (loop :for note+vote :in (get-note+votes-by-note-id (anki-note-id note))
        :for value = (note+vote-value note+vote)
        :when (> value 0)
          :sum value :into upvotes
        :when (< value 0)
          :sum (- value) :into downvotes
        :finally (return (list :upvotes upvotes :downvotes downvotes))))

(defun anki-note-upvotes (note)
  (getf (anki-note-votes note) :upvotes))

(defun anki-note-downvotes (note)
  (getf (anki-note-votes note) :downvotes))

(defun note+vote-create-or-update (&key user-id note-id value)
  (locked-sexpr
    (let* ((note+vote (mito:find-dao 'note+vote
                                     :note-id note-id :user-id user-id)))
      (if note+vote
          (let ((previous-value (note+vote-value note+vote)))
            (if (= previous-value value)
                ;; undo the vote
                (mito:delete-dao note+vote)
                ;; otherwise replace the vote
                (progn
                  (setf (note+vote-value note+vote) value)
                  (mito:save-dao note+vote))))
          (mito:insert-dao (make-instance 'note+vote
                                          :user-id user-id
                                          :note-id note-id
                                          :value value))))))
