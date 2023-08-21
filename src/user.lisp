(in-package :anki-share)

(mito:deftable user ()
  ((username :col-type :text)
   (email :col-type :text)
   (hpassword :col-type :text)
   (confirmed :col-type (or :integer :null)
              :inflate #'plusp
              :deflate #'(lambda (bool) (if bool 1 0))
              :documentation "Whether the user has confirmed their email
              address. Stored as an integer: 0 for nil, 1 for t")))

(common-db-functions user
                     :get-single-by (id email username))

(defmethod user-id ((user user))
  (mito:object-id user))

(defun user-email-to-username (email)
  (let ((user (get-user-by-email email)))
    (when user (user-username user))))

(defun add-new-user (email username password)
  (locked mito:insert-dao (make-instance 'user
                                         :email email
                                         :username username
                                         :hpassword (cl-pass:hash password)
                                         ;; TODO temporary
                                         :confirmed t)))

(defun confirm-user (user)
  (setf (user-confirmed user) t)
  (locked mito:save-dao user))

(defun delete-user (user)
  (locked mito:delete-dao user))
