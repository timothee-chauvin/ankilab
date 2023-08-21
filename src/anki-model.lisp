(in-package :anki-share)

(mito:deftable anki-model ()
  ((id :col-type :integer
       :primary-key t
       :documentation "The same ID as the one given by Anki")
   (data :col-type :text
         :deflate #'st-json:write-json-to-string
         :inflate #'st-json:read-json
         :documentation "The full JSON given by Anki, converted to a Lisp object")))

(common-db-functions anki-model
                     :get-single-by (id))

(defun add-anki-model (anki-model)
  "If this model isn't in the collection, add it, otherwise do nothing.
Updating models leads to problems that will be worked on later."
  (unless (get-anki-model-by-id (anki-model-id anki-model))
    (locked mito:insert-dao anki-model)))

(defun extract-models-from-anki2 (fn)
  "Extract the models from filename `fn' and add them to the collection"
  (let* ((models-json (access (first (fetch-db fn "select models from col"))
                              :|models|))
         (models-data (st-json::jso-alist (st-json:read-json models-json))))
    (dolist (model-data models-data)
      (add-anki-model (make-instance 'anki-model
                                         :id (st-json:getjso "id" (rest model-data))
                                         :data (rest model-data))))))
