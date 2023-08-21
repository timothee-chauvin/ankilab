(in-package :anki-share)

(defun main ()
  (start-all)
  ;; Set up a listening slynk server on port 4002
  (bt:make-thread (lambda ()
                    (slynk:create-server :port 4002))
                  :name "slynk"))
