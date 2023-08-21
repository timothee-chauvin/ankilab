;;; Utilities

(in-package :anki-share)

(alexandria:define-constant +alphanum+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  :test #'string=)

(defmacro build-symbol (args &key package &allow-other-keys)
  "Build a new symbol by concatenating the values in ARGS, each of which can
be either a string or a symbol"
  `(intern (str:concat ,@(mapcar (lambda (arg)
                                   (etypecase arg
                                     (symbol `(symbol-name ,arg))
                                     (string arg)))
                                 args))
           ,(if package package "ANKI-SHARE")))

(defmacro build-keyword-symbol (args)
  "The same as build-symbol, but in the KEYWORD package"
  `(build-symbol ,args :package "KEYWORD"))

(defun multi-union (lists &key (test #'eql))
  (loop :with m-u = '()
        :for l :in lists
        :do (setf m-u (union m-u l :test test))
        :finally (return m-u)))

;; Create macros str-union, etc, which call the initial operation
;; but with a :test #'string= at the end
(macrolet ((make-str-ops ((&rest ops) &rest test-args)
             `(progn
                ,@(mapcar (lambda (op)
                            `(defmacro ,(build-symbol ("STR-" op)) (&rest args)
                               ;; A let to get out of the huge conundrum of
                               ;; nested backquotes...
                               (let ((op ',op)
                                     (test-args ',test-args))
                                   `(,op ,@args ,@test-args))))
                          ops))))
  (make-str-ops (access
                 union
                 multi-union
                 intersection
                 set-difference
                 adjoin
                 find
                 remove-duplicates) :test #'string=)
  (make-str-ops (sort) #'string-lessp))

(defun random-string (length)
  (coerce (loop :repeat length
                :collect (aref +alphanum+ (random (length +alphanum+))))
          'string))

(defun list-to-string (item-sep items)
  (str:join item-sep items))

(defun get-file-hash (ffn)
  "Return the first <= 10 characters of a custom fast hash of `ffn'"
  ;; This is UTF-8, values of (char-code char) may be higher than 255 (but it
  ;; doesn't matter)
  (format nil "~36R"
          (mod
           (loop :for char :across (str:from-file ffn)
                 :for i :from 1
                 :sum (* i (char-code char)))
           (expt 36 10))))
