(in-package :trivial-types)

(defun character-designator-p (object)
  (and (typep object 'string-designator)
       (= 1 (length (string object)))))

(deftype character-designator ()
  `(or (string 1)
       character
       ;; Can also be a symbol of length 1
       ;; http://clhs.lisp.se/Body/26_glo_c.htm#character_designator
       ;; However, this does not work well with subtypep
       (and symbol (satisfies character-designator-p))))

(deftype function-designator ()
  '(or symbol
       function))

(deftype file-position-designator ()
  '(or (member :start :end)
       (integer 0)))

(deftype list-designator ()
  '(or (non-nil atom)
       proper-list))

(deftype package-designator ()
  '(or string-designator
       package))

(deftype pathname-designator ()
  '(or string
       file-associated-stream
       pathname))

(deftype stream-designator ()
  '(or (member t nil)
       stream))

(deftype string-designator ()
  '(or character
       symbol
       string))
