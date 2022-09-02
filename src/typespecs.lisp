(in-package :trivial-types)

(defun type-specifier-p (type-specifier)
  "Returns true if TYPE-SPECIFIER is a valid type specfiier."
  (or (when (symbolp type-specifier)
        (documentation type-specifier 'type))
      (block nil
        #+sbcl (return (sb-ext:valid-type-specifier-p type-specifier))
        #+openmcl (return (ccl:type-specifier-p type-specifier))
        #+ecl (return (c::valid-type-specifier type-specifier))
        #+clisp (return (null
                         (nth-value 1 (ignore-errors
                                       (ext:type-expand type-specifier)))))
        #-(or sbcl openmcl ecl clisp)
        (error "TYPE-SPECIFIER-P not available for this implementation"))))

(deftype type-specifier () `(satisfies type-specifier-p))

(defun type-expand (type-specifier &optional env)
  "Expand TYPE-SPECIFIER in the lexical environment ENV."
  (or (block nil
        #+sbcl (return (sb-ext::typexpand type-specifier env))
        #+openmcl (return (ccl::type-expand type-specifier env))
        #+clisp (return (ext:type-expand type-specifier)))
      (prog1 type-specifier
        (warn "TYPE-EXPAND not available for this implementation"))))

(defun type= (type1 type2)
  "Returns a primary value of T is TYPE1 and TYPE2 are the same type,
and a secondary value that is true is the type equality could be reliably
determined: primary value of NIL and secondary value of T indicates that the
types are not equivalent."
  (multiple-value-bind (sub ok) (subtypep type1 type2)
    (cond ((and ok sub)
           (subtypep type2 type1))
          (ok
           (values nil ok))
          (t
           (multiple-value-bind (sub ok) (subtypep type2 type1)
             (declare (ignore sub))
             (values nil ok))))))
