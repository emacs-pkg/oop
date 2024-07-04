(require 'eieio)
(require 'json)

(defun !to-json (x)
  (json-encode x))

(defun !from-json (x)
  (json-parse-string x :null-object nil :false-object nil))

(defmacro !return (x) `(throw 'return ,x))

(defmacro !program (&rest body)
  `(catch 'return
     ,@body)
  )
;;(get 'progn 'lisp-indent-function)
;;0
(put '!program 'lisp-indent-function 0)

(defmacro !method (name super &rest body)
  `(cl-defmethod ,name ,super
     (catch 'return
       ,@body)
     )
  )
(put '!method 'lisp-indent-function 'defun)

(defmacro !class ($class $super &rest $spec-list)
  (if (not (listp $spec-list))
      (error "$spec list is not list")
    (let ( $spec-list2 )
      (dolist ($spec $spec-list)
        (setq $spec-list2 (nconc $spec-list2 (list (!class::spec $spec))))
        )
      (let (($form `(defclass ,$class ,$super ,$spec-list2)))
        ;;(xdump $form)
        $form
        )
      )
    )
  )

(defun !class::spec ($spec)
  (let* (($sym-name (!class::symbol-name $spec))
         ($ini-form (!class::init-form $spec))
         )
    `( ,(intern $sym-name)
       :initarg ,(intern (concat ":" $sym-name))
       :initform ,$ini-form )
    )
  )

(defun !class::symbol-name ($spec)
  (let* (($sym $spec))
    (if (listp $sym)
        (setq $sym (nth 0 $sym)))
    (let* (($sym-name (symbol-name $sym)))
      (if (string-match "^:" $sym-name)
          (setf $sym-name (substring $sym-name 1)))
      $sym-name
      )
    )
  )

(defun !class::init-form ($spec)
  (if (symbolp $spec)
      nil
    (nth 1 $spec)))

(provide 'oop)
