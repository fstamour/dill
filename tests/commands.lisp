
(in-package #:dill.test)

(define-test replace-aliases
  (is equalp nil (dill::replace-aliases nil nil))
  (is equalp '(:type "blob")
      (dill::replace-aliases '(:t "blob") '((:t . :type)))))

(define-test parse-argv
  (is equalp 'nil (dill::parse-argv nil))
  (is equalp '(nil :version t) (dill::parse-argv '("--version")))
  (is equalp '(nil :tag "2.0") (dill::parse-argv '("--tag" "2.0")))
  (is equalp '(nil :tag "2.0") (dill::parse-argv '("-t" "2.0")
						 '((:t . :tag))))
  (is equalp '(("A") :tag "2.0") (dill::parse-argv '("--tag" "2.0" "A")))
  (is equalp '(("A" "b") :tag "2.0")
      (dill::parse-argv '("--tag" "2.0" "A" "b"))))
