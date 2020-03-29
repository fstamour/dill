
;; create an executable
(uiop/image:dump-image
 (string-downcase #.dill.asd:project-name)
 :executable t
 :compression nil)
