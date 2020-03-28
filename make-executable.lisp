
;; create an executable
(uiop/image:dump-image
 (string-downcase project-name)
 :executable t
 :compression nil)
