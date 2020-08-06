
;; create an executable
(uiop/image:dump-image
 (string-downcase '#:dill)
 :executable t
 :compression nil)
