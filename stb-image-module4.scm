(module stb-image (
		   read-image read-image-info
		   load-image load-image-info
		   )

(import scheme chicken foreign)
(use
 (only extras read-string)
 (only lolevel number-of-bytes move-memory!))

(include "stb-image.scm"))
