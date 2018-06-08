(module stb-image (
		   read-image read-image-info
		   load-image load-image-info
		   )

(import scheme chicken foreign)
(use
 (only extras read-string)
 (only lolevel number-of-bytes move-memory!)
 (only srfi-4 u8vector-length make-u8vector read-u8vector))

(include "stb-image.scm"))
