(module stb-image (
		   read-image read-image-info
		   load-image load-image-info
		   )

(import
 scheme
 (chicken base)
 (only (chicken io) read-string)
 (only (chicken memory representation) number-of-bytes)
 (only (chicken memory) move-memory!)
 (only (chicken blob) make-blob blob-size)
 (only (chicken string) conc)
 (only (srfi-4) u8vector-length make-u8vector read-u8vector u8vector)
 (chicken foreign))

(include "stb-image.scm"))
