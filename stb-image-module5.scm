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
 (srfi-4)
 (chicken foreign))

(include "stb-image.scm"))
