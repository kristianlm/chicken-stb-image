(foreign-declare "
#define STB_IMAGE_IMPLEMENTATION
#define STBI_NO_STDIO
#include \"stb_image.h\"
")

(define (check i loc)
  (if (or (eq? i #f) (eq? i 0))
      (error loc ((foreign-lambda c-string "stbi_failure_reason")))
      i))

(define stbi-image-free (foreign-lambda void "stbi_image_free" (c-pointer void)))

(define (load-image blob #!key channels)
  (let-location
   ((x int)
    (y int)
    (channels int (or channels 0)))

   (define ptr
     (check
      ((foreign-lambda* (c-pointer char)
			((scheme-pointer buffer) ;;(stbi_uc const *buffer)
			 (int len)
			 ((c-pointer int) x)
			 ((c-pointer int) y)
			 ((c-pointer int) channels))
			"int file_channels;"
			"stbi_uc* ret = "
			" stbi_load_from_memory("
			"   buffer, len, x, y, &file_channels, *channels);"
			"if(*channels == 0) *channels = file_channels;"
			"return(ret);")
       blob (number-of-bytes blob)
       (location x) (location y) (location channels))
      'stbi-load-from-memory))
   
   (let ((pixels (make-blob (* x y channels))))
     (move-memory! ptr pixels (blob-size pixels))
     (stbi-image-free ptr)
     (values pixels x y channels))))

(define (load-image-info blob)
  (let-location
   ((x int)
    (y int)
    (channels int))
   (check
     ((foreign-lambda* int
		       ((scheme-pointer buffer) ;;(stbi_uc const *buffer)
			(int len)
			((c-pointer int) x)
			((c-pointer int) y)
			((c-pointer int) channels))
		       "return(stbi_info_from_memory(buffer, len, x, y, channels));")
      blob (number-of-bytes blob)
      (location x) (location y) (location channels))
     'stbi-info-from-memory)
   (values x y channels)))

;; ==================== externals ====================

(define-external (port_read
		  ((c-pointer void) user)
		  ((c-pointer char) data)
		  (int size))
  int
  (let ((r (read-string size)))
    (move-memory! r data) ;; how to avoid copying?
    (string-length r)))

(define-external (port_skip
		  ((c-pointer void) user)
		  (int n))
  void
  (if (>= n 0)
      (read-string n)
      (error "backwards seek not implemented")))

(define-external (port_eof
		  ((c-pointer void) user))
  int
  (error "eof callback not implemented"))
;; ====================

(define (read-image #!key channels)
  (let-location ((x int) (y int) (channels int (or channels 0)))
    (define ptr
      (check
       ((foreign-safe-lambda*
	 c-pointer (((c-pointer int) x)
		    ((c-pointer int) y)
		    ((c-pointer int) channels))
	 "int _channels;"
	 "stbi_io_callbacks io = "
	 "  {.read = port_read, .skip = port_skip, .eof = port_eof};"
	 "stbi_uc* ret = stbi_load_from_callbacks"
	 "  (&io, 0, x, y, &_channels, *channels);"
	 "if(*channels == 0) *channels = _channels;"
	 "return(ret);")
	(location x) (location y) (location channels))
       'read-image))
    (let* ((size (* x y channels))
	   (pixels (make-blob size)))
      (move-memory! ptr pixels size)
      (stbi-image-free ptr)
      (values pixels x y channels))))

(define (read-image-info)
  (let-location ((x int) (y int) (channels int))
    (check
     ((foreign-safe-lambda*
       int (((c-pointer int) x)
	    ((c-pointer int) y)
	    ((c-pointer int) channels))
       "int _channels;"
       "stbi_io_callbacks io = "
       "  {.read = port_read, .skip = port_skip, .eof = port_eof};"
       "return("
       " stbi_info_from_callbacks(&io, 0, x, y, channels)"
       ");")
      (location x) (location y) (location channels))
     'read-image)
    (values x y channels)))

