(foreign-declare "
#ifdef __TINYC__
#define STBI_NO_SIMD
#endif

// keep procedures like stbi_load_from_callbacks to ourselves
 #define STB_IMAGE_STATIC

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
			((u8vector buffer) ;;(stbi_uc const *buffer)
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
       blob (u8vector-length blob)
       (location x) (location y) (location channels))
      'load-image))
   
   (let ((pixels (make-u8vector (* x y channels))))
     (move-memory! ptr pixels (u8vector-length pixels))
     (stbi-image-free ptr)
     (values pixels x y channels))))

(define (load-image-info blob)
  (let-location
   ((x int)
    (y int)
    (channels int))
   (check
     ((foreign-lambda* int
		       ((u8vector buffer) ;;(stbi_uc const *buffer)
			(int len)
			((c-pointer int) x)
			((c-pointer int) y)
			((c-pointer int) channels))
		       "return(stbi_info_from_memory(buffer, len, x, y, channels));")
      blob (u8vector-length blob)
      (location x) (location y) (location channels))
     'load-image-info)
   (values x y channels)))

;; ==================== externals ====================

(define-external (chicken_port_read
		  ((c-pointer void) user)
		  ((c-pointer char) data)
		  (int size))
  int
  (let ((r (read-string size)))
    (if (eof-object? r)
        0
        (begin
          (move-memory! r data) ;; how to avoid copying?
          (string-length r)))))

(define-external (chicken_port_skip
		  ((c-pointer void) user)
		  (int n))
  void
  (if (>= n 0)
      (read-string n)
      (error "backwards seek not implemented" n)))

;; C callbacks for feeding image data coming from
;; (current-input-port). user data is used to store the one-byte
;; read-ahead used for eof predicate (< 0 if unknown).
(foreign-declare "
int  chicken_port_read(void*, char*, int);
void chicken_port_skip(void*, int);

int port_read(void* user, char* data, int size) {
  int next = *(int*)user;
  if(next >= 0) {
    data[0] = next;
    *((int*)user) = -1; // clear next
    return 1 + chicken_port_read(user, data + 1, size - 1);
  } else {
    return chicken_port_read(user, data, size);
  }
}

void port_skip(void* user, int n) {
  int next = *(int*)user;
  if(next >= 0) {
    *((int*)user) = -1; // clear next
    if(n >= 1)
      chicken_port_skip(user, n - 1);
  } else {
    chicken_port_skip(user, n);
  }
}

int port_eof(void* user) {
  int next = *(int*)user;
  if(next >= 0) return 0;

  char buff;
  int read = chicken_port_read(user, &buff, 1);
  // 0 bytes read => eof
  if(read == 0) {
     *((int*)user) = -1; // clear next
     return 1;
  } else {
     *((int*)user) = buff;
     return 0;
  }
}
")
;; ====================

(define (read-image #!key channels)
  (apply
   (lambda (ptr x y channels)
     (check ptr 'read-image)
     (let* ((size (* x y channels))
	    (pixels (make-u8vector size)))
       (move-memory! ptr pixels size)
       (stbi-image-free ptr)
       (values pixels x y channels)))
   ((foreign-safe-lambda*
     scheme-object ((int desired_channels))
     "int x, y, channels, last = -1;\n"
     "C_word *ptr = C_alloc(C_SIZEOF_LIST(4));\n"
     "C_word *ptr_p = C_alloc(C_SIZEOF_POINTER);\n"
     "stbi_io_callbacks io = "
     "  {.read = port_read, .skip = port_skip, .eof = port_eof};\n"
     "stbi_uc* pixels = stbi_load_from_callbacks"
     "  (&io, &last, &x, &y, &channels, desired_channels);\n"
     "return(C_list(&ptr, 4, C_mpointer_or_false(&ptr_p, pixels), "
     "  C_fix(x), C_fix(y), C_fix(desired_channels == 0 ? channels : desired_channels)));")
    (or channels 0))))

(define (read-image-info)
  (apply
   (lambda (ret x y channels)
     (check ret 'read-image-info)
     (values x y channels))
   ((foreign-safe-lambda*
     scheme-object ()
     "int x, y, channels, last = -1;"
     "C_word *ptr = C_alloc(C_SIZEOF_LIST(4));"
     "stbi_io_callbacks io = "
     "  {.read = port_read, .skip = port_skip, .eof = port_eof};"
     "int ret = stbi_info_from_callbacks(&io, &last, &x, &y, &channels);"
     "return(C_list(&ptr, 4, C_fix(ret), C_fix(x), C_fix(y), C_fix(channels)));"))))

