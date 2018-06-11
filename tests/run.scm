(cond-expand
 (chicken-5 (import stb-image (chicken blob) srfi-4
		    (chicken file)
		    (chicken pathname)
		    test))
 (else (use stb-image test srfi-4 posix)))

(test
 "read-image 2x2 grayscale png"
 ;;   pixels       w h c
 `(#u8(00 69 171 255) 2 2 1)
 (receive
     (with-input-from-string
	 (conc "\211PNG\r\n\x1a\n\x00\x00\x00\r"
	       "IHDR\x00\x00\x00\x02\x00\x00\x00\x02\b\x00"
	       "\x00\x00\x00W\335R\370\x00\x00\x00\x11"
	       "IDAT\b\x1d\x01\x06\x00\371\377\x00\x00E\x00"
	       "\253\377\x03o\x01\xf0\xd7\xb5\x30}\x00\x00\x00\x00"
	       "IEND\256B`\202")
       read-image)))

(test
 "load-image 2x2 grayscale tga"
 ;;   pixels       w h c
 `(#u8(00 69 171 255) 2 2 1)
 (receive
     (load-image (blob->u8vector #${00000b00000000000000000002000200080001
		                    abff0100450000000000000000545255455649
		                    53494f4e2d5846494c452e00}))))


(test
 "load-image-info 2x2 grayscale tga"
 ;;   pixels       w h c
 `(2 2 1)
 (receive
     (load-image-info (blob->u8vector #${00000b00000000000000000002000200080001
			                 abff0100450000000000000000545255455649
			                 53494f4e2d5846494c452e00}))))

(test
 "read-image-info 2x2 grayscale tga"
 ;;   pixels       w h c
 `(2 2 1)
 (receive
     (with-input-from-string
	 (blob->string #${00000b00000000000000000002000200080001
			  abff0100450000000000000000545255455649
			  53494f4e2d5846494c452e00})
       read-image-info)))

(define (source file) (make-pathname "img" file))
(define (target file #!optional (appending ""))
  (create-directory "out")
  (make-pathname "out" (conc (pathname-strip-directory file) appending)))

(receive (d w h c) (with-input-from-file (source "big.png") read-image)
  (test "png big rgb24 width" 1024 w)
  (test "png big rgb24 width" 1024 h)
  (test "png big rgb24 width" 3 c)
  ;; just test the first few pixels.
  (test "png big rgb24 pixels" #u8(255 255 255 255 0 0 0 255 0 0 0 255)
	(subu8vector d 0 (* 3 4))))

(define (write-ppm blob w h c)
  (assert (or (= c 3) (= c 4)))
  (print "P3")
  (print "# ppm writer by chicken-stb-image test")
  (print w " " h)
  (print 255)
  (do ((y 0 (+ y 1)))
      ((>= y h))
    (do ((x 0 (+ x 1)))
	((>= x w))
      (display (u8vector-ref blob (+ 0 (* c x) (* c w y)))) (display " ")
      (display (u8vector-ref blob (+ 1 (* c x) (* c w y)))) (display " ")
      (display (u8vector-ref blob (+ 2 (* c x) (* c w y)))) (display " "))
    (newline)))


;; expecgted pixels. see source.ppm
(define-values (pixels w h c)
  (values
   (u8vector 000 000 000   255 255 255   255 000 000  000 255 000   000 000 255
             000 000 000   001 002 003   004 005 006  007 008 009   010 011 012)
   5 2 3))

(define (testing file pixels rgb-read? rgb-load? channels)

  (when rgb-read?
    (receive (d w h c)
	(with-input-from-file file (lambda () (read-image channels: channels)))
      (test (conc "rgb pixel data read " file) pixels d)))

  (when rgb-load?
    (receive (d w h c)
	(load-image (with-input-from-file file read-u8vector) channels: channels)
      (test (conc "rgb pixel data load " file) pixels d))))

;;                                read load  channels
(testing (source "rgb.png") pixels  #t   #t  c)
(testing (source "rgb.bmp") pixels  #t   #t  c)
(testing (source "rgb.tga") pixels  #t   #t  c)
(testing (source "rgb.pnm") pixels  #f   #t  c)
(testing (source "rgb.psd") pixels  #t   #t  c)
(testing (source "rgb.hdr")
         (u8vector 000 000 000 255 255 255 255 000 000 000 255 000 000 000 255
                   000 000 000 006 009 011 012 013 014 016 017 017 018 019 020)
         #f   #t  c) ;; TODO: fix this big time


;; TODO: find a way to convert my rgb.png to rgb.pic and test that too

;; jpg is special since it's lossy. no point in comparing
;; pixel-for-pixel.  but we can produce a ppm and inspect manually if
;; anybody cares.
(let ((file (source "rgb.jpg")))
 (receive (d w h c)
     (with-input-from-file file read-image)
   (with-output-to-file (conc (target file ".ppm"))
     (lambda () (write-ppm d w h c)))))

(test
 "error message propagation"
 "\nError: (read-image) unknown image type\n"
 (handle-exceptions e (with-output-to-string (lambda () (print-error-message e)))
		    (with-input-from-string "bad example" read-image)
		    #f))

(print "there are PPM files under tests/out/ for manual inspection")

(test-exit)
