(cond-expand
 (chicken-5 (import stb-image (chicken blob) srfi-4
		    (chicken file)
		    (chicken pathname)
		    test))
 (else (use stb-image test srfi-4 posix)))

(test
 "read-image 2x2 grayscale png"
 ;;   pixels       w h c
 `(#${00 45 ab ff} 2 2 1)
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
 `(#${00 45 ab ff} 2 2 1)
 (receive
     (load-image #${00000b00000000000000000002000200080001
		    abff0100450000000000000000545255455649
		    53494f4e2d5846494c452e00})))


(test
 "load-image-info 2x2 grayscale tga"
 ;;   pixels       w h c
 `(2 2 1)
 (receive
     (load-image-info #${00000b00000000000000000002000200080001
			 abff0100450000000000000000545255455649
			 53494f4e2d5846494c452e00})))

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

(receive (d w h c) (with-input-from-file (source "4k.png") read-image)
  (test "png 4k rgb24 width" 3840 w)
  (test "png 4k rgb24 width" 2160 h)
  (test "png 4k rgb24 width" 3 c)
  ;; just test the first few pixels.
  (test "png 4k rgb24 pixels" #u8(255 255 255 255 0 0 0 255 0 0 0 255)
	(subu8vector (blob->u8vector/shared d) 0 (* 3 4))))

(define (write-ppm blob w h c)
  (assert (or (= c 3) (= c 4)))
  (let ((v (blob->u8vector/shared blob)))
    (print "P3")
    (print "# ppm writer by chicken-stb-image test")
    (print w " " h)
    (print 255)
    (do ((y 0 (+ y 1)))
	((>= y h))
      (do ((x 0 (+ x 1)))
	  ((>= x w))
	(display (u8vector-ref v (+ 0 (* c x) (* c w y)))) (display " ")
	(display (u8vector-ref v (+ 1 (* c x) (* c w y)))) (display " ")
	(display (u8vector-ref v (+ 2 (* c x) (* c w y)))) (display " "))
      (newline))))

(define (ppm file)
  (receive (d w h c)
      (with-input-from-file file read-image)
    (with-output-to-file (conc (target file ".ppm"))
      (lambda () (write-ppm d w h c)))))

(define (rgbtest file)
  (receive (d w h c)
      (with-input-from-file file (lambda () (read-image channels: 3)))
    (test (conc "rgb pixel data " file)
	  #u8(0 0 0   255 0 0   0 255 0  0 0 255 255 255 255)
	  (subu8vector (blob->u8vector/shared d) 0 (* 3 5)))))

(rgbtest (source "rgb.png"))  (ppm (source "rgb.png"))
#|rgbtest pointless: lossy |# (ppm (source "rgb.jpg"))
(rgbtest (source "rgb.bmp"))  (ppm (source "rgb.bmp"))
(rgbtest (source "rgb.tga"))  (ppm (source "rgb.tga"))

(receive (d w h c)
    (load-image (with-input-from-file (source "rgb.hdr") read-string))
  (test "rgb pixel data hdr from memory"
	#u8(0 0 0   255 0 0   0 255 0  0 0 255 255 255 255)
	(subu8vector (blob->u8vector/shared d) 0 (* 3 5)))
  (with-output-to-file (target "test.hdr" ".ppm") (lambda () (write-ppm d w h c))))

(test
 "error message propagation"
 "\nError: (read-image) unknown image type\n"
 (handle-exceptions e (with-output-to-string (lambda () (print-error-message e)))
		    (with-input-from-string "bad example" read-image)
		    #f))

(test-exit)
