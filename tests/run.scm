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


(define (write-pgm blob w h)
  (let ((v (blob->u8vector/shared blob)))
    (print "P2")
    (print "# pgm writer by chicken-stb-image test")
    (print w " " h)
    (print 255)
    (do ((y 0 (+ y 1)))
	((>= y h))
      (do ((x 0 (+ x 1)))
	  ((>= x w))
	(display (u8vector-ref v (+ x (* w y))))
	(display " "))
      (newline))))

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

(define (target file #!optional (appending ""))
  (create-directory "out")
  (make-pathname "out" (conc file appending)))

(define (convert file) (system (conc "convert test.png " file)))

(define (testing file)
  (print "loading " file)
  (convert (target file))
  (receive (d w h c)
      (with-input-from-file (target file) read-image)
    (with-output-to-file (conc (target file ".ppm")) (lambda () (write-ppm d w h c))))

  ;; test conversion to grayscale
  (receive (d w h c)
      (with-input-from-file (target file) (lambda () (read-image channels: 1)))
    (with-output-to-file (conc (target file ".pgm")) (lambda () (write-pgm d w h)))))

(testing "test.jpg")
(testing "test.png")
(testing "test.bmp")
(testing "test.tga")
;; (convert "test.hdr") ;; uses eof, so didn't bother with this one yet (load works)

(print "testing hdr (from memory)")
(convert (target "test.hdr"))
(receive (d w h c)
    (load-image (with-input-from-file (target "test.hdr") read-string))
  (with-output-to-file (target "test.hdr" ".ppm") (lambda () (write-ppm d w h c))))

(display "\nExpecting \"unknown image type\" error here: ")
(handle-exceptions e (print-error-message e)
		   (with-input-from-string "bad example" read-image))

(test-exit)
