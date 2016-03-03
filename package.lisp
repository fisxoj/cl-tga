;;;; package.lisp

(defpackage #:cl-tga
  (:use #:cl)
  (:nicknames #:tga)
  (:export #:read-tga
	   #:read-file
	   #:read-stream
	   #:image-width
	   #:image-height
	   #:image-bpp
	   #:image-data
	   #:image-channels))

