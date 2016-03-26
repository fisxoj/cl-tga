;;;; cl-tga.lisp

(in-package #:cl-tga)

(defparameter +TGA-RGB+ 2)
(defparameter +TGA-A+ 3)
(defparameter +TGA-RLE+ 8)

(defclass tga ()
  ((length :type fixnum
	   :initform 0)
   (width :type fixnum
	  :initform 0
	  :accessor image-width)
   (height :type fixnum
	   :initform 0
	   :accessor image-height)
   (img-type :type fixnum
	     :initform 0)
   (bits :type fixnum
	 :initform 0
	 :accessor image-bpp)
   (channels :type fixnum
	     :initform 0
	     :accessor image-channels)
   (stride :type fixnum
	   :initform 0)
   (img-data :initform 0
	     :accessor image-data)))

(defun read-word (stream)
  (let ((r 0))
    (setf (ldb (byte 16 0) r) (read-byte stream)
	  (ldb (byte 16 8) r) (read-byte stream))
    r))

(defun skip (stream &optional (bytes 1))
  (dotimes (i bytes)
    (read-byte stream)))

(defun read-rle (buffer stream bits)
  (declare (optimize speed))
  (check-type buffer (simple-array (unsigned-byte 8) (*)))
  (check-type bits (member 8 15 16 24 32))
  (let ((i 0)
        (bytes (ceiling bits 8)))
    (declare (type (unsigned-byte 34) i));; 16 bit width*height * 4 bytes/pixel
    (flet ((read-raw (n)
             (let ((c (* n bytes)))
               (read-sequence buffer stream :start i :end (+ i c))
               (incf i c)))
           (read-run (n)
             (if (= bytes 1)
                 (let ((color (read-byte stream)))
                   (fill buffer color :start i :end (+ i n))
                   (incf i n))
                 (let* ((color (make-array bytes
                                           :element-type '(unsigned-byte 8))))
                   (declare (dynamic-extent color))
                   (read-sequence color stream)
                   (loop repeat n
                         do (replace buffer color :start1 i)
                            (incf i bytes))))))
      (loop
        for h of-type (unsigned-byte 8) = (read-byte stream)
        do (if (logbitp 7 h)
               (read-run (1+ (ldb (byte 7 0) h)))
               (read-raw (1+ (ldb (byte 7 0) h))))
        while (< i (length buffer))))))

(defun map-colors (img-data color-map bits color-map-bits start end)
  (declare (optimize speed))
  (check-type bits (member 8 15 16))
  (check-type color-map-bits (member 8 15 16 24 32))
  (check-type start (unsigned-byte 16))
  (check-type end (unsigned-byte 16))
  (check-type img-data (simple-array (unsigned-byte 8) (*)))
  (check-type color-map (simple-array (unsigned-byte 8) (*)))
  (locally (declare (type (unsigned-byte 8) bits color-map-bits))
    (let* ((pixels (floor (length img-data) (ceiling bits 8)))
           (new (if (= (ceiling bits 8) (ceiling color-map-bits 8))
                    img-data
                    (make-array (* pixels (ceiling color-map-bits 8))
                                :element-type '(unsigned-byte 8)))))
      (declare (type (unsigned-byte 32) pixels))
      (macrolet ((in (n i)
                   (ecase n
                     (1 `(aref img-data ,i))
                     (2 `(logior (aref img-data (* ,i 2))
                                 (ash (aref img-data (1+ (* ,i 2))) 8)))))
                 (out (n c i)
                   (ecase n
                     (1 `(setf (aref new ,i) (aref color-map (- ,c start))))
                     ((2 3 4)
                      (cons 'progn
                            (loop
                              for x below n
                              collect
                              `(setf (aref new (+ ,x (* ,n ,i)))
                                     (aref color-map (+ ,x (* ,n (- ,c start))))))))))
                 (copy (index-size color-size)
                   `(loop for i below pixels
                          for c = (in ,index-size i)
                          do (out ,color-size c i))))
        (ecase color-map-bits
          (8
           (ecase bits
             (8 (copy 1 1))
             ((15 16) (copy 2 1))))
          ((15 16)
           (ecase bits
             (8 (copy 1 2))
             ((15 16) (copy 2 2))))
          (24
           (ecase bits
             (8 (copy 1 3))
             ((15 16) (copy 2 3))))
          (32
           (ecase bits
             (8 (copy 1 4))
             ((15 16) (copy 2 4)))))
        new))))

(defun read-tga-stream (s)
  (declare (optimize speed))
  (let ((image (make-instance 'tga))
        (color-map-start 0)
        (color-map-end 0)
        (color-map-bits 0)
        (has-color-map 0)
        (color-map nil))
    (with-slots (length width height img-type bits channels stride img-data) image
      (setf length (read-byte s))
      (setf has-color-map (read-byte s))
      (setf img-type (read-byte s))
      (setf color-map-start (read-word s))
      (setf color-map-end (read-word s))
      (setf color-map-bits (read-byte s))
      (skip s 4) ;; Skip x origin, y origin
      (setf width (read-word s))
      (setf height (read-word s))
      (setf bits (read-byte s))
      (skip s (1+ length)) ;; Skip to image information
      (setf channels (ecase img-type
                       ((1 9)
                        (ecase color-map-bits
                          (8 1)
                          ((15 24) 3)
                          ((16 32) 4)))
                       ((2 10)
                        (ecase bits
                          (8 1)
                          ((15 24) 3)
                          ((16 32) 4)))
                       ((3 11)
                        1)))
      (when (plusp has-color-map)
        (assert (= has-color-map 1))
        (setf color-map (make-array (* (- color-map-end color-map-start)
                                       (ceiling color-map-bits 8))
                                    :element-type '(unsigned-byte 8)))
        (read-sequence color-map s))
      (let* ((stride (* (ceiling bits 8) width))
             (data (make-array (* height stride)
                               :element-type '(unsigned-byte 8)
                               :initial-element 255)))
        (declare (type (simple-array (unsigned-byte 8) (*)) data))
        (cond
          ((logtest img-type +TGA-RLE+)
           (read-rle data s bits))
          (t (read-sequence data s)))
        (when (or (= img-type 1) (= img-type 9))
          (setf data (map-colors data color-map bits color-map-bits
                                 color-map-start color-map-end))
          (setf bits color-map-bits))
        (setf img-data data)))
    image))

(defun read-tga (filespec)
  (with-open-file (s filespec :element-type '(unsigned-byte 8))
    (read-tga-stream s)))
