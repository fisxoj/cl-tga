(defpackage #:cl-tga-opengl
  (:use #:cl)
  (:nicknames #:tga-gl)
  (:export #:tex-image-2d))

(in-package #:cl-tga-opengl)


(defun tex-image-2d (image &key (target :texture-2d)
                             (level 0) (internal-format :rgba))
  (gl:tex-image-2d target
                   level
                   internal-format
                   (tga:image-width image) (tga:image-height image)
                   0
                   (ecase (tga:image-channels image)
                     (1 :luminance)
                     (3 :bgr)
                     (4 :bgra))
                   (ecase (tga:image-bpp image)
                     ((8 24 32) :unsigned-byte)
                     ((15 16) :unsigned-short-1-5-5-5-rev))
                   (tga:image-data image)))
