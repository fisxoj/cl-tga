(asdf:defsystem #:cl-tga-opengl
  :serial t
  :description "Generate an Opengl texture from a cl-tga image"
  :author "Bart Botta <00003b@gmail.com>"
  :license "MIT"
  :depends-on (:cl-opengl)
  :components ((:file "cl-tga-opengl")))
