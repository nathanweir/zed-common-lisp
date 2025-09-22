;; (ql:quickload :cl-opengl)
;; (ql:quickload :glfw)
;; (ql:quickload :pngload)

(defsystem "cl-opengl-test"
    :description "Test 123"
    :author "Nathan Weir"
    :license "MIT"
    :version "0.0.1"
    :depends-on (
        "cl-opengl"
        "glfw"
        "pngload"
    )
    :components ((:file "main")))
