#++ (ql:quickload '(3b-bmfont parachute))
(defpackage #:3b-bmfont-test
  (:use :cl :parachute)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:b #:3b-bmfont)))
(in-package #:3b-bmfont-test)

(defun load-ref (ref)
  (with-open-file (r (asdf:system-relative-pathname '3b-bmfont ref))
    (loop for c = (read r nil r)
          until (eql c r)
          collect c)))

(defun compare-font (fnt ref &key round)
  (flet ((r (x)
           (if round
               (loop for i in x
                     collect (typecase i
                               (float (round i))
                               (string i)
                               (vector (map 'vector 'round i))
                               (t i)))
               x)))
    (loop for c being the hash-keys of (3b-bmfont:chars fnt)
            using (hash-value v)
          for a = (list (char-code c)
                        (3b-bmfont:glyph-id v)
                        (3b-bmfont:glyph-x v)
                        (3b-bmfont:glyph-y v)
                        (3b-bmfont:glyph-width v)
                        (3b-bmfont:glyph-height v)
                        (3b-bmfont:glyph-xoffset v)
                        (3b-bmfont:glyph-yoffset v)
                        (3b-bmfont:glyph-page v)
                        (3b-bmfont:glyph-chnl v)
                        (3b-bmfont:glyph-origin v)
                        (3b-bmfont:glyph-origin-y-up v)
                        (3b-bmfont:glyph-letter v)
                        (3b-bmfont:glyph-char v))
          for b = (assoc (char-code c) ref)
          do (assert (equalp (r a) (r b))
                     nil
                     "char ~a:~% ~s~% ~s~%" c a b))))

(define-test load1
  (let ((fnt (finish
              (b:read-bmfont
               (asdf:system-relative-pathname '3b-bmfont "tests/arial.fnt"))))
        (xml (finish
              (b:read-bmfont
               (asdf:system-relative-pathname '3b-bmfont "tests/arial.xml"))))
        (json (finish
               (b:read-bmfont
                (asdf:system-relative-pathname '3b-bmfont "tests/arial.json"))))
        (rref (finish (load-ref "tests/arial.rref")))
        (ref (finish (load-ref "tests/arial.ref"))))
    (finish (compare-font fnt rref :round t))
    (finish (compare-font xml rref :round t))
    (finish (compare-font json ref))))


(defun test-layout (font ref)
  (let ((s "testing
123")
        (g))
    (flet ((p (x- y- x+ y+ u- v- u+ v+)
             (push (list x- y- x+ y+ u- v- u+ v+) g)))
      (destructuring-bind (measure (ref-dd ref-ud ref-du ref-uu)) ref
        (is = measure (b:measure-glyphs font "testing123"))
        (setf g nil)
        (finish (b:map-glyphs font #'p s
                              :x 1 :y 3 :texture-y-up nil :model-y-up nil))
        (is equalp g ref-dd)
        (setf g nil)
        (finish (b:map-glyphs font #'p s
                              :x 1 :y 3 :texture-y-up t :model-y-up nil))
        (is equalp g ref-ud)
        (setf g nil)
        (finish (b:map-glyphs font #'p s
                              :x 1 :y 3 :texture-y-up nil :model-y-up t))
        (is equalp g ref-du)
        (setf g nil)
        (finish (b:map-glyphs font #'p s
                              :x 1 :y 3 :texture-y-up t :model-y-up t))
        (is equalp g ref-uu)))))

(define-test layout
  ;; layout a string and make sure we get the expected positions and
  ;; uvs
  (let ((fnt (finish
              (b:read-bmfont
               (asdf:system-relative-pathname '3b-bmfont "tests/arial.fnt"))))
        (xml (finish
              (b:read-bmfont
               (asdf:system-relative-pathname '3b-bmfont "tests/arial.xml"))))
        (json (finish
               (b:read-bmfont
                (asdf:system-relative-pathname '3b-bmfont "tests/arial.json"))))
        (ref (with-open-file (i (asdf:system-relative-pathname
                                 '3b-bmfont "tests/layout.ref"))
               (list (read i) (read i)))))
    (test-layout json (first ref))
    (test-layout fnt (second ref))
    (test-layout xml (second ref))))

(define-test write
  ;; read a file, write it to different format, and repeat for all
  ;; formats and make sure we end up with what we started with (start
  ;; with text or xml since json has floats and rounding wouldn't line
  ;; up)
  (let* ((fnt (finish
               (b:read-bmfont
                (asdf:system-relative-pathname '3b-bmfont "tests/arial.fnt"))))
         (xs (flex:with-output-to-sequence (s)
               (finish (3b-bmfont-xml:write-bmfont-xml fnt s))))
         (xf (finish (flex:with-input-from-sequence (s xs)
                       (3b-bmfont-xml:read-bmfont-xml s))))
         (js (with-output-to-string (s)
               (finish (3b-bmfont-json:write-bmfont-json xf s))))
         (jf (finish
              (with-input-from-string (s js)
                (3b-bmfont-json:read-bmfont-json s))))
         (ts (with-output-to-string (s)
               (finish (3b-bmfont-text:write-bmfont-text jf s))))
         (ref (a:read-file-into-string
               (asdf:system-relative-pathname '3b-bmfont "tests/arial.fnt")))
         #++
         (tf (with-input-from-string (s ts)
               (finish (3b-bmfont-text:read-bmfont-text s))))
         )
    (assert (string= ref ts))
    (is string= ref ts)
    )
  )
#++
(parachute:test '3b-bmfont-test :report 'interactive)
