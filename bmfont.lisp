(in-package :3b-bmfont)

#++
(ql:quickload '3b-bmfont)

(defun fs (f p)
  (let ((p (find-package p)))
    (when p
      (find-symbol (string f) p))))

(defun read-bmfont (filename)
  (with-open-file (f filename)
    (let ((c (peek-char t f nil nil)))
      (case c
        (#\<
         (let ((rf (fs '#:read-bmfont-xml '#:3b-bmfont-xml)))
           (if rf
               (funcall rf f)
               (error "can't read font metadata from ~s, xml backend not loaded"
                      filename))))
        (#\{
         (let ((rf (fs '#:read-bmfont-json '#:3b-bmfont-json)))
           (if rf
               (funcall rf f)
               (error "can't read font metadata from ~s, json backend not loaded"
                      filename))))
        (#\i
         (let ((rf (fs '#:read-bmfont-text '#:3b-bmfont-text)))
           (if rf
               (funcall rf f)
               (error "can't read font metadata from ~s, text backend not loaded"
                      filename))))
        (#\B
         (error "binary bmfont metadata format not implemented yet"))
        (t
         (error "unable to detect format of file ~s?" filename))))))

(defun write-bmfont (font filename &key (type :text))
  (ecase type
    (:text
     (let ((wf (fs '#:read-bmfont-text '#:3b-bmfont-text)))
       (with-open-file (f filename :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede)
         (funcall wf font f))))
    (:xml
     (let ((wf (fs '#:read-bmfont-xml '#:3b-bmfont-xml)))
       (with-open-file (f filename :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
         (funcall wf font f))))
    (:json
     (let ((wf (fs '#:read-bmfont-json '#:3b-bmfont-json)))
       (with-open-file (f filename :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede)
         (funcall wf font f))))))

(defun call-with-glyph-info (function font char &key model-y-up texture-y-up)
  (let ((prev NIL)
        (x 0) (y 0)
        (w (float (scale-w font)))
        (h (float (scale-h font)))
        (line (line-height font))
        (space (or (getf (gethash #\space (chars font)) :xadvance)
                   ;; try to guess a good 'space' size if font
                   ;; doesn't have space char
                   (getf (gethash #\n (chars font)) :xadvance)
                   (/ (loop for c in (alexandria:hash-table-values
                                      (chars font))
                            sum (or (getf c :xadvance) 0))
                      (float (hash-table-count (chars font))))))
        (char-map (chars font))
        (kern-map (kernings font)))
    (flet ((call-for (char)
             (let ((info (or (gethash char char-map)
                             (gethash :invalid char-map)
                             (list :xoffset 0 :yoffset 0 :x 0 :y 0
                                   :width 0 :height 0 :xadvance 0)))
                   (kern (gethash (cons prev char) kern-map 0)))
               (case char
                 (#\newline
                  (setf x 0)
                  (incf y line))
                 (#\space
                  (incf x space))
                 (#\tab
                  ;; todo: make this configurable, add tab stop option?
                  (incf x (* 8 space)))
                 (t
                  (incf x kern)
                  (let ((x- (+ x (getf info :xoffset)))
                        (y- (+ y (getf info :yoffset)))
                        (x+ (+ x (getf info :xoffset) (getf info :width)))
                        (y+ (+ y (getf info :yoffset) (getf info :height)))
                        (u- (/ (getf info :x) w))
                        (v- (/ (getf info :y) h))
                        (u+ (/ (+ (getf info :x) (getf info :width)) w))
                        (v+ (/ (+ (getf info :y) (getf info :height)) h)))
                    (incf x (getf info :xadvance))
                    (when model-y-up
                      (psetf y- (- line y+)
                             y+ (- line y-)))
                    (when texture-y-up
                      (psetf v- (- 1 v+)
                             v+ (- 1 v-)))
                    (funcall function x- y- x+ y+ u- v- u+ v+)))))))
      (loop for next = (call-for char)
            while next
            do (shiftf prev char next)))))

(defun map-glyphs (font function string &key model-y-up texture-y-up)
  (let ((i 0)
        (length (length string)))
    (when (< 0 length)
      (flet ((thunk (&rest args)
               (incf i)
               (apply function args)
               (when (< i length)
                 (char string i))))
        (call-with-glyph-info #'thunk font (char string 0) :model-y-up model-y-up :texture-y-up texture-y-up)))))

#++
(ql:quickload '3b-bmfont/xml)
#++
(map-glyphs (read-bmfont "/tmp/r2.fnt")
            (lambda (x y x2 y2 u1 v1 u2 v2)
              (format t "~s ~s : ~s ~s   @   ~s ~s : ~s ~s~%"
                      x y x2 y2 u1 v1 u2 v2))
            "testing, 1 2 3
next line	tabbed")
