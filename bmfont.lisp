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


(defun map-glyphs (font function string &key y-up)
  (loop with w = (float (scale-w font))
        with h = (float (scale-h font))
        with y = (if y-up
                     (- (base font))
                     (base font))
        with x = 0
        with space = (or (getf (gethash #\space (chars font)) :xadvance)
                         ;; try to guess a good 'space' size if font
                         ;; doesn't have space char
                         (getf (gethash #\n (chars font)) :xadvance)
                         (/ (loop for c in (alexandria:hash-table-values
                                            (chars font))
                                  sum (or (getf c :xadvance) 0))
                            (float (hash-table-count (chars font)))))
        for p = nil then c
        for c across string
        for char = (or (gethash c (chars font))
                       (gethash :invalid (chars font))
                       (list :xoffset 0 :yoffset 0 :x 0 :y 0
                             :width 0 :height 0 :xadvance 0))
        for k = (gethash (cons p c) (kernings font) 0)
        do (unless (zerop k)
             (format t "kerning ~s ~s = ~s~%" p c k))
           (case c
             (#\newline
              (setf x 0)
              (incf y (if y-up
                          (- (line-height font))
                          (line-height font))))
             (#\space
              (incf x space))
             (#\tab
              ;; todo: make this configurable, add tab stop option?
              (incf x (* 8 space)))
             (t
              (incf x k)
              (funcall function
                       (+ x (getf char :xoffset))
                       (if y-up
                           (+ y (- (getf char :yoffset))
                              (- (getf char :height)))
                           (+ y (getf char :yoffset)))
                       (+ x (getf char :xoffset)
                          (getf char :width))
                       (if y-up
                           (+ y (- (getf char :yoffset)))
                           (+ y (getf char :yoffset) (getf char :height)))
                       (/ (getf char :x) w) (/ (getf char :y) h)
                       (/ (+ (getf char :x) (getf char :width))
                          w)
                       (/ (+ (getf char :y) (getf char :height))
                          h))
              (incf x (getf char :xadvance))))))


#++
(ql:quickload '3b-bmfont/xml)
#++
(map-glyphs (read-bmfont "/tmp/r2.fnt")
            (lambda (x y x2 y2 u1 v1 u2 v2)
              (format t "~s ~s : ~s ~s   @   ~s ~s : ~s ~s~%"
                      x y x2 y2 u1 v1 u2 v2))
            "testing, 1 2 3
next line	tabbed")
