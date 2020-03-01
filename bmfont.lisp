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
     (let ((wf (fs '#:write-bmfont-text '#:3b-bmfont-text)))
       (with-open-file (f filename :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede)
         (funcall wf font f))))
    (:xml
     (let ((wf (fs '#:write-bmfont-xml '#:3b-bmfont-xml)))
       (with-open-file (f filename :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
         (funcall wf font f))))
    (:json
     (let ((wf (fs '#:write-bmfont-json '#:3b-bmfont-json)))
       (with-open-file (f filename :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede)
         (funcall wf font f))))))

(defun space-size (font)
  (or (getf (gethash #\space (chars font)) :xadvance)
      ;; try to guess a good 'space' size if font
      ;; doesn't have space char
      (getf (gethash #\n (chars font)) :xadvance)
      (/ (loop for c in (alexandria:hash-table-values
                         (chars font))
               sum (or (getf c :xadvance) 0))
         (float (hash-table-count (chars font))))))

(defun char-data (char font)
  (let ((chars (chars font)))
    (or (gethash char chars)
        (gethash :invalid chars)
        (gethash (code-char #xFFFD) chars)
        (list :xoffset 0 :yoffset 0 :x 0 :y 0
              :width 0 :height 0 :xadvance 0))))

(defun map-glyphs (font function string &key model-y-up texture-y-up start end)
  (loop with w = (float (scale-w font))
        with h = (float (scale-h font))
        with y = 0
        with x = 0
        with line = (line-height font)
        with space = (space-size font)
        for p = nil then c
        for i from (or start 0) below (or end (length string))
        for c = (aref string i)
        for char = (char-data c font)
        for k = (gethash (cons p c) (kernings font) 0)
        do (case c
             (#\newline
              (setf x 0)
              (incf y line))
             (#\space
              (incf x space))
             (#\tab
              ;; todo: make this configurable, add tab stop option?
              (incf x (* 8 space)))
             (t
              (incf x k)
              (let ((x- (+ x (getf char :xoffset)))
                    (y- (+ y (getf char :yoffset)))
                    (x+ (+ x (getf char :xoffset) (getf char :width)))
                    (y+ (+ y (getf char :yoffset) (getf char :height)))
                    (u- (/ (getf char :x) w))
                    (v- (/ (getf char :y) h))
                    (u+ (/ (+ (getf char :x) (getf char :width)) w))
                    (v+ (/ (+ (getf char :y) (getf char :height)) h)))
                (when model-y-up
                  (psetf y- (- line y+)
                         y+ (- line y-)))
                (when texture-y-up
                  (psetf v- (- 1 v+)
                         v+ (- 1 v-)))
                (funcall function x- y- x+ y+ u- v- u+ v+))
              (incf x (getf char :xadvance))))))

(defun measure-glyphs (font string &key start end)
  (loop with y = 0
        with x = 0
        with line = (line-height font)
        with space = (space-size font)
        for p = nil then c
        for i from (or start 0) below (or end (length string))
        for c = (aref string i)
        for char = (char-data c font)
        for k = (gethash (cons p c) (kernings font) 0)
        do (case c
             (#\newline
              (setf x 0)
              (incf y line))
             (#\space
              (incf x space))
             (#\tab
              ;; todo: make this configurable, add tab stop option?
              (incf x (* 8 space)))
             (t
              (incf x k)
              (incf x (getf char :xadvance))))
        finally (return (values x (+ y (base font))))))

#++
(ql:quickload '3b-bmfont/xml)
#++
(map-glyphs (read-bmfont "/tmp/r2.fnt")
            (lambda (x y x2 y2 u1 v1 u2 v2)
              (format t "~s ~s : ~s ~s   @   ~s ~s : ~s ~s~%"
                      x y x2 y2 u1 v1 u2 v2))
            "testing, 1 2 3
next line	tabbed")
