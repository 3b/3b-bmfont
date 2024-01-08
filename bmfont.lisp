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
               (error "can't read font metadata from ~s,~%xml backend not loaded"
                      filename))))
        (#\{
         (let ((rf (fs '#:read-bmfont-json '#:3b-bmfont-json)))
           (if rf
               (funcall rf f)
               (error "can't read font metadata from ~s,~%json backend not loaded"
                      filename))))
        (#\i
         (let ((rf (fs '#:read-bmfont-text '#:3b-bmfont-text)))
           (if rf
               (funcall rf f)
               (error "can't read font metadata from ~s,~%text backend not loaded"
                      filename))))
        (#\B
         (error "binary bmfont metadata format not implemented yet"))
        (t
         (error "unable to detect format of file ~s?" filename))))))

(defun write-bmfont (font filename &key type)
  (let ((type (cond (type type)
                    ((string-equal "txt" (pathname-type filename)) :text)
                    ((string-equal "json" (pathname-type filename)) :json)
                    ((string-equal "xml" (pathname-type filename)) :xml)
                    (T (restart-case (error "Unknown file format ~s.~%Please specify the desired format type." (pathname-type filename))
                         (specify-type (type)
                           :interactive (lambda () (read *query-io*))
                           :report "Specify a new type."
                           type))))))
    (ecase type
      (:text
       (let ((wf (fs '#:write-bmfont-text '#:3b-bmfont-text)))
         (with-open-file (f filename :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede)
           (if wf
               (funcall wf font f)
               (error "can't write font metadata to ~s,~%text backend not loaded"
                      filename)))))
      (:xml
       (let ((wf (fs '#:write-bmfont-xml '#:3b-bmfont-xml)))
         (with-open-file (f filename :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede
                                     :element-type '(unsigned-byte 8))
           (if wf
               (funcall wf font f)
               (error "can't write font metadata to ~s,~%xml backend not loaded"
                      filename)))))
      (:json
       (let ((wf (fs '#:write-bmfont-json '#:3b-bmfont-json)))
         (with-open-file (f filename :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede)
           (if wf
               (funcall wf font f)
               (error "can't write font metadata to ~s,~%json backend not loaded"
                      filename))))))))

(defun char-data (char font &key (default (invalid-glyph font)))
  (let ((chars (chars font)))
    (or (gethash char chars)
        default)))

(defun map-glyphs (font function string &key model-y-up texture-y-up start end
                                          extra-space (x 0) (y 0))
  (declare (optimize speed)
           (type string string))
  (macrolet ((q (&body body)
               ;; don't try to optimize things known to need the slow
               ;; path
               `(locally (declare (optimize (speed 1)))
                  ,@body)))
    (flet ((f (x)
             (q (float x 1f0))))
      (declare (inline f))
      (loop with sw = (f (scale-w font))
            with sh = (f (scale-h font))
            with y single-float = (f y)
            with x single-float = (f x)
            with line = (f (line-height font))
            with space = (f (space-size font))
            with kernings = (kernings font)
            with function function = (coerce function 'function)
            with extra-space = (when extra-space (f extra-space))
            with start fixnum = (or start 0)
            with end fixnum = (or end (length string))
            for p = nil then c
            for i from start below end
            for c = (q (aref string i))
            for char = (char-data c font)
            for k = (%kerning kernings p c)
            for dxy of-type v2 = (if model-y-up
                                     (glyph-origin-y-up char)
                                     (glyph-origin char))
            for dx = (aref dxy 0) for dy = (aref dxy 1)
            do (case c
                 (#\newline
                  (setf x 0f0)
                  (incf y (if model-y-up (- line) line)))
                 (#\space
                  (incf x space))
                 (#\tab
                  ;; todo: make this configurable, add tab stop option?
                  (incf x (* 8 space)))
                 (t
                  (incf x k)
                  (let* ((x- (+ x dx))
                         (y- (+ y dy))
                         (cw (glyph-width char))
                         (ch (glyph-height char))
                         (x+ (+ x- cw))
                         (y+ (if model-y-up
                                 (- y- ch)
                                 (+ y- ch)))
                         (cx (glyph-x char))
                         (cy (glyph-y char))
                         (u- (/ cx sw))
                         (v- (/ cy sh))
                         (u+ (/ (+ cx cw) sw))
                         (v+ (/ (+ cy ch) sh)))
                    (when texture-y-up
                      (psetf v- (- 1 v-)
                             v+ (- 1 v+)))
                    (funcall function x- y- x+ y+ u- v- u+ v+))
                  (incf x (glyph-xadvance char))
                  (when extra-space (incf x extra-space))))
            finally (return (values x y))))))

(defun measure-glyphs (font string &key start end)
  (declare (optimize speed) (type string string))
  (macrolet ((q (&body body)
               ;; don't try to optimize things known to need the slow
               ;; path
               `(locally (declare (optimize (speed 1)))
                  ,@body)))
    (loop with y single-float = 0f0
          with x single-float = 0f0
          with line = (q (float (line-height font) 0f0))
          with space = (q (float (space-size font) 0f0))
          with kernings = (kernings font)
          with base = (q (float (base font) 0f0))
          with start fixnum = (or start 0)
          with end fixnum = (q (or end (length string)))
          for p = nil then c
          for i from start below end
          for c = (q (aref string i))
          for char = (char-data c font)
          for k = (%kerning kernings p c)
          do (case c
               (#\newline
                (setf x 0f0)
                (incf y line))
               (#\space
                (incf x space))
               (#\tab
                ;; todo: make this configurable, add tab stop option?
                (incf x (* 8 space)))
               (t
                (incf x k)
                (incf x (glyph-xadvance char))))
          finally (return (values x (+ y base))))))

#++
(ql:quickload '3b-bmfont/xml)
#++
(map-glyphs (read-bmfont "/tmp/r2.fnt")
            (lambda (x y x2 y2 u1 v1 u2 v2)
              (format t "~s ~s : ~s ~s   @   ~s ~s : ~s ~s~%"
                      x y x2 y2 u1 v1 u2 v2))
            "testing, 1 2 3
next line	tabbed")
