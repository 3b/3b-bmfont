(in-package :3b-bmfont-text)

;;; writer/translator for bmfont text format
#++
(ql:quickload '3b-bmfont/text)

(defvar *font*)

(defun add-line (tag values)
  (let* ((k (make-keyword tag))
         (atts (filter-plist
                (loop for (a v) on values by #'cddr
                      collect (make-keyword a)
                      collect v)
                (getf *filters* k '(:count parse-integer)))))
    (ecase k
      ((:font))
      ((:chars :kernings)
       (assert (not (getf *font* k)))
       (setf (getf *font* k) (make-array (max (getf atts :count 1) 0)
                                         :fill-pointer 0 :initial-element nil
                                         :adjustable (not (getf atts :count)))))
      ((:info :common :distance-field)
       ;; should be singleton nodes
       (assert (not (getf *font* k)))
       (setf (getf *font* k) atts)
       (when (eq k :common)
         (setf (getf *font* :pages)
               (make-array (getf atts :pages)
                           :fill-pointer 0))))
      ((:page :char :kerning)
       ;; add to corresponding array
       (vector-push-extend
        atts
        (getf *font*
              (ecase k (:char :chars) (:page :pages) (:kerning :kernings))))))))

(defun tokenize-line (line)
  (let ((s 0)
        (white-space #(#\space #\tab #\newline #\return #\linefeed)))
    (labels ((until (chars)
               (coerce
                (loop with s1 = s
                      with escape = nil
                      for i from s below (length line)
                      for c = (aref line i)
                      until (and (not escape)
                                 (position c chars))
                      do (setf escape (and (not escape) (eql c #\\)))
                      unless escape
                        collect c
                      finally (setf s (1+ i)))
                'string))
             (skip-white ()
               (loop while (and (array-in-bounds-p line s)
                                (position (char line s) white-space))
                     do (incf s)))
             (tag ()
               (skip-white)
               (until white-space))
             (key ()
               (skip-white)
               (until "="))
             (value ()
               (if (char= (char line s) #\")
                   (progn (incf s) (until "\""))
                   (until white-space))))
      (list* (tag)
             (loop while (< s (length line))
                   collect (key)
                   collect (value))))))


(defun read-bmfont-text (stream)
  (let ((*font* (list :info nil :commmon nil :pages nil :chars nil :kerning nil :distance-field nil)))
    (loop for line = (read-line stream nil nil)
          for tokens = (when (and line (not (alexandria:emptyp line)))
                         (tokenize-line line))
          while line
          do (add-line (first tokens) (rest tokens)))
    (let* ((info (getf *font* :info))
           (chars (getf *font* :chars))
           (f (apply #'make-instance '3b-bmfont:bmfont
                     (append info
                             (getf *font* :common)))))
      (setf (chars f)
            (make-chars-hash info chars))
      (setf (pages f) (getf *font* :pages))
      (setf (distance-field f) (getf *font* :distance-field))
      (setf (kernings f)
            (make-kerning-hash info (chars f)
                               (getf *font* :kernings)))
      f)))

(defun write-bmfont-text (f stream)
  (let ((ac #(:glyph :outline :glyph+outline :zero :one))
        (round nil))
    (flet ((b (x)
             (if x 1 0))
           (f (x)
             (cond
               ((integerp x)
                x)
               (round
                (round x))
               (t
                (with-simple-restart (continue
                                      "Round non-integral values")
                  (error "float values not supported in text and xml"))
                (setf round t)
                (round x))))
           (escape (s)
             (let ((escapes '(#\" #\\)))
               (when (position-if (alexandria:rcurry #'member escapes) s)
                 (coerce
                  (loop for i across s
                        when (member i escapes)
                          collect #\\
                        collect i)
                  'string)))))
      (format stream
              "info face=~s size=~a bold=~a italic=~a charset=~s unicode=~a ~
              stretchH=~a smooth=~a aa=~a padding=~{~a~^,~} spacing=~{~a~^,~}~%"
              (face f)
              (f (size f))
              (b (bold f))
              (b (italic f))
              (charset f)
              (b (unicode f))
              (f (stretch-h f))
              (b (smooth f))
              (b (aa f))
              (padding f)
              (spacing f))
      (format stream
              "common lineHeight=~a base=~a scaleW=~a scaleH=~a pages=~a ~
             packed=~a~@[ alphaChnl=~a~]~@[ redChnl=~a~]~@[ greenChnl=~a~]~
             ~@[ blueChnl=~a~]~%"
              (f (line-height f))
              (f (base f))
              (f (scale-w f))
              (f (scale-h f))
              (length (pages f))
              (b (packed f))
              (position (alpha-chnl f) ac)
              (position (red-chnl f) ac)
              (position (green-chnl f) ac)
              (position (blue-chnl f) ac))
      (loop for p across (pages f)
            for i from 0
            do (format stream "page id=~a file=~s~%"
                       (or (getf p :id) i)
                       (getf p :file)))
      ;; non-standard extension
      ;; not sure if anyone uses this in text format?
      (when (distance-field f)
        (format stream "distanceField fieldType=~s distanceRange=~a~%"
                (string-downcase
                 (getf (distance-field f) :field-type))
                (getf (distance-field f) :distance-range)))
      (format stream "chars count=~a~%"
              (hash-table-count (chars f)))
      (loop
        for c in (sort (alexandria:hash-table-values
                        (chars f))
                       '<
                       :key (lambda (a) (char-id a)))
        do (format stream "char id=~a x=~a y=~a~@[ index=~a~]~@[ char=\"~a\"~] ~
                           width=~a height=~a xoffset=~a yoffset=~a ~
                           xadvance=~a page=~a chnl=~a~
                           ~@[ letter=\"~c\"~]~%"
                   (char-id c)
                   (f (glyph-x c))
                   (f (glyph-y c))
                   ;; non-standard
                   (glyph-index c)
                   ;; non-standard
                   (escape (glyph-char c))
                   (f (glyph-width c))
                   (f (glyph-height c))
                   (f (glyph-xoffset c))
                   (f (glyph-yoffset c))
                   (f (glyph-xadvance c))
                   (glyph-page c)
                   (glyph-chnl c)
                   ;; non-standard
                   (escape (glyph-letter c))))
      (format stream "kernings count=~a~%" (hash-table-count (kernings f)))
      (flet ((id (x)
               (char-id (gethash x (chars f)) x)))
        (loop
          for (kidx . a) in (sort (alexandria:hash-table-alist
                                   (kernings f))
                                  (lambda (a b)
                                    (destructuring-bind (a1 . a2) (kerning-index-characters (car a))
                                      (destructuring-bind (b1 . b2) (kerning-index-characters (car b))
                                        (if (= (id a1) (id b1))
                                            (< (id a2) (id b2))
                                            (< (id a1) (id a2)))))))
          for (c1 . c2) = (kerning-index-characters kidx)
          do (format stream "kerning first=~a second=~a amount=~a~%"
                     (id c1)
                     (id c2)
                     (f a)))))))

#++
(with-open-file (s2 "/tmp/r2.fnt" :direction :output
                                  :if-does-not-exist :create
                                  :if-exists :supersede
                                  :element-type 'character)
  (let ((f (with-open-file (s (asdf:system-relative-pathname
                               'sdf-test
                               "fonts/DejaVu-sdf.fnt"))
             (read-bmfont-text s))))
    (write-bmfont-text f s2)
    f))

