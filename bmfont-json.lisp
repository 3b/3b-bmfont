(in-package :3b-bmfont-json)

#++(ql:quickload '3b-bmfont/json)
(defvar *font*)

(defun add-obj (tag values)
  (let* ((k (make-keyword tag))
         (atts (unless (member k '(:chars :kernings :pages))
                 (when (eql (first values) :obj)
                   (pop values))
                 (filter-plist
                  (loop for (a . v) in values
                        collect (make-keyword a)
                        collect v)
                  (getf *filters* k '(:count parse-integer))))))
    (ecase k
      ((:font))
      ((:chars :kernings)
       (assert (not (getf *font* k)))
       (setf (getf *font* k) (make-array (length values)
                                         :fill-pointer 0 :initial-element nil))
       (loop for v in values
             do (add-obj (ecase k
                           (:pages "page")
                           (:chars "char")
                           (:kernings "kerning"))
                         v)))
      (:pages
       (let ((pages (getf *font* k)))
         (cond
           (pages
            (assert (fill-pointer pages))
            (assert (<= (+ (fill-pointer pages) (length values))
                        (array-dimension pages 0)))
            (loop for p in values
                  for i from (fill-pointer pages)
                  do (vector-push-extend (list :id i :file p) pages)))
           (t
            (setf (getf *font* k)
                  (coerce
                   (loop for p in values
                         for i from 0
                         collect (list :id i :file p))
                   'vector))))))
      ((:info :common :distance-field)
       ;; should be singleton nodes
       (assert (not (getf *font* k)))
       (setf (getf *font* k) atts)
       (when (and (eq k :common)
                  (null (getf *font* :pages)))
         (setf (getf *font* :pages)
               (make-array (getf atts :pages)
                           :fill-pointer 0))))
      ((:page :char :kerning)
       ;; add to corresponding array
       (vector-push-extend
        atts
        (getf *font*
              (ecase k (:char :chars) (:page :pages) (:kerning :kernings))))))))

(defvar *j*)
(defun read-bmfont-json (stream)
  (let ((*font* (list :info nil :commmon nil :pages nil :chars nil :kerning nil :distance-field nil))
        (json
          (jsown:parse (alexandria:read-stream-content-into-string stream))))
    (setf *j* json)
    (loop for (a . b) in (cdr json)
          do (add-obj a b))
    (let* ((info (getf *font* :info))
           (chars (getf *font* :chars))
           (f (apply #'make-instance '3b-bmfont:bmfont
                     (append info
                             (getf *font* :common)))))
      ;; generator sets 'charset' to list of characters instead of
      ;; encoding name, so undo that for now so it doesn't confuse
      ;; other exporters
      (when (listp (charset f))
        (setf (charset f) ""))
      (setf (chars f)
            (make-chars-hash info chars))
      (setf (pages f) (getf *font* :pages))
      (setf (distance-field f) (getf *font* :distance-field))
      (setf (kernings f)
            (make-kerning-hash info (chars f)
                               (getf *font* :kernings)))
      f)))

(defun write-bmfont-json (f stream)
  (let ((ac #(:glyph :outline :glyph+outline :zero :one))
        (json (jsown:empty-object)))
    (flet ((b (x)
             (if x 1 0)))
      (jsown:extend-js
          json
        ("info"
         (jsown:new-js
           ("face"(face f))
           ("size"(size f))
           ("bold"(b (bold f)))
           ("italic"(b (italic f)))
           ;; json format seems to have list of characters here, so
           ;; try to match that
           ("charset"
            (mapcar 'string
                    (sort (remove-if-not 'characterp
                                         (alexandria:hash-table-keys (chars f)))
                          'char<)))
           ("unicode" (b (unicode f)))
           ("stretchH"(stretch-h f))
           ("smooth"(b (smooth f)))
           ("aa"(b (aa f)))
           ("padding"(padding f))
           ("spacing"(spacing f))))
        ("common"
         (jsown:new-js
           ("lineHeight" (line-height f))
           ("base" (base f))
           ("scaleW" (scale-w f))
           ("scaleH"(scale-h f))
           ("pages"(length (pages f)))
           ("packed"(b (packed f)))
           ("alphaChnl"(position (alpha-chnl f) ac))
           ("redChnl"(position (red-chnl f) ac))
           ("greenChnl"(position (green-chnl f) ac))
           ("blueChnl"(position (blue-chnl f) ac))))
        ("pages"
         (loop for p across (pages f)
               for i from 0
               when (getf p :id) do (assert (= (getf p :id) i))
                 collect (getf p :file)))

        ("chars"
         (loop
           for c in (sort (alexandria:hash-table-values
                           (chars f))
                          '<
                          :key (lambda (a) (char-id a)))
           for j = (jsown:new-js
                     ("id"(char-id c))
                     ("x"(glyph-x c))
                     ("y"(glyph-y c))
                     ("width"(glyph-width c))
                     ("height"(glyph-height c))
                     ("xoffset"(glyph-xoffset c))
                     ("yoffset"(glyph-yoffset c))
                     ("xadvance"(glyph-xadvance c))
                     ("page"(glyph-page c))
                     ("chnl"(glyph-chnl c)))
           ;; non-standard
           when (glyph-index c)
             do (jsown:extend-js j ("index"(glyph-index c)))
           when (glyph-char c)
             do (jsown:extend-js j ("char"(glyph-char c)))
           when (glyph-letter c)
             do (jsown:extend-js j ("letter"(glyph-letter c)))
           collect j))
        ("kernings"
         (flet ((id (x)
                  (char-id (gethash x (chars f)) x)))
           (loop
             for ((c1 . c2) . a) in (sort (alexandria:hash-table-alist
                                           (kernings f))
                                          (lambda (a b)
                                            (if (= (id (caar a))
                                                   (id (caar b)))
                                                (< (id (cdar a))
                                                   (id (cdar b)))
                                                (< (id (caar a))
                                                   (id (caar b))))))
             collect (jsown:new-js
                       ("first"(id c1))
                       ("second"(id c2))
                       ("amount" a))))))
      (when (distance-field f)
        (jsown:extend-js
            json
          ("distanceField"
           (jsown:new-js
             ("fieldType" (string-downcase
                           (getf (distance-field f) :field-type "")))
             ("distanceRange"
              (getf (distance-field f) :distance-range 0))))))
      (format
       stream
       "~a"
       (jsown:to-json json)))))

#++
(with-open-file (s2 "/tmp/r2.fnt" :direction :output
                                  :if-does-not-exist :create
                                  :if-exists :supersede
                                  :element-type 'character)
  (let ((f (with-open-file (s (asdf:system-relative-pathname
                               'sdf-test
                               "fonts/Roboto-msdf.json"))
             (read-bmfont-json s))))
    (write-bmfont-json f s2)
    f))
