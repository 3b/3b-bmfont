(in-package :3b-bmfont-xml)

;;; writer/translator for bmfont xml format
#++
(ql:quickload '3b-bmfont/xml)

(defvar *font*)
(defclass bmfont-xml (sax:default-handler)
  ((f :initform nil :accessor f)))

(defmethod sax:start-document ((h bmfont-xml))
  (setf *font*
        (list :info nil :commmon nil :pages nil :chars nil :kerning nil :distance-field nil)))


(defmethod sax:start-element ((h bmfont-xml) namespace local-name qname attributes)
  (let* ((k (make-keyword local-name))
         (atts (filter-plist
                (loop for a in attributes
                      collect (make-keyword (sax:attribute-local-name a))
                      collect (sax:attribute-value a))
                (getf *filters* k '(:count parse-integer)))))
    (ecase k
      ((:font))
      ((:chars :pages :kernings)
       (assert (not (getf *font* k)))
       (setf (getf *font* k) (make-array (getf atts :count 1)
                                         :fill-pointer 0 :initial-element nil
                                         :adjustable (not (getf atts :count)))))
      ((:info :common :distance-field)
       ;; should be singleton nodes
       (assert (not (getf *font* k)))
       (setf (getf *font* k) atts))
      ((:page :char :kerning)
       ;; add to corresponding array
       (vector-push-extend
        atts
        (getf *font*
              (ecase k (:char :chars) (:page :pages) (:kerning :kernings))))))))

(defmethod sax:end-document ((b bmfont-xml))
  (let ((info (getf *font* :info))
        (chars (getf *font* :chars)))
    (setf (f b)
          (apply #'make-instance 'bmfont
                 (append info  (getf *font* :common))))
    (setf (chars (f b))
          (3b-bmfont::make-chars-hash info chars))
    (setf (pages(f b)) (getf *font* :pages))
    (setf (distance-field (f b)) (getf *font* :distance-field))
    (setf (kernings (f b))
          (3b-bmfont::make-kerning-hash info chars
                                        (getf *font* :kernings)))))

(defun read-bmfont-xml (stream)
  (flet ((resolver (pubid sysid)
           (declare (ignore pubid sysid))
           (flexi-streams:make-in-memory-input-stream nil)))
    (let* ((*font* nil)
           (b (make-instance 'bmfont-xml)))
      (cxml:parse-file
       stream
       b
       :entity-resolver #'resolver)
      (f b))))

(defun write-bmfont-xml (f stream)
  (let ((ac #(:glyph :outline :glyph+outline :zero :one)))
    (cxml:with-xml-output (cxml:make-octet-stream-sink stream
                                                       :indentation 4
                                                       :canonical nil)
      (flet ((b (x)
               (if x 1 0))
             (f (x)
               (if (and (rationalp x)
                        (not (integerp x)))
                   (float x)
                   x)))
        (cxml:with-element "font"
          (cxml:with-element "info"
            (cxml:attribute "face" (face f))
            (cxml:attribute "size" (f (size f)))
            (cxml:attribute "bold" (b (bold f)))
            (cxml:attribute "italic" (b (italic f)))
            (cxml:attribute "charset" (charset f))
            (cxml:attribute "unicode" (b (unicode f)))
            (cxml:attribute "stretchH" (stretch-h f))
            (cxml:attribute "smooth" (b (smooth f)))
            (cxml:attribute "aa" (b (aa f)))
            (cxml:attribute "padding"
                            (format nil "~{~a~^,~}" (padding f)))
            (cxml:attribute "spacing"
                            (format nil "~{~a~^,~}" (spacing f))))
          (cxml:with-element "common"
            (cxml:attribute "lineHeight" (f (line-height f)))
            (cxml:attribute "base" (f (base f)))
            (cxml:attribute "scaleW" (f (scale-w f)))
            (cxml:attribute "scaleH" (f (scale-h f)))
            (cxml:attribute "pages" (length (pages f)))
            (cxml:attribute "packed" (b (packed f)))
            (cxml:attribute "alphaChnl"
                            (position (alpha-chnl f) ac))
            (cxml:attribute "redChnl"
                            (position (red-chnl f) ac))
            (cxml:attribute "greenChnl"
                            (position (green-chnl f) ac))
            (cxml:attribute "blueChnl"
                            (position (blue-chnl f) ac)))
          (cxml:with-element "pages"
            (loop for p across (pages f)
                  for i from 0
                  do (cxml:with-element "page"
                       ;; not sure if ID needs to be equal to position in
                       ;; list or not? use specified value if any
                       (cxml:attribute "id" (or (getf p :id) i))
                       (cxml:attribute "file" (getf p :file)))))
          ;; non-standard extension
          (when (distance-field f)
            (cxml:with-element "distanceField"
              (cxml:attribute "fieldType"
                              (string-downcase
                               (getf (distance-field f) :field-type)))
              (cxml:attribute "distanceRange"
                              (getf (distance-field f) :distance-range))))
          (cxml:with-element "chars"
            (cxml:attribute "count" (hash-table-count (chars f)))
            (loop for c in (sort (alexandria:hash-table-values
                                  (chars f))
                                 '<
                                 :key (lambda (a) (getf a :id)))
                  do (cxml:with-element "char"
                       (cxml:attribute "id" (char-id c))
                       ;; non-standard
                       (cxml:attribute "index" (getf c :index))
                       ;; non-standard
                       (cxml:attribute "char" (when (getf c :char)
                                                (string (getf c :char))))
                       (cxml:attribute "width" (f (getf c :width)))
                       (cxml:attribute "height" (f (getf c :height)))
                       (cxml:attribute "xoffset" (f (getf c :xoffset)))
                       (cxml:attribute "yoffset" (f (getf c :yoffset)))
                       (cxml:attribute "xadvance" (f (getf c :xadvance)))
                       (cxml:attribute "chnl" (getf c :chnl))
                       (cxml:attribute "x" (f (getf c :x)))
                       (cxml:attribute "y" (f (getf c :y)))
                       (cxml:attribute "page" (getf c :page))
                       ;; non-standard
                       (cxml:attribute "letter" (when (getf c :letter)
                                                  (string (getf c :letter)))))))
          (cxml:with-element "kernings"
            (cxml:attribute "count" (hash-table-count (kernings f)))
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
                do (cxml:with-element "kerning"
                     (cxml:attribute "first" (id c1))
                     (cxml:attribute "second" (id c2))
                     (cxml:attribute "amount" a))))))))))

#++
(with-open-file (s2 "/tmp/r2.fnt" :direction :output
                                  :if-does-not-exist :create
                                  :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
  (let ((f (with-open-file (s "/tmp/Roboto-Regular.fnt")
             (read-bmfont-xml s))))
    (write-bmfont-xml f s2)
    f))

