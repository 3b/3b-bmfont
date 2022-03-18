(in-package :3b-bmfont-common)

(defclass bmfont ()
  (;; 'info' fields
   (face :accessor face :initarg :face)
   (size :accessor size :initarg :size)
   (bold :accessor bold :initarg :bold :initform nil)
   (italic :accessor italic :initarg :italic :initform nil)
   (unicode :accessor unicode :initarg :unicode :initform t)
   (charset :accessor charset :initarg :charset :initform "")
   (stretch-h :accessor stretch-h :initarg :stretch-h :initform 100)
   (smooth :accessor smooth :initform nil :initarg :smooth)
   (aa :accessor aa :initarg :aa :initform nil)
   ;; padding =   (up, right, down, left)
   (padding :accessor padding :initform '(0 0 0 0) :initarg :padding)
   (spacing :accessor spacing :initarg :spacing :initform '(0 0))
   ;; 'common' fields
   (line-height :accessor line-height :initarg :line-height)
   (base :accessor base :initarg :base)
   (scale-w :accessor scale-w :initarg :scale-w)
   (scale-h :accessor scale-h :initarg :scale-h)
   (packed :accessor packed :initarg :packed :initform nil)
   (alpha-chnl :accessor alpha-chnl :initform :glyph :initarg :alpha-chnl)
   (red-chnl :accessor red-chnl :initform :glyph :initarg :red-chnl)
   (green-chnl :accessor green-chnl :initform :glyph :initarg :green-chnl)
   (blue-chnl :accessor blue-chnl :initform :glyph :initarg :blue-chnl)
   (chars :accessor chars :initarg :chars)
   ;; data arrays/hashes
   (pages :accessor pages :initarg :pages)
   (kernings :accessor kernings :initarg :kernings
             :initform (make-hash-table :test 'equal))
   ;; extension to bmfont spec from msdf-bmfont-xml
   (distance-field :accessor distance-field :initarg :distance-field)))


(defmethod padding-up ((f bmfont))
  (first (padding f)))
(defmethod padding-right ((f bmfont))
  (second (padding f)))
(defmethod padding-down ((f bmfont))
  (third (padding f)))
(defmethod padding-left ((f bmfont))
  (fourth (padding f)))

;;; common utilities used by parsers
(defun remap-char (c)
  ;; :letter isn't in real bmfont, but common extension, so use if
  ;; available. same for :char
  (or (when (getf c :letter)
        ;; possibly should store as string to allow for ligatures?
        (assert (= 1 (length (getf c :letter))))
        (char (getf c :letter) 0))
      (when (getf c :char)
        (assert (= 1 (length (getf c :char))))
        (char (getf c :char) 0))
      ;; bmfont uses -1 for "missing character" glyph
      (if (minusp (getf c :id))
          :invalid
          (code-char (getf c :id)))))

(defun make-chars-hash (info chars)
  (unless (or (getf info :unicode)
              (equalp (getf info :charset) "us-ascii")
              ;; assume "" is ascii/unicode
              (equalp (getf info :charset) ""))
    ;; not sure how other other charecter sets work yet, need sample
    ;; fonts to look at
    (error "only unicode and us-ascii supported currently."))
  (let ((h (make-hash-table)))
    (loop for c across chars
          do (setf (gethash (remap-char c) h) c))
    h))

(defun make-kerning-hash (info chars kernings)
  (unless (or (getf info :unicode)
              (equalp (getf info :charset) "us-ascii")
              (equalp (getf info :charset) ""))
    ;; not sure how other other charecter sets work yet, need sample
    ;; fonts to look at
    (error "only unicode and us-ascii supported currently."))
  (let ((id-hash (make-hash-table))
        (h (make-hash-table :test 'equal)))
    (loop for c across chars
          for id = (getf c :id)
          do (setf (gethash id id-hash) (remap-char c)))
    (loop for k across kernings
          for c1 = (getf k :first)
          for c2 = (getf k :second)
          for key = (cons (gethash c1 id-hash)
                          (gethash c2 id-hash))
          unless (gethash c2 id-hash) do (break "???")
            do (setf (gethash key h) (getf k :amount)))
    h))

(defun make-keyword (x)
  (if (string= x "")
      nil
      (alexandria:make-keyword
       (coerce
        (loop for c across x
              when (upper-case-p c)
                collect #\-
              collect (char-upcase c))
        'string))))

(defun filter-plist (plist mapping)
  (loop for (k v) on plist by #'cddr
        for f = (getf mapping k)
        collect k
        if f collect (funcall f v) else collect v))

(defun int (x)
  (if (stringp x) (parse-integer x) x))

(defun int-bool (x)
  (cond
    ((= (int x) 0) nil)
    ((= (int x) 1) t)
    (t (error "expected 0/1 boolean, got ~s?" x))))

(defun int-list (x)
  (if (stringp x)
      (mapcar 'parse-integer (split-sequence:split-sequence #\, x))
      x))

(defun int-chnl (x)
  (let ((i (int x)))
    (ecase i
      (0 :glyph)
      (1 :outline)
      (2 :glyph+outline)
      (3 :zero)
      (4 :one))))

(defun char0 (x)
  (assert (typep x '(string 1)))
  (char x 0))

(defvar *filters*
  '(:info (:size int
           :bold int-bool
           :italic int-bool
           ;; :charset string
           :unicode int-bool
           :stretch-h int
           :smooth int-bool
           :aa int-bool
           :padding int-list
           :spacing int-list
           :outline int)
    :common (:line-height int
             :base int
             :scale-w int
             :scale-h int
             :pages int
             :packed int-bool
             :alpha-chnl int-chnl
             :red-chnl int-chnl
             :green-chnl int-chnl
             :blue-chnl int-chnl)
    :page (:id int
           ;;:file string
           )
    :char (:id int
           :x int
           :y int
           :width int
           :height int
           :xoffset int
           :yoffset int
           :xadvance int
           :page int
           :chnl int
           :letter char0)
    :kerning (:first int
              :second int
              :amount int)
    :distance-field (:field-type make-keyword
                     :distance-range int)))

(defun char-id (c &optional cc)
  (unless cc
    (setf cc (or (getf c :letter) (getf c :char))))
  (cond
    ((getf c :id))
    ((characterp cc) (char-code cc))
    ((eql cc :invalid) -1)
    ((numberp cc) cc)
    (t "error, unknown id for character ~s / ~s" cc c)))
