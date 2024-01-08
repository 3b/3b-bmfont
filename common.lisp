(in-package :3b-bmfont-common)

(deftype v2 () '(simple-array single-float (2)))
(declaim (inline v2))
(defun v2 (x y)
  (make-array 2 :element-type 'single-float
                :initial-contents (list (coerce x 'single-float)
                                        (coerce y 'single-float))))
(defstruct (glyph (:constructor %make-glyph))
  (id 0 :type (signed-byte 32))
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  (width 0f0 :type single-float)
  (height 0f0 :type single-float)
  (xoffset 0f0 :type single-float)
  (yoffset 0f0 :type single-float)
  (xadvance 0f0 :type single-float)
  (page 0 :type (unsigned-byte 16))
  (chnl 0 :type (unsigned-byte 32))
  (char NIL :type (or null string))
  (letter NIL :type (or null string))
  (index NIL :type (or null (unsigned-byte 32)))
  (origin NIL :type (or null v2))
  (origin-y-up NIL :type (or null v2)))

(defun make-glyph (&key (id 0) (x 0f0) (y 0f0)
                     (width 0f0) (height 0f0) (xoffset 0f0) (yoffset 0f0)
                     (xadvance 0f0) (page 0) (chnl 0)
                     char letter index origin origin-y-up)
  (flet ((f (x) (coerce x 'single-float)))
    (declare (inline f))
    (%make-glyph :id id :x (f x) :y (f y)
                 :width (f width) :height (f height)
                 :xoffset (f xoffset) :yoffset (f yoffset)
                 :xadvance (f xadvance)
                 :page page :chnl chnl
                 :char char :letter letter
                 :index index
                 :origin origin :origin-y-up origin-y-up)))

(defmethod make-load-form ((glyph glyph) &optional env)
  (make-load-form-saving-slots glyph :environment env))

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
   ;; supersampling level used, 1 means no supersampling
   (aa :accessor aa :initarg :aa :initform 1)
   ;; if OUTLINE is non-zero, alpha contains an expanded version of
   ;; glyph to use for drawing outlines
   (outline :accessor outline :initarg :outline :initform 0)
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
   ;; some additional properties cached on creation
   (space-size :accessor space-size)
   (invalid-glyph :accessor invalid-glyph)
   ;; data arrays/hashes
   (pages :accessor pages :initarg :pages)
   (kernings :accessor kernings :initarg :kernings
             :initform (make-hash-table :test 'eql))
   ;; extension to bmfont spec from msdf-bmfont-xml
   (distance-field :accessor distance-field :initarg :distance-field)))

(defmethod initialize-instance :after ((o bmfont) &key distance-range)
  ;; some text format files store distance range in 'common', so fill
  ;; distance-field slot from that
  (when distance-range
    (assert (not (slot-boundp o 'distance-field)))
    (setf (distance-field o)
          (list :distance-range distance-range
                ;; no idea what type is correct here, but that
                ;; particular font has alpha, so guess that
                :field-type :msdf+a))))

(defun calculate-space-size (font)
  (let ((glyph (or (gethash #\space (chars font))
                   (gethash #\n (chars font)))))
    (if glyph
        (glyph-xadvance glyph)
        (/ (loop for c in (alexandria:hash-table-values
                           (chars font))
                 sum (or (glyph-xadvance c) 0))
           (float (hash-table-count (chars font)))))))

(defun find-invalid-glyph (font)
  (let ((chars (chars font)))
    (or (gethash :invalid chars)
        (gethash (code-char #xFFFD) chars)
        (load-time-value (make-glyph :origin (v2 0 0) :origin-y-up (v2 0 0))))))

(defun add-origins (font)
  ;; pre-calculate offsets to move glyph to baseline, accounting for
  ;; padding and Y-Axis direction
  (when font
    (loop with (nil nil down nil) = (padding font) ;; up right down left
          with chars = (chars font)
          with base = (base font)
          for c being the hash-keys of chars
            using (hash-value v)
          unless (glyph-origin v)
            do (let ((x (glyph-xoffset v))
                     (y (glyph-yoffset v)))
                 (setf (glyph-origin v) (v2 x (- y down base)))
                 (setf (glyph-origin-y-up v) (v2 x (- base (- y down)))))))
  font)

(defun update-font-properties (font)
  (setf (space-size font) (calculate-space-size font))
  (setf (invalid-glyph font) (find-invalid-glyph font))
  (add-origins font)
  font)

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
  (or (when (glyph-letter c)
        ;; possibly should store as string to allow for ligatures?
        (assert (= 1 (length (glyph-letter c))))
        (char (glyph-letter c) 0))
      (when (glyph-char c)
        (assert (= 1 (length (glyph-char c))))
        (char (glyph-char c) 0))
      ;; bmfont uses -1 for "missing character" glyph
      (if (minusp (glyph-id c))
          :invalid
          (code-char (glyph-id c)))))

(declaim (inline kerning-index))
(declaim (ftype (function (T T) (unsigned-byte 42)) kerning-index))
(defun kerning-index (lhs rhs)
  (let ((lhs (etypecase lhs
               (null 0)
               (character (char-code lhs))
               (fixnum lhs)))
        (rhs (etypecase rhs
               (null 0)
               (character (char-code rhs))
               (fixnum rhs))))
    (ldb (byte 42 0) (+ (ash lhs 21) rhs))))

(defun kerning-index-characters (idx)
  (cons (code-char (ldb (byte 21 21) idx))
        (code-char (ldb (byte 21 0) idx))))

(declaim (inline %kerning))
(declaim (ftype (function (hash-table T T) single-float) %kerning))
(defun %kerning (table lhs rhs)
  (gethash (kerning-index lhs rhs) table 0f0))

(defun (setf %kerning) (value table lhs rhs)
  (setf (gethash (kerning-index lhs rhs) table) (float value 0f0)))

(defun kerning (font lhs rhs)
  (%kerning (kernings font) lhs rhs))

(defun (setf kerning) (value font lhs rhs)
  (setf (%kerning (kernings font) lhs rhs) value))

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
          for glyph = (make-glyph :id (getf c :id)
                                  :x (getf c :x)
                                  :y (getf c :y)
                                  :width (getf c :width)
                                  :height (getf c :height)
                                  :xoffset (float (getf c :xoffset) 0f0)
                                  :yoffset (float (getf c :yoffset) 0f0)
                                  :xadvance (getf c :xadvance)
                                  :page (getf c :page)
                                  :chnl (getf c :chnl)
                                  :char (getf c :char)
                                  :letter (getf c :letter)
                                  :index (let ((i (getf c :index)))
                                           (etypecase i
                                             (string (parse-integer i))
                                             (number i)
                                             (null i))))
          do (setf (gethash (remap-char glyph) h) glyph))
    h))

(defun make-kerning-hash (info chars kernings)
  (unless (or (getf info :unicode)
              (equalp (getf info :charset) "us-ascii")
              (equalp (getf info :charset) ""))
    ;; not sure how other other charecter sets work yet, need sample
    ;; fonts to look at
    (error "only unicode and us-ascii supported currently."))
  (let ((id-hash (make-hash-table))
        (h (make-hash-table :test 'eql)))
    (loop for c being the hash-values of chars
          for id = (glyph-id c)
          do (setf (gethash id id-hash) (remap-char c)))
    (loop for k across kernings
          for c1 = (getf k :first)
          for c2 = (getf k :second)
          unless (gethash c2 id-hash) do (break "???")
            do (setf (%kerning h c1 c2) (getf k :amount)))
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

(defun sfloat (x)
  (when (stringp x)
    (setf x (parse-number:parse-number x :float-format 'single-float)))
  (if (floatp x)
      x
      (coerce x 'single-float)))

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
  (etypecase x
    ((string 1)
     x)
    (string
     (cond
       ((string-equal x "space") " ")
       (t (error "unknown character name ~s?" x))))))

(defvar *filters*
  '(:info (:size int
           :bold int-bool
           :italic int-bool
           ;; :charset string
           :unicode int-bool
           :stretch-h int
           :smooth int-bool
           :aa int
           :padding int-list
           :spacing int-list
           :outline int)
    :common (:line-height sfloat
             :base sfloat
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
           :x sfloat
           :y sfloat
           :width sfloat
           :height sfloat
           :xoffset sfloat
           :yoffset sfloat
           :xadvance sfloat
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
    (setf cc (or (glyph-letter c) (glyph-char c))))
  (cond
    ((glyph-id c))
    ((characterp cc) (char-code cc))
    ((eql cc :invalid) -1)
    ((numberp cc) cc)
    (t "error, unknown id for character ~s / ~s" cc c)))
