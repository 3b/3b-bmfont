(defpackage #:3b-bmfont
  (:use :cl)
  ;; temp hack until packages are split or otherwise cleaned up
  (:intern #:3b-bmfont
           #:remap-char
           #:make-chars-hash
           #:make-kerning-hash
           #:make-keyword
           #:filter-plist
           #:*filters*
           #:char-id)
  (:export #:bmfont
           #:info
           #:common
           #:kernings
           #:chars
           #:pages
           #:distance-field
           #:face
           #:size
           #:bold
           #:italic
           #:unicode
           #:charset
           #:stretch-h
           #:smooth
           #:aa
           #:padding
           #:spacing
           #:line-height
           #:base
           #:scale-w
           #:scale-h
           #:packed
           #:alpha-chnl
           #:red-chnl
           #:green-chnl
           #:blue-chnl
           ))

;; fixme: move these to separate files
(defpackage #:3b-bmfont-xml
  (:use :cl #:3b-bmfont)
  (:import-from #:3b-bmfont
                #:remap-char
                #:make-chars-hash
                #:make-kerning-hash
                #:make-keyword
                #:filter-plist
                #:*filters*
                #:char-id)
  (:export #:read-bmfont-xml #:write-bmfont-xml))

(defpackage #:3b-bmfont-text
  (:use :cl #:3b-bmfont)
  (:import-from #:3b-bmfont
                #:remap-char
                #:make-chars-hash
                #:make-kerning-hash
                #:make-keyword
                #:filter-plist
                #:*filters*
                #:char-id)
  (:export #:read-bmfont-text #:write-bmfont-text))

(defpackage #:3b-bmfont-json
  (:use :cl #:3b-bmfont)
  (:import-from #:3b-bmfont
                #:remap-char
                #:make-chars-hash
                #:make-kerning-hash
                #:make-keyword
                #:filter-plist
                #:*filters*
                #:char-id)
  (:export #:read-bmfont-json #:write-bmfont-json))
