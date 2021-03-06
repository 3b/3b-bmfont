(defpackage #:3b-bmfont-common
  (:use :cl)
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
           #:remap-char
           #:make-chars-hash
           #:make-kerning-hash
           #:make-keyword
           #:filter-plist
           #:*filters*
           #:char-id))

;; fixme: move these to separate files?
(defpackage #:3b-bmfont-xml
  (:use :cl #:3b-bmfont-common)
  (:export #:read-bmfont-xml #:write-bmfont-xml))

(defpackage #:3b-bmfont-text
  (:use :cl #:3b-bmfont-common)
  (:export #:read-bmfont-text #:write-bmfont-text))

(defpackage #:3b-bmfont-json
  (:use :cl #:3b-bmfont-common)
  (:export #:read-bmfont-json #:write-bmfont-json))



;; main API package
(defpackage #:3b-bmfont
  (:use :cl #:3b-bmfont-common)
  (:export #:read-bmfont
           #:write-bmfont
           #:bmfont
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
           #:map-glyphs
           #:measure-glyphs))
