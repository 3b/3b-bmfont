(defpackage #:3b-bmfont-common
  (:use :cl)
  (:export
   #:glyph
   #:glyph-id
   #:glyph-x
   #:glyph-y
   #:glyph-width
   #:glyph-height
   #:glyph-xoffset
   #:glyph-yoffset
   #:glyph-xadvance
   #:glyph-page
   #:glyph-chnl
   #:glyph-char
   #:glyph-letter
   #:make-glyph
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
   #:remap-char
   #:make-chars-hash
   #:make-kerning-hash
   #:make-keyword
   #:filter-plist
   #:*filters*
   #:char-id
   #:padding-up
   #:padding-right
   #:padding-down
   #:padding-left))

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
           #:glyph
           #:glyph-id
           #:glyph-x
           #:glyph-y
           #:glyph-width
           #:glyph-height
           #:glyph-xoffset
           #:glyph-yoffset
           #:glyph-xadvance
           #:glyph-page
           #:glyph-chnl
           #:glyph-char
           #:glyph-letter
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
           #:padding-up
           #:padding-right
           #:padding-down
           #:padding-left
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
