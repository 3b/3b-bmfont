
(defsystem 3b-bmfont
  :description "BMFont file format readers/writers"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (alexandria split-sequence)
  :components ((:file "package")
               (:file "bmfont")))

(defsystem 3b-bmfont/text
  :description "Load/Save BMFont text format"
  :depends-on (3b-bmfont)
  :components ((:file "package")
               (:file "bmfont-text")))

(defsystem 3b-bmfont/xml
  :description "Load/Save BMFont xml format"
  :depends-on (3b-bmfont cxml split-sequence flexi-streams)
  :components ((:file "package")
               (:file "bmfont-xml")))

(defsystem 3b-bmfont/json
  :description "Load/Save BMFont-like json format"
  :depends-on (3b-bmfont jsown)
  :components ((:file "package")
               (:file "bmfont-json")))
