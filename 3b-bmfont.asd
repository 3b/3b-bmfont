
(defsystem 3b-bmfont
  :description "BMFont file format readers/writers"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  ;; currently only load text backend by default, will use others
  ;; if loaded
  :depends-on (3b-bmfont/text)
  :components ((:file "package")
               (:file "bmfont"))
  :in-order-to ((test-op (test-op 3b-bmfont/tests))))

(defsystem 3b-bmfont/common
  :depends-on (alexandria split-sequence parse-number)
  :components ((:file "package")
               (:file "common")))

(defsystem 3b-bmfont/text
  :description "Load/Save BMFont text format"
  :depends-on (3b-bmfont/common)
  :components ((:file "package")
               (:file "bmfont-text")))

(defsystem 3b-bmfont/xml
  :description "Load/Save BMFont xml format"
  :depends-on (3b-bmfont/common cxml split-sequence flexi-streams)
  :components ((:file "package")
               (:file "bmfont-xml")))

(defsystem 3b-bmfont/json
  :description "Load/Save BMFont-like json format"
  :depends-on (3b-bmfont/common jsown)
  :components ((:file "package")
               (:file "bmfont-json")))

(defsystem "3b-bmfont/tests"
  :depends-on ("3b-bmfont/text"
               "3b-bmfont/xml"
               "3b-bmfont/json"
               "parachute")
  :serial t
  :components ((:file "test"))
  :perform (test-op (op c)
                    (declare (ignore op c))
                    (or
                     (symbol-call "PARACHUTE" "TEST"'#:3b-bmfont-test)
                     (error "tests failed"))))
