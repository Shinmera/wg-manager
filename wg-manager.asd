(asdf:defsystem wg-manager
  :components ((:file "wg-manager"))
  :build-operation "program-op"
  :build-pathname "wg-manager"
  :entry-point "wg-manager::main"
  :depends-on (:postmodern
               :cl-qrencode
               :zippy
               :drakma))
