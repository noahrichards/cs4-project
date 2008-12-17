(defpackage :com.noahsmark.cs4project
  (:use :common-lisp :ext)
  (:nicknames "The CS4 project, ported to common lisp"))

(in-package :com.noahsmark.cs4project)

(load (compile-file "puzzle.lisp"))
(load (compile-file "solver.lisp"))
(load (compile-file "unittestfm.lisp"))
(load (compile-file "clock.lisp"))
(load (compile-file "change.lisp"))
(load (compile-file "slide.lisp"))
(load (compile-file "water.lisp"))
(load (compile-file "alltests.lisp"))