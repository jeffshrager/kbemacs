;;;; /tmp/lets/package.lisp

(defpackage :lets
  (:use :cl)
  (:export :defunS :letS :letS* :done :&sequence :&unitary :mapS :previouS :scanS :reduceS
           :generateS :filterS :truncateS :enumerateS :at-start :at-end :at-unwind
           :Pvalue :Rlast :Gsequence :Glist :Gsublists :Elist :Esublists :Grange
           :Erange :Eplist :Evector :Efile :Elist* :Ealist :Fpositive :Fgreater :Fselect :Tselect
           :Rignore :Rlist :Rbag :Rlist* :Rplist :Rnconc :Rappend :Rset :Reqset :Ralist
           :Reqalist :Rvector :Rfile :Rsum :Rsum$ :Rmax :Rmin :Rcount :Rand :Rand-fast 
           :Ror :Ror-fast))

