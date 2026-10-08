(in-package :cl)
(defpackage :lets-tests
  (:use :cl :lets :rtest)
  (:import-from :lets
                . #.(let (ans)
                      (do-symbols (s :lets)
                        (when (eq (symbol-package s)
                                  (find-package :lets))
                          (push (make-symbol (string s)) ans)))
                      ans)))

(in-package :lets-tests)

(rem-all-tests)

(deftest :memq.0
  (memq 'a '(a b c d))
  (a b c d))

(deftest :assq.0
  (assq 'a '((a . 1) (b . 2) (c . 3) (d . 4)))
  (a . 1))

(deftest :putprop.0
  (progn
    (putprop 'foo 3 'lets-tests)
    (get 'foo 'lets-tests))
  3)

(deftest :comment.0
  (comment a b c d e f)
  comment)

(deftest :gf.0
  (with-output-to-string (out)
    (let ((*standard-output* out))
      (gf :dummy 42)))
  "42
")

#|(deftest :s-frag.0
  )|#

(deftest :s-new-var.0
  (let ((*gensym-counter* 0))
    (format nil "~A" (s-new-var 'g)))
  "G0")

#|(deftest :s-b)|#

(deftest :s-eq-car.0
  (s-eq-car '(a b c d)
                  'a)
  t)

(deftest :s-eq-car.1
  (s-eq-car '(a b c d)
                  'b)
  nil)

(deftest :s-variablep.0
  (s-variablep nil)
  nil)

(deftest :s-variablep.1
  (s-variablep "a")
  nil)

(deftest :s-variablep.2
  (s-variablep 'a)
  t)

(deftest :s-variablep.3
  (s-variablep t)
  nil)

(deftest :s-copyable-constant.0
  (s-copyable-constant 1)
  t)

(deftest :s-copyable-constant.1
  (s-copyable-constant "1")
  t)

(deftest :s-copyable-constant.2
  (s-copyable-constant t)
  (t nil))

(deftest :s-copyable-constant.3
  (s-copyable-constant nil)
  (nil))

(deftest :s-copyable-constant.4
  (s-copyable-constant '#'foo)
  t)

(deftest :s-copyable-constant.5
  (s-copyable-constant ''a)
  t)

;; '(&input &optional &rest &aux &output &flag)
(deftest :kind-order.0
  (kind-order '&input '&optional)
  (&optional &rest &aux &output &flag))

(deftest :kind-order.1
  (kind-order '&optional '&input)
  nil)

(deftest :s-kind.0
  (s-kind '(kind mode var info))
  kind)

(deftest :s-mode.0
  (s-mode '(kind mode var info))
  mode)

(deftest :s-var.0
  (s-var '(kind mode var info))
  var)

(deftest :s-info.0
  (s-info '(kind mode var info))
  info)

(deftest :kind-order.1
  (sort (reverse '((&input 1)
                 (&optional 2)
                 (&rest 3)
                 (&aux 4)
                 (&output 5)
                 (&flag 6)))
        #'kind-order :key #'car)
  ((&input 1) (&optional 2) (&rest 3) (&aux 4) (&output 5)
   (&flag 6)))

;; &inputと&outputしかないのでは?
(deftest :s-compress-arg-list.00
  (s-compress-arg-list '&input
                       '((&input &unitary foo nil)
                         (&input &unitary bar nil)
                         (&input &unitary baz nil)))
  (foo bar baz))

(deftest :s-compress-arg-list.01
  (s-compress-arg-list '&output
                       '((&output &unitary foo nil)
                         (&output &unitary bar nil)
                         (&output &unitary baz nil)))
  (foo bar baz))

(deftest :s-compress-arg-list.0
  (s-compress-arg-list '&input
                             '((&input mode var info)
                               (&optional mode var info)
                               (&rest mode var info)
                               (&aux mode var info)
                               (&output mode var info)
                               (&flag mode var info)))
  (mode var
   &optional (var info)
   &rest var
   &aux var
   &output var
   &flag (var info)))

(deftest :s-compress-arg-list.1
  (s-compress-arg-list '&input
                       '((&input mode var info)
                         (&optional mode var info)
                         (&rest mode var info)
                         (&aux mode var info)
                         (&output mode var info)
                         (&flag mode var info)
                         ;; input1
                         (&input mode1 var1 info1)
                         ;; input2
                         (&input mode2 var3 info3)
                         ))
  (MODE VAR MODE1 VAR1 MODE2 VAR3 &OPTIONAL MODE (VAR INFO) &REST VAR &AUX VAR
        &OUTPUT VAR &FLAG (VAR INFO)))


(deftest :s-compress-arg-list.2
  (s-compress-arg-list '&output
                       '((&input mode var info)
                         (&optional mode var info)
                         (&rest mode var info)
                         (&aux mode var info)
                         (&output mode var info)
                         (&flag mode var info)
                         ;; output1
                         (&output mode1 var1 info1)
                         ;; output2
                         (&output mode2 var2 info2)
                         ))
  (&INPUT MODE VAR
          &OPTIONAL (VAR INFO)
          &REST VAR
          &AUX VAR
          &OUTPUT VAR MODE1 VAR1 MODE2 VAR2
          &FLAG MODE (VAR INFO)))

(deftest :s-expand-arg-list.0
  (s-expand-arg-list '&input '(foo bar baz))
  ((&input &unitary foo nil)
   (&input &unitary bar nil)
   (&input &unitary baz nil)))

;(Rlist (Elist 1 2 3 4))

#|'(s-args
  s-returns
  s-icode
  s-code1
  s-code2
  s-pcode
  s-ucode)|#

(deftest :letS.Rlist.0
    (letS* ((x 1)
            (y (Elist '(1 2 3 4))))
      (Rlist (list x y)))
  ((1 1) (1 2) (1 3) (1 4)))


(deftest :letS.Rlist.1
    (letS* ((x (Elist '(1 2 3 4)))
            (y (Elist '(1 2 3 4))))
      (Rlist (list x y)))
  ((1 1) (2 2) (3 3) (4 4)))


(deftest :letS.Rlist.2
    (letS* (((x y z . ignore)
             (Elist '((1 2 3 4)
                      (1 2 3 4)
                      (1 2 3 4)
                      (1 2 3 4)
                      (1 2 3 4))))
            (a (Elist '(1 2 3 4))))
      (Rlist (list x y z a)))
  ((1 2 3 1) (1 2 3 2) (1 2 3 3) (1 2 3 4)) )

(deftest :Rlist.Elist.0
    (Rlist (Elist '(1 2 3 4)))
  (1 2 3 4))

(deftest :Rlist.Elist.0
    (Rlist (Elist '(1 2 3 4)))
  (1 2 3 4))

(deftest :letS*.0
    (with-output-to-string (out)
      (letS* ((l (Elist '(1 2 3 4))))
        (princ l out)))
  "1234")

(deftest :Rlist.mapS.0
    (Rlist (mapS #'values (Elist '(1 2 3 4))))
  (1 2 3 4))

(deftest :Rlist.mapS.1
  (Rlist (mapS #'1+ (Elist '(1 2 3 4))))
  (2 3 4 5) )

(deftest :Rlist.implicitmap.0
    (Rlist (1+ (Elist '(1 2 3 4))))
  (2 3 4 5) )


(deftest :Rlist.implicitmap.1
    (Rlist ((lambda (x) (1+ x)) (Elist '(1 2 3 4))))
  (2 3 4 5) )

(deftest :Rlist.implicitmap.2
    (Rlist ((lambda (x y) (list y x))
        (Elist '(1 2 3 4))
        (Elist '(10 20 30 40))))

  ((10 1) (20 2) (30 3) (40 4)) )

(deftest :Rlist.implicitmap.3
    (Rlist (list (Elist '(1 2 3 4))
                 (Elist '(10 20 30 40))))
  ((1 10) (2 20) (3 30) (4 40)) )

(defun square-alist (alist)
  (letS* (((x . y)
           (Elist alist))
          (z (* x y)))
    (Rlist z)))

(deftest :Rlist.defun.0
    (square-alist '((1 . 2) (3 . 4) (5 . 6) (7 . 8)))
  (2 12 30 56) )

;; test
;(s-expand-arg-list '&input '(foo bar baz))
;(s-make-arg '&optional '&sequence 'foo '(bar baz))


(deftest :mapS.0
    (Rlist (mapS #'/ (Elist (list 1 2 3 4))))
  (1 1/2 1/3 1/4) )

(deftest :Ror.0
    (Ror (Elist '(nil nil nil nil)))
  nil )

(deftest :Ror.1
    (Ror (Elist '(nil nil nil nil)))
  nil )

(deftest :test.100
  (Rmax (Erange 1 10))
  10 )

(deftest :test.1
  (Rcount (Erange 1 10))
  10 )

(deftest :test.2
    (letS* ((s (Erange 1 100))
            (2* (* 2 s)))
      (list
       (Rvector (make-array 100) s)
       (Rlist s)
       (Rsum s)
       (Rsum$ s)
       (Rmin s)
       (Rmax s)
       (Rcount s)
       (Rand s)
       (Ror s)))
  (#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
     29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53
     54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78
     79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100)
    (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
       30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55
       56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81
       82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100)
    5050 5050.0 1 100 100 100 1) )


(deftest :test.3
    (Rappend (list (Elist '(1 2 3 4))
                   (Elist '(1 2 3 4))))
  (1 1 2 2 3 3 4 4) )

(deftest :test.4
  (Rlist (Fpositive (Elist '(-1 2 nil 3))))
  (2 3) )

(deftest :test.5
  (Rlist (Fpositive (Erange -10 10)))
  (1 2 3 4 5 6 7 8 9 10) )

(deftest :test.6
  (reduceS #'+ 0
           (Erange -20 -10)
           (Grange))
  -99 )

(deftest :test.7
    (letS* ((s (Erange 1 100))
            (p (previouS 0 (lambda (x) (* -1 x)) s)))
      (nconc (Rlist s)
             (Rlist (Fpositive p))))
  (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
     30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55
     56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81
     82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100) )

(deftest :test.8
    (Rvector (make-array 100)
             (Erange 1 100))
  #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
    30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55
    56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81
    82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100) )

(deftest :test.9
    (Rappend (list (previouS 0 #'values (Erange 1 10))
                   (Erange 1 10)))
  (0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10) )

;(do-tests)