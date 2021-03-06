;;;; ========================================================================================== ;;;;
;;;; main.lisp                                                                                  ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:cl-latex/tests)

(deftest string-manipulation-test
  (testing "surround string"
    (ok (equal (tex::surround-string "aa" "cc" "bb")
               "aabbcc"))
    (ok (equal (tex::surround-string "aa" "cc" "bb" "dd")
               "aabbddcc"))
    (ok (equal (tex::surround-string "ss" "aa" nil)
               "ssaa"))
    (ok (equal (tex::surround-string nil "aa" "ss")
               "ssaa"))
    (ok (equal (tex::surround-string "aa" nil "ss")
               "aass")))

  (testing "concat only strings"
    (ok (equal "aabb" (tex::concat "aa" "bb")))
    (ok (equal "aabb11" (tex::concat "aa" "bb" "11")))
    (ok (equal "aabb112233445566" (tex::concat "aa" "bb" "11" "22" "33" nil "44" "55" "66")))
    (ok (signals (tex::concat "aa" "bb" 'ab) 'type-error))
    (ok (signals (tex::concat "aa" 11) 'type-error)))

  (testing "concat anything as string"
    (ok (equal "aa23.3" (tex::concat-as-string nil "aa" nil 2 3.3 nil)))
    (ok (equal "" (tex::concat-as-string nil)))
    (ok (equal "2.23.3" (tex::concat-as-string 2.2 3.3)))
    (ok (equal "2.2aa" (tex::concat-as-string 2.2 "aa")))
    (ok (equal "22110" (tex::concat-as-string 22 11 00)))
    (ok (equal "aa11" (tex::concat-as-string "aa" 11))))

  (testing "concat as lines"
    (ok (equal "aa~%bb~%cc~%" (tex::concat-as-lines "aa" nil nil "bb" "cc")))
    (ok (equal "" (tex::concat-as-lines nil)))
    (ok (equal "aa~%" (tex::concat-as-lines "aa")))
    (ok (equal "aa~%bb~%cc~%dd~%ee~%ff~%" (tex::concat-as-lines "aa" "bb" "cc" "dd" "ee" "ff")))))

