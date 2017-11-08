(ns matcher-test
  (:require [matcher :refer [super-match deepest-path parent]]
            [clojure.test :refer [is deftest]]))

(deftest super-matcher

  (is (=
       ;; most basic query:
       (super-match [:?x] "5")

       ;; returns:
       {:x [{:path [], :val "5"}]}))

  (is (=
       ;; Lets add some paths:
       (super-match {"D" :?d} {"D" "5"})

       ;; returns:
       {:d [{:path ["D"], :val "5"}]}))

  (is (=
       ;; and:
       (super-match {"A" {"D" :?d}} {"A" {"D" "5"}})

       ;; returns:
       {:d [{:path ["A" "D"], :val "5"}]}))

  (is (=
       ;; Wildcard can be used:
       ;; single :* for a single place holder:
       (super-match {[:* "D"] :?d}
                    {"A" {"D" "5"}})

       ;; returns:
       {:d [{:path ["A" "D"], :val "5"}]}))
  
  (is (=
       ;; and :** for any number (including zero!):
       (super-match {[:** "D"] :?d}
                    {"Q" {"L" "b"
                          "D" "4"}
                     "A" {"B" {"C" {"D" "2"}}}})

       ;; returns:
       {:d [{:path ["Q" "D"], :val "4"}
            {:path ["A" "B" "C" "D"], :val "2"}]}))
  
  (is (=
       ;; and we can mix them:
       (super-match {[:* "B" :** "D"] :?d}
                    {"Q" {"L" "b"
                          "D" "4"}
                     "A" {"B" {"C" {"E" {"D" "2"}}
                               "L" {"D" "5"}}}})

       ;; returns:
       {:d [{:path ["A" "B" "C" "E" "D"], :val "2"}
            {:path ["A" "B" "L" "D"], :val "5"}]}))
  
  (is (=
       ;; This does not match, because the first :** is eager, so it ate "D" as well.
       (super-match {[:**] {"D" :?d}}
                    {"Q" {"L" "b"
                          "D" "4"}
                     "A" {"B" {"C" {"D" "2"}}}})
       ;; returns:
       nil))
  
  (is (=
       ;; This works, because we limited :** 
       (super-match {[:** [:not "D"]] {"D" :?d}}
                    {"Q" {"L" "b"
                          "D" "4"}
                     "A" {"B" {"C" {"D" "2"}}}})

       ;; returns:
       {:d [{:path ["Q" "D"], :val "4"}
            {:path ["A" "B" "C" "D"], :val "2"}]}))

  (is (=
       ;; So now, getting "deepest" is trivial:
       (super-match {[deepest-path :** "D"] :?d}
                    {"Q" {"L" "c"
                          "D" "J"}
                     "A" {"B" {"C" {"D" "O"
                                    "L" "c"}}}})

       ;; returns:
       {:d [{:path ["A" "B" "C" "D"], :val "O"}]}))

  (is (=
       ;; We could also ensure that some path items do not appear
       (super-match {[:** [:all-but "C" "T"] :** "D"] :?d}
                    {"Q" {"L" "c"
                          "D" "J"}
                     "A" {"B" {"C" {"D" "O"
                                    "L" "c"}
                               "D"  "E"}}})

       ;; returns:
       {:d [{:path ["Q" "D"], :val "J"}
            {:path ["A" "B" "D"], :val "E"}]}))

  ;; Using functions to control path selection and ordering:

  (is (=
       ;; Lets take the deepest match:
       (super-match {[deepest-path :** [:all-but "C" "T"] :** "D"] :?d}
                    {"Q" {"L" "c"
                          "D" "J"}
                     "A" {"B" {"C" {"D" "O"
                                    "L" "c"}
                               "D"  "E"}}})

       ;; returns:
       {:d [{:path ["A" "B" "D"], :val "E"}]}))

  (is (=
       ;; Add another constraint:
       (super-match {[deepest-path :** [:all-but "C" "B"] :** "D"] :?d}
                    {"Q" {"L" "c"
                          "D" "J"}
                     "A" {"B" {"C" {"D" "O"
                                    "L" "c"}
                               "D"  "E"}}})

       ;; returns:
       {:d [{:path ["Q" "D"], :val "J"}]}))

  (is (=
       ;; Only take the deepest of a specific group. In this example, Z.D.A should also get matched by [:** "D"] but its shorter than the
       ;; other so it is skipped, however, [:** "A"] does return two items:
       (super-match {[deepest-path :** "D"] {[:** "A"] :?a}}
                    {"Z" {"X" {"C" {"D" {"B" {"A" "good"}
                                         "A" "bad"}}}
                          "D" {"L" {"A" "amazing"}
                               "A" "nay"}}})

       ;; returns:
       {:a [{:path ["Z" "X" "C" "D" "B" "A"], :val "good"}
            {:path ["Z" "X" "C" "D" "A"], :val "bad"}]}))

  (is (=
       ;; In this example, deepest-path only selects the deepest path for each group of [:** "D"], so, "good" is deeper that "bad" in the
       ;; Z.X.C group, and "amazing" is deeper than "nay" in "Z" group
       (super-match {[:** "D"] {[deepest-path :** "A"] :?a}}
                    {"Z" {"X" {"C" {"D" {"B" {"A" "good"}
                                         "A" "bad"}}}
                          "D" {"L" {"A" "amazing"}
                               "A" "nay"}}})       

       ;; returns:
       {:a [{:path ["Z" "X" "C" "D" "B" "A"], :val "good"}
            {:path ["Z" "D" "L" "A"], :val "amazing"}]}))
  
  (is (=
       ;; And we could of course take the deepest of the deepest:
       (super-match {[deepest-path :** "D"] {[deepest-path :** "A"] :?a}}
                    {"Z" {"X" {"C" {"D" {"B" {"A" "good"}
                                         "A" "bad"}}}
                          "D" {"L" {"A" "amazing"}
                               "A" "nay"}}})

       ;; returns:
       {:a [{:path ["Z" "X" "C" "D" "B" "A"], :val "good"}]}))

  (comment
    ;; These dont work here. Super-matcher only uses the selected path names to choose the actual data node from the real
    ;; data-structure (i.e not the ng-data)
    
    (defn lowerify-val [ngrams]
      (println "lowerify")
      (for [[a[path val]] ngrams]
        [a[path  (.toLowerCase val)]]))

    (defn lowerify-path [ngrams]
      (println "lowerify")
      (for [[{:keys [matched] :as m} b]  ngrams]
        [(assoc m :matched (mapv (memfn toLowerCase) matched)) b]))
    )

  (is (=
       (super-match {[parent :** "D"] :?d}
                    {"Q" {"L" "c"
                          "D" "J"}
                     "A" {"B" {"C" {"D" "O"
                                    "L" "c"}}}})

       ;; returns:
       {:d [{:path ["Q"], :val {"L" "c", "D" "J"}}
            {:path ["A" "B" "C"], :val {"D" "O", "L" "c"}}]}))

  ;; Instead of this,
  {:d [{:path ["Q" "D"], :val "J"}
       {:path ["A" "B" "C" "D"], :val "O"}]}

  (is (=
       ;; This matches, since there is a grouping such that D != 4
       (super-match {[:** "D"] "~4"}
                    {"Q" {"L" "b"
                          "D" "4"}
                     "A" {"B" {"C" {"D" "2"}}}})

       ;; returns
       {}))


  (is (=
       ;; When using a vector for the right query side, the matcher ANDs all the operation.
       ;; For example, the following will match all "D"s that are not 2 or 4
       (super-match {[:** "D"] [:?d "~4" "~2"]}
                    {"Q" {"L" "3"
                          "D" "4"}
                     "H" {"E" {"D" "5"
                               "L" "a"}}
                     "A" {"B" {"C" {"D" "2"
                                    "L" "c"}}}})

       ;; returns: 
       {:d [{:path ["H" "E" "D"], :val "5"}]}))

  (is (=
       ;; This will match all D's that are not 4 or 2, and their corresponding L, if L='c'
       (super-match {[:** [:not "D"]] {"D" [:?d  "~2" "~4"]
                                       "L" ["c" :?c]}}
                    {"Q" {"L" "c"
                          "D" "4"}
                     "A" {"B" {"C" {"D" "1"
                                    "L" "c"}}}})

       ;; returns:
       {:d [{:path ["A" "B" "C" "D"], :val "1"}]
        :c [{:path ["A" "B" "C" "L"], :val "c"}]}))

  (is (=
       ;; match all D and L together (in the same group), when D!=2. Notice that L=a is never selected.
       (super-match {[:** [:not "D"]] {"D" [:?d  "~2"]
                                       "L" :?c}}
                    {"Q" {"L" "3"
                          "D" "4"}
                     "H" {"E" {"D" "2"
                               "L" "a"}}
                     "A" {"B" {"C" {"D" "1"
                                    "L" "c"}}}})

       ;; returns: Note how the grouping appears by the ordering of results within each variable!
       {:d [{:path ["Q" "D"], :val "4"}
            {:path ["A" "B" "C" "D"], :val "1"}]
        :c [{:path ["Q" "L"], :val "3"}
            {:path ["A" "B" "C" "L"], :val "c"}]}))

  (is (=
       ;; However, notice in here grouping is not maintained:
       (super-match {[:** "D"] [:?d "~2"]
                     [:** "L"] [:?c]
                     }
                    {"Q" {"L" "3"
                          "D" "4"}
                     "H" {"E" {"D" "2"
                               "L" "a"}}
                     "A" {"B" {"C" {"D" "1"
                                    "L" "c"}}}})

       ;; returns:
       {:d [{:path ["Q" "D"], :val "4"}
            {:path ["A" "B" "C" "D"], :val "1"}]
        :c [{:path ["Q" "L"], :val "3"}
            {:path ["H" "E" "L"], :val "a"}
            {:path ["A" "B" "C" "L"], :val "c"}]}))

  (is (=
       ;; you could also nest more queries, and :d in this example will only bind if it has an internal "S" with a value that is not 5 (and also bind that value to :s):
       (super-match {[:** "D"] [:?d {"S" [:?s "~5"]}]
                     }
                    {"Q" {"L" "3"
                          "D" "4"}
                     "H" {"E" {"D" {"S" "5"}
                               "L" "a"}}
                     "A" {"B" {"C" {"D" {"S" "2"}
                                    "L" "c"}}}})

       ;; returns:
       {:d [{:path ["A" "B" "C" "D"], :val {"S" "2"}}]
        :s [{:path ["A" "B" "C" "D" "S"], :val "2"}]}))

  (is (=
       ;; You could use another qualifier, for instance :#s and "!5" (both allows for non-existence):
       (super-match {[:** "D"] [:?d {"S" [:#s "!5"]}]}
                    {"Q" {"L" "3"
                          "D" "4"}
                     "H" {"E" {"D" {"S" "5"}
                               "L" "a"}}
                     "A" {"B" {"C" {"D" {"S" "2"}
                                    "L" "c"}}}})

       ;; Since both :d and :s are in the same group, we want to keep the items in :d and :s on the same index, so the i'th item in :d
       ;; corrensponds to the i'th item in :s. Now, since :s did not have to match, we return a nil value for the position where we try to
       ;; fetch the value but its missing:
       {:d [{:path ["Q" "D"], :val "4"}                ;;; <-- matched because both :#s and "!5" allow for non-existance of :s. try to
                                                       ;;;     make any of these a "path must exist operator" and "4" would not be selected
            {:path ["A" "B" "C" "D"], :val {"S" "2"}}]
        :s [{:path ["Q" "D" "S"], :val nil}            ;;; <-- corresponds to the first item in :d (Q.D=4)
            {:path ["A" "B" "C" "D" "S"], :val "2"}]}))

  (is (=
       ;; Now we can see that the `parent` trick from before literally picks the parent path, so we could actually
       ;; match against "D" again:
       (super-match {[parent :** "D"] [:?d-parent {"D" :?d-real}]}
                    {"Q" {"L" "c"
                          "D" "J"}
                     "A" {"B" {"C" {"D" "O"
                                    "L" "c"}}}})

       ;; returns:
       {:d-parent [{:path ["Q"], :val {"L" "c", "D" "J"}}
                   {:path ["A" "B" "C"], :val {"D" "O", "L" "c"}}]
        :d-real   [{:path ["Q" "D"], :val "J"}
                   {:path ["A" "B" "C" "D"], :val "O"}]}))

  (is (=
       ;; This should succeed, because since the grouping is happening on the surrounding clause (["Q" :*])
       ;; this means that we dont want a group that satisfies the ["A" ....] and has a "D", which is true:
       ;; There is no Q.L.D but there is a Q.L.A - Q.H.D is not in the group!
       (super-match {["Q" :*] {"D" :-
                               ["A" :*] {"B" :?res,
                                         ["C" :**] "c"}}}
                    
                    {"Q" {"L" {"A" [{"B" "b", "C" "c"}
                                    {"B" "c", "C" {"Q" "a"}}
                                    {"B" "a", "C" "c"}]}
                          "H" {"D" "d"}}})

       ;; returns:
       {:res [{:path ["Q" "L" "A" 0 "B"], :val "b"}
              {:path ["Q" "L" "A" 2 "B"], :val "a"}]}))

  (is (=
       ;; However, this will not match, because this time there is a Q.L.D for the satisfing Q.L.A
       (super-match {["Q" :*] {"D" :-
                               ["A" :*] {"B" :?res,
                                         ["C" :**] "c"}}}
                    
                    {"Q" {"L" {"A" [{"B" "b", "C" "c"}
                                    {"B" "c", "C" {"Q" "a"}}
                                    {"B" "a", "C" "c"}]
                               "D" "d"}}})
       ;; returns:
       nil))

  (is (=
       ;; This shuld match, because D needs to be with A
       (super-match {["Q" :*] {"D" :?d
                               ["A" :*] {"B" :?res,
                                         ["C" :**] "c"}}}
                    
                    {"Q" {"L" {"A" [{"B" "b", "C" "c"}
                                    {"B" "c", "C" {"Q" "a"}}
                                    {"B" "a", "C" "c"}]
                               "D" "d"}}})

       ;;returns:
       {:d   [{:path ["Q" "L" "D"], :val "d"}]
        :res [{:path ["Q" "L" "A" 0 "B"], :val "b"}
              {:path ["Q" "L" "A" 2 "B"], :val "a"}]}))

  (is (=
       ;; This shuld not match, because :d needs to be in the same grouping as :res
       (super-match {["Q" :*] {"D" :?d
                               ["A" :*] {"B" :?res,
                                         ["C" :**] "c"}}}
                    
                    {"Q" {"L" {"A" [{"B" "b", "C" "c"}
                                    {"B" "c", "C" {"Q" "a"}}
                                    {"B" "a", "C" "c"}]}
                          "H" {"D" "d"}}})
       ;;returns:
       nil))

  (is (=
       ;; This should match, because we now dont have a grouping constraints on :d and :res
       (super-match {"Q" {[:* "D"] :?d
                          [:* "A" :*] {"B" :?res,
                                       ["C" :**] "c"}}}
                    
                    {"Q" {"L" {"A" [{"B" "b", "C" "c"}
                                    {"B" "c", "C" {"Q" "a"}}
                                    {"B" "a", "C" "c"}]}
                          "H" {"D" "d"}}})

       ;; returns:
       {:d   [{:path ["Q" "H" "D"], :val "d"}]
        :res [{:path ["Q" "L" "A" 0 "B"], :val "b"}
              {:path ["Q" "L" "A" 2 "B"], :val "a"}]}))

  (is (=
       ;; We could also specify :or and :and (even :not) in a single path patter:
       (super-match {[:* "A" [:or "C" [:** [:all-but "C"] :**]]] :?r}
                    {"P" {"A" {"C" 5
                               "Q" {"L" {"R" 6}}
                               "H" {"C" {"B" 7}}}}})

       ;; returns:
       {:r [{:path ["P" "A" "C"], :val 5}             ;;; <-- matched by the first :or clause
            {:path ["P" "A" "Q" "L" "R"], :val 6}     ;;; <-- matched by the second :or clause
            {:path ["P" "A" "H"], :val {"C" {"B" 7}}} ;;; <-- matched by NOT taking the path starting with "C" as a path,
                                                      ;;;     rather, it becomes a part of the result
            ]}))

  (is (=
       ;; how are we dealing with an elaborate test of the original matcher? Only thing that must be corrected is adding the
       ;; wildcard to the "ADJUNCT" query:
       (super-match {["ADJUNCT" :*] {"OBJ" :?
                                     "CHECK" {"LIT" "FOR"}}
                     "MUST" "Exist"
                     "CAN" {"BE" :*}
                     "NOT-EXIST" :-
                     "NOT-A"   "~A"
                     "NOT-A-B" ["~A" "~B"]
                     "NOT-EXIST2" "!a"}
                    
                    {"MUST" "Exist"
                     "CAN" {"BE" ["anything"]}
                     "NOT-A-B" "C"
                     "NOT-A"   "B"
                     "ADJUNCT" [{"OBJ" [1 2 3]
                                 "CHECK" {"LIT" "FOR"}}
                                {"OBJ" [4 5 6]
                                 "CHECK" {"LIT" "FOR"}}]})

       ;; returns:
       {:adjunct.obj [{:path ["ADJUNCT" 0 "OBJ"], :val [1 2 3]}
                      {:path ["ADJUNCT" 1 "OBJ"], :val [4 5 6]}]}))
  
  )
