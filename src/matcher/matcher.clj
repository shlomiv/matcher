(ns matcher
  (:require [automat.core :as a]
            [clojure.string :as str]
            [clojure.core.memoize :as m]
            [com.rpl.specter :refer :all]))
;;(map #(ns-unmap *ns* %) (keys (ns-interns *ns*)))

(defn conform [fsm data]
  (when-let [r (reduce (fn[s n] (a/advance fsm s n (reduced nil))) nil data)]
    (when (:accepted? r) r)))

(defn partial-conform
  "Like conform, but allows trailing items after it matched. However, matching is done eagerly"
  [fsm data]
  (def fsm fsm)
  (def data data)
  (when-let [[_ r] (reduce (fn[[state res] n]
                         (let [a (a/advance fsm state n nil)]
                           (if (:accepted? a)
                             [a a] (if (nil? a) (reduced [state res]) [a res]))))
                           [nil nil] data)]
    (when (:accepted? r) (with-meta {:matched (subvec data 0 (:stream-index r))
                                     :rest    (subvec data (:stream-index r))}
                           r))))
;;(partial-conform (a/compile (compile-path-to-fsm ["A" "ADJUNCT" :*])) ["A" "ADJUNCT" "1" "PRED"])

;;(a/find (a/compile (compile-path-to-fsm ["A" "ADJUNCT" :*])) nil ["B" "A" "ADJUNCT" "1" "PRED" "A"])

(defn atomic? [x] (or (keyword? x) (string? x) (number? x) (symbol? x)))

(defn compound? [x] (or (coll? x)))

(def fset #{:and :not :or :all-but :any :***})

(defn functional? [x] (or (fset x)))

(defn compile-path-to-fsm
  "Creates an fsm that accepts path `p`
   (*) currently, cannot compile [:all-but [:not 'a']] - This is unsupported by the automata"
  [p]
  (cond
    (compound? p) (if-not (functional? (first p))
                    (mapv compile-path-to-fsm p)
                    (let [[f & r] p]
                      (condp = f
                        :not     (apply a/not (compile-path-to-fsm r))
                        :and     (apply a/and (compile-path-to-fsm r))
                        :or      (apply a/or (compile-path-to-fsm r))
                        :all-but (apply a/and (map (fn[x] (a/not x)) (compile-path-to-fsm r)))
                        :any     (apply a/or (compile-path-to-fsm r))
                        :***     (if r
                                   (let [[a b] r
                                         d     (if b (- b a) 0)]
                                     (into [] (concat (repeat a a/any)
                                                      (repeat d (a/? a/any)))))
                                   (a/* a/any)))))
    (atomic? p)   (condp = p
                    :*  a/any
                    :** (a/* a/any)
                    p)))

(defn compile-path-implicit
  "The compiled function accepts both the ngrams list and a comap
  function which is being used to extract the path information from
  any arbitrary data structure (such as [path val]) without affecting
  the returned data."
  ([path]
   (let [[fns paths] (split-with fn? path)
         f           (when-not (empty? fns) (reduce comp (reverse fns)))]
     (let [fsm (a/compile (compile-path-to-fsm paths))]
       (fn find-paths
         ([ngrams comap]
          (let [r (into []
                        (comp (map (juxt (comp (partial partial-conform fsm) comap) identity))
                              (filter first))
                        ngrams)]
            (if f (f r) r))))))))

(def compile-path-implicit (m/lu compile-path-implicit :lu/threshold (* 1 1024)))

#_((compile-path-implicit
  [["B" :*]])
 [[["B" "A" "Q"] "a"]
  [["B" "C" "K"] "b"]] first)

(defn leaf? [x] (or (string? x) (number? x) ))

(defn ngramize
  ([tree] (ngramize tree []))
  ([tree parents]
   (cond
     (map? tree) (mapcat (fn[[k v]]
                           (cond
                             (and (vector? v) (every? leaf? v)) (mapcat (fn[i] [[(conj parents k) i]]) v)
                             (leaf? v) [[(conj parents k) v]]
                             :else (ngramize v (conj parents k)))) tree)
     (coll? tree) (mapcat (fn[i n]
                            (ngramize i (conj parents n))) tree (range)))))

(def ngramize (m/lu ngramize :lu/threshold (* 1 1024)))

;;(ngramize {"A" [{"B" 2} {"B" 3}]})

(defn simple-query?
  "A simple query as a query that has no nested queries."
  ([m] (and (map? m) (simple-query? nil (vals m))))
  ([_ vs] (if (coll? vs)
            (every? (complement coll?) vs)
            (not (coll? vs)))))

(defn specific-path?
  "ensure a path can only return a single item (i.e. no wildcards)"
  [xs] (or (not (coll? xs))
           (and (coll? xs) (every? (complement (into fset [:* :**]))xs))))

(comment
  ;; tests
  (specific-path? ["A" "B"])
  (simple-query? {"B" :?res
                  ["C" "L"] "c"}))

(defn combine-paths
  "combine paths. If the paths contains functions (which must be at the top), the functions are rearranged to be at the top, starting from left to right."
  ([p] p)
  ([p q] (let [[f1 p1] (split-with fn? p)
               [f2 p2] (split-with fn? q)]
           (into [] (concat f1 f2 p1 p2))))
  ([p q l] (let [[f1 p1] (split-with fn? p)
                 [f2 p2] (split-with fn? q)
                 [f3 p3] (split-with fn? l)]
             (into [] (concat f1 f2 f3 p1 p2 p3))))
  ([p q l & r] (let [[f1 p1] (split-with fn? p)
                     [f2 p2] (split-with fn? q)
                     [f3 p3] (split-with fn? l)]
                 (apply combine-paths (into [] (concat f1 f2 f3 p1 p2 p3)) r))))

(defn fix-name [n pt]
  (let [nk (if (= n "")
             (keyword
              (str/replace
               (str/join "."
                         (map (fn[c] (str/replace (if (keyword? c) (name c) (str/lower-case(name (str c)))) \_ \-))
                              pt)) \space \-))
             (keyword n))]
    nk))

(defn ensure-vector [x]
  (cond
    (vector? x) x
    (seq? x) (into [] x)
    :else [x]))

(defn leaf-eq [dk vn]
  (or (= dk vn)
      (and (keyword? dk) (= (name dk) vn))
      (and (coll? dk) (some #{vn} dk))))

(defn handle-leaf
  "Handle ends of query. This function MUST return a hash-map if it accepts (even an empty hash-map for things like 'must not exist') "
  [query action data in]
  (let [vn    (name action)
        fc    (.charAt vn 0)
        nm    (subs vn 1)
        k     (if (coll? query) (into in query) (conj in query))
        the-k (fix-name nm (remove number? k))
        dk    (get-in data (ensure-vector query))]
    (cond ;;\? \- \* \~ \! \# \$
      (= \? fc) (when dk {the-k [{:path k :val dk}]})
      (= \# fc) {the-k [{:path k :val dk }]}
      (= \- fc) (if dk nil [])
      (= \* fc) (when dk [])
      (= \~ fc) (if (leaf-eq dk nm) nil (when dk []))
      (= \! fc) (if (leaf-eq dk nm) nil [])
      (= \$ fc) (when (or (nil? dk) (= dk nm)) [])
      :else     (when (leaf-eq dk vn) []))))

(defn super-match-simple
  "match a simple query (as defined in `simple-query?`). This is the base case of the recursion"
  [pattern data data-ng in]
  (reduce (fn[o [query action]]
            (if (specific-path? query)
              (let [r (map (fn[action] (handle-leaf query action data in)) (ensure-vector action))]
                (if (and r (every? identity r)) 
                  (apply merge-with into o r)
                  (reduced nil)))
              (let [l (compile-path-implicit (combine-paths in query))
                    r (l data-ng first)
                    y (keep (fn[[{:keys [matched]} [path val]]]
                              (let [r (mapv (fn[action] (handle-leaf (drop (count in) matched) action data in)) (ensure-vector action))]
                                (when (every? identity r) 
                                  (apply merge-with into r))))
                            r)]
                (if (empty? y) (reduced nil)
                    (apply merge-with into o y)))))
          {} pattern))

(defn super-match
  "Takes in arbitrary query pattern and data and returns the matched items or an empty hash-map if the pattern
  accepts but does not extract information.
  
  A query pattern is defined in terms of two things:
  1. Path pattern - Path descriptors such as [:* \"A\" [:or [:** [:all-but \"C] :**] \"C\"]]
  2. Queries      - Either a terminal operator (as listed below), or a vector of Queries that all must match together.

  a query pattern could either be only a Query, or a map where keys are all `Path patterns` and vals are all `Queries`.

  Terminal operators (items in [] are optional):
   :?[name] - return the value of the path (must exist) inside `name` or the matched path if `name` is absent
   :#[name] - return the value of the path, if it exists
   :-       - ensure that path does not exist
   :*       - ensure that path exists without caring for the value
   :~val    - ensures that the path exists but does not equal to `val`
   :!val    - either path does not exist, or it exist with value that is not `val`
   :$val    - either path does not exist, or it exist with value that is exackly `val`
   :_val    - ensure the path exists and equals to val. This is also the default behavior
              for any string not starting with any of the previous commands. However, use this
              if you need to match a string that starts with a command character, such as 
              matching '-' where it should be '_-' or :_-

  The function returns a map, from var names to a vector of their results. When the function succeed it always returns a map,
  even if its an empty map. nil means that matching has failed. Each returned item is a map containing two fiels:
  :path - a vector representing the path to the item in the original data.
  :val  - the retrived value.

  If r is a result, the following holds true: (= (get-in data (:path r)) (:val r))"
  
  ([pattern data] (super-match pattern data (ngramize data) []))
  ([pattern data ng-data in]
   (cond
     (simple-query? pattern) (super-match-simple pattern data ng-data in)
     (vector? pattern)       (let [r (mapv (fn[i]
                                             (if (map? i)
                                               (super-match i data ng-data in) 
                                               (handle-leaf [] i data in))) pattern)]
                               (when (every? identity r)
                                 (apply merge-with into r)))
     :else
     (reduce (fn[o [k v]]
               (cond
                 (simple-query? k v) (if-let [r (super-match-simple {k v} data ng-data in)]
                                       (merge-with into o r)
                                       (reduced nil))
                 :else
                 (let [q (combine-paths in (ensure-vector k))
                       l (compile-path-implicit q)
                       r (l ng-data first)]
                   (if (empty? r)
                     (reduced nil)
                     (let [n (mapv (fn[[matched d]]
                                     (super-match v
                                                  (or (get-in data (drop (count in) matched))
                                                      (throw (Error. (str "bad transform:\ndata\t" data "\n\tget-in:\t" (into [](drop (count in) matched))
                                                                          "\tin q:\t" q))))
                                                  (mapv second d)
                                                  matched))
                                   (group-by (comp :matched first) r))]
                       (if (every? nil? n)
                         (reduced nil)
                         (apply merge-with into o n)))))))
             {} pattern))))

;; functions can be stacked together, and are piped through left to right. functions can only appear at the beginning of a path
;; section, and must be consecutive.
;;
;; Functions can only control the ordering and filtration of the fetched results. They cant (yet?) change or transform to path
;; representation or the fetched value. 

(defn reverse-order[a] (transform [(filterer [FIRST :matched])] (fn[r] (reverse r)) a))

(defn deepest-order [ngrams]
  (transform [(filterer [FIRST :matched])] (fn[r] (sort-by #(count (select-one [FIRST :matched] %)) > r )) ngrams))

(defn deepest-path [ngrams]
  (let [a (deepest-order ngrams)
        i (count (:matched (ffirst a)))]
    (take-while (fn[[{d :matched}]] (== i (count d))) a)))

;; We can manipulate the nodes we could fetch (a nasty trick):
(defn parent [ngrams]
  (transform [ALL FIRST :matched] pop ngrams))
