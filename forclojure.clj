(ns forclojure.core)

;; 1 Nothing but the Truth

;; This is a clojure form. Enter a value which will make the form
;; evaluate to true. Don't over think it! If you are confused, see the
;; getting started page. Hint: true is equal to true.
(let [__ true]
  (= __ true))

;; 2 Simple Math

;; If you are not familiar with polish notation, simple arithmetic
;; might seem confusing.
(let [__ 4]
  (= (- 10 (* 2 3)) __))

;; 3. Intro to Strings

;; Clojure strings are Java strings. This means that you can use any
;; of the Java string methods on Clojure strings.

(let [__ "HELLO WORLD"]
  (= __ (.toUpperCase "hello world")))

;; 4. Intro to Lists

;; Lists can be constructed with either a function or a quoted form.
(= (list :a :b :c) '(:a :b :c))

;; 5. Lists: conj

;; When operating on a list, the conj function will return a new list
;; with one or more items "added" to the front.
(let [__ '(1 2 3 4)]
  (and
   (= __ (conj '(2 3 4) 1))
   (= __ (conj '(3 4) 2 1))))

;; 6. Intro to Vectors

;; Vectors can be constructed several ways. You can compare them with
;; lists.
(=
 [:a :b :c]
 (list :a :b :c)
 (vec '(:a :b :c))
 (vector :a :b :c))

;; 7. Vectors: conj
(let [__ [1 2 3 4]]
  (and
   (= __ (conj [1 2 3] 4))
   (= __ (conj [1 2] 3 4))))

;; 8. Intro to Sets
(let [__ #{:a :b :c :d}]
  (and
   (= __ (set '(:a :a :b :c :c :c :c :d :d)))
   (= __ (clojure.set/union #{:a :b :c} #{:b :c :d}))))

;; 9. Sets: conj

;; When operating on a set, the conj function returns a new set with
;; one or more keys "added".
(let [__ 2]
  (= #{1 2 3 4} (conj #{1 4 3} __)))

;; 10. Intro to Maps Maps store key-value pairs.

;; Both maps and keywords can be used as lookup functions. Commas can
;; be used to make maps more readable, but they are not required.
(let [__ 20]
  (and
   (= __ ((hash-map :a 10, :b 20, :c 30) :b))
   (= __ (:b {:a 10, :b 20, :c 30}))))

;; 11. Maps: conj

;; When operating on a map, the conj function returns a new map with
;; one or more key-value pairs "added".

(let [__ [:b 2]]
  (= {:a 1, :b 2, :c 3} (conj {:a 1} __ [:c 3])))

;; 12. Intro to Sequences

;; All Clojure collections support sequencing. You can operate on
;; sequences with functions like first, second, and last.

(let [__ 3]
  (and
   (= __ (first '(3 2 1)))
   (= __ (second [2 3 4]))
   (= __ (last (list 1 2 3)))))

;; 13. Sequences: rest

;; The rest function will return all the items of a sequence except
;; the first.

(let [__ [20 30 40]]
  (= __ (rest [10 20 30 40])))

;; 14. Intro to Functions

;; Clojure has many different ways to create functions.

(let [__ 8]
  (and
   (= __ ((fn add-five [__] (+ __ 5)) 3))
   (= __ ((fn [__] (+ __ 5)) 3))
   (= __ (#(+ % 5) 3))
   (= __ ((partial + 5) 3))))

;; 15. Double Down

;; Write a function which doubles a number.
(let [__ (fn [x] (+ x x))]
  (and
   (= (__ 2) 4)
   (= (__ 3) 6)
   (= (__ 11) 22)
   (= (__ 7) 14)))

;; 16. Hello World

;; Write a function which returns a personalized greeting.
(let [__ (fn [x] (str "Hello, " x "!"))]
  (= (__ "Dave") "Hello, Dave!"))

;; 17. Sequences: map

;; The map function takes two arguments: a function (f) and a sequence
;; (s). Map returns a new sequence consisting of the result of
;; applying f to each item of s. Do not confuse the map function with
;; the map data structure.
(let [__ '(6 7 8)]
  (= __ (map #(+ % 5) '(1 2 3))))

;; 18. Sequences: filter

;; The filter function takes two arguments: a predicate function (f)
;; and a sequence (s). Filter returns a new sequence consisting of all
;; the items of s for which (f item) returns true.
(let [__ '(6 7)]
  (= __ (filter #(> % 5) '(3 4 5 6 7))))

;; 19. Last Element

;; Write a function which returns the last element in a
;; sequence. (Special Restrictions: last)
(let [__ (fn [x] (first (reverse x)))]
  (and
   (= (__ [1 2 3 4 5]) 5)
   (= (__ '(5 4 3)) 3)
   (= (__ ["b" "c" "d"]) "d")))

;; 20. Penultimate Element

;; Write a function which returns the second to last element from a
;; sequence.
(let [__ (fn [x] (second (reverse x)))]
  (and
   (= (__ (list 1 2 3 4 5)) 4)
   (= (__ ["a" "b" "c"]) "b")
   (= (__ [[1 2] [3 4]]) [1 2])))

;; 21. Nth element

;; Write a function which returns the Nth element from a
;; sequence. (Special restrictions: nth)
(let [__ (fn [sq ndx]
           (if (= 0 ndx)
             (first sq)
             (recur (rest sq) (dec ndx))))]
  (= (__ '(4 5 6 7) 2) 6))

;; 22. Count a sequence

;; Write a function which returns the total number of elements in a
;; sequence. Special restrictions: count
(let [__ (fn [sq]
           (let [aux (fn [sq acc]
                       (if (empty? sq)
                         acc
                         (recur (rest sq) (inc acc))))]
             (aux sq 0)))]
  (= (__ '(1 2 3 3 1)) 5))

;; 23. Reverse a sequence

;; Write a function which reverses a sequence.
(let [__ (fn [sq]
           (let [aux (fn [sq acc]
                       (if (empty? sq)
                         acc
                         (recur
                          (rest sq)
                          (cons (first sq) acc))))]
             (aux sq nil)))]
  (= (__ [1 2 3 4 5]) [5 4 3 2 1]))

;; 24. Sum It All Up

;; Write a function which returns the sum of a sequence of numbers.
(let [__ #(reduce + %)]
  (and
   (= (__ [1 2 3]) 6)
   (= (__ (list 0 -2 5 5)) 8)
   (= (__ #{4 2 1}) 7)
   (= (__ '(0 0 -1)) -1)
   (= (__ '(1 10 3)) 14)))

;; 25. Find the odd numbers

;; Write a function which returns only the odd numbers from a sequence.
(let [__ (partial filter odd?)]
  (and
   (= (__ #{1 2 3 4 5}) '(1 3 5))
   (= (__ [4 2 1 6]) '(1))
   (= (__ [2 2 4 6]) '())
   (= (__ [1 1 1 3]) '(1 1 1 3))))

;; 26. Fibonacci Sequence

;; Write a function which returns the first X fibonacci numbers.
(let [__ (fn [n]
           (seq
            (reduce (fn [a b]
                      (conj a (+' (last a) (last (butlast a)))))
                    [1 1]
                    (range (dec (dec n))))))]
  (and
   (= (__ 3) '(1 1 2))
   (= (__ 6) '(1 1 2 3 5 8))
   (= (__ 8) '(1 1 2 3 5 8 13 21))))

;; 27. Palindrome Detector

;; Write a function which returns true if the given sequence is a
;; palindrome.
(let [__ (fn [s]
           (if (< (count s) 2)
             true
             (and
              (= (first s) (last s))
              (recur (drop 1 (drop-last 1 s))))))]
  (and
   (false? (__ '(1 2 3 4 5)))
   (true? (__ "racecar"))
   (true? (__ [:foo :bar :foo]))
   (true? (__ '(1 1 3 3 1 1)))
   (false? (__ '(:a :b :c)))))

;; 28. Flatten a Sequence

;; Write a function which flattens a sequence. Special restrictions:
;; flatten
(let [__ (fn[x]
           (filter (complement sequential?)
                   (rest (tree-seq sequential? seq x))))]
  (and
   (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
   (= (__ ["a" ["b"] "c"]) '("a" "b" "c"))
   (= (__ '((((:a))))) '(:a))))


;; 29. Get the Caps

;; Write a function which takes a string and returns a new string
;; containing only the capital letters.
(let [__ (fn [val] (apply str (filter #(Character/isUpperCase %) val)))]
  (and (= (__ "HeLlO, WoRlD!") "HLOWRD")
       (empty? (__ "nothing"))
       (= (__ "$#A(*&987Zf") "AZ")))

;; 30. Compress a Sequence

;; Write a function which removes consecutive duplicates from a
;; sequence.
(let [__ (fn [sq]
           (let [aux
                 (fn [[head & tail] last-seen acc]
                   (cond
                    (nil? head)
                    acc

                    (= head last-seen)
                    (recur tail head acc)

                    :else
                    (recur tail head (conj acc head))))]
             (aux sq nil [])))]
  (and
   (= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
   (= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
   (= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))))

;; 31. Pack a Sequence

;; Write a function which packs consecutive duplicates into sub-lists.
(let [__ (fn [sq]
           (let [aux
                 (fn [[head & tail] last-seen curr acc]
                   (cond
                    (nil? head)
                    (conj acc curr)

                    (= head last-seen)
                    (recur tail head (conj curr head) acc)

                    :else
                    (recur tail head (list head) (conj acc curr))))]

             (filter (complement empty?)
                     (aux sq nil (list) []))))]

  (and
   (= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
   (= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
   (= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))))

;; 32. Duplicate a Sequence

;; Write a function which duplicates each element of a sequence.
(let [__ (fn [sq]
           (let [aux
                 (fn [[head & tail] acc]
                   (cond
                    (nil? head)
                    acc

                    :else
                    (recur tail (conj acc head head))))]
             (aux sq [])))]
  (and
   (= (__ [1 2 3]) '(1 1 2 2 3 3))
   (= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
   (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
   (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))))

;; 33. Replicate a Sequence

;; Write a function which replicates each element of a sequence a
;; variable number of times.
(let [__ (fn [sq n]
           (let [aux
                 (fn [[head & tail] n acc]
                   (cond
                    (nil? head)
                    acc

                    :else
                    (recur tail n (apply conj acc (repeat n head)))))]
             (aux sq n [])))]
  (and
   (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
   (= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b))
   (= (__ [4 5 6] 1) '(4 5 6))
   (= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
   (= (__ [44 33] 2) [44 44 33 33])))

;; 34. Implement range

;; Write a function which creates a list of all integers in a given
;; range.
(let [__ (fn [from to]
           (let [aux
                 (fn [from to acc]
                   (cond
                    (= from to)
                    acc

                    :else
                    (recur (inc from) to (conj acc from))))]
             (aux from to [])))]
  (and
   (= (__ 1 4) '(1 2 3))
   (= (__ -2 2) '(-2 -1 0 1))
   (= (__ 5 8) '(5 6 7))))

;; 35. Local binding

;; Clojure lets you give local names to values using the special
;; let-form.

(let [__ 7]
  (and
   (= __ (let [x 5] (+ 2 x)))
   (= __ (let [x 3, y 10] (- y x)))
   (= __ (let [x 21] (let [y 3] (/ x y))))))

;; 36. Let it be

;; Can you bind x, y, and z so that these are all true?

(let [z 1 y 3 x 7]
  (and
   (= 10 (+ x y))
   (= 4  (+ y z))
   (= 1  z)))

;; 37. Regular Expressions

;; Regex patterns are supported with a special reader macro.
(let [__ "ABC"]
  (= __ (apply str (re-seq #"[A-Z]+" "bA1B3Ce "))))

;; 38. Maximum value

;; Write a function which takes a variable number of parameters and
;; returns the maximum value.

(let [__ (fn [& s]
           (let [aux (fn [acc s]
                       (if (empty? s)
                         acc
                         (let [f (first s)
                               n (if (<= acc f) f acc)]
                           (recur n (rest s)))))]
             (aux 0 s)))]

  (and
   (= (__ 1 8 3 4) 8)
   (= (__ 30 20) 30)
   (= (__ 45 67 11) 67)))

;; 39. Interleave two Seqs

;; Write a function which takes two sequences and returns the first
;; item from each, then the second item from each, then the third,
;; etc.
(let [__ (fn [sq1 sq2]
           (let [aux
                 (fn [[hd1 & tl1] [hd2 & tl2] acc]
                   (cond
                    (or
                     (nil? hd1)
                     (nil? hd2))
                    acc

                    :else
                    (recur tl1 tl2 (conj acc hd1 hd2))))]

             (aux sq1 sq2 [])))]
  (and
   (= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
   (= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
   (= (__ [1 2 3 4] [5]) [1 5])
   (= (__ [30 20] [25 15]) [30 25 20 15])))

;; 40. Interpose a Seq

;; Write a function which separates the items of a sequence by an
;; arbitrary value.
(let [__ (fn [sep sq]
           (let [aux
                 (fn [[head & tail] acc]
                   (cond
                    (empty? tail)
                    (conj acc head)

                    :else
                    (recur tail (conj acc head sep))))]
             (aux sq [])))]
  (and
   (= (__ 0 [1 2 3]) [1 0 2 0 3])
   (= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
   (= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d])))

;; 41. Drop every Nth item

;; Write a function which drops every Nth item from a sequence.
(let [__ (fn [sq n]
           (let [n- (dec n)
                 aux
                 (fn [[head & tail] idx acc]
                   (cond
                    (nil? head)
                    acc

                    (zero? idx)
                    (recur tail n- acc)

                    :else
                    (recur tail (dec idx) (conj acc head))))]
             (aux sq n- [])))]
  (and
   (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
   (= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
   (= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6])))

;; 42. Factorial fun

;; Write a function which calculates factorials.
(let [__ (fn [x]
           (let [aux
                 (fn[n acc]
                   (cond
                    (= 1 n)
                    acc

                    :else
                    (recur (dec n) (* n acc))))]
             (aux x 1)))]
  (and
   (= (__ 1) 1)
   (= (__ 3) 6)
   (= (__ 5) 120)
   (= (__ 8) 40320)))

;; 43. Reverse Interleave

;; Write a function which reverses the interleave process into x
;; number of subsequences.
(let [__ (fn [sq n]
           (letfn [(vassoc-in [m [k & ks] v]
                     (if ks
                       (assoc m k (vassoc-in (get m k []) ks v))
                       (assoc m k v)))

                   (aux [[head & tail] idx acc]
                     (cond
                      (nil? head)
                      acc

                      :else
                      (recur tail (mod (inc idx) n)
                             (vassoc-in acc [idx (count (get acc idx))] head))))]

             (aux sq 0 (vec (repeat n [])))))]
  (and
   (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
   (= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
   (= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))))

;; 44. Rotate Sequence

;; Write a function which can rotate a sequence in either direction.
(let [__ (fn [n sq]
           (let [v (vec sq)
                 len (count v)
                 n_ (if (<= 0 n) n (+ len n))
                 mid (mod n_ len)
                 a (subvec v mid len)
                 b (subvec v 0 mid)]
             (concat a b)))]
  (and
   (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
   (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
   (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
   (= (__ 1 '(:a :b :c)) '(:b :c :a))
   (= (__ -4 '(:a :b :c)) '(:c :a :b))))

;; 45. Intro to Iterate

;; The iterate function can be used to produce an infinite lazy
;; sequence.
(= '(1 4 7 10 13)
   (take 5 (iterate #(+ 3 %) 1)))

;; 46. Flipping out

;; Write a higher-order function which flips the order of the
;; arguments of an input function.

(let [__ (fn[f]
           (fn [b a]
             (f a b)))]
  (and
   (= 3 ((__ nth) 2 [1 2 3 4 5]))
   (= true ((__ >) 7 8))
   (= 4 ((__ quot) 2 8))
   (= [1 2 3] ((__ take) [1 2 3 4 5] 3))))

;; 47. Contain Yourself

;; The contains? function checks if a KEY is present in a given
;; collection. This often leads beginner clojurians to use it
;; incorrectly with numerically indexed collections like vectors and
;; lists.

(let [__ 4]
  (and
   (contains? #{4 5 6} __)
   (contains? [1 1 1 1 1] __)
   (contains? {4 :a 2 :b} __)
   (not (contains? [1 2 4] __))))

;; 48. Intro to some

;; The some function takes a predicate function and a collection. It
;; returns the first logical true value of (predicate x) where x is an
;; item in the collection.

(let [__ 6]
  (and
   (= __ (some #{2 7 6} [5 6 7 8]))
   (= __ (some #(when (even? %) %) [5 6 7 8]))))

;; 49. Split a sequence

;; Write a function which will split a sequence into two parts.

(let [ __ (fn[n sq]
            [(take n sq) (drop n sq)])]
  (and
   (= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
   (= (__ 1 [:a :b :c :d]) [[:a] [:b :c :d]])
   (= (__ 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))

;; 50. Split by type

;; Write a function which takes a sequence consisting of items with
;; different types and splits them up into a set of homogeneous
;; sub-sequences. The internal order of each sub-sequence should be
;; maintained, but the sub-sequences themselves can be returned in any
;; order (this is why 'set' is used in the test cases).

(let [__
      (fn[coll]
        (loop [[head & tail] coll res {}]
          (if (nil? head)
            (into #{} (vals res))
            (let [t (type head)]
              (recur tail
                     (assoc res t (conj (get res t []) head)))))))]

  (and
   (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
   (= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
   (= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})))

;; 51. Advanced Destructuring

;; Here is an example of some more sophisticated destructuring.
(= [1 2 [3 4 5] [1 2 3 4 5]]
   (let [[a b & c :as d] [1 2 3 4 5]] [a b c d]))

;;  52. Intro to Destructuring

;; Let bindings and function parameter lists support destructuring.
(= [2 4] (let [[a b c d e] [0 1 2 3 4]] [c e]))

;; 53. Longest Increasing Sub-Seq

;; Given a vector of integers, find the longest consecutive
;; sub-sequence of increasing numbers. If two sub-sequences have the
;; same length, use the one that occurs first. An increasing
;; sub-sequence must have a length of 2 or greater to qualify.
(let [__
      (fn [w]
        (let [aux
              (fn [[head & tail] last-seen curr res]
                (cond
                 (nil? head)
                 (let [tmp (if (<=
                                (count curr)
                                (count res))
                             res
                             curr)]
                   (if (< 1 (count tmp))
                     tmp
                     []))

                 (< last-seen head)
                 (recur tail head (conj curr head) res)

                 :else
                 (recur tail head [head] (if (< (count res)
                                                (count curr))
                                           curr
                                           res))))]
          (aux w -1 [] [])))]
  (and
   (= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
   (= (__ [5 6 1 3 2 7]) [5 6])
   (= (__ [2 3 3 4 5]) [3 4 5])
   (= (__ [7 6 5 4]) [])))

;; 54. Partition a Sequence

;; Write a function which returns a sequence of lists of x items
;; each. Lists of less than x items should not be returned. Special
;; restrictions: partition, partition-all
(let [__
      (fn [n w]
        (let [aux
              (fn [[head & tail] curr res]
                (cond
                 (nil? head)
                 (let [out (if (= n (count curr))
                             (conj res curr)
                             res)]
                   (seq out))

                 (< (count curr) n)
                 (recur tail (conj curr head) res)

                 :else
                 (recur tail [head] (conj res curr))))]
          (aux w [] [])))]
  (and
   (= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
   (= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
   (= (__ 3 (range 8)) '((0 1 2) (3 4 5)))))

;; 55. Count Occurrences

;; Write a function which returns a map containing the number of
;; occurences of each distinct item in a sequence. Special
;; restrictions: frequencies

(let [__
      (fn [coll]
        (reduce (fn [freqs x]
                  (assoc freqs x
                         (inc (get freqs x 0))))
                {}  coll))]

(and
   (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
   (= (__ [:b :a :b :a :b]) {:a 2, :b 3})
   (= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})))

;; 56. Write a function which removes the duplicates from a
;; sequence. Order of the items must be maintained. Special
;; restrictions: distinct.
(let [__
      (fn [coll]
        (let [aux
              (fn step [xs seen]
                (lazy-seq
                 ((fn [[x :as xs] seen]
                    (when-let [s (seq xs)]
                      (if (contains? seen x)
                        (recur (rest s) seen)
                        (cons x (step (rest s) (conj seen x))))))
                  xs seen)))]
          (aux coll #{})))]
  (and
   (= (__ [1 2 1 3 1 2 4]) [1 2 3 4])
   (= (__ [:a :a :b :b :c :c]) [:a :b :c])
   (= (__ '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
   (= (__ (range 50)) (range 50))))

;; 57. Simple recursion

;; A recursive function is a function which calls itself. This is one
;; of the fundamental techniques used in functional programming.

(let [__ '(5 4 3 2 1)]
  (= __ ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5)))


;; 58. Function Composition

;; Write a function which allows you to create function
;; compositions. The parameter list should take a variable number of
;; functions, and create a function that applies them from
;; right-to-left. Special restrictions: comp

(let [__
      (fn rec
        ([] identity)
        ([f] f)
        ([f g]
         (fn
           ([] (f (g)))
           ([x] (f (g x)))
           ([x y] (f (g x y)))
           ([x y z] (f (g x y z)))
           ([x y z & args] (f (apply g x y z args)))))
        ([f g & fs]
         (reduce rec (list* f g fs))))]

  (and
   (= [3 2 1] ((__ rest reverse) [1 2 3 4]))

   (= 5 ((__ (partial + 3) second) [1 2 3 4]))

   (= true ((__ zero? #(mod % 8) +) 3 5 7 9))

   (= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))))

;; 59. Juxtaposition

(let [__
      (fn
        ([f]
         (fn
           ([] [(f)])
           ([x] [(f x)])
           ([x y] [(f x y)])
           ([x y z] [(f x y z)])
           ([x y z & args] [(apply f x y z args)])))
        ([f g]
         (fn
           ([] [(f) (g)])
           ([x] [(f x) (g x)])
           ([x y] [(f x y) (g x y)])
           ([x y z] [(f x y z) (g x y z)])
           ([x y z & args] [(apply f x y z args) (apply g x y z args)])))
        ([f g h]
         (fn
           ([] [(f) (g) (h)])
           ([x] [(f x) (g x) (h x)])
           ([x y] [(f x y) (g x y) (h x y)])
           ([x y z] [(f x y z) (g x y z) (h x y z)])
           ([x y z & args] [(apply f x y z args) (apply g x y z args) (apply h x y z args)])))
        ([f g h & fs]
         (let [fs (list* f g h fs)]
           (fn
             ([] (reduce #(conj %1 (%2)) [] fs))
             ([x] (reduce #(conj %1 (%2 x)) [] fs))
             ([x y] (reduce #(conj %1 (%2 x y)) [] fs))
             ([x y z] (reduce #(conj %1 (%2 x y z)) [] fs))
             ([x y z & args] (reduce #(conj %1 (apply %2 x y z args)) [] fs))))))]

  (and
   (= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
   (= ["HELLO" 5] ((__ #(.toUpperCase %) count) "hello"))
   (= [2 6 4] ((__ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))))

;; 60. Sequence Reductions

(let [__
      (fn rec
        ([f coll]
         (lazy-seq
          (if-let [s (seq coll)]
            (rec f (first s) (rest s))
            (list (f)))))
        ([f init coll]
         (cons init
               (lazy-seq
                (when-let [s (seq coll)]
                  (rec f (f init (first s)) (rest s)))))))]

  (and
   (= (take 5 (__ + (range))) [0 1 3 6 10])

   (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])

   (= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)))


;; 61. Map Construction

;; Write a function which takes a vector of keys and a vector of
;; values and constructs a map from them. Special restrictions: zipmap

(let [__
      (fn[keys vals]
        (loop [map {}
               ks (seq keys)
               vs (seq vals)]
          (if (and ks vs)
            (recur (assoc map (first ks) (first vs))
                   (next ks)
                   (next vs))
            map)))]
  (and
   (= (__ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
   (= (__ [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
   (= (__ [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

;; 62. Re-implement iterate

;; Given a side-effect free function f and an initial value x write a
;; function which returns an infinite lazy sequence of x, (f x), (f (f
;; x)), (f (f (f x))), etc. Special restrictions: iterate

(let [__
      (fn [f base]
        (let [aux
              (fn step [curr]
                (lazy-seq
                 ((fn[]
                    (cons curr (step (f curr)))))))]
          (aux base)))]
  (and
   (= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
   (= (take 100 (__ inc 0)) (take 100 (range)))
   (= (take 9 (__ #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

;; 63. Group a Sequence

;; Given a function f and a sequence s, write a function which returns
;; a map. The keys should be the values of f applied to each item in
;; s. The value at each key should be a vector of corresponding items
;; in the order they appear in s.

(let [__
      (fn [f s]
        (reduce
         (fn [map x]
           (let [key (f x)]
             (assoc map key
                    (conj (get map key []) x))))
         {} s))]
  (and
   (= (__ #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})
   (= (__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
      {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
   (= (__ count [[1] [1 2] [3] [1 2 3] [2 3]])
      {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))

;; 64. Intro to Reduce

;;  Reduce takes a 2 argument function and an optional starting
;;  value. It then applies the function to the first 2 items in the
;;  sequence (or the starting value and the first element of the
;;  sequence). In the next iteration the function will be called on
;;  the previous return value and the next item from the sequence,
;;  thus reducing the entire collection to one value. Don't worry,
;;  it's not as complicated as it sounds.

(let [__ +]
  (and
   (= 15 (reduce __ [1 2 3 4 5]))
   (=  0 (reduce __ []))
   (=  6 (reduce __ 1 [2 3]))))

;; 65. Black Box Testing

;; Clojure has many sequence types, which act in subtly different
;; ways. The core functions typically convert them into a
;; uniform "sequence" type and work with them that way, but it can be
;; important to understand the behavioral and performance differences
;; so that you know which kind is appropriate for your application.

;; Write a function which takes a collection and returns one
;; of :map, :set, :list, or :vector - describing the type of
;; collection it was given.

;; You won't be allowed to inspect their class or use the built-in
;; predicates like list? - the point is to poke at them and understand
;; their behavior.

;; Special Restrictions: class type Class vector? sequential? list?
;; seq? map? set? instance? getClass

(let [__
      (fn[coll]
        (let [tmp (conj (empty coll)
                        [:key :vector] [:key :list] [:key :list])
              cnt (count tmp)]
          (cond
           (= 1 cnt)
           :map

           (= 2 cnt)
           :set

           :else
           (-> tmp first second))))]

  (and
   (= :map (__ {:a 1, :b 2}))
   (= :list (__ (range (rand-int 20))))
   (= :vector (__ [1 2 3 4 5 6]))
   (= :set (__ #{10 (rand-int 5)}))
   (= [:map :set :vector :list] (map __ [{} #{} [] ()]))))

;; 66. Greatest Common Divisor

;; Given two integers, write a function which returns the greatest
;; common divisor.

(let [__
      (fn[a b]
        (cond
         (< a b)
         (recur a (- b a))

         (< b a)
         (recur (- a b) b)

         :else
         a))]

(and
 (= (__ 2 4) 2)
 (= (__ 10 5) 5)
 (= (__ 5 7) 1)
 (= (__ 1023 858) 33)))

;; 67. Prime Numbers

;; Write a function which returns the first x number of prime numbers.

(let [__
      (fn[cnt]
        (let [prime? (fn[n]
                       (let [candidates (range 2 n)]
                         (not (some identity
                                    (map #(zero? (mod n %)) candidates)))))]
          (take cnt (filter prime? (iterate inc 2)))))]

  (and
   (= (__ 2) [2 3])
   (= (__ 5) [2 3 5 7 11])
   (= (last (__ 100)) 541)))

;; 68. Recurring Theme

;; Clojure only has one non-stack-consuming looping construct:
;; recur. Either a function or a loop can be used as the recursion
;; point. Either way, recur rebinds the bindings of the recursion
;; point to the values it is passed. Recur must be called from the
;; tail-position, and calling it elsewhere will result in an error.

(let [__
      [7 6 5 4 3]]

  (= __
  (loop [x 5
         result []]
    (if (> x 0)
      (recur (dec x) (conj result (+ 2 x)))
      result))))

;; 69. Merge with a Function

;; Write a function which takes a function f and a variable number of
;; maps. Your function should return a map that consists of the rest
;; of the maps conj-ed onto the first. If a key occurs in more than
;; one map, the mapping(s) from the latter (left-to-right) should be
;; combined with the mapping in the result by calling (f val-in-result
;; val-in-latter)

(let [__
      (fn
        [f & maps]
        (when (some identity maps)
          (let [merge-entry (fn [m e]
                              (let [k (key e) v (val e)]
                                (if (contains? m k)
                                  (assoc m k (f (get m k) v))
                                  (assoc m k v))))
                merge2 (fn [m1 m2]
                         (reduce merge-entry (or m1 {}) (seq m2)))]
            (reduce merge2 maps))))]

  (and
   (= (__ * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
      {:a 4, :b 6, :c 20})

   (= (__ - {1 10, 2 20} {1 3, 2 10, 3 15})
      {1 7, 2 10, 3 15})

   (= (__ concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
      {:a [3 4 5], :b [6 7], :c [8 9]})))



;; 70. Word Sorting

;; Write a function that splits a sentence up into a sorted list of
;; words. Capitalization should not affect sort order and punctuation
;; should be ignored.

(let [__
      (fn[s]
        (sort
         #(apply compare (map clojure.string/lower-case [%1 %2]))
         (map #(clojure.string/replace % #"[\.|!]" "")
              (clojure.string/split s #" "))))]

  (and
   (= (__  "Have a nice day.")
      ["a" "day" "Have" "nice"])

   (= (__  "Clojure is a fun language!")
      ["a" "Clojure" "fun" "is" "language"])

   (= (__  "Fools fall for foolish follies.")
   ["fall" "follies" "foolish" "Fools" "for"])))

;; 71. Rearranging Code: ->

;; The -> macro threads an expression x through a variable number of
;; forms. First, x is inserted as the second item in the first form,
;; making a list of it if it is not a list already. Then the first
;; form is inserted as the second item in the second form, making a
;; list of that form if necessary. This process continues for all the
;; forms. Using -> can sometimes make your code more readable.

(let [__ last]
  (= (__ (sort (rest (reverse [2 5 4 1 3 6]))))
     (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (__))
     5))

;; 72. Rearranging Code: ->>

;; The ->> macro threads an expression x through a variable number of
;; forms. First, x is inserted as the last item in the first form,
;; making a list of it if it is not a list already. Then the first
;; form is inserted as the last item in the second form, making a list
;; of that form if necessary. This process continues for all the
;; forms. Using ->> can sometimes make your code more readable.

(let [__ (partial reduce +)]
  (= (__ (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (__))
   11))

;; 73. Analyze a Tic-Tac-Toe Board

;; A tic-tac-toe board is represented by a two dimensional vector. X
;; is represented by :x, O is represented by :o, and empty is
;; represented by :e. A player wins by placing three Xs or three Os in
;; a horizontal, vertical, or diagonal row. Write a function which
;; analyzes a tic-tac-toe board and returns :x if X has won, :o if O
;; has won, and nil if neither player has won.

(let [__
      (fn [g]
        (let [g (into [] (flatten g))
              x? #(= :x %)
              o? #(= :o %)
              all? (partial every? identity)
              any? (partial some identity)
              ws ['(0 1 2) '(3 4 5) '(6 7 8)
                  '(0 3 6) '(1 4 7) '(2 5 8)
                  '(0 4 8) '(2 4 6)]
              winner
              (fn [player?]
                (any? (map #(all? (map player? (map g %))) ws)))]

          (cond (winner x?) :x
                (winner o?) :o)))]

  (and
   (= nil (__ [[:e :e :e]
               [:e :e :e]
               [:e :e :e]]))

   (= :x (__ [[:x :e :o]
              [:x :e :e]
              [:x :e :o]]))

   (= :o (__ [[:e :x :e]
              [:o :o :o]
              [:x :e :x]]))

   (= nil (__ [[:x :e :o]
               [:x :x :e]
               [:o :x :o]]))

   (= :x (__ [[:x :e :e]
              [:o :x :e]
              [:o :e :x]]))

   (= :o (__ [[:x :e :o]
              [:x :o :e]
              [:o :e :x]]))

   (= nil (__ [[:x :o :x]
               [:x :o :x]
               [:o :x :o]]))))

;; 74. Filter Perfect Squares

;; Given a string of comma separated integers, write a function which
;; returns a new comma separated string that only contains the numbers
;; which are perfect squares.

(let [s "4,5,6,7,8,9"]
  (map read-string (clojure.string/split s #",")))

(let [__
      (fn[s]
        (let [square?
              (fn[n]
                ((fn
                   [low high]
                   (let [mid (quot (+ high low) 2)
                         msq (* mid mid)]

                     (cond
                      (= n msq)
                      true

                      (> low high)
                      false

                      :else
                      (if (< n msq)
                        (recur low (dec mid))
                        (recur (inc mid) high))))) 1 n))]

          (apply str (interpose ","
                                (filter square?
                                        (map read-string
                                             (clojure.string/split s #",")))))))]
  (and
   (= (__ "4,5,6,7,8,9") "4,9")
   (= (__ "15,16,25,36,37") "16,25,36")))

;; 75. Euler Totient Function

;; Two numbers are coprime if their greatest common divisor equals
;; 1. Euler's totient function f(x) is defined as the number of
;; positive integers less than x which are coprime to x. The special
;; case f(1) equals 1. Write a function which calculates Euler's
;; totient function.

(let [__
      (fn [n]
        (let [gcd
              (fn[a b]
                (cond
                 (< a b)
                 (recur a (- b a))

                 (< b a)
                 (recur (- a b) b)

                 :else
                 a))]
          (inc (count (filter #(= 1 (gcd n %)) (range 2 n))))))]
  (and
   (= (__ 1) 1)
   (= (__ 10) (count '(1 3 7 9)) 4)
   (= (__ 40) 16)
   (= (__ 99) 60)))

;; 76. Intro to Trampoline

;; The trampoline function takes a function f and a variable number of
;; parameters. Trampoline calls f with any parameters that were
;; supplied. If f returns a function, trampoline calls that function
;; with no arguments. This is repeated, until the return value is not
;; a function, and then trampoline returns that non-function
;; value. This is useful for implementing mutually recursive
;; algorithms in a way that won't consume the stack.

(let [__
      [1 3 5 7 9 11]]
  (= __
     (letfn
         [(foo [x y]
            #(bar (conj x y) y))
          (bar [x y]
            (if (> (last x) 10)
              x
              #(foo x (+ 2 y))))]
       (trampoline foo [] 1))))

;; 77. Anagram Finder

;; Write a function which finds all the anagrams in a vector of
;; words. A word x is an anagram of word y if all the letters in x can
;; be rearranged in a different order to form y. Your function should
;; return a set of sets, where each sub-set is a group of words which
;; are anagrams of each other. Each sub-set should have at least two
;; words. Words without any anagrams should not be included in the
;; result.

(let [__
      (fn[words]
        (let [pairs
              (fn [a]
                (for [xa a xb a
                      :let [sxa (sort xa)
                            sxb (sort xb)]
                      :when (and (not (= xa xb))
                                 (= sxa sxb))] [sxa [xa xb]]))
              ->map
              (fn[coll]
                (loop [[head & more] coll
                       map {}]
                  (if (nil? head)
                    map
                    (let [[key [fst snd]] head]
                      (recur more
                             (assoc map key
                                    (conj (get map key #{}) fst snd)))))))]

          (set (vals (->map (pairs words))))))]

  (and
   (= (__ ["meat" "mat" "team" "mate" "eat"])
      #{#{"meat" "team" "mate"}})

   (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
      #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})))


;; 78. Reimplement trampoline

(let [__
      (fn rec
        ([f]
         (let [ret (f)]
           (if (fn? ret)
             (recur ret)
             ret)))
        ([f & args]
         (rec #(apply f args))))]

  (and
   (= (letfn [(triple [x] #(sub-two (* 3 x)))
          (sub-two [x] #(stop?(- x 2)))
          (stop? [x] (if (> x 50) x #(triple x)))]
        (__ triple 2))
      82)

   (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
          (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
    (map (partial __ my-even?) (range 6)))
  [true false true false true false])))

;; Reimplement the function described in "Intro to Trampoline".

(letfn [__
        (fn
          ([f]
           (let [ret (f)]
             (if (fn? ret)
               (recur ret)
               ret)))
          ([f & args]
           (recur #(apply f args))))]

  (and
   (= (letfn [(triple [x] #(sub-two (* 3 x)))
              (sub-two [x] #(stop?(- x 2)))
              (stop? [x] (if (> x 50) x #(triple x)))]
        (__ triple 2))
      82)

   (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
              (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
        (map (partial __ my-even?) (range 6)))
      [true false true false true false])))

;; 80. Perfect Numbers

;; A number is "perfect" if the sum of its divisors equal the number
;; itself. 6 is a perfect number because 1+2+3=6. Write a function
;; which returns true for perfect numbers and false otherwise.

(let [__
      (fn[n]
        (let [factors
              (fn[n]
                (for [i (range 1 n) :when (zero? (mod n i))] i))]
          (= n (reduce + (factors n)))))]
  (and
   (= (__ 6) true)
   (= (__ 7) false)
   (= (__ 496) true)
   (= (__ 500) false)
   (= (__ 8128) true)))

;; 81. Set Intersection

;; Write a function which returns the intersection of two sets. The
;; intersection is the sub-set of items that each set has in
;; common. Special restrictions: intersection.

(let [__
      (fn
        ([a] a)
        ([a b]
         (if (< (count b) (count a))
           (recur b a)
           (reduce
            (fn [res item]
              (if (contains? b item)
                res
                (disj res item)))
            a a))))]
  (and
   (= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3})
   (= (__ #{0 1 2} #{3 4 5}) #{})
   (= (__ #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})))

;; 82. Word Chains

;; A word chain consists of a set of words ordered so that each word
;; differs by only one letter from the words directly before and after
;; it. The one letter difference can be either an insertion, a
;; deletion, or a substitution. Here is an example word chain:

;; cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog

;; Write a function which takes a sequence of words, and returns true
;; if they can be arranged into one continous word chain, and false if
;; they cannot.

(let [__
      (fn [in]
        (let [words (into [] in)

              dist
              (fn[s t]
                (let [lev
                      (memoize
                       (fn aux[rec s t]
                         (cond
                          (empty? s)
                          (count t)

                          (empty? t)
                          (count s)

                          :else
                          (let [s' (butlast s)
                                t' (butlast t)]

                            (min (inc (rec rec s' t))
                                 (inc (rec rec s t'))
                                 (+ (rec rec s' t')
                                    (if (= (last s)
                                           (last t))
                                      0 1)))))))

                      ;; rebind
                      lev (partial lev lev)]
                  (lev s t)))

              adj
              (fn[s]
                (loop [[fst & more] s]
                  (cond

                   (nil? more)
                   true

                   (= 1 (dist fst (first more)))
                   (recur more)

                   :else
                   false)))

              permutations
              (fn rec [a-set]
                (cond (empty? a-set) '(())
                      (empty? (rest a-set)) (list (apply list a-set))
                      :else (for [x a-set y (rec (remove #{x} a-set))]
                              (cons x y))))]

          (not (empty? (filter adj (permutations words))))))]

  (and
   (= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
   (= false (__ #{"cot" "hot" "bat" "fat"}))
   (= false (__ #{"to" "top" "stop" "tops" "toss"}))
   (= true (__ #{"spout" "do" "pot" "pout" "spot" "dot"}))
   (= true (__ #{"share" "hares" "shares" "hare" "are"}))
   (= false (__ #{"share" "hares" "hare" "are"}))))


;; 83. A Half Truth

;; Write a function which takes a variable number of booleans. Your
;; function should return true if some of the parameters are true, but
;; not all of the parameters are true. Otherwise your function should
;; return false.

(let [__
      (fn [& args]
        (and
         (or (some identity args) false)
         (not (every? identity args))))]

  (and
   (= false (__ false false))
   (= true (__ true false))
   (= false (__ true))
   (= true (__ false true false))
   (= false (__ true true true))
   (= true (__ true true true false))))

;; 84. Transitive Closure

;; Write a function which generates the transitive closure of a binary
;; relation. The relation will be represented as a set of 2 item
;; vectors.

(let [__
      (fn[in]
        (loop [tuples in]
          (let [res
                (clojure.set/union tuples
                                   (into #{} (for [a tuples b tuples
                                                   :when (= (second a) (first b))]
                                               [(first a) (second b)])))]
            (if (= res tuples)
              res
              (recur res)))))]

  (and
   (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
     (= (__ divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]})))

  (let [more-legs
        #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
    (= (__ more-legs)
       #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
         ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}))

  (let [progeny
        #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
    (= (__ progeny)
       #{["father" "son"] ["father" "grandson"]
         ["uncle" "cousin"] ["son" "grandson"]})))

;; 85. Power Set

;; Write a function which generates the power set of a given set. The
;; power set of a set x is the set of all subsets of x, including the
;; empty set and x itself.

(let [__
      (fn [x]
        (let [x' (vec x)
              two-n #(reduce * (repeat % 2))
              n (count x')
              p (two-n n)

              make-set
              (fn [x bw]
                (let []
                  (loop [bit 0
                         res #{}]
                    (if (= bit n)
                      res
                      (if (zero? (bit-and (two-n bit) bw))
                        (recur (inc bit) res)
                        (recur (inc bit) (conj res (nth x' bit))))))))]

          (set (for [bw (range (two-n n))]
                 (make-set x bw)))))]
  (and
   (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
   (= (__ #{}) #{#{}})
   (= (__ #{1 2 3})
      #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
   (= (count (__ (into #{} (range 10)))) 1024)
))

;; 86. Happy Numbers

;; Happy numbers are positive integers that follow a particular
;; formula: take each individual digit, square it, and then sum the
;; squares to get a new number. Repeat with the new number and
;; eventually, you might get to a number whose squared sum is 1. This
;; is a happy number. An unhappy number (or sad number) is one that
;; loops endlessly. Write a function that determines if a number is
;; happy or not.

(let [__
      (fn [n]
        (loop [x n
               seen #{}]
          (cond
           (= 1 x)
           true

           (some #{x} seen)
           false

           :else
           (recur (reduce +
                          (map #(* % %)
                               (map #(- (int %) 48) (str x))))
                  (conj seen x)))))]
  (and
   (= (__ 7) true)
   (= (__ 986543210) true)
   (= (__ 2) false)
   (= (__ 3) false)))


;; 88. Symmetric Difference

;; Write a function which returns the symmetric difference of two
;; sets. The symmetric difference is the set of items belonging to one
;; but not both of the two sets.

(let [__
      (fn [a b]
        (clojure.set/union (clojure.set/difference a b)
                           (clojure.set/difference b a)))]
  (and
   (= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
   (= (__ #{:a :b :c} #{}) #{:a :b :c})
   (= (__ #{} #{4 5 6}) #{4 5 6})
   (= (__ #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})))

;; 89. Graph Tour

;; Starting with a graph you must write a function that returns true
;; if it is possible to make a tour of the graph in which every edge
;; is visited exactly once.

;; The graph is represented by a vector of tuples, where each tuple
;; represents a single edge.

;; The rules are:

;; - You can start at any node.
;; - You must visit each edge exactly once.
;; - All edges are undirected.

(let [__
      (fn [tuples]
        (let [connected?
              (fn[tuples]
                (let [vertexes
                      (into #{} (mapcat identity tuples))

                      neighbors
                      (fn[v]
                        (let [edges
                              (filter
                               #(or
                                 (= v (first %))
                                 (= v (second %))) tuples)

                              outgoing
                              (fn[e]
                                (if (= v (first e))
                                  e
                                  (into (empty e) (reverse e))))]

                          (into #{}
                                (map second
                                     (map outgoing edges)))))

                      reachable
                      (fn[v]
                        (loop [res #{v}]
                          (let [next
                                (into (empty res)
                                      (clojure.set/union res
                                                         (mapcat neighbors res)))]
                            (if (= res next)
                              res
                              (recur next)))))]

                  (every? identity
                          (map #(= vertexes %)
                               (map reachable vertexes)))))]

          (and
               (connected? tuples)

               ;; Euler's theorem
               (let [ranks
                     (map second
                          (frequencies
                           (mapcat identity tuples)))]

                 (or
                  (every? even? ranks)
                  (= 2 (count (map odd? ranks))))))))]

  (and
   (= true (__ [[:a :b]]))
   (= false (__ [[:a :a] [:b :b]]))
   (= false (__ [[:a :b] [:a :b] [:a :c] [:c :a] [:a :d] [:b :d] [:c :d]]))
   (= true (__ [[1 2] [2 3] [3 4] [4 1]]))
   (= true (__ [[:a :b] [:a :c] [:c :b] [:a :e] [:b :e] [:a :d] [:b :d] [:c :e] [:d :e] [:c :f] [:d :f]]))
   (= false (__ [[1 2] [2 3] [2 4] [2 5]]))))

;; 90. Cartesian Product

;; Write a function which calculates the Cartesian product of two
;; sets.
(let [__
      (fn [a b]
        (into #{} (for [xa a xb b] [xa xb])))]

  (and
   (= (__ #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
      #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
        ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
        ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})

   (= (__ #{1 2 3} #{4 5})
      #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})

   (= 300 (count (__ (into #{} (range 10))
                     (into #{} (range 30)))))))


(let [[eye & more] #{:ace :king :queen :jack}]
  (prn eye)
  (prn more))


;; 91. Graph Connectivity

;; Given a graph, determine whether the graph is connected. A
;; connected graph is such that a path exists between any two given
;; nodes.
;;
;; - Your function must return true if the graph is connected and
;; false otherwise.
;;
;; - You will be given a set of tuples representing the edges of a
;; graph. Each member of a tuple being a vertex/node in the graph.
;;
;; - Each edge is undirected (can be traversed either direction).

(let [__
      (fn[tuples]
        (let [vertexes
              (into #{} (mapcat identity tuples))

              neighbors
              (fn[v]
                (let [edges
                      (filter #(or (= v (first %)) (= v (second %))) tuples)

                      outgoing
                      (fn[e] (if (= v (first e))
                               e (into (empty e) (reverse e))))]

                  (->> edges
                       (map outgoing)
                       (map second)
                       (into #{}))))

              reachable
              (fn[v]
                (loop [res #{v}]
                  (let [next
                        (into (empty res)
                              (clojure.set/union res
                                                 (mapcat neighbors res)))]
                    (if (= res next)
                      res (recur next)))))]

          (every? identity (->> vertexes
                                (map reachable)
                                (map #(= vertexes %))))))]

  (and
   (= true (__ #{[:a :a]}))

   (= true (__ #{[:a :b]}))

   (= false (__ #{[1 2] [2 3] [3 1]
                  [4 5] [5 6] [6 4]}))

   (= true (__ #{[1 2] [2 3] [3 1]
                 [4 5] [5 6] [6 4] [3 4]}))

   (= false (__ #{[:a :b] [:b :c] [:c :d]
                  [:x :y] [:d :a] [:b :e]}))

   (= true (__ #{[:a :b] [:b :c] [:c :d]
                 [:x :y] [:d :a] [:b :e] [:x :a]}))))

;; 92. Read Roman numerals

;; Roman numerals are easy to recognize, but not everyone knows all
;; the rules necessary to work with them. Write a function to parse a
;; Roman-numeral string and return the number it represents. You can
;; assume that the input will be well-formed, in upper-case, and
;; follow the subtractive principle. You don't need to handle any
;; numbers greater than MMMCMXCIX (3999), the largest number
;; representable with ordinary letters.

(let [__
      (fn [w]
        (let [lits
              { \I 1
                \V 5
                \X 10
                \L 50
                \C 100
                \D 500
                \M 1000 }

              lit2int
              (fn [lit]
                (if (nil? lit) 0 (lits lit)))

              aux
              (fn [[head & tail] res tmp last]
                ;; (prn head tail res tmp last)
                (let [head-value (lit2int head)
                      last-value (lit2int last)]

                  (cond
                   (nil? head)
                   (+ res tmp)

                   (<= head-value last-value)
                   (recur tail (+ res tmp) head-value head)

                   :else
                   (recur tail res (- head-value tmp) head))))]

          (aux w 0 0 nil)))]

  (and
   (= 14 (__ "XIV"))
   (= 827 (__ "DCCCXXVII"))
   (= 3999 (__ "MMMCMXCIX"))
   (= 48 (__ "XLVIII"))))

;; 93. Partial Flatten a Sequence

;; Write a function which flattens any nested combination of
;; sequential things (lists, vectors, etc.), but maintains the lowest
;; level sequential items. The result should be a sequence of
;; sequences with only one level of nesting.
(let [__
      (fn[x]
        (filter
         #(and (sequential? %)
               (not (sequential? (first %))))
         (tree-seq sequential? seq x)))]

  (and
   (= (__ [["Do"] ["Nothing"]])
      [["Do"] ["Nothing"]]))

  (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
     [[:a :b] [:c :d] [:e :f]])

  (= (__ '((1 2)((3 4)((((5 6)))))))
     '((1 2)(3 4)(5 6))))

;; 94. The Game of Life

;; The game of life is a cellular automaton devised by mathematician John Conway.

;; The 'board' consists of both live (#) and dead ( ) cells. Each cell
;; interacts with its eight neighbours (horizontal, vertical,
;; diagonal), and its next state is dependent on the following rules:

;; 1) Any live cell with fewer than two live neighbours dies, as if caused by under-population.
;; 2) Any live cell with two or three live neighbours lives on to the next generation.
;; 3) Any live cell with more than three live neighbours dies, as if by overcrowding.
;; 4) Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

;; Write a function that accepts a board, and returns a board
;; representing the next generation of cells.

(let [__
      (fn[grid]
        (let [nrows (count grid)
              ncols (count (grid 0))

              grid->set
              (fn[grid]
                (loop [r 0 c 0 res #{}]
                  (let [cell (nth (nth grid r) c)
                        next-c (mod (inc c) ncols)
                        next-r (if (zero? next-c) (inc r) r)]
                    (if (= nrows next-r)
                      res
                      (recur next-r next-c
                             (if (= \# cell)
                               (conj res [r c])
                               res))))))

              set->grid
              (fn[set]
                (let [tmp (for [r (range nrows)
                                c (range ncols)]
                            (some #{[r c]} set))]
                  (into[] (for [r (partition ncols
                                             (map #(if (nil? %) \space \#) tmp))]
                            (apply str (interpose "" r))))))

              neighbors
              (fn[[x y]]
                (for [dx [-1 0 1]
                      dy (if (zero? dx)
                           [-1 1]
                           [-1 0 1])]
                  [(+ dx x) (+ dy y)]))

              live
              (fn[n alive?]
                (or (= n 3)
                    (and (= n 2) alive?)))

              step
              (fn[world]
                (set
                 (for [[cell n] (frequencies (mapcat neighbors world))
                       :when (live n (world cell))]
                   cell)))]

          (-> grid
              grid->set
              step
              set->grid)))]

  (and
   (= (__ ["      "
           " ##   "
           " ##   "
           "   ## "
           "   ## "
           "      "])
      ["      "
       " ##   "
       " #    "
       "    # "
       "   ## "
       "      "])

   (= (__ ["     "
           "     "
           " ### "
           "     "
           "     "])
      ["     "
       "  #  "
       "  #  "
       "  #  "
       "     "])

   (= (__ ["      "
           "      "
           "  ### "
           " ###  "
           "      "
           "      "])
      ["      "
       "   #  "
       " #  # "
       " #  # "
       "  #   "
       "      "])))

;; 95. To Tree, or not to Tree

;; Write a predicate which checks whether or not a given sequence
;; represents a binary tree. Each node in the tree must have a value,
;; a left child, and a right child.

(let [__
      (fn rec[t]

        (cond
         (nil? t)
         true

         (and (or (vector? t)
                  (seq? t))
              (= 3 (count t)))

         (let [[root lhs rhs] t]
           (and
            (rec lhs)
            (rec rhs)))

         :else
         false))]

  (and
   (= (__ '(:a (:b nil nil) nil))
      true)

   (= (__ '(:a (:b nil nil)))
      false)

   (= (__ [1 nil [2 [3 nil nil] [4 nil nil]]])
      true)

   (= (__ [1 [2 nil nil] [3 nil nil] [4 nil nil]])
      false)

   (= (__ [1 [2 [3 [4 nil nil] nil] nil] nil])
      true)

   (= (__ [1 [2 [3 [4 false nil] nil] nil] nil])
      false)

   (= (__ '(:a nil ()))
      false)))

;; 96. Beauty is symmetry

;; Let us define a binary tree as "symmetric" if the left half of the
;; tree is the mirror image of the right half of the tree. Write a
;; predicate to determine whether or not a given binary tree is
;; symmetric. (see To Tree, or not to Tree for a reminder on the tree
;; representation we're using).

(let [__
      (fn [t]
        (let [lhs (fn[t] (nth t 1))
              rhs (fn[t] (nth t 2))

              aux
              (fn rec[l r]
                (cond

                 (nil? l)
                 (nil? r)

                 (nil? r)
                 (nil? l)

                 :else
                 (and (= (first l)
                         (first r))

                      (rec (lhs l) (rhs r))
                      (rec (rhs l) (lhs r)))))]
          (if (nil? t)
            true
            (aux (lhs t) (rhs t)))))]

  (and
   (= (__ '(:a (:b nil nil) (:b nil nil)))
      true)
   (= (__ '(:a (:b nil nil) nil))
      false)
   (= (__ '(:a (:b nil nil) (:c nil nil)))
      false)

   (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
           [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
      true)


   (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
           [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
      false)
   ))

;; 97. Pascal's Triangle

;; Pascal's triangle is a triangle of numbers computed using the
;; following rules:

;; - The first row is 1.
;; - Each successive row is computed by adding together adjacent
;; numbers in the row above, and adding a 1 to the beginning and end
;; of the row.

;; Write a function which returns the nth row of Pascal's Triangle.
(let [__
      (fn rec [n]
        (if (= 1 n)
          [1]
          (let [r (rec (dec n))
                rl (concat [0] r)
                rr (concat r [0])]
            (vec (for [[x y]
                       (map vector rl rr)]
                   (+ x y))))))]

  (and
   (= (__ 1) [1])

   (= (map __ (range 1 6))
   [     [1]
        [1 1]
       [1 2 1]
      [1 3 3 1]
     [1 4 6 4 1]])

   (= (__ 11)
   [1 10 45 120 210 252 210 120 45 10 1])))

;; 98. Equivalence Classes

;; A function f defined on a domain D induces an equivalence relation
;; on D, as follows: a is equivalent to b with respect to f if and
;; only if (f a) is equal to (f b). Write a function with arguments f
;; and D that computes the equivalence classes of D with respect to f.

(let [__
      (fn[f coll]
        (loop [[eye & more] (seq coll)
               map {}]
          (if (nil? eye)
            (into #{} (vals map))
            (let [class (f eye)]
              (recur more (assoc map class
                                 (conj (get map class #{}) eye)))))))]
  (and
   (= (__ #(* % %) #{-2 -1 0 1 2})
      #{#{0} #{1 -1} #{2 -2}})

   (= (__ #(rem % 3) #{0 1 2 3 4 5 })
   #{#{0 3} #{1 4} #{2 5}})

   (= (__ identity #{0 1 2 3 4})
   #{#{0} #{1} #{2} #{3} #{4}})

   (= (__ (constantly true) #{0 1 2 3 4})
   #{#{0 1 2 3 4}})))

;; 99. Product Digits

;; Write a function which multiplies two numbers and returns the
;; result as a sequence of its digits.

(let [__
      (fn[a b]
        (let [tmp (* a b)]
          (map #(- (int %) 48) (vec (str tmp)))))]
  (and
   (= (__ 1 1) [1])
   (= (__ 99 9) [8 9 1])
   (= (__ 999 99) [9 8 9 0 1])))

;; 100. Least Common Multiple

;; Write a function which calculates the least common multiple. Your
;; function should accept a variable number of positive integers or
;; ratios.

(let [__
      (fn rec
        ([x y]
         (let [gcd
               (fn[a b]
                 (cond
                  (< a b)
                  (recur a (- b a))

                  (< b a)
                  (recur (- a b) b)

                  :else
                  a))

               lcm (fn[a b]
                     (/ (* a b) (gcd a b)))]

           (lcm x y)))
        ([x y & more]
         (reduce rec (rec x y) more)))]

  (and
   (== (__ 2 3) 6)
   (== (__ 5 3 7) 105)
   (== (__ 1/3 2/5) 2)
   (== (__ 3/4 1/6) 3/2)
   (== (__ 7 5/7 2 3/5) 210)))


;; 101. Levenshtein distance

;; Given two sequences x and y, calculate the Levenshtein distance of
;; x and y, i. e. the minimum number of edits needed to transform x
;; into y. The allowed edits are:

;; - insert a single item
;; - delete a single item
;; - replace a single item with another item

;; WARNING: Some of the test cases may timeout if you write an
;; inefficient solution!

(let [__
      (fn[s t]
        (let [lev
              (memoize
               (fn aux[rec s t]
                 (cond
                  (empty? s)
                  (count t)

                  (empty? t)
                  (count s)

                  :else
                  (let [s' (butlast s)
                        t' (butlast t)]

                    (min (inc (rec rec s' t))
                         (inc (rec rec s t'))
                         (+ (rec rec s' t')
                            (if (= (last s)
                                   (last t))
                              0 1)))))))

              ;; rebind
              lev (partial lev lev)]
          (lev s t)))]

  (and
   (= (__ "kitten" "sitting") 3)
   (= (__ "closure" "clojure") (__ "clojure" "closure") 1)
   (= (__ "xyx" "xyyyx") 2)
   (= (__ "" "123456") 6)
   (= (__ "Clojure" "Clojure") (__ "" "") (__ [] []) 0)
   (= (__ [1 2 3 4] [0 2 3 4 5]) 2)
   (= (__ '(:a :b :c :d) '(:a :d)) 2)
   (= (__ "ttttattttctg" "tcaaccctaccat") 10)
   (= (__ "gaattctaatctc" "caaacaaaaaattt") 9)))

;; 102. intoCamelCase

;; When working with java, you often need to create an object with
;; fieldsLikeThis, but you'd rather work with a hashmap that
;; has :keys-like-this until it's time to convert. Write a function
;; which takes lower-case hyphen-separated strings and converts them
;; to camel-case strings.

(let [__
      (fn[s]
        (let [[ft & rt] (clojure.string/split s #"\-")]
          (str ft (apply str (map clojure.string/capitalize rt)))))]
  (and
   (= (__ "something") "something")
   (= (__ "multi-word-key") "multiWordKey")
   (= (__ "leaveMeAlone") "leaveMeAlone")))

;; 103. Generating k-combinations

;; Given a sequence S consisting of n elements generate all
;; k-combinations of S, i. e. generate all possible sets consisting of
;; k distinct elements taken from S. The number of k-combinations for
;; a sequence is equal to the binomial coefficient.

(let [__
      (fn [k set]
        (let [coll (into [] set)]
          (letfn [(comb-aux [k start]
                    (if (= 1 k)
                      (for [x (range start (count coll))]
                        (conj #{} (set (nth coll x))))
                      (for [x (range start (count coll))
                            xs (comb-aux (dec k) (inc x))]
                        (into #{} (conj xs (nth coll x))))))]
            (into #{} (comb-aux k 0)))))]
  (and
   (= (__ 1 #{4 5 6}) #{#{4} #{5} #{6}})
   (= (__ 10 #{4 5 6}) #{})
   (= (__ 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})
   (= (__ 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                            #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})
   (= (__ 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}})
   (= (__ 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                         #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})))

;; 104. Write Roman Numerals

;; This is the inverse of Problem 92, but much easier. Given an
;; integer smaller than 4000, return the corresponding roman numeral
;; in uppercase, adhering to the subtractive principle.

(let [__
      (fn[n]
        (loop [drum [["M" 1000]["CM" 900]["D" 500]["CD" 400]["C" 100]["XC" 90]
                     ["L" 50]["XL" 40] ["X" 10]["IX" 9]["V" 5]["IV" 4] ["I" 1]]
               in  n
               out []]

          (let [head (first drum)
                lit (first head)
                val (second head)]

            (cond
             (zero? in)
             (apply str (interpose "" out))

             :else
             (if (<= val in)
               (recur drum (- in val) (conj out lit))
               (recur (rest drum) in out))))))]

  (and
   (= "I" (__ 1))
   (= "XXX" (__ 30))
   (= "IV" (__ 4))
   (= "CXL" (__ 140))
   (= "DCCCXXVII" (__ 827))
   (= "MMMCMXCIX" (__ 3999))
   (= "XLVIII" (__ 48))))

;; 105. Identify keys and values

;; Given an input sequence of keywords and numbers, create a map such
;; that each key in the map is a keyword, and the value is a sequence
;; of all the numbers (if any) between it and the next keyword in the
;; sequence.

(let [__
      (fn[coll]
        (loop [[head & more] coll last nil res {}]
          (cond
           (nil? head)
           res

           (keyword? head)
           (recur more head (assoc res head []))

           :else
           (recur more last (assoc res last
                                   (conj (get res last) head))))))]
  (and
   (= {} (__ []))
   (= {:a [1]} (__ [:a 1]))
   (= {:a [1], :b [2]} (__ [:a 1, :b 2]))
   (= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))))

;; 106. Number Maze

;; Given a pair of numbers, the start and end point, find a path
;; between the two using only three possible operations:

;; double
;; halve (odd numbers cannot be halved)
;; add 2

;; Find the shortest path through the "maze". Because there are
;; multiple shortest paths, you must return the length of the shortest
;; path, not the path itself.

(let [__
      (fn[a b]
        (let [path-length
              (fn[a b parents]
                (loop [res 1
                       x b]
                  (if (= x a)
                    res
                    (recur (inc res) (parents x)))))

              neighbors
              (fn[n]
                (remove nil?
                        [(* n 2) (+ n 2) (if (even? n) (quot n 2))]))]

              (loop [open (conj clojure.lang.PersistentQueue/EMPTY a)
                     parents {}
                     closed #{}]

                (if (empty? open)
                  false
                  (let
                      [head (peek open)
                       open (pop open)]
                    (if (= b head)
                      (path-length a b parents)
                      (let [new (remove closed (neighbors head))]
                        ;; (prn closed "::" head "->" new)
                        (recur (into open new)
                               (into parents (zipmap new (repeat head)))
                               (apply conj closed new)))))))))]

  (and
   (= 1 (__ 1 1))
   (= 3 (__ 3 12))
   (= 3 (__ 12 3))
   (= 3 (__ 5 9))
   (= 9 (__ 9 2))
   (= 5 (__ 9 12))))

;; 107. Simple closures

;; Lexical scope and first-class functions are two of the most basic
;; building blocks of a functional language like Clojure. When you
;; combine the two together, you get something very powerful called
;; lexical closures. With these, you can exercise a great deal of
;; control over the lifetime of your local bindings, saving their
;; values for use later, long after the code you're running now has
;; finished.

;; It can be hard to follow in the abstract, so let's build a simple
;; closure. Given a positive integer n, return a function (f x) which
;; computes xn. Observe that the effect of this is to preserve the
;; value of n for use outside the scope in which it is defined.

(let [__
      (fn[n]
        (fn[x]
          (reduce * 1 (repeat n x))))]

  (and
   (= 256 ((__ 2) 16),((__ 8) 2))
   (= [1 8 27 64] (map (__ 3) [1 2 3 4]))
   (= [1 2 4 8 16] (map #((__ %) 2) [0 1 2 3 4]))))

;; 108. Lazy Searching

;; Given any number of sequences, each sorted from smallest to
;; largest, find the smallest single number which appears in all of
;; the sequences. The sequences may be infinite, so be careful to
;; search lazily.

(let [__
      (fn [& colls]
        (if (some empty? colls) nil
            (let [heads
                  (into [] (map first colls))]

              (if (apply = heads)
                (first heads)
                (let [pivot
                      (apply min heads)

                      trim
                      (fn [coll]
                             (if (= pivot (first coll))
                               (rest coll)  coll))]

                  (recur (map trim colls)))))))]

  (and
   (= nil (__ []))

   (= nil (__ [0] [1] ))

   (= 3 (__ [3 4 5]))

   (= 4 (__ [1 2 3 4 5 6 7] [0.5 3/2 4 19]))

   (= 7 (__ (range) (range 0 100 7/6) [2 3 5 7 11 13]))

   (= 64 (__ (map #(* % % %) (range)) ;; perfect cubes
          (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
          (iterate inc 20))) ;; at least as large as 20
   ))

;; 110. Sequence of pronunciations

;; Write a function that returns a lazy sequence of "pronunciations"
;; of a sequence of numbers. A pronunciation of each element in the
;; sequence consists of the number of repeating identical numbers and
;; the number itself. For example, [1 1] is pronounced as [2 1] ("two
;; ones"), which in turn is pronounced as [1 2 1 1] ("one two, one
;; one").

;; Your function should accept an initial sequence of numbers, and
;; return an infinite lazy sequence of pronunciations, each element
;; being a pronunciation of the previous element.

(let [__
      (fn [sq]
        (let [pronunciation
              (fn[sq]
                (let [aux
                      (fn[res eye cnt [hd & more]]
                        (prn res eye cnt (cons hd more))
                        (cond
                         (nil? hd)
                         (conj res cnt eye)

                         (nil? eye)
                         (recur res hd 1 more)

                         (= eye hd)
                         (recur res hd (inc cnt) more)

                         :otherwise
                         (recur (conj res cnt eye) hd 1 more)))]

                  (aux [] nil 0 sq)))]
          (rest (iterate pronunciation sq))))]

  (and
   (= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))
   (= [3 1 2 4] (first (__ [1 1 1 4 4])))
   (= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))
   (= 338 (count (nth (__ [3 2]) 15)))))

;; 111. Crossword puzzle

;; Write a function that takes a string and a partially-filled
;; crossword puzzle board, and determines if the input string can be
;; legally placed onto the board.

;; The crossword puzzle board consists of a collection of
;; partially-filled rows. Empty spaces are denoted with an underscore
;; (_), unusable spaces are denoted with a hash symbol (#), and
;; pre-filled spaces have a character in place; the whitespace
;; characters are for legibility and should be ignored.

;; For a word to be legally placed on the board:
;; - It may use empty spaces (underscores)
;; - It may use but must not conflict with any pre-filled characters.
;; - It must not use any unusable spaces (hashes).
;; - There must be no empty spaces (underscores) or extra characters
;; - before or after the word (the word may be bound by unusable
;; - spaces though).
;; - Characters are not case-sensitive.
;; - Words may be placed vertically (proceeding top-down only), or
;; - horizontally (proceeding left-right only).

(let [__
      (fn[word split-rows]
        (let [parse-row
              (fn[s]
                clojure.string/join ""
                (clojure.string/split s #" "))

              rows
              (into [] (map parse-row split-rows))

              no-rows
              (count rows)

              no-cols
              (count (rows 0))

              char-eq
              (fn [a b]
                (= (str a)
                   (str b)))

              ;; cell accessor
              cell
              (fn [r c]
                (let [row (get rows r nil)]
                  (if row
                    (get row c))))

              ;; match when either is _ or they're the same
              match-letter
              (fn [a b]
                (or (char-eq a \_)
                    (char-eq b \_)
                    (char-eq a b)))

              ;; brick matcher
              match-brick
              (fn [c]
                (char-eq c \#))

              ;; cell matcher
              match-cell
              (fn [r c ch]
                (match-letter (cell r c) ch))

              ;; boundary matcher
              match-boundary
              (fn [r c]
                (or
                 (> 0 r)
                 (> 0 c)
                 (>= r no-rows)
                 (>= c no-cols)
                 (match-brick (cell r c))))

              ;; word matcher
              match-word
              (fn [r c o]
                (let [word-constraint
                      (cond

                       (= :vert o)
                       (every? identity (for [i (range (count word))]
                                          (match-cell (+ i r) c (get word i "#"))))

                       :else
                       (every? identity (for [i (range (count word))]
                                          (match-cell r (+ i c) (get word i "#")))))

                      boundary-constraint
                      (cond

                       (= :vert o)
                       (and
                        (match-boundary (- r 1) c)
                        (match-boundary (+ r (count word)) c))

                       :else
                       (and
                        (match-boundary r (- c 1))
                        (match-boundary r (+ c (count word)))))

                      ret
                      (and word-constraint
                           boundary-constraint)]

                  ;; (prn r c o " --> " word-constraint boundary-constraint ret)
                  ret))

              checkers
              (for [r (vec (range no-rows))
                    c (vec (range no-cols))
                    o [:vert :horiz]]
                (match-word r c o))]

          (or (some identity checkers) false)))]

  (and

   (= true  (__ "the" ["_ # _ _ e"]))

   (= false (__ "the" ["c _ _ _"
                       "d _ # e"
                       "r y _ _"]))

   (= true  (__ "joy" ["c _ _ _"
                       "d _ # e"
                       "r y _ _"]))

   (= false (__ "joy" ["c o n j"
                       "_ _ y _"
                       "r _ _ #"]))

   (= true  (__ "clojure" ["_ _ _ # j o y"
                           "_ _ o _ _ _ _"
                           "_ _ f _ # _ _"]))))

;; 114. Global take-while

;; take-while is great for filtering sequences, but it limited: you
;; can only examine a single item of the sequence at a time. What if
;; you need to keep track of some state as you go over the sequence?

;; Write a function which accepts an integer n, a predicate p, and a
;; sequence. It should return a lazy sequence of items in the list up
;; to, but not including, the nth item that satisfies the predicate.

(let [__
      (fn [n p sq]
        (let [aux
              (fn [res n p [hd & more]]
                (cond
                 (nil? hd)
                 res

                  (p hd)
                  (let [xn (dec n)]
                    (if (zero? xn) res
                        (recur (conj res hd) xn p more)))

                  :otherwise
                  (recur (conj res hd) n p more)))]

          (aux [] n p sq)))]

  (and
   (= [2 3 5 7 11 13]
      (__ 4 #(= 2 (mod % 3))
         [2 3 5 7 11 13 17 19 23])))

  (= ["this" "is" "a" "sentence"]
   (__ 3 #(some #{\i} %)
         ["this" "is" "a" "sentence" "i" "wrote"]))

  (= ["this" "is"]
   (__ 1 #{"a"}
         ["this" "is" "a" "sentence" "i" "wrote"])))

;; 115. The Balance of N

;; A balanced number is one whose component digits have the same sum
;; on the left and right halves of the number. Write a function which
;; accepts an integer n, and returns true iff n is balanced.

(let [__
      (fn[n]
        (let [chr->int #(- (int %) (int \0))]
          (loop [s (str n)
                 lhs 0
                 rhs 0]

            (cond
             (<= (count s) 1)
             (= lhs rhs)

             :else
             (let [l (chr->int (first s))
                   r (chr->int (last s))
                   s' (rest (butlast s))]
               (recur s' (+ lhs l) (+ rhs r)))))))]

  (and
   (= true (__ 11))
   (= true (__ 121))
   (= false (__ 123))
   (= true (__ 0))
   (= false (__ 88099))
   (= true (__ 89098))
   (= true (__ 89089))
   (= (take 20 (filter __ (range)))
   [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])))

;; 116. Prime Sandwich

;; A balanced prime is a prime number which is also the mean of the
;; primes directly before and after it in the sequence of valid
;; primes. Create a function which takes an integer n, and returns
;; true iff it is a balanced prime.


(let [__
      (fn[n]
        (let [prime? (fn[n]
                       (if (< n 2)
                         false
                         (let [candidates (range 2 n)]
                           (not (some identity
                                      (map #(zero? (mod n %)) candidates))))))]
          (and
           (>= n 5)
           (prime? n)
           (let [prev (first (filter prime? (iterate dec (dec n))))
                 next (first (filter prime? (iterate inc (inc n))))]
             (= (+ prev next) (* 2 n))))))]
  (and
   (= false (__ 4))
   (= true (__ 563))
   (= 1103 (nth (filter __ (range)) 15))))

;; 117. For Science!

;;  A mad scientist with tenure has created an experiment tracking
;;  mice in a maze. Several mazes have been randomly generated, and
;;  you've been tasked with writing a program to determine the mazes
;;  in which it's possible for the mouse to reach the cheesy
;;  endpoint. Write a function which accepts a maze in the form of a
;;  collection of rows, each row is a string where:

;;  spaces represent areas where the mouse can walk freely hashes (#)
;;  represent walls where the mouse can not walk, M represents the
;;  mouse's starting point and C represents the cheese which the mouse
;;  must reach

;; The mouse is not allowed to travel diagonally in the maze (only
;; up/down/left/right), nor can he escape the edge of the maze. Your
;; function must return true iff the maze is solvable by the mouse.

(let [__
      (fn[grid]
        (let [index-of
              (fn [xs x]
                (loop [a (first xs)
                       r (rest xs)
                       i 0]
                  (cond
                   (= a x)    i
                   (empty? r) nil
                   :else      (recur (first r) (rest r) (inc i)))))

              find-obj
              (fn[obj]
                (loop [r 0]
                  (let [c (index-of (grid r) obj)]
                    (if (not (nil? c))
                      [r c]
                      (recur (inc r))))))

              walkable?
              (fn[[r c]]
                (let [cell (nth (nth grid r) c)]
                  (or (= \space cell)
                      (= \C cell))))

              goal?
              (fn[[r c]]
                (= \C (nth (nth grid r) c)))

              neighbors
              (fn[[r c]]
                (remove nil?
                        [(if (< 0 r) [(dec r) c])
                         (if (< 0 c) [r (dec c)])
                         (if (< r (dec (count grid))) [(inc r) c])
                         (if (< c (dec (count (grid 0)))) [r (inc c)])]))

              [cr cc] (find-obj \C)]
          (loop [open (conj clojure.lang.PersistentQueue/EMPTY (find-obj \M))
                 closed #{}]

            (if (empty? open)
              false
              (let
                  [head (peek open)
                   open (pop open)
                   closed (conj closed head)]
                ;; (prn "H: " head)
                (if (goal? head)
                  true
                  (let [new (for [node (neighbors head)
                                  :when (and
                                         (not (contains? closed node))
                                         (walkable? node))] node)]
                    ;; (prn "N: " new)
                    (recur (into open new) closed))))))))]

  (and
   (= true  (__ ["M   C"]))
   (= false (__ ["M # C"]))

   (= true  (__ ["#######"
                 "#     #"
                 "#  #  #"
                 "#M # C#"
                 "#######"]))

   (= false (__ ["########"
                 "#M  #  #"
                 "#   #  #"
                 "# # #  #"
                 "#   #  #"
                 "#  #   #"
                 "#  # # #"
                 "#  #   #"
                 "#  #  C#"
                 "########"]))

   (= false (__ ["M     "
                 "      "
                 "      "
                 "      "
                 "    ##"
                 "    #C"]))

   (= true  (__ ["C######"
                 " #     "
                 " #   # "
                 " #   #M"
                 "     # "]))

   (= true  (__ ["C# # # #"
                 "        "
                 "# # # # "
                 "        "
                 " # # # #"
                 "        "
                 "# # # #M"]))))

;; 118. Reimplement Map

;; Map is one of the core elements of a functional programming
;; language. Given a function f and an input sequence s, return a lazy
;; sequence of (f x) for each element x in s. Special restrictions:
;; map map-indexed mapcat for

(let [__
      (fn rec [f coll]
        (lazy-seq
         (when-let [s (seq coll)]
           (cons (f (first s))
                 (rec f (rest s))))))]

  (and
   (= [3 4 5 6 7]
      (__ inc [2 3 4 5 6]))

   (= (repeat 10 nil)
      (__ (fn [_] nil) (range 10)))

   (= [1000000 1000001]
      (->> (__ inc (range))
           (drop (dec 1000000))
           (take 2)))))


;; 120. Sum of Square of digits

;; Write a function which takes a collection of integers as an
;; argument. Return the count of how many elements are smaller than
;; the sum of their squared component digits. For example: 10 is
;; larger than 1 squared plus 0 squared; whereas 15 is smaller than 1
;; squared plus 5 squared.

(let [__
      (fn [coll]
        (let [selector
              (fn [n]
                (let [digits (map #(- (int %) 48) (str n))
                      sum-square-digits (reduce + (map (fn[n] (* n n)) digits))]
                  (< n sum-square-digits)))]
          (count (filter selector coll))))]

  (and
   (= 8 (__ (range 10)))
   (= 19 (__ (range 30)))
   (= 50 (__ (range 100)))
   (= 50 (__ (range 1000)))))


;; 121. Universal Computation Engine

;; Given a mathematical formula in prefix notation, return a function
;; that calculates the value of the formula. The formula can contain
;; nested calculations using the four basic mathematical operators,
;; numeric constants, and symbols representing variables. The returned
;; function has to accept a single parameter containing the map of
;; variable names to their values.

(let [__
      (fn [expr]
        (let [ops {'* * '/ / '+ + '- -}
              aux (fn rec [env expr]
                    (cond
                     (list? expr)
                     (apply (ops (first expr))
                            (map #(rec env %) (rest expr)))

                     (symbol? expr)
                     (get env expr nil)

                     :else
                     expr))]

          (fn [env]
            (aux env expr))))]

  (and
   (= 2 ((__ '(/ a b))
         '{b 8 a 16}))

   (= 8 ((__ '(+ a b 2))
         '{a 2 b 4}))

   (= [6 0 -4]
      (map (__ '(* (+ 2 a)
                   (- 10 b)))
           '[{a 1 b 8}
             {b 5 a -2}
             {a 2 b 11}]))
   (= 1 ((__ '(/ (+ x 2)
                 (* 3 (+ y 1))))
         '{x 4 y 1}))))


;; 122. Read a binary number

;; Convert a binary number, provided in the form of a string, to its
;; numerical value.

(let [__
      (fn [s]
        (loop [[f & r] (reverse s)
               res 0
               mul 1]
          (if (nil? f)
            res
            (recur r (+ res (* mul (- (int f) 48))) (* 2 mul)))))]
  (and
   (= 0     (__ "0"))
   (= 7     (__ "111"))
   (= 8     (__ "1000"))
   (= 9     (__ "1001"))
   (= 255   (__ "11111111"))
   (= 1365  (__ "10101010101"))
   (= 65535 (__ "1111111111111111"))))

;; 125. Gus' Quinundrum

;; Create a function of no arguments which returns a string that is an
;; exact copy of the function itself.

;; Fun fact: Gus is the name of the 4Clojure dragon.

;; 126. Through the Looking Glass

;; Enter a value which satisfies the following

(let [x java.lang.Class]
  (and (= (class x) x) x))

;; 128. Recognize Playing Cards

(let [__
      (fn
        [[suit rank]]
        (let [suits (vec "HDCS")
              suit->idx (zipmap suits (range (count suits)))

              ranks (vec "23456789TJQKA")
              rank->idx (zipmap ranks (range (count ranks)))]

          {:suit ({0 :heart 1 :diamond 2 :club 3 :spade} (get suit->idx suit nil))
           :rank (get rank->idx rank nil)}))]

  (and
   (= {:suit :diamond :rank 10} (__ "DQ"))
   (= {:suit :heart :rank 3} (__ "H5"))
   (= {:suit :club :rank 12} (__ "CA"))
   (= (range 13) (map (comp :rank __ str)
                   '[S2 S3 S4 S5 S6 S7
                     S8 S9 ST SJ SQ SK SA]))))

;; 131. Sum Some Set Subsets

;; Given a variable number of sets of integers, create a function
;; which returns true iff all of the sets have a non-empty subset with
;; an equivalent summation.

(let [__
      (fn [& sets]
        (letfn
            [(powerset [lst]
               (if (empty? lst)
                 '([])
                 (let [ps (powerset (next lst))]
                   (clojure.set/union ps (map #(conj % (first lst)) ps)))))

             (sums [coll]
               (vec (sort (distinct (map (partial reduce +) (remove empty? (powerset coll)))))))

             (common-elem [& colls]
               (if (some empty? colls) nil
                   (let [heads
                         (into [] (map first colls))]

                     (if (apply = heads)
                       (first heads)
                       (let [pivot
                             (apply min heads)

                             trim
                             (fn [coll]
                               (if (= pivot (first coll))
                                 (rest coll)  coll))]

                         (recur (map trim colls)))))))]
          (not
           (nil?
            (apply common-elem (map sums sets))))))]

  (and
   (= true
      (__
       #{-1 1 99}
       #{-2 2 888}
       #{-3 3 7777}))

   (= false
      (__ #{1}
          #{2}
          #{3}
          #{4}))

   (= true
      (__ #{1}))

   (= false
      (__
       #{1 -3 51 9}
       #{0}
       #{9 2 81 33}))

   (= true
      (__
       #{1 3 5}
       #{9 11 4}
       #{-3 12 3}
       #{-3 4 -2 10}))

   (= false
      (__
       #{-1 -2 -3 -4 -5 -6}
       #{1 2 3 4 5 6 7 8 9}))

   (= true
      (__
       #{1 3 5 7}
       #{2 4 6 8}))

   (= true
      (__
       #{-1 3 -5 7 -9 11 -13 15}
       #{1 -3 5 -7 9 -11 13 -15}
       #{1 -1 2 -2 4 -4 8 -8}))

   (= true
      (__
       #{-10 9 -8 7 -6 5 -4 3 -2 1}
       #{10 -9 8 -7 6 -5 4 -3 2 -1}))))

;; 132. Insert between two items

;; Write a function that takes a two-argument predicate, a value, and
;; a collection; and returns a new collection where the value is
;; inserted between every two items that satisfy the predicate.

(defn __
  [p v coll]
  (letfn [(aux[prev coll]
            (prn prev coll)
            (Thread/sleep 500)
            (if (seq coll)
              (let [hd (first coll)
                    more (rest coll)]
                (if (and (not (nil? prev)) (p prev hd))
                  (cons prev (cons v (lazy-seq (aux hd more))))
                  (if (nil? prev)
                    (lazy-seq (aux hd more))
                    (cons prev (lazy-seq (aux hd more))))))
              (if (not (nil? prev))
                (list prev))))]
    (aux nil coll)))

(__ < :less [1 6 7 4 3])

(defn __
  [p v coll]
  (letfn [(aux [res prev coll]
            (let [sq (seq coll)]
              (if (nil? sq)
                res
                (let [hd (first sq)]
                  (if (and (not (nil? prev)) (p prev hd))
                    (recur (lazy-seq (conj res v hd)) hd (rest sq))
                    (recur (lazy-seq (conj res hd)) hd (rest sq)))))))]
    (reverse  (aux [] nil coll))))

(__ < :less [1 6 7 4 3])

(let [__
      ]

  (and
   (= '(1 :less 6 :less 7 4 3) )
   (= '(2) (__ > :more [2]))
   (= [0 1 :x 2 :x 3 :x 4]  (__ #(and (pos? %) (< % %2)) :x (range 5)))
   (empty? (__ > :more ()))
   (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
      (take 12 (->> [0 1]
                    (iterate (fn [[a b]] [b (+' a b)]))
                    (map first) ; fibonacci numbers
                    (__ (fn [a b] ; both even or both odd
                          (= (mod a 2) (mod b 2)))
                        :same))))
   ))


(= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
      (take 12 (->> [0 1]
                    (iterate (fn [[a b]] [b (+' a b)]))
                    (map first) ; fibonacci numbers
                    (__ (fn [a b] ; both even or both odd
                          (= (mod a 2) (mod b 2)))
                        :same))))

(fun < :less [1 6 7 4 3] )

(for [pair (rest (map vector (cons nil coll) coll))]
  (apply p pair))


(let [__
      (fn [p v coll]

)])

;; 134. A nil key

;; Write a function which, given a key and map, returns true iff the
;; map contains an entry with that key and its value is nil.

(let [__
      (fn[key map]
        (nil? (key map :not-nil)))]
  (and
   (true?  (__ :a {:a nil :b 2}))
   (false? (__ :b {:a nil :b 2}))
   (false? (__ :c {:a nil :b 2}))))

;; 135. Infix Calculator

;; Your friend Joe is always whining about Lisps using the prefix
;; notation for math. Show him how you could easily write a function
;; that does math using the infix notation. Is your favorite language
;; that flexible, Joe? Write a function that accepts a variable length
;; mathematical expression consisting of numbers and the operations +,
;; -, *, and /. Assume a simple calculator that does not do precedence
;; and instead just calculates left to right.

(let [__
      (fn[& expr]
        (loop [[eye & more] expr
               symb :term
               op +
               res 0]
          (if (nil? eye)
            res
            (case symb
              :term (recur more :oper nil (op res eye))
              :oper (recur more :term eye res)))))]
  (and
   (= 7  (__ 2 + 5))
   (= 42 (__ 38 + 48 - 2 / 2))
   (= 8  (__ 10 / 2 - 1 * 2))
   (= 72 (__ 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))))

;; 137. Digits and bases

;; Write a function which returns a sequence of digits of a
;; non-negative number (first argument) in numerical system with an
;; arbitrary base (second argument). Digits should be represented with
;; their integer values, e.g. 15 would be [1 5] in base 10, [1 1 1 1]
;; in base 2 and [15] in base 16.
(let [__
      (fn [num base]
        (loop [num num
               res '()]
          (if (zero? num)
            (if (empty? res) [0] (vec res))
            (recur (quot num base)
                   (cons (rem num base) res)))))]

  (and
   (= [1 2 3 4 5 0 1] (__ 1234501 10))
   (= [0] (__ 0 11))
   (= [1 0 0 1] (__ 9 2))
   (= [1 0] (let [n (rand-int 100000)](__ n n)))
   (= [16 18 5 24 15 1] (__ Integer/MAX_VALUE 42))))

;; 141. Tricky Card Games

;; In trick-taking card games such as bridge, spades, or hearts, cards
;; are played in groups known as "tricks" - each player plays a single
;; card, in order; the first player is said to "lead" to the
;; trick. After all players have played, one card is said to
;; have "won" the trick. How the winner is determined will vary by
;; game, but generally the winner is the highest card played in the
;; suit that was led. Sometimes (again varying by game), a particular
;; suit will be designated "trump", meaning that its cards are more
;; powerful than any others: if there is a trump suit, and any trumps
;; are played, then the highest trump wins regardless of what was led.

;; Your goal is to devise a function that can determine which of a
;; number of cards has won a trick. You should accept a trump suit,
;; and return a function winner. Winner will be called on a sequence
;; of cards, and should return the one which wins the trick. Cards
;; will be represented in the format returned by Problem 128,
;; Recognize Playing Cards: a hash-map of :suit and a
;; numeric :rank. Cards with a larger rank are stronger.

(let [__
      (fn[trump]
        (fn[cards]
          (let [lead
                (-> cards first :suit)

                value
                (fn[card]
                  (- (+ (if (= (:suit card) trump) 30 0)
                        (if (= (:suit card) lead) 15 0)
                        (:rank card))))]

              (first (sort-by value cards)))))]

  (and
   (let [notrump (__ nil)]
     (and (= {:suit :club :rank 9}  (notrump [{:suit :club :rank 4}
                                              {:suit :club :rank 9}]))
          (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                              {:suit :club :rank 10}])))))

  (= {:suit :club :rank 10} ((__ :club) [{:suit :spade :rank 2}
                                         {:suit :club :rank 10}]))

  (= {:suit :heart :rank 8}
     ((__ :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                   {:suit :diamond :rank 10} {:suit :heart :rank 4}])))

;; 143. Dot product

;; Create a function that computes the dot product of two
;; sequences. You may assume that the vectors will have the same
;; length.

(let [__
      (fn [u v]
        (loop [[fu & ru] u
               [fv & rv] v
               res 0]
          (if (nil? fu)
            res
            (recur ru rv (+ res (* fu fv))))))]
  (and
   (= 0 (__ [0 1 0] [1 0 0]))
   (= 3 (__ [1 1 1] [1 1 1]))
   (= 32 (__ [1 2 3] [4 5 6]))
   (= 256 (__ [2 5 6] [100 10 1]))))

;; 144. Oscilrate

;; Write an oscillating iterate: a function that takes an initial
;; value and a variable number of functions. It should return a lazy
;; sequence of the functions applied to the value in order, restarting
;; from the first function after it hits the end.

(let [__
      (fn [base & fs]
        (let [aux
              (fn step [curr idx]
                (lazy-seq
                 ((fn[]
                    (let [f (nth fs idx)
                          next (mod (inc idx) (count fs))]
                    (cons curr (step (f curr) next)))))))]
          (aux base 0)))]
  (and
   (= (take 3 (__ 3.14 int double)) [3.14 3 3.0])
   (= (take 5 (__ 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])
   (= (take 12 (__ 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])))

;; 145. For the win

;; Clojure's for macro is a tremendously versatile mechanism for
;; producing a sequence based on some other sequence(s). It can take
;; some time to understand how to use it properly, but that investment
;; will be paid back with clear, concise sequence-wrangling
;; later. With that in mind, read over these for expressions and try
;; to see how each of them produces the same result.


(let [__ '(1 5 9 13 17 21 25 29 33 37)]

  (and
   (= __ (for [x (range 40)
               :when (= 1 (rem x 4))]
           x))

   (= __ (for [x (iterate #(+ 4 %) 0)
               :let [z (inc x)]
               :while (< z 40)]
           z))

   (= __ (for [[x y] (partition 2 (range 20))]
           (+ x y)))))

;; 146. Trees into tables

;; Because Clojure's for macro allows you to "walk" over multiple
;; sequences in a nested fashion, it is excellent for transforming all
;; sorts of sequences. If you don't want a sequence as your final
;; output (say you want a map), you are often still best-off using
;; for, because you can produce a sequence and feed it into a map, for
;; example.

;; For this problem, your goal is to "flatten" a map of hashmaps. Each
;; key in your output map should be the "path"[*] that you would have to
;; take in the original map to get to a value, so for example {1 {2
;; 3}} should result in {[1 2] 3}. You only need to flatten one level
;; of maps: if one of the values is a map, just leave it alone.

;; [*] That is, (get-in original [k1 k2]) should be the same as (get
;; result [k1 k2])

(let [__
      (fn[map]
        (into {}
              (for [k1 (keys map)
                    k2 (keys (map k1))]
                [[k1 k2] ((map k1) k2)])))]

  (and
   (= (__ '{a {p 1, q 2}
            b {m 3, n 4}})
      '{[a p] 1, [a q] 2
        [b m] 3, [b n] 4})

   (= (__ '{[1] {a b c d}
            [2] {q r s t u v w x}})
      '{[[1] a] b, [[1] c] d,
        [[2] q] r, [[2] s] t,
        [[2] u] v, [[2] w] x})

   (= (__ '{[1] {a b c d}
            [2] {q r s t u v w x}})
      '{[[1] a] b, [[1] c] d,
        [[2] q] r, [[2] s] t,
        [[2] u] v, [[2] w] x})))


;; 147. Pascal's Trapezoid

;; Write a function that, for any given input vector of numbers,
;; returns an infinite lazy sequence of vectors, where each next one
;; is constructed from the previous following the rules used in
;; Pascal's Triangle. For example, for [3 1 2], the next row is [3 4 3
;; 2].

;; Beware of arithmetic overflow! In clojure (since version 1.3 in
;; 2011), if you use an arithmetic operator like + and the result is
;; too large to fit into a 64-bit integer, an exception is thrown. You
;; can use +' to indicate that you would rather overflow into
;; Clojure's slower, arbitrary-precision bigint.

(let [__
      (fn rec [v]
        (lazy-seq
         (let [vl (concat [0] v)
               vr (concat v [0])
               vv (vec (for [[x y] (map vector vl vr)] (+' x y)))]
           (cons v (rec vv)))))]

  (and
   (= (second (__ [2 3 2])) [2 5 5 2])
   (= (take 5 (__ [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
   (= (take 2 (__ [3 1 2])) [[3 1 2] [3 4 3 2]])
   (= (take 100 (__ [2 4 2])) (rest (take 101 (__ [2 2]))))))


;; 148. The Big Divide

;; Write a function which calculates the sum of all natural numbers
;; under n (first argument) which are evenly divisible by at least one
;; of a and b (second and third argument). Numbers a and b are
;; guaranteed to be coprimes.

;; Note: Some test cases have a very large n, so the most obvious
;; solution will exceed the time limit.

(let [__
      (fn[n a b]
        (let [multiple?
              (comp zero? rem)

              pred?
              (fn[n] (or (multiple? n a)
                         (multiple? n b)))]

          (reduce +' (filter pred? (range n)))))]
  (and
   (= 0 (__ 3 17 11))
   (= 23 (__ 10 3 5))
   (= 233168 (__ 1000 3 5))
   (= "2333333316666668" (str (__ 100000000 3 5)))
   (= "110389610389889610389610"
      (str (__ (* 10000 10000 10000) 7 11)))
   (= "1277732511922987429116"
      (str (__ (* 10000 10000 10000) 757 809)))
   (= "4530161696788274281"
  (str (__ (* 10000 10000 1000) 1597 3571)))))

;; 150. Palindromic Numbers

;; A palindromic number is a number that is the same when written
;; forwards or backwards (e.g., 3, 99, 14341).

;; Write a function which takes an integer n, as its only argument,
;; and returns an increasing lazy sequence of all palindromic numbers
;; that are not less than n.

;; The most simple solution will exceed the time limit!
(let [__
      (fn[n]
        (letfn [(palindrome?[s]
                  (if (< (count s) 2) true
                      (and (= (first s) (last s)) (recur (drop 1 (drop-last 1 s))))))

                (first-palindrome-greater-or-equal-than
                  [n] (first (filter #(palindrome? (str %)) (iterate inc n))))

                (palindrome-seed
                  [n] (let [repr (str n)
                            length (count repr)
                            stub-length (int  (Math/ceil (/ (count repr) 2)))]
                        (biginteger (subs repr 0 stub-length))))

                (pow10 [n] (reduce *' (repeat n 10)))

                (log10 [n] (first (filter #(> (pow10 %) n) (range))))

                (mirror-symmetric [k]
                  (let [lhs (str k)
                        rhs (clojure.string/reverse lhs)]
                    (biginteger (str lhs rhs))))

                (mirror-asymmetric [k]
                  (let [lhs (str k)
                        rhs (subs (clojure.string/reverse lhs) 1)]
                    (biginteger (str lhs rhs))))

                (all-palindromes-aux[toggle i seed]
                  (cons
                   (if toggle
                     (mirror-symmetric seed)
                     (mirror-asymmetric seed))
                   (lazy-seq
                    (let [limit (pow10 i)
                          next-seed (inc seed)]
                      (cond
                       (and (= next-seed limit) (not toggle))
                       (all-palindromes-aux true i (pow10 (dec i)))

                       (and (= next-seed limit) toggle)
                       (all-palindromes-aux false (inc i) (pow10 i))

                       :else
                       (all-palindromes-aux toggle i (inc seed)))))))]

          (let [first (first-palindrome-greater-or-equal-than n)
                seed (palindrome-seed first)
                test (mirror-symmetric seed)]

            (all-palindromes-aux (= test first)  (log10 seed) seed))))]

  (and
   (= (take 26 (__ 0))
      [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101 111 121 131 141 151 161])

   (= (take 16 (__ 162))
      [171 181 191 202 212 222 232 242 252 262 272 282 292 303 313 323])

   (= (take 6 (__ 1234550000))
      [1234554321 1234664321 1234774321 1234884321 1234994321 1235005321])

   (= (first (__ (* 111111111 111111111)))
      (* 111111111 111111111))

   (= (set (take 199 (__ 0)))
      (set (map #(first (__ %)) (range 0 10000))))

   (= true
      (apply < (take 6666 (__ 9999999))))

   (= (nth (__ 0) 10101) 9102019)))

;; 153. Pairwise Disjoint Sets

;; Given a set of sets, create a function which returns true if no two
;; of those sets have any elements in common and false
;; otherwise. Some of the test cases are a bit tricky, so pay a little
;; more attention to them.

;; Such sets are usually called pairwise disjoint or mutually
;; disjoint.

(let [__
      (fn[xs]
        (every? identity
                (for [a xs b xs
                      :when (not (= a b))]
                  (empty? (clojure.set/intersection a b)))))]
  (and
   (= (__ #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
      true)

   (= (__ #{#{:a :b :c :d :e}
            #{:a :b :c :d}
            #{:a :b :c}
            #{:a :b}
            #{:a}})
      false)


   (= (__ #{#{[1 2 3] [4 5]}
            #{[1 2] [3 4 5]}
            #{[1] [2] 3 4 5}
            #{1 2 [3 4] [5]}})
      true)

   (= (__ #{#{'a 'b}
            #{'c 'd 'e}
            #{'f 'g 'h 'i}
            #{''a ''c ''f}})
      true)

   (= (__ #{#{'(:x :y :z) '(:x :y) '(:z) '()}
            #{#{:x :y :z} #{:x :y} #{:z} #{}}
            #{'[:x :y :z] [:x :y] [:z] [] {}}})
      false)

   (= (__ #{#{(= "true") false}
            #{:yes :no}
            #{(class 1) 0}
            #{(symbol "true") 'false}
            #{(keyword "yes") ::no}
            #{(class '1) (int \0)}})
      false)

   (= (__ #{#{distinct?}
            #{#(-> %) #(-> %)}
            #{#(-> %) #(-> %) #(-> %)}
            #{#(-> %) #(-> %) #(-> %)}})
      true)

   (= (__ #{#{(#(-> *)) + (quote mapcat) #_ nil}
            #{'+ '* mapcat (comment mapcat)}
            #{(do) set contains? nil?}
            #{, , , #_, , empty?}})
      false)))




;; 156. Map Defaults

;; When retrieving values from a map, you can specify default values
;; in case the key is not found:

;; (= 2 (:foo {:bar 0, :baz 1} 2))

;; However, what if you want the map itself to contain the default
;; values? Write a function which takes a default value and a sequence
;; of keys and constructs a map.

(let [__
      (fn[a-val a-map]
        (loop [[head & tail] a-map
               res {}]
          (if (nil? head)
            res
            (recur tail (assoc res head a-val)))))]

  (and
   (= (__ 0 [:a :b :c]) {:a 0 :b 0 :c 0})
   (= (__ "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})
   (= (__ [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})))


;; 157. Indexing Sequences

;; Transform a sequence into a sequence of pairs containing the
;; original elements along with their index.

(let [__
      (fn[sq]
        (map-indexed #(-> [%2 %1]) sq))]

  (and
   (= (__ [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
   (= (__ [0 1 3]) '((0 0) (1 1) (3 2)))
   (= (__ [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])))

;; 158. Decurry

;; Write a function that accepts a curried function of unknown arity
;; n. Return an equivalent function of n arguments. You may wish to
;; read this [http://en.wikipedia.org/wiki/Currying].

(let [__
      (fn[f]
        (fn[& coll]
          (reduce (fn[g h] (g h)) f coll)))]

  (and
   (= 10 ((__ (fn [a]
                (fn [b]
                  (fn [c]
                    (fn [d]
                      (+ a b c d))))))
          1 2 3 4))

   (= 24 ((__ (fn [a]
                (fn [b]
                  (fn [c]
                    (fn [d]
                      (* a b c d))))))
          1 2 3 4))

   (= 25 ((__ (fn [a]
                (fn [b]
                  (* a b))))
          5 5))))

;; 161. Subset and Superset

;; Set A is a subset of set B, or equivalently B is a superset of A,
;; if A is "contained" inside B. A and B may coincide.

(let [__
      #{1 2 3}]

(and
 (clojure.set/superset? __ #{2})
 (clojure.set/subset? #{1} __)
 (clojure.set/superset? __ #{1 2})
 (clojure.set/subset? #{1 2} __)))


;; 162. Logical falsity and truth

;; In Clojure, only nil and false represent the values of logical
;; falsity in conditional tests - anything else is logical truth

(let [__
      1]

  (and
   (= __ (if-not false 1 0))
   (= __ (if-not nil 1 0))
   (= __ (if true 1 0))
   (= __ (if [] 1 0))
   (= __ (if [0] 1 0))
   (= __ (if 0 1 0))
   (= __ (if 1 1 0))))

;; 166. Comparisons

;; For any orderable data type it's possible to derive all of the
;; basic comparison operations (<, ≤, =, ≠, ≥, and >) from a single
;; operation (any operator but = or ≠ will work). Write a function
;; that takes three arguments, a less than operator for the data and
;; two items to compare. The function should return a keyword
;; describing the relationship between the two items. The keywords for
;; the relationship between x and y are as follows:

;; x = y → :eq
;; x > y → :gt
;; x < y → :lt

(let [__
      (fn[lt a b]
        (cond
         (lt a b)
         :lt

         (lt b a)
         :gt

         :else
         :eq))]
  (and
   (= :gt (__ < 5 1))
   (= :eq (__ (fn [x y] (< (count x) (count y))) "pear" "plum"))
   (= :lt (__ (fn [x y] (< (mod x 5) (mod y 5))) 21 3))
   (= :gt (__ > 0 2))))

;; 171. Intervals

;; Write a function that takes a sequence of integers and returns a
;; sequence of "intervals". Each interval is a vector of two
;; integers, start and end, such that all integers between start and
;; end (inclusive) are contained in the input sequence.

(let [__
      (fn [coll]
        (loop [[eye & more :as coll] (sort coll)
               tmp []
               res []]
          (cond
           (nil? eye)
           (remove empty? (conj res tmp))

           (empty? tmp)
           (recur more [eye eye] res)

           (and
            (<= (first tmp) eye)
            (<= eye (second tmp)))
           (recur more tmp res)

           (= eye (inc (last tmp)))
           (recur more [(first tmp) eye] res)

           :else
           (recur coll [] (conj res tmp)))))]

  (and
   (= (__ [1 2 3]) [[1 3]])
   (= (__ [10 9 8 1 2 3]) [[1 3] [8 10]])
   (= (__ [1 1 1 1 1 1 1]) [[1 1]])
   (= (__ []) [])
   (= (__ [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
      [[1 4] [6 6] [9 11] [13 17] [19 19]])))

;; 173. Intro to Destructuring 2

;; Sequential destructuring allows you to bind symbols to parts of
;; sequential things (vectors, lists, seqs, etc.): (let [bindings* ]
;; exprs*) Complete the bindings so all let-parts evaluate to 3.

(= 3
   (let [[x y] [+ (range 3)]] (apply x y))
   (let [[[x y] b] [[+ 1] 2]] (x y b))
   (let [[x y] [inc 2]] (x y)))

;; 177. Balancing Brackets

;; When parsing a snippet of code it's often a good idea to do a
;; sanity check to see if all the brackets match up. Write a function
;; that takes in a string and returns truthy if all square [ ] round (
;; ) and curly { } brackets are properly paired and legally nested, or
;; returns falsey otherwise.

(let [__
      (fn [s]
        (let [char->int
              (fn [c]
                (case c \{ 1, \} -1, \( 2, \) -2, \[ 3, \] -3, 0))

              abs
              (fn [n]
                (if (<= 0 n) n (- n)))]

          (loop [[eye & more :as in] (remove zero?
                                             (map char->int s))
                 stack []]

            (let [head (or (peek stack) 0)]
              (cond
               (nil? eye)
               (zero? head)

               (>= eye 0)
               (recur more (conj stack eye))

               (zero? (+ eye head))
               (recur more (pop stack))

               :else
               false)))))]

  (and
   (__ "This string has no brackets.")
   (__ "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }")
   (not (__ "(start, end]"))
   (not (__ "())"))
   (not (__ "[ { ] } "))
   (__ "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))")
   (not (__ "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))"))
   (not (__ "["))))

;; 178. Best Hand

;; Following on from Recognize Playing Cards, determine the best poker
;; hand that can be made with five cards. The hand rankings are listed
;; below for your convenience.

;; Straight flush: All cards in the same suit, and in sequence
;; Four of a kind: Four of the cards have the same rank
;; Full House: Three cards of one rank, the other two of another rank
;; Flush: All cards in the same suit
;; Straight: All cards in sequence (aces can be high or low, but not both at once)
;; Three of a kind: Three of the cards have the same rank
;; Two pair: Two pairs of cards have the same rank
;; Pair: Two cards have the same rank
;; High card: None of the above conditions are met

(let [__
      (fn [strings]
        (let [str->card
              (fn
                [[suit rank]]
                (let [suits (vec "HDCS")
                      suit->idx (zipmap suits (range (count suits)))

                      ranks (vec "23456789TJQKA")
                      rank->idx (zipmap ranks (range (count ranks)))]

                  { :suit ({0 :heart 1 :diamond 2 :club 3 :spade}
                           (get suit->idx suit nil))
                   :rank (get rank->idx rank nil) } ))]

          (let [cards (map str->card strings)
                suits (map :suit cards)
                ranks (map :rank cards)
                same-suit (apply = suits)
                straight  (let [sorted-ranks (sort ranks)]
                            (or
                             (= sorted-ranks (range (apply min ranks)
                                                    (inc (apply max ranks))))
                            (= sorted-ranks '(0 1 2 3 12))))
                groups (map (fn[[rank cnt]] [(count cnt) rank])
                            (reverse (sort-by (comp count second)
                                              (group-by identity ranks))))]

            (cond
             (and same-suit straight)
             :straight-flush

             (= 4 (ffirst groups))
             :four-of-a-kind

             (and (= 3 (ffirst groups)) (= 2 (first (second groups))))
             :full-house

             same-suit
             :flush

             straight
             :straight

             (= 3 (ffirst groups))
             :three-of-a-kind

             (and (= 2 (ffirst groups)) (= 2 (first (second groups))))
             :two-pair

             (= 2 (ffirst groups))
             :pair

             :else
             :high-card)
            )
          )
        )
      ]

  (and
   (= :high-card (__ ["HA" "D2" "H3" "C9" "DJ"]))
   (= :pair (__ ["HA" "HQ" "SJ" "DA" "HT"]))
   (= :two-pair (__ ["HA" "DA" "HQ" "SQ" "HT"]))
   (= :three-of-a-kind (__ ["HA" "DA" "CA" "HJ" "HT"]))
   (= :straight (__ ["HA" "DK" "HQ" "HJ" "HT"]))
   (= :straight (__ ["HA" "H2" "S3" "D4" "C5"]))
   (= :flush (__ ["HA" "HK" "H2" "H4" "HT"]))
   (= :full-house (__ ["HA" "DA" "CA" "HJ" "DJ"]))
   (= :four-of-a-kind (__ ["HA" "DA" "CA" "SA" "DJ"]))
   (= :straight-flush (__ ["HA" "HK" "HQ" "HJ" "HT"]))))

;; 195. Parentheses... Again

;; In a family of languages like Lisp, having balanced parentheses is
;; a defining feature of the language. Luckily, Lisp has almost no
;; syntax, except for these "delimiters" -- and that hardly qualifies
;; as "syntax", at least in any useful computer programming sense.

;; It is not a difficult exercise to find all the combinations of
;; well-formed parentheses if we only have N pairs to work with. For
;; instance, if we only have 2 pairs, we only have two possible
;; combinations: "()()" and "(())". Any other combination of length 4
;; is ill-formed. Can you see why?

;; Generate all possible combinations of well-formed parentheses of
;; length 2n (n pairs of parentheses). For this problem, we only
;; consider '(' and ')', but the answer is similar if you work with
;; only {} or only [].

;; There is an interesting pattern in the numbers!
(let [__
      (fn[n]
        (let [generate
              (fn[n]
                (loop [i n res #{""}]
                  (let [extend
                        (fn[radix]
                          (let [freqs (frequencies radix)
                                open (get freqs \( 0)
                                close (get freqs \) 0)

                                can-open (> i (- open close))
                                can-close (> open close)]

                            (clojure.set/union
                             (if can-open #{(str radix \()})
                             (if can-close #{(str radix \))}))))]

                    (cond
                     (zero? i) res
                     :else (recur (dec i) (mapcat extend res))))))]

          (into #{} (generate (* 2 n)))))]

  (and
   (= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (__ n)) [0 1 2]))
   (= #{"((()))" "()()()" "()(())" "(())()" "(()())"} (__ 3))
   (= 16796 (count (__ 10)))
   (= (nth (sort (filter #(.contains ^String % "(()()()())") (__ 9))) 6) "(((()()()())(())))")
   (= (nth (sort (__ 12)) 5000) "(((((()()()()()))))(()))")))

;; Hamiltonian cycle (TSP)
(let [
      __
      (fn [edges]
        (letfn [(gen-perms [vertexes]
                  (cond
                   (empty? vertexes)
                   '(()) (empty? (rest vertexes)) (list (apply list vertexes))

                   :else
                   (for [x vertexes y (gen-perms (remove #{x} vertexes))] (cons x y))))]

          (let [mirror
                (fn[[a b]] [b a])

                all-edges
                (into #{} (clojure.set/union edges (map mirror edges)))

                vertexes
                (into [] (distinct (mapcat identity edges)))

                perms
                (gen-perms vertexes)

                make-cycle
                (fn [lst]
                  (let [v (into [] lst)]
                    (conj v (first v))))

                cycles
                (map make-cycle perms)

                is-valid-cycle?
                (fn [cycle]
                  ;; (prn cycle)
                  (let [rcycle (reverse cycle)
                        j (cons nil (into '() rcycle))
                        k (into '() rcycle)
                        steps (into [] (rest (map vector j k)))]
                    ;; (prn steps)
                    (if (every? all-edges steps) cycle)))]

            ;; (prn all-edges)
            (let [solution (some is-valid-cycle? cycles)]
              (prn solution)
              (if solution true false)))))]

  (and
   (= true (__ [[:a :b]]))
   (= false (__ [[:a :a] [:b :b]]))
   (= true (__ [[:a :b] [:a :b] [:a :c] [:c :a] [:a :d] [:b :d] [:c :d]]))
   (= true (__ [[1 2] [2 3] [3 4] [4 1]]))
   (= true (__ [[:a :b] [:a :c] [:c :b] [:a :e] [:b :e] [:a :d] [:b :d] [:c :e] [:d :e] [:c :f] [:d :f]]))
   (= false (__ [[1 2] [2 3] [2 4] [2 5]]))))
