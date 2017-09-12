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

                                        ; 46. Flipping out

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

;; 72. Rearraning Cide: ->>

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
          (aux w 0 0 nil))
        )]
  (and
   (= 14 (__ "XIV"))
   (= 827 (__ "DCCCXXVII"))
   (= 3999 (__ "MMMCMXCIX"))
   (= 48 (__ "XLVIII"))))
