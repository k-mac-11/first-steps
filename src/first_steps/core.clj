(ns first-steps.core)

;Euler Project 1
;Find the sum of all the multiples of 3 or 5 below 1000.

(defn three-or-five [x]
  (if (or (= (rem x 3) 0) (= (rem x 5) 0)) x 0))

(reduce + (map three-or-five (range 1001)))

;Euler Project 2
;By considering the terms in the Fibonacci sequence whose values do not exceed four million,
;find the sum of the even-valued terms.

(defn fib-add [[a b]] [b (+ a b)])

(reduce + (filter even? (take-while #(< % 4000000) (map last (iterate fib-add [1 1])))))

;Euler Project 3
;What is the largest prime factor of the number 600851475143?
(defn primes [n]
  (loop [n n div 2 primes ()]
    (if (< n 2)
      primes
      (if (= (rem n div) 0)
        (recur (/ n div) div (conj primes div))
        (recur n (inc div) primes)))))

;;Alternate implementation based on sequences,
;;more or less a direct translation.
;;clojure.core/iterate is useful for defining
;;sequences from a seed and a generating
;;function.
(comment

  (defn prime-divisors [n]
    (->> [n 2 []]
         (iterate (fn [[n div primes]]
                    (when (>= n 2)
                      (if (zero? (rem n div))
                        [(/ n div) div (conj primes div)]
                        [n (inc div) primes]))))
         (map last) ;;strip out the primes results
         (take-while identity) ;;stop if we hit a nil
         last)) ;;get the final element, containing said primes.

  (defn greatest-prime-divisor [n]
    (last (prime-divisors n)))

)


;Euler Project 4
;Find the largest palindrome made from the product of two 3-digit numbers.
(defn palindrome [n]
  (if (= (apply str (reverse (str n))) (str n))
    n
    0))

(reduce max (map palindrome (for [x (range 100 1000) y (range 100 1000)] (* x y))))

(comment
  ;;Alternate implementation, separating out the palindrome predicate
  ;;and composing it with a filter.
  (defn palindrome? [s]
     (= (clojure.string/reverse s) s))

  (->> (range 100 1000)
       (map    #(* % %))
       (filter #(palindrome? (str %)))
       (reduce max))

  ;;Anothe implementation, this time leveraging some of
  ;;the sequence comprehension DSL.  We can define intermediate
  ;;variables used in the sequence with a :let binding,
  ;;and a filter with :when.  This can make the sequence
  ;;generation a bit more expressive, although I rarely
  ;;use :let in practice.
  (->> (for [n (range 100 1000)
             :let [k (* n n)]
             :when (palindrome? (str k))]
         k)
       (reduce max))
)

;Euler Project 5
;What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
(defn div-20 [ ]
  (loop [n 1 div 1]
    (if (= div 21)
      n
      (if (= 0 (rem n div))
        (recur n (inc  div))
        (recur (inc n) 1)))))

(comment

  ;;Variations on a theme from Euler 5:
 
;;232792560

  (defn divisible? [n d]
    (if (= d 1) n
        (and (zero? (rem n d))
             (recur n (dec d)))))
  (def bound  (reduce * (range 2 21)))

  ;;re-imagined as a naive seq implementation
  ;;funny, if we use (iterate inc 20) instead
  ;;of range, we get a 4x speed hit, likely due
  ;;to iterate's implementation.
  (->> (range 20 bound)
       (filter #(divisible? % 20))
       first)

  ;;similar functional programming idiom,
  ;;where we define an auxillary function
  ;;to help drive the computation.
  ;;Function is tail-recursive, so we
  ;;effectively get a loop out of it without
  ;;any stack allocation.
  (let [step       (fn [n]
                     (if (divisible? n 20)
                       n
                       (recur (inc n))))]
    (step 20))

  ;;Higher order version using reduce.  Since
  ;;reduce is effectively a loop (with some
  ;;magical properties if you learn about
  ;;reducers and transducers), you can
  ;;often express loop-like things
  ;;in a composeable fashion with reduce.
  ;;reduce also allows (reduced ...)
  ;;to signal early termination.
  (reduce (fn [acc n]
            (when (divisible? n 20)
              (reduced n))) bound
          (range 20 bound))

  ;;into allows us to supply an additional
  ;;argument, a transducer, which are very
  ;;similarlly defined with the core sequence
  ;;functions.  Most of them have transducer
  ;;variants (e.g. map, filter, take, drop, etc.).
  ;;We simply elide the collection argument,
  ;;and to compose multiple transducers, as if
  ;;by the ->> idiom for sequence transformation
  ;;pipelines, we use clojure.core/comp to get
  ;;a similar result.

  ;;Transducers (in this case via clojure.core/into)
  ;;allow us to have effectively a reduction that
  ;;transforms in-step.  That is, we define one or
  ;;more transformations, then reduce.  The difference
  ;;here is that we're not creating any intermediate
  ;;lazy sequences, which - depending on the task -
  ;;may incur a performance or memory hit.  Transducers
  ;;provide a similar feel to lazy sequences, but
  ;;are eager (in the case of into), and a bit more
  ;;efficient.  They also admit options for parallelization
  ;;and work with asynchronous channels from clojure.core.async.
  (->> (range 20 bound)
       (into [] (comp (filter #(divisible? % 20))
                      (take 1)))
       first)
)


;Euler Project 6
;Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
(defn diff-sum-square [n]
  (def sum (reduce + (range 1 n)))
  (def square-of-sum (* sum sum))
  (def sum-of-square (reduce + (for [x (range 1 n)] (* x x))))
  (- square-of-sum sum-of-square)
  )

(comment
  ;;Prefer to avoid useing clojure.core/def in the bodies of functions or
  ;;other expressions.  Reserve that for the top-level, since it's
  ;;akin to defining global vars.  There "are" use cases for
  ;;def inside expressions, but they're beyond the scope here.
  (defn diff-sum-square [n]
    (let  [sum (reduce + (range 1 n))
           square-of-sum (* sum sum)
           sum-of-square (reduce + (for [x (range 1 n)] (* x x)))]
      (- square-of-sum sum-of-square)))

  ;;We can do it in one pass if we compute the stats in the reduction.
  ;;There's a balance between "clever" and performant.
  ;;Some libraries, like xforms and kixi.stats, provide really nice
  ;;statistical transducers you can use to collect these types of things
  ;;as well (for future refence).
  (defn dss [n]
    (let [[sum-of-square sum]  (->> (range 1 n)
                                    (reduce (fn [[sos sum] n]
                                              [(+ sos (* n n))
                                               (+ sum n)]) [0 0]))]
      (- (* sum sum) sum-of-square)))

  )

;Euler Project 7
;What is the 10,001st prime number?
(defn prime [n]
  (loop [n n div 2]
    (if (> div (inc (Math/sqrt n)))
      true
      (if (= (rem n div) 0)
        false
        (recur n (inc div))))))

(defn primes [n]
  (loop [last-prime 2 candidate 3 count 1]
    (if (= count n)
      last-prime
      (if (prime candidate)
        (recur candidate (inc candidate) (inc count))
        (recur last-prime (inc candidate) count)))))

(comment

  ;;It's idiomatic to use a ? suffix for predicate functions
  ;;in Clojure.  Clojure itself doesn't care, but for readability,
  ;;? should communicate a function that projects onto a truthy
  ;;value (e.g. True/False), although some predicates in the wild
  ;;suffice to return a "truthy" value, e.g. where truthy
  ;;means (and (not False) (not nil)), falsey is the complement
  ;;of truthy.
  (defn prime?
    ([n div]
     (cond (> div (inc (Math/sqrt n))) true
           (zero? (rem n div))         false
           :else (recur n (inc div))))
    ([n] (prime? n 2)))

  ;;I typically pull out all my bindings vertically.  Idioms
  ;;and style guides vary on this.  I leave it to the eye of
  ;;the beholder (my hueristic is to trade vertical space
  ;;for enhanced readability, while trying to limit
  ;;expressions to 20 lines of code or less).
  (defn primes [n]
    (loop [last-prime 2
           candidate  3
           count      1]
      (if (= count n)
        last-prime
        (if (prime? candidate)
          (recur candidate (inc candidate) (inc count))
          (recur last-prime (inc candidate) count)))))

  ;;can we define a sequence builder that does this? One way to do so is with
  ;;lazy-seq. We define a function with multiple arities, which is a common
  ;;idiom for supplying optional arguments. There are other common idioms (such
  ;;as using keyword arguments and destructuring), but this idiom is fairly
  ;;simple. We can have multiple arities for each function body, rather than a
  ;;single arg vector.

  ;;We can only have 1 variadic arg vector (e.g. [x & xs]), and it must not be
  ;;shorter than any other discrete arg vectors. In this case, we define
  ;;naive-primes to take 2 args, similar to what primes used, and an optional
  ;;0-arg version, that invokes naive-primes with default args for convenience.
  (defn naive-primes
    ([] (naive-primes 2 3))
    ([last-prime n]
     (if (prime? n)
       ;;construct a lazy-sequence by consing (e.g. lazy lists)
       ;;the new last-prime onto the pending (lazily generated)
       ;;sequence of naive-primes to come:
       (lazy-seq (cons last-prime (naive-primes n (inc n))))
       ;;Otherwise, recur to the nearest function entry (in this
       ;;case, the 2-arg variant of naive-primes we entered from)
       ;;and bumped the candidate prime.
       (recur last-prime (inc n)))))

  ;;Then we merely call our naive-primes function,
  ;;composed with our general sequence functions...
  (defn nth-prime [n]
    (->> (naive-primes)
         (drop (dec n))
         first))

  ;;or the converse (using take)
  (defn nth-prime [n]
    (->> (naive-primes)
         (take (dec n))
         last))

  )
