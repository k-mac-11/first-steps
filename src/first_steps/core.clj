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
        (recur n (inc div) primes))
      )
    )
  )

;Euler Project 4
;Find the largest palindrome made from the product of two 3-digit numbers.
(defn palindrome [n] (if (= (apply str (reverse (str n))) (str n)) n 0))
(reduce max (map palindrome (for [x (range 100 1000) y (range 100 1000)] (* x y))))

;Euler Project 5
;What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
(defn div-20 [ ]
  (loop [n 1 div 1]
    (if (= div 21)
      n
      (if (= 0 (rem n div))
        (recur n (inc  div))
        (recur (inc n) 1)
        )
      )
    )
  )

;Euler Project 6
;Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
(defn diff-sum-square [n]
  (def sum (reduce + (range 1 n)))
  (def square-of-sum (* sum sum))
  (def sum-of-square (reduce + (for [x (range 1 n)] (* x x))))
  (- square-of-sum sum-of-square)
)

;Euler Project 7
;What is the 10,001st prime number?
(defn prime [n]
  (loop [n n div 2]
    (if (> div (inc (Math/sqrt n)))
      true
      (if (= (rem n div) 0)
        false       
        (recur n (inc div))
      )
    )
   )
  )

(defn primes [n]
  (loop [last-prime 2 candidate 3 count 1]
    (if (= count n)
      last-prime
      (if (prime candidate)
        (recur candidate (inc candidate) (inc count))
        (recur last-prime (inc candidate) count)
        )
      )
    )
  )
