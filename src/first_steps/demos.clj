;;A little namespace to demonstrate
;;proximately useful clojure functions
;;and idioms to aid in solving
;;bootstrapping problems and puzzles.
(ns first-steps.demos
  (:require [clojure.set :as s]))

(def letters ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"  "n" "o"
              "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"])

(defn change-letter [word  index letter]
  (str (subs word  0 index) letter (subs word (inc index) (count word))))

(defn remove-letter [word  index]
  (str (subs word  0 index) (subs word (inc index) (count word))))

(defn add-letter [word index letter]
  (str (subs word 0 index) letter (subs word index (count word))))

;;concat takes a variable number of args.  If you have a finite
;;bunch of sequences you'd like to concat, just apply them directly.
;;If you have a potentially infinite sequence of lazy sequences
;;to concat, use (apply concat ...) to get a lazy concatenation of
;;all the sequences.
(defn new-words [word letters]
  (concat (for [index (range (count word)) letter letters]
            (change-letter word index letter))
          (for [index (range (count word))]
            (remove-letter word index))
          (for [index (range (inc (count word))) letter letters]
            (add-letter word index letter))))


;;There's not much reason to leverage with-open outside of the def invocation
;;here. It's atypical.

;;Note: the invocation of clojure.core/line-seq on the (temporarily opened)
;;reader is lazy. If you don't force the sequence or do something with it before
;;leaving the scope of the with-open body, you'll end up with an empy
;;sequence (since the reader will have closed). The call to clojure.core/set
;;here has to traverse the input (the line-seq), so it ends up actually doing
;;work.
(def candidates
  (with-open [rdr (clojure.java.io/reader "word.list")]
    (set (line-seq rdr))))

;;another common idiom using clojure.core/slurp. This will realize the whole
;;string, then split it up. Depending on how big said file is, whether or not
;;parallel processing is desired, the streamining approach of line-seq may be
;;preferred.  split-lines will produce a vector, so it's also eager vs. lazy (
;;line-seq is lazy)
#_(def candidates (->>  "word.list"
                        slurp
                        clojure.string/split-lines))

(defn altered-words [word]
  (set (remove #(= % word) (new-words word letters))))

;;There's another useful idiom where you can use sets (or maps, or anything
;;that can act as a function) as a predicate.  In this case, if the
;;set is applied to an argument, the result is equivalent to calling
;;clojure.core/get on the set, either returning the argument if it's
;;in the set (a truthy value e.g. for anything non-nil or false), or nil.
;; (defn altered-words [word]
;;   (set (remove #{word} (new-words word letters))))

(defn to-visit [altered-words candidates]
  (clojure.set/intersection altered-words candidates))

(defn social-network [init]
  (loop [candidates      candidates
         friends         #{init}
         pending         #{init}]
    (if (empty? pending)
      friends
      (let [next-word          (first pending)
            new-friends        (remove friends (to-visit (altered-words next-word) candidates))]
        (recur (clojure.set/difference candidates new-friends)
               (conj  friends next-word)
               (into  (disj pending next-word) new-friends))))))

(comment
  ;;This is an approach that using a vector for the search fringe instead of a set.
  ;;Since we're already pruning detected words before they go onto the fringe,
  ;;we can ensure all the value are unique.  Then we just use clojure.core/peek and
  ;;clojure.core/pop, to get the last element of the vector, and compute a vector with
  ;;it removed.  You could go from the front of the vector, but that's a tad inefficient
  ;;and requires using clojure.core/subvec.  Alternately, you could swap out the vector
  ;;fringe for a list, and peek/pop will still work, but draw from the front of the list
  ;;as with a stack.
  (defn social-network-vec [init]
    (loop [candidates      candidates
           friends         #{}
           pending         [init]]
      (if (empty? pending)
        friends
        (let [next-word          (peek pending)
              new-friends        (remove friends
                                         (to-visit (altered-words next-word) candidates))]
          (recur (clojure.set/difference candidates new-friends)
                 (conj  friends next-word)
                 (into  (pop pending) new-friends)))))))

;;We pay some overhead for going with sets, particularly in
;;construction (we make a lot of sets).  It turns out
;;we don't really need those properties, and sequences are
;;fine.
(defn altered-words-faster [word]
  (remove #(= % word) (new-words word letters)))

;;no need for set operations, no need for sets.
;;Just filter over a sequence of words based on presence
;;in the candidate set.
(defn to-visit-faster [altered-words candidates]
  (filter candidates altered-words))

(defn social-network-faster [init]
  (loop [candidates      candidates
         friends         #{init}
         pending         [init]]
    (if (empty? pending)
      friends
      (let [next-word          (peek pending)
            new-friends        (remove friends
                                  (to-visit-faster
                                     (altered-words-faster next-word) candidates))]
        (recur (reduce disj candidates new-friends)
               (conj  friends next-word)
               (into  (pop pending) new-friends))))))
(comment
  ;;OLD
  (def alphabet
    ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
     "t" "u" "v" "w" "x" "y" "z"])

  ;;note: the alphabet can be trivially typed in, or easily generated by mapping
  ;;over the ascii char codes:

  #_(->> (range (int \a) (int \z))
         (map (comp str char))
         vec)

  ;;clojure.core/subs is a powerful function for
  ;;string manipulation.  Combined with clojure.core/str,
  ;;we can use it to define arbitrary transforms on
  ;;strings by selecting regions and conjoining them
  ;;with additional input.  This is one way to define
  ;;string transforms.  There are many (to include
  ;;using character seqs - which are amneable to
  ;;sequence functions and recombination with str,
  ;;but are slow in practice) such as
  ;;regex-based operations and other tools in
  ;;clojure.string.

  (defn set-character
    "Given a string s, and a valid index idx, return
   a string identical to s, except for character placed
   at the index in s."
    [s idx character]
    (str (subs s 0 idx)
         character
         (subs s (inc idx) (count s))))

  (defn substitutions
    "given string s, and idx denoting a position, generate all
   variations of s based on changing characters from the alphabet
   at idx"
    [s idx]
    (let [old (nth s idx)]
      (->> alphabet
           (filter #(not= % old))
           (map #(set-character s idx %)))))

  ;;first-steps.demo>=>(substitutions "hello" 0)
  ;;("aello" "bello" "cello" "dello" "eello" "fello" "gello" "iello" "jello" "kello"
  ;; "lello" "mello" "nello" "oello" "pello" "qello" "rello" "sello" "tello" "uello"
  ;; "vello" "wello" "xello" "yello" "zello")
  )

