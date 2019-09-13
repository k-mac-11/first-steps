(def letters ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"  "n" "o"
              "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"])

(defn change-letter [word  index letter]
  (str (subs word  0 index) letter (subs word (inc index) (count word))))

(defn remove-letter [word  index]
  (str (subs word  0 index) (subs word (inc index) (count word))))

(defn add-letter [word index letter]
  (str (subs word 0 index) letter (subs word index (count word))))

(defn new-words [word letters]
  (->> (for [index (range (count word)) letter letters] (change-letter word index letter))
   (concat (for [index (range (count word))] (remove-letter word index)))
   (concat (for [index (range (inc (count word))) letter letters] (add-letter word index letter)))))

(def candidates (set (clojure.string/split-lines (slurp "word.list"))))

(defn altered-words [word]
  (remove #(= % word) (new-words word letters)))

(defn to-visit [altered-words candidates]
  (filter candidates altered-words))

(defn network [word]
  (loop [candidates candidates
         friends #{word}
         pending [word]]
  (if (empty? pending)
    (count friends)
    (let [next-word (peek pending)
          new-friends (remove friends (to-visit (altered-words next-word) candidates))]
      (recur (reduce disj candidates new-friends)
             (conj friends next-word)
             (into (pop  pending) new-friends))))))

(defn friends [word candidates]
  (filter candidates (remove #(= % word) (new-words word letters))))

(defn my-network [word]
  (loop [visited word
         to-visit (friends word candidates)]
    (if (empty? to-visit)
      (count visited)
      (let [new-friend (first to-visit)
            new-to-visit (rest to-visit)
            new-friends (remove (set visited) (friends new-friend candidates))]
        (recur (conj visited new-friend) (concat new-to-visit new-friends))))))

(defn seq-contains? [xs k]
  (some (fn [v] (when (= v k) v)) xs))

(defn my-network [word]
  (loop [visited  [word]
         to-visit (friends word candidates)]
    (if (empty? to-visit)
      visited
      (let [new-friend   (first to-visit)
            new-to-visit (rest  to-visit)
            new-friends  (friends new-friend candidates)
            really-new-friends (filter #(seq-contains? visited %)
                                       new-friends)
            ]
        (recur (conj visited new-friend)
               (distinct
                (concat new-to-visit
                        really-new-friends)))))))
