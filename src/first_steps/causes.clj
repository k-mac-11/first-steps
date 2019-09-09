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
       (conj (for [index (range (count word))] (remove-letter word index)))
       (conj (for [index (range (inc (count word))) letter letters] (add-letter word index letter)))))

(new-words "causes" letters)
