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

(defn altered-words [word]
  (set (remove #(= % word) (new-words word letters))))

(defn to-visit [altered-words candidates]
  (clojure.set/intersection altered-words candidates))

(loop [friends #{}
       to-visit (to-visit (altered-words "causes") candidates)]
  (if (empty? to-visit)
    friends
    (recur (clojure.set/union  friends (set (first to-visit)))
           (clojure.set/union  to-visit (clojure.set/difference (to-visit (altered-words (first to-visit)) candidates) friends)))))
