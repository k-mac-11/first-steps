(defn remove-quotes  [line]
  (into [] (map clojure.edn/read-string line)))

(defn remove-dashes  [line]
  (into [] (map #(if (= % "-") "0" %) line)))

(defn get-matrix [file]
  (->> (clojure.string/split-lines (slurp file))
       (map #(clojure.string/split % #","))
       (map remove-dashes)
       (into [] (map remove-quotes))))

(defn get-edges [network] 
  (->> (for [x (range (count network))
             y (range (count network))
             :when (and (> y x) (not= ((network x) y) '-))]
         [x y ((network x) y)])
       (sort-by last)
       (into [])
       ))

(defn get-list [network]
  (->> (for [x (range (count network))
             y (range (count network))
             :when (not= ((network x) y) '-)]
         [x y])       
       ))

(def matrix (get-matrix "p107_network.txt"))
(def edges (get-edges matrix))
