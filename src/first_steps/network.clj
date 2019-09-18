(defn remove-quotes  [line]
  (into [] (map clojure.edn/read-string line)))

(defn get-network [file]
  (->> (clojure.string/split-lines (slurp file))
       (map #(clojure.string/split % #","))
       (into [] (map remove-quotes))))

(defn get-edges [network] 
  (->> (for [x (range (count network))
             y (range (count network))
             :when (and (> y x) (not= ((network x) y) '-))]
         [x y ((network x) y)])
       (sort-by last)
       (into [])
       ))

(defn no-cycle? [edges edge]
  (true))

(defn  get-mst [edges]
  (loop [min-edges [] x 0]
    (if (= x (count edges))
      min-edges    
      (if no-cycle? 
        (recur (conj min-edges (edges x)) (inc x))
        (recur min-edges (inc x))))))

(defn  get-weight [edges] (reduce + (map last edges)))

(def network (get-network "p107_network.txt"))
(def edges (get-edges network))
(def min-edges (get-mst edges))
(- (get-weight edges) (get-weight min-edges))
