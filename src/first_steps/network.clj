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

(defn  get-mst [edges]
  (loop [min-edges [] first-nodes [] second-nodes [] x 0]
    (if (= x (count edges))
      min-edges    
      (if (or (not (contains? first-nodes (first (edges x)))) (not (contains? second-nodes (second (edges x)))))
        (recur (conj min-edges (edges x)) (into [] (map first min-edges)) (into [] (map second min-edges)) (inc x))
        (recur min-edges (into [] (map first min-edges)) (into [] (map second min-edges)) (inc x))))))

(defn  get-weight [edges] (reduce + (map last edges)))

(def network (get-network "p107_network.txt"))
(def edges (get-edges network))
(def min-edges (get-mst edges))
(- (get-weight edges) (get-weight min-edges))
