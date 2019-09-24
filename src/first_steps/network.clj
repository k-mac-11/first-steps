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
         [[x y] ((network x) y)])
       (sort-by last)
       (into [])
       ))

(defn get-edge-list [edges]
  (reduce (fn [acc [[from to] w] ]
            (-> acc
                (assoc-in [from to] w)
                (assoc-in [to from] w)))
          {} edges))

(defn dfs [edge-list node]
  (loop [visited #{node} to-visit (into [] (keys (edge-list node)))]
    (if (empty? to-visit)
      visited
      (let [new-visited (conj visited (peek to-visit))
            new-nodes-to-visit (into [] (remove visited (keys (edge-list (peek to-visit)))))
            new-to-visit (into (pop to-visit) new-nodes-to-visit)]        
        (recur new-visited new-to-visit)))))

(defn no-cycle? [edge-list node]
  (loop [visited #{node} to-visit (into [] (keys (edge-list node)))]
    (if (empty? to-visit)
      true
      (let [new-visited (conj visited (peek to-visit))
            new-nodes-to-visit (into [] (remove visited (keys (edge-list (peek to-visit)))))
            new-nodes-to-visit-untrimmed (into [] (keys (edge-list (peek to-visit))))
            new-to-visit (into (pop to-visit) new-nodes-to-visit)]        
        (if (> (- (count new-nodes-to-visit-untrimmed) (count new-nodes-to-visit)) 1)
          false
          (recur new-visited new-to-visit))))))

(defn get-mst [edges]
  (loop [min-edges [] x 0]
    (if (= x (count edges))
      min-edges    
      (if (no-cycle? (get-edge-list (conj min-edges (edges x))) (((edges x) 0) 0)) 
        (recur (conj min-edges (edges x)) (inc x))
        (recur min-edges (inc x))))))

(def network (get-network "p107_network.txt"))
(def edges (get-edges network))
(def edge-list (get-edge-list edges))
(def mst (get-mst edges))

(reduce + (map last edges))
(reduce + (map last mst))
