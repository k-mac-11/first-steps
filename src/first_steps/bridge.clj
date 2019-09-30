(def initial {:t 0 :p1 0 :p2 0 :p3 0 :p4 0})

(def goal {:t 1 :p1 1 :p2 1 :p3 1 :p4 1})

(def weights [1 2 5 8])

(def goal-weight 15)

(def all-states (for [y [0 1] x1 [0 1] x2 [0 1] x3 [0 1] x4 [0 1]]
                  {:t y :p1 x1 :p2 x2 :p3 x3 :p4 x4}))

(defn torch-0-1 [state1 state2]
  (and (= (:t state1) 0) (= (:t state2) 1)))

(defn torch-1-0 [state1 state2]
  (and (= (:t state1) 1) (= (:t state2) 0)))

(defn two-ps-0-1 [state1 state2]
  (let [diff (map - (rest (vals state2)) (rest (vals state1)))
        pluses (filter pos? diff)
        minuses (filter neg? diff)]    
    (if (and (= (count pluses) 2) (empty? minuses))
      true
      false)))

(defn one-p-1-0 [state1 state2]
  (let [diff (map - (rest (vals state2)) (rest (vals state1)))
        pluses (filter pos? diff)
        minuses (filter neg? diff)]    
    (if (and (empty? pluses) (= (count minuses) 1))
      true
      false)))

(defn possible-state [state1 state2]
  (if (or (and (torch-0-1 state1 state2) (two-ps-0-1 state1 state2))
          (and (torch-1-0 state1 state2) (one-p-1-0 state1 state2)))
    true
    false))

(defn get-weight [node neighbor weights]
  (if (= (node :t) 0)
    (reduce max (map * weights (map - (rest (vals neighbor)) (rest (vals node)))))
    (reduce max (map * weights (map - (rest (vals node)) (rest (vals neighbor)))))))

(defn all-possible-states [state all-states weights]
  (reduce (fn [acc possible]
            (if (possible-state state possible)
              (assoc acc possible (get-weight state possible weights))
              acc)) {} all-states))

(defn build-graph [state all-states weights]
  (loop [current state
         visited #{current}
         neighbors (all-possible-states current all-states weights)
         to-visit (into [] (keys neighbors))
         graph {current neighbors}]
    (if (empty? to-visit)
      graph      
      (let [next (peek to-visit)
            new-visited (conj visited next)
            new-neighbors (all-possible-states next all-states weights)
            new-to-visit (into (pop to-visit) (remove visited (into [] (keys new-neighbors))))
            new-graph (assoc graph  next  new-neighbors)]
        (recur next new-visited new-neighbors new-to-visit new-graph)))))

(def graph (build-graph initial all-states weights))

(defn update-paths [paths state neighbors]
  (reduce (fn [acc [neighbor length]]
            (if (neighbors neighbor)
              (if (< (+ (paths state) (neighbors neighbor)) length)
                  (assoc acc neighbor (+ (paths state) (neighbors neighbor)))
                  (assoc acc neighbor length))
              (assoc acc neighbor length))) {} paths))

(def initial-paths
  (assoc (reduce
          (fn [acc state]
            (assoc acc state ##Inf)) {} (keys graph)) initial 0))

(defn shortest-path [graph state initial-paths]
  (loop [current state
         visited #{current}
         to-visit (into [] (keys (graph current)))
         paths (update-paths initial-paths current (graph current))]
    (if (empty? to-visit)
      paths
      (let [next (peek to-visit)
            new-visited (conj visited next)
            new-to-visit (into (pop to-visit) (remove visited (into [] (keys (graph next)))))
            new-paths (update-paths paths next (graph next))]
        (recur next new-visited new-to-visit new-paths)))))

(def shortest (shortest-path graph initial initial-paths))
(def answer (shortest goal))
