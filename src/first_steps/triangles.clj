(def file (clojure.string/split-lines (slurp "p102_triangles.txt")))

(def strings (into [] (map #(clojure.string/split % #",") file)))

(defn remove-quotes  [line]
  (into [] (map read-string line)))

(def triangles (into [] (map remove-quotes strings)))

(defn abs [x]
  (max x (* -1 x)))

(defn area [triangle]
  (let [x1 (triangle 0) y1 (triangle 1) x2 (triangle 2) y2 (triangle 3) x3 (triangle 4) y3 (triangle 5)]
    (abs (/ (+ (+ (* (- y2 y3) x1) (* (- y3 y1) x2)) (* (- y1 y2) x3)) 2.0)))

(defn contains-origin? [triangle]
  (let [x1 (triangle 0) y1 (triangle 1) x2 (triangle 2) y2 (triangle 3) x3 (triangle 4) y3 (triangle 5)
        area0 (area triangle)
        area1 (area [x1 y1 x2 y2 0 0])
        area2 (area [x1 y1 0 0 x3 y3])
        area3 (area [0 0 x2 y2 x3 y3])]

    (= area0 (+ area3 (+ area1 area2)))))

(count (filter contains-origin? triangles))
