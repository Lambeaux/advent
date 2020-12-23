(ns advent2020)

;; ----------------------------------------------------------------------
;; # Advent 2020 Common Utilities
;;
;; These functions can be used by any puzzle.
;;

(defn- load-input [rsrc-name]
  (with-open [rdr (clojure.java.io/reader
                    (.toString (clojure.java.io/resource rsrc-name)))]
    (doall (line-seq rdr))))

;; ----------------------------------------------------------------------
;; # Advent 2020 Day 01 Part One
;;

(defn- load-input-1 []
  (map #(Integer/parseInt %)
       (with-open [rdr (clojure.java.io/reader
                         (.toString (clojure.java.io/resource "input1.txt")))]
         (doall (line-seq rdr)))))

(defn- solve-1-1 [inputs]
  (let [all (map #(hash-map :orig % :diff (- 2020 %)) inputs)
        agg (into #{} (map #(- 2020 %) inputs))]
    (as-> all a
          (filter #(agg (:orig %)) a)
          (first a)
          (* (:orig a) (:diff a)))))

(defn- solve-1-2 [inputs]
  ())

(comment (frequencies (load-input-1)))
(comment (solve-1-1 (load-input-1)))
(comment (solve-1-2 (load-input-1)))

;; ----------------------------------------------------------------------
;; # Advent 2020 Day 20 Part One
;;
;; Helper functions and solution.
;;

(defn- load-input-20 []
  (with-open [rdr (clojure.java.io/reader
                    (.toString (clojure.java.io/resource "input20.txt")))]
    (doall (line-seq rdr))))

(defn- parse-tiles [input]
  (reduce
    (fn [val next]
      (cond
        (.startsWith next "Tile")
        (conj val {:id (Integer/parseInt (.substring next 5 9)) :raw '()})
        (.isEmpty next)
        val
        :default
        (let [new-grid (conj (:raw (first val)) next)]
          (conj (rest val) (assoc (first val) :raw new-grid)))))
    '()
    input))

(defn- border->encoding [border]
  (let [binary-seq (map #(Math/pow 2 %) (range 0 10))
        symbol-map #(if (= % \#) 1 0)]
    (->> border
         (map symbol-map)
         (interleave binary-seq)
         (partition 2)
         (map #(* (first %) (last %)))
         (reduce +)
         (int))))

(defn- apply-norms [tile]
  (let [grid (:raw tile)]
    (merge tile
           {:norms {:top (first grid)
                    :bot (apply str (reverse (last grid)))
                    :lhs (apply str (reverse (map first grid)))
                    :rhs (apply str (map last grid))}})))

(defn- apply-has [tile]
  (let [norms (:norms tile)]
    (merge tile
           {:has {:top (border->encoding (:top norms))
                  :bot (border->encoding (:bot norms))
                  :lhs (border->encoding (:lhs norms))
                  :rhs (border->encoding (:rhs norms))}})))

(defn- apply-needs [tile]
  (let [norms (:norms tile)]
    (merge tile
           {:needs {:top (border->encoding (apply str (reverse (:top norms))))
                    :bot (border->encoding (apply str (reverse (:bot norms))))
                    :lhs (border->encoding (apply str (reverse (:lhs norms))))
                    :rhs (border->encoding (apply str (reverse (:rhs norms))))}})))

(defn- sets-only [tile]
  (as-> tile t
        (update t :has #(into #{} (vals %)))
        (update t :needs #(into #{} (vals %)))
        ;; Combining to account for flipping possibilities
        (update t :needs #(into % (:has t)))))

(defn- neighbors [tiles]
  (let [apply-neighbors
        (fn [tile]
          (let [dependency-pred
                (every-pred
                  ;; Added as a precaution now that flipping makes the data reflexive
                  #(not= (:id tile) (:id %))
                  #(not-empty (clojure.set/intersection (:needs tile) (:has %)))
                  ;; Adding both sides made no difference
                  #_#(not-empty (clojure.set/intersection (:needs %) (:has tile))))
                related-tiles
                (filter dependency-pred tiles)
                related-tile-ids
                (map #(:id %) related-tiles)]
            (merge tile {:neighbor-ids   related-tile-ids
                         :neighbor-count (count related-tile-ids)})))]
    (map apply-neighbors tiles)))

(defn- generate-tile-metadata
  "Provides all tiles with attached metadata on the map itself (not Clojure metadata)."
  []
  (->> "input20.txt"
       load-input
       parse-tiles
       (map apply-norms)
       (map apply-has)
       (map apply-needs)
       (map sets-only)
       (neighbors)
       (map #(into (sorted-map) %))))

(defn- solve-20-1
  "Evaluate this function to obtain the puzzle result for Day 20 Part One."
  []
  (->> (generate-tile-metadata)
       (filter #(= (:neighbor-count %) 2))
       (map #(:id %))
       (reduce *)))

;; ----------------------------------------------------------------------
;; # Advent 2020 Day 20 Part Two
;;

;; ----------------------------------------------------------------------
;; # Advent 2020 Day 20 Part One
;;
;; REPL support functions and scratch pad.
;;

(defn- filter-loud-keys [maps]
  (let [ignore #{:raw :norms}
        ignore-when (complement #(contains? ignore (first %)))]
    (->> maps
         (map #(filter ignore-when %))
         (map #(into (sorted-map) %)))))

(defn- frequencies-of [key tiles]
  (as-> tiles t
        (map #(key %) t)
        (map #(into '() %) t)
        (flatten t)
        (frequencies t)
        (into (sorted-map-by #(compare [(get t %2) %2] [(get t %1) %1])) t)))

(comment

  (solve-20-1)
  ;; 11788777383197, the solution to part one.

  (generate-tile-metadata)
  ;; Full tile data model used for solving

  (filter-loud-keys (generate-tile-metadata))
  ;; Only crucial tile data used for mapping the arrangement

  (count (generate-tile-metadata))
  ;; 144 total tiles

  (->> (generate-tile-metadata) (map #(:id %)) frequencies)
  ;; No ID is repeated

  (->> (generate-tile-metadata) (map #(:neighbor-count %)) frequencies)
  ;; {4 100, 3 40, 2 4}, which  means:
  ;; - 100 nodes have 4 neighbors
  ;; -  40 nodes have 3 neighbors
  ;; - and only 4 nodes have 2 neighbors, as expected. See following diagram.

  (comment
    ;; 12 x 12 grid for verifying the above.
    x x x x x x x x x x x x
    x x x x x x x x x x x x
    x x x x x x x x x x x x
    x x x x x x x x x x x x
    x x x x x x x x x x x x
    x x x x x x x x x x x x
    x x x x x x x x x x x x
    x x x x x x x x x x x x
    x x x x x x x x x x x x
    x x x x x x x x x x x x
    x x x x x x x x x x x x
    x x x x x x x x x x x x)

  (frequencies-of :has (generate-tile-metadata))
  (frequencies-of :needs (generate-tile-metadata))
  ;; Observing occurrance count of IDs within connections.

  (load-input-20)
  (filter #(.startsWith % "Tile") (load-input-20))
  (filter #(not (.startsWith % "Tile")) (load-input-20))
  (parse-tiles (load-input-20))
  ;; Tests for working with the raw data

  (count (parse-tiles (load-input-20)))
  ;; Count didn't change, still 144

  (apply str (reverse "###..."))
  (border->encoding "###...#...")
  (border->encoding (apply str (reverse "###...#...")))
  (border->encoding "...###.###"))

(comment
  "          ^^          "
  "         0000         "
  "<- 0000 [1657] 0000 ->"
  "         0000         "
  "          vv          ")