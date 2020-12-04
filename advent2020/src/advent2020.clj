(ns advent2020)

(defn- get-working-dir [] (System/getProperty "data.dir"))

(defn- load-input-1 []
  (map #(Integer/parseInt %)
    (with-open [rdr (clojure.java.io/reader
                      (.toString (clojure.java.io/resource "input1.txt")))]
      (doall (line-seq rdr)))))

(comment (println (slurp (str (get-working-dir) "/input1.txt"))) (get-working-dir))
(comment (slurp) (str (get-working-dir) "/input1.txt"))

(comment (frequencies (load-input-1)))

(defn- solve-1 [inputs]
  (let [all (map #(hash-map :orig % :diff (- 2020 %)) inputs)
        agg (into #{} (map #(- 2020 %) inputs))]
    (as-> all a
         (filter #(agg (:orig %)) a)
         (first a)
         (* (:orig a) (:diff a)))))

(comment
  (let [inputs (load-input-1)
        all (map #(hash-map :orig % :diff (- 2020 %)) inputs)
        agg (into #{} (map #(- 2020 %) inputs))]
    agg))

(comment)
(comment (solve-1 (load-input-1)))
(comment (map #(+ (:orig %) (:diff %)) (solve-1 (load-input-1))))