(ns pfds.ch1)

(defn suffixes
  "A list of all the suffixes of `coll` in decreasing order of length"
  [coll]
  (loop [accum []
         coll coll]
    (if (seq coll)
      (recur (conj accum coll) (vec (rest coll)))
      (conj accum []))))
