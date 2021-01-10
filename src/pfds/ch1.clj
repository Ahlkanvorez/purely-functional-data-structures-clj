(ns pfds.ch1)

(defn suffixes
  "Ex. 2.1: list all suffixes of `coll` in decreasing order of length"
  [coll]
  (loop [accum []
         coll coll]
    (if (seq coll)
      (recur (conj accum coll) (vec (rest coll)))
      (conj accum []))))

(defprotocol Ordered
  (eq [this o])
  (lt [this o])
  (leq [this o]))

(defprotocol Set
  (empty [this])
  (member? [this e])
  (insert [this e]))

(declare -member?)
(declare -insert)
(declare unbalanced-set)

(deftype UnbalancedSet [value]
  Set
  (empty [this] (unbalanced-set nil))
  (member? [this x] (-member? x this))
  (insert [this x] (-insert unbalanced-set x this))

  Object
  (toString [this] (str value)))

(defn unbalanced-set [value] (UnbalancedSet. value))

(defn -member? [x t]
  (if-not (instance? UnbalancedSet t)
    false
    (let [[a y b] (.value t)]
      (cond (lt x y) (-member? x a)
            (lt y x) (-member? x b)
            :else true))))

(defn -insert [T x t]
  (if (nil? t)
    (T [nil x nil])
    (let [[a y b] (.value t)]
      (cond (lt x y) (T [(-insert x a) y b])
            (lt y x) (T [a y (-insert x b)])
            :else t))))

(extend-protocol Ordered
  Number
  (eq [this o] (= this o))
  (lt [this o] (< this o))
  (leq [this o] (<= this o)))

(defn better-member?
  "Ex 2.2: Use at most d + 1 comparisons to check for membership of `x`
  where d is the depth of the tree `t`.

  This uses at most d + 1 comparisons, since it performes one leq per
  node as it traverses down to depth d, and keeps track of the most
  recent node which was leq to the desired value. Once it reaches the
  bottom of the tree, either that node is the desired value, or the
  candidate carried along is the desired value, or the value is not in
  the tree; since if it were in the tree, it could not be less than
  the node at the bottom of the tree, and it could not be greater than
  the candidate for which it was leq."
  [x t]
  (letfn [(better-member [x t c]
            (if-not (instance? UnbalancedSet t)
              (or (= x t) (= x c))
              (let [[a y b] (.value t)]
                (if (leq x y)
                  (better-member x a y)
                  (better-member x b c)))))]
    (better-member x t nil)))
