(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [previous tail]
                 (if (empty? tail)
                   previous
                   (recur (first tail) (rest tail))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [e1 (empty? seq1)
        e2 (empty? seq2)]
    (cond
      (and e1 e2) true
      (or e1 e2) false
      (not (= (first seq1) (first seq2))) false
      :else (recur (rest seq1) (rest seq2)))))

(defn find-first-index [pred a-seq]
  (loop [pos 0
         sqz a-seq]
    (cond
      (empty? sqz) nil
      (pred (first sqz)) pos
      :else (recur (inc pos) (rest sqz)))))

(defn avg [a-seq]
  (loop [bunch a-seq
         acc 0
         n 0]
    (if (empty? bunch)
      (/ acc n)
      (recur (rest bunch) (+ (first bunch) acc) (inc n)))))

(defn parity [a-seq]
  (loop [remaining a-seq
         result #{}]
    (let [toggle (fn [a-set elem]
                   (if (contains? a-set elem)
                     (disj a-set elem)
                     (conj a-set elem)))]
      (if (empty? remaining)
        result
        (recur (rest remaining) (toggle result (first remaining)))))))

(defn fast-fibo [n]
  (loop [cur 1
         prev 0
         remain n]
    (if (zero? remain)
      prev
      (recur (+ cur prev) cur (dec remain)))))

(defn cut-at-repetition [a-seq]
  (loop [result []
         others a-seq]
    (let [head (first others)]
      (if (or
            (empty? others)
            (contains? (set result) head))
        result
        (recur (conj result head) (rest others))))))

