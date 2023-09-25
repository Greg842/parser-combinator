(ns parser-combinator.core)

(defn constant [value]
  (fn [obj] value))
(defn variable [value]
  (fn [obj]
    (cond
      (= value "x") (get obj "x")
      (= value "y") (get obj "y")
      (= value "z") (get obj "z")
      :else nil)))
(defn add [& args]
  (fn [obj]
    (apply + ((apply juxt args) obj))))
(defn sub [& args]
  (fn [obj]
    (apply - ((apply juxt args) obj))))
(defn mul [& args]
  (fn [obj]
    (apply * ((apply juxt args) obj))))
(defn div [& args]
  (fn [obj]
    (apply / ((apply juxt args) obj))))
(defn -return [value tail] {:value value :tail tail})
(def -value :value)
(def -tail :tail)
(def -valid boolean)
(defn -show [r]
  (if (-valid r)
    (str "-> " (pr-str (-value r)) " | " (pr-str (-tail r)))
    "!"))
(defn -tabulate [p & inputs]
  (run! (fn [input] (printf "   %-10s %s\n" (pr-str input) (-show (p input)))) inputs))
(defn _empty [value]
  (fn [input] (-return value input)))
(defn _char [p]
  (fn [[c & cs]]
    (if (and c (p c))
      (-return c cs))))
(defn _map [f r]
  (if (-valid r)
    (-return (f (-value r)) (-tail r))))
(defn _combine [f a b]
  (fn [input]
    (let [ar (a input)]
      (if (-valid ar)
        (_map (partial f (-value ar))
              ((force b) (-tail ar)))))))
(defn _either [a b]
  (fn [input]
    (let [ar (a input)]
      (if (-valid ar)
        ar
        (b input)))))
(defn _parser [p]
  (let [p' (_combine (fn [v _] v) p (_char #{\u0000}))]
    (fn [input] (-value (p' (str input \u0000))))))
(defn +char [chars]
  (_char (set chars)))
(defn +char-not [chars]
  (_char (comp not (set chars))))
(defn +map [f p]
  (comp (partial _map f) p))
(def +parser _parser)
(defn +ignore [p]
  (+map (constantly 'ignore) p))
(defn iconj [coll value]
  (if (= value 'ignore)
    coll
    (conj coll value)))
(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))
(defn +seqf [f & ps]
  (+map (partial apply f) (apply +seq ps)))
(defn +seqn [n & ps]
  (apply +seqf #(nth %& n) ps))
(defn +or [p & ps]
  (reduce _either p ps))
(defn +opt [p]
  (+or p (_empty nil)))
(defn +star [p]
  (+or (+seqf cons p (delay (+star p))) (_empty ())))
(defn +plus [p]
  (+seqf cons p (+star p)))
(defn +str [p]
  (+map (partial apply str) p))
(def *digit (+char "0123456789"))
(def *constant (+map constant (+map read-string (+str (+seq (+opt (+char "-")) (+str (+plus *digit)))))))
(def *variable (+map variable (+map str (+char "xyz"))))
(def *operand (+or *constant *variable))
(def *space (+star (+char " \t\n")))
(def *ws (+ignore *space))

(def *mul (+seqf (partial apply mul) *operand (+plus (+seqn 0 *ws (+ignore (+char "*")) *ws *operand))))
(def *div (+seqf (partial apply div) (+or
                                       *mul
                                       *operand) (+plus (+seqn 0 *ws (+ignore (+char "/")) *ws *operand))))
(def *add (+seqf add (+or
                       *mul
                       *div
                       *operand) *ws (+ignore (+char "+")) *ws (+or *mul
                                                                    *div
                                                                    *operand)))
(def *sub (+seqf sub (+or
                       *mul
                       *div
                       *operand) *ws (+ignore (+char "-")) *ws (+or *mul
                                                                    *div
                                                                    *operand)))
