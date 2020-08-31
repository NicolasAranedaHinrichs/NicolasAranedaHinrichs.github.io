(ns cvhtml)

(def in-file
  (->> (clojure.string/split (slurp "cv.txt") #"\n")
       (map (comp clojure.string/trim-newline clojure.string/trim))
       (filter #(not= % ""))
       (take-while #(not= "END" %))))
       
(defn get-blocks [lines]
  (if (empty? lines)
    '()
    (let [l (first lines)]
      (cons {:section (apply str (rest l))
             :entries (take-while #(not= (first %) \#) (rest lines))}
            (get-blocks (drop-while #(not= (first %) \#) (rest lines)))))))

(defn parse-entry [line]
  (let [[a b] (clojure.string/split line #"\|")]
    {:year (clojure.string/trim a)
     :content (clojure.string/trim b)}))

(defn parse []
  (->> in-file
       (get-blocks)
       (map #(update % :entries (fn [x] (mapv parse-entry x))))))

(defn html-of-block [{:keys [section entries] :as block}]
  (str "<center><h3>" section "</h3></center>\n<table class=cvtable>\n"
       (apply str
         (mapcat (fn [{:keys [year content]}]
                   (str "<tr>\n<th>" year "</th>\n<td>" content "</td>\n</tr>\n"))
                 entries))
       "</table>"))

(defn get-html []
  (apply str (mapcat #(str (html-of-block %) "\n\n") (parse))))
            
(println (get-html))
