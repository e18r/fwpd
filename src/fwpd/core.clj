(ns fwpd.core)
(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn my-mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [[name glitter-index]]
         {:name (convert :name name)
          :glitter-index (convert :glitter-index glitter-index)})
       rows))

(defn mapify
  [rows]
  (map (fn [row]
         (reduce (fn [result [key value]]
                   (assoc result key (convert key value)))
                 {}
                 (into {} (map vector vamp-keys row))))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (map :name (filter #(>= (% :glitter-index)
               minimum-glitter)
          records)))

(defn append
  [new records]
  (conj records new))

(def list (mapify (parse (slurp filename))))

list

(def record {:name "Emi" :glitter-index 38})
(append record list)

(def validations {:name #(not (nil? %))
                  :glitter-index #(and (not (nil? %)) (Integer. %))})

(not= (record :name) nil)

((validations :name) :a)

(contains? validations :name)

(contains? {:a nil :b 1} :c)

(defn validate
  [validations record]
  (reduce (fn [result [key validator]]
              (if (not (contains? record key))
                (throw (Exception. (str "key `" key "` not present in `" record "`")))
                (let [value (record key)]
                  (if (not (validator value))
                    (throw (Exception. (str "invalid value for key `" key "` (" value ")")))
                    (assoc result key value)))))
          {}
          validations))

(validate validations {:glitter-index 1 :name ""})

(def valid? (partial validate validations))

(def new-list (append (valid? {:name "Emilio" :glitter-index 4}) list))

new-list

(clojure.string/join "," ["hola" "papi"])

(defn demapify
  [list]
  (reduce (fn [document dic]
          (conj document
                (reduce (fn [row [key value]]
                          (conj row value))
                        []
                        dic)))
        []
        list)
  )

(demapify new-list)

(defn stringify
  [arrays]
  (clojure.string/join "\n"
                       (map (fn [array]
                              (clojure.string/join "," array))
                            arrays)))

(def new-string (stringify (demapify new-list)))

(spit filename new-string)
