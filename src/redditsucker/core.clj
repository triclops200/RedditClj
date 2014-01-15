(ns redditsucker.core
  (:require [clj-http.client :as client])
  (:require [clojure.data.json :as json])
  (:require [clj-time.format :as timefmt])
  (:require [clj-time.coerce :as timeco])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn get-subreddit-json
  "Gets the literal json string from a subreddit"
  ([subreddit]
     (get-subreddit-json subreddit "top"))
  ([subreddit sorting]
     (get-subreddit-json subreddit sorting "day"))
  ([subreddit sorting time]
     (:body
      (client/get
       (str "http://reddit.com/r/" subreddit "/" sorting
            ".json?t=" time "&limit=" 100))))
  ([subreddit sorting time after]
     (:body
      (client/get
       (str "http://reddit.com/r/" subreddit "/" sorting
            ".json?t=" time "&limit=" 100 "&after=" after)))))

(defn get-subreddit-data-chunk
  "Gets the map from the subreddit"
  ([subreddit]
     (get-subreddit-data-chunk subreddit "top"))
  ([subreddit sorting]
     (get-subreddit-data-chunk subreddit sorting "day"))
  ([subreddit sorting time]
     (json/read-str (get-subreddit-json subreddit sorting time)))
  ([subreddit sorting time after]
     (json/read-str (get-subreddit-json subreddit sorting time after))))


(defn get-posts
  [subreddit-data]
  (map #(get % "data") (get-in subreddit-data ["data" "children"])))

(defn get-top-subreddit-data
  ([subreddit]
     (get-subreddit-data-chunk subreddit "top" "month"))
  ([subreddit chunkcount]
     (loop
         [acc (get-posts (get-subreddit-data-chunk subreddit "top" "month"))
          after (get (last acc) "name")
          count 1]
       (if (>= count chunkcount)
         acc
         (let [next (concat acc (get-posts (get-subreddit-data-chunk subreddit "top" "month" after)))]
           (recur next
                  (get (last next) "name")
                  (inc count)))))))

(defn filter-by-upvotes
  ([posts]
     (filter-by-upvotes posts 100 >))
  ([posts number]
     (filter-by-upvotes posts number >))
  ([posts number comp]
     (filter #(comp (get % "ups") number) posts)))

(defn pretty-compress
  [posts]
  (map #(vector (get % "title") (get % "created_utc") (get % "ups")) posts))

(defn sort-compressed-data
  "Sorts the compressed data"
  ([posts]
     (sort-compressed-data posts >))
  ([posts comp]
     (sort #(comp (get %1 2) (get %2 2)) posts)))

(defn get-times
  "Gets the times from the compressed data"
  [compressed-posts]
  (map #(timeco/from-long (long (* 1000 (get % 1)))) compressed-posts))

(defn time-seq-to-hour-seq
  "Gets the time seq and returns a seq of the times in hours"
  [time-seq]
  (map #(subs (timefmt/unparse (timefmt/formatters :time) %) 0 5) time-seq))

(defn hour-seq-to-MAM-seq
  "Returns a sequence of minutes after midnight"
  [hour-seq]
  (->> hour-seq 
       (map #(vector (subs % 0 2) (subs % 3 5)))
       (map (fn [[hr min]] (+ (* (Integer. hr) 60) (Integer. min))))))

(defn get-most-common-post-times-in-MAM
  [subreddit]
  (into []  (->
             (get-top-subreddit-data subreddit 10)
             filter-by-upvotes
             pretty-compress
             get-times
             time-seq-to-hour-seq
             hour-seq-to-MAM-seq)))

(defn median [ns]
  (let [cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (get ns mid)
      (/ (+ (get ns mid) (get ns (dec mid))) 2))))

(defn get-median-time-MAM
  [time-MAM-seq]
  (-> (into [] (sort time-MAM-seq))
      median
      double
      Math/round))

(defn MAM-to-hours
  [mam]
  (format "%02d:%02d"  (quot mam 60) (rem mam 60)))

(defn assoc-swap
  [vec index func]
  (assoc vec index (func (get vec index))))

(defn box-data
  [data boxes max]
  (reduce (fn [acc datum] (assoc-swap acc (int (quot datum (/ max (dec boxes)))) inc))
          (into [] (repeat boxes 0)) data))

(defn zip
  [& colls]
  (apply map (fn [& xs] (apply vector xs)) colls))

(defn max-with-index
  [data]
  (reduce (fn [[max maxind] [num ind]] (if (> num max) [num ind] [max maxind]))
          [(first data) 0]  (zip data (range (count data)))))

(defn best-section-from-times
  [mam-times sections]
  (let [[num time] (-> (box-data mam-times sections (dec (* 24 60)))
               max-with-index)]
    (MAM-to-hours (int (* time (* 60 (/ 24 sections)))))))

(defn pretty-data
  [mam-times sections]
  (map (fn [[num time]] [(MAM-to-hours (int (* time (* 60 (/ 24 sections))))) num])
       (zip (box-data mam-times sections (dec (* 24 60))) (range sections))))

(defn best-time-from-subreddit
  [subreddit sections]
  (best-section-from-times (get-most-common-post-times-in-MAM subreddit) sections))
