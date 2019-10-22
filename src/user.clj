(ns user)

;;;; why Specter?

(def conferences [{:name      "Clojure Conj"
                   :organiser "Cognitect"
                   :events    [{:start-date "Oct 12, 2017"
                                :city       "Baltimore"
                                :country    "US"
                                :attendees  307
                                :grants     #{:opportunity}}
                               {:start-date "Nov 29, 2018"
                                :city       "Durham"
                                :country    "US"
                                :attendees  350
                                :grants     #{:opportunity}}]}
                  {:name      "Clojure Sydney Meetup"
                   :organiser "Rokt"
                   :events    [{:start-date "Oct 22, 2019"
                                :city       "Sydney"
                                :country    "AU"
                                :attendees  20
                                :presenters #{"Carlo" "Steve"}}]}])

; Clojure Core
; easy nested reads
(get-in conferences [1])

(get-in conferences [1 :events 0 :attendees])

(assoc-in conferences [1 :events 0 :attendees] 25)

; easy nested changes e.g. increment a single value
(update-in conferences [1 :events 0 :attendees] inc)

; not easy: get all attendee counts in a seq
; not easy: increment N nested values, not easy at all (see github for side-by-side)
; this is the primary value prop of Specter

;;;;

(require '[com.rpl.specter :refer [select select-first select-one transform setval
                                   ALL LAST FIRST BEGINNING END MAP-KEYS MAP-VALS NONE
                                   collect-one pred must filterer walker submap srange
                                   recursive-path if-path continue-then-stay]])

(select [FIRST] conferences)                                ; "select" means select all
(select-first [FIRST] conferences)

(select [ALL :name] conferences)

(select [ALL :events ALL :attendees] conferences)

; fits well with thread-last macro
(->> conferences
     (select [ALL :events ALL :attendees])
     (reduce +))

; nested changes are much easier
(->> conferences
     (transform [ALL :events ALL :attendees] inc)
     ;(select [ALL :events ALL :attendees])
     )

(defn aussie-event?
  [event]
  (= "AU" (:country event)))

; the "Path" DSL is the big idea!!
; https://github.com/redplanetlabs/specter/wiki/List-of-Navigators
; paths are made out of "navigator"s
(let [aussies [ALL :events ALL aussie-event?]]
  (select aussies conferences))

; predicate fns
;    - fns are navigators
;    - use [ALL fn] or [ALL (pred fn)]

; path composition: a vector means AND
(def events [ALL :events ALL])

(select [events aussie-event?] conferences)

; navigators can be HOF-like/parameterised
(select [events (pred aussie-event?)] conferences)

(select [events :grants] conferences)
(select [events (must :grants)] conferences)
(transform [events (must :grants)] #(conj % :celebrity) conferences) ; only those with :grants get addition

; at what cost?
;    - learning to write navigators is a skill. Nathan Marz is really supportive in Slack
;    - performance is often better (see github benchmarks)
;    - readability? a matter of opinion: the concept of ALL vs select-first takes acclimatisation
;    - thread last macros, even for non-sequences, not matching core thread-first macro idioms. not a big deal IMO

; day to day workflow

; 1/ use to select/select-one to verify correct path
; 2/ switch to setval or transform if changing data
(->> conferences
     (select [ALL :events ALL :attendees])
     ;(transform [ALL :events ALL :attendees] inc)
     )

; more advanced use

; nested seq concat: start vs end of seq
(->> conferences
     (setval [ALL #(= "Rokt" (:organiser %)) :events END]
             [{:start-date "Nov 26, 2019"
               :city       "Sydney"
               :country    "AU"
               :attendees  nil
               :presenters #{}}]))

(->> conferences
     (setval [ALL #(= "Rokt" (:organiser %)) :events BEGINNING]
             [{:start-date "Nov 26, 2019"
               :city       "Sydney"
               :country    "AU"
               :attendees  nil
               :presenters #{}}]))

; collect interim values in the path

(->> conferences
     (select [ALL (collect-one :name) :events ALL :attendees]))

(->> conferences
     (transform [ALL (collect-one :name) :events ALL :attendees]
                (fn [event-name attendee-count]
                  ;(clojure.pprint/pprint event-name)
                  (if (= "Clojure Conj" event-name)
                    (+ attendee-count 10)
                    attendee-count))))

; key renaming

(->> conferences
     (transform [ALL :events ALL MAP-KEYS]
                {:start-date "STARTING"
                 :city       "CITY"
                 :country    "COUNTRY"
                 :attendees  "ATTENDEES"
                 :grants     "GRANTS"
                 :presenters "SPEAKERS"}))

; transform ids using nested lookups maps

(let [id-lookup {"SH" "Stu Halloway"
                 "RH" "Rich Hickey"}
      talks [{:speaker "SH"}
             {:speaker "RH"}]]
  (->> talks
       (transform [ALL :speaker] id-lookup)))

; recursive paths
; https://github.com/redplanetlabs/specter/wiki/Using-Specter-Recursively
; (also supports zippers)

(def NODES                                                  ; visit all nodes in a tree, one at a time
  (recursive-path [] p
                  (if-path map?
                           ; continue-then-stay does post-order/depth first traversal
                           (continue-then-stay [:children ALL] p))))

(def tree {:id       "root"
           :children [{:id "first child"}
                      {:id       "second child"
                       :children [{:id "grandchild"}]}]})

(->> tree
     (select [NODES :id]))

(->> tree
     (transform [NODES]
                (fn [node]
                  (assoc node :name-length (count (:id node))))))

