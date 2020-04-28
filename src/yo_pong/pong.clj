(ns yo-pong.pong
  (:require [clojure.set :as set]
            [yaw.world :as world]
            [yaw-reactive.reaction :as react]
            [yaw-reactive.render :as render]
            [yaw.keyboard :as kbd]))

(def S 0.005)
(def A 0.01)
(def MAX-X 0.15)
(def MAX-Y 0.04)
(def nb-ticks 200)

(defonce +myctrl+ (world/start-universe!))


(declare update-ball-state)
(declare move-pad)
(declare modif-ball)

;;; =====================
;;; The states part
;;; =====================

(def init-global-state
  {:ball-state {:pos [0 0 -5]
                :delta [0.04 0 0]}
   :pad1-state {:pos [3 0 -5]
                :delta [0 0.08 0]}
   :pad2-state {:pos [-3 0 -5]
                :delta [0 0.08 0]}
   :pad1-action :nil
   :pad2-action :nil
   :counter 0})

(react/register-state ::global-state init-global-state)

;;; =====================
;;; Subscription(s)
;;; =====================
(react/register-subscription
 +myctrl+ ::global-state ::pad1-changed
 (fn [global-state]
   (:pos (:pad1-state global-state)))) 

(react/register-subscription
 +myctrl+ ::global-state ::pad2-changed
 (fn [global-state]
   (:pos (:pad2-state global-state))))

(react/register-subscription
 +myctrl+ ::global-state ::ball-changed
 (fn [global-state]
   (:pos (:ball-state global-state))))


;;; ==============
;;; Event handlers
;;; ==============

(react/register-event
 :react/frame-update
 (fn [_ _]
   {:events [[::update-counter] [::move-ball] [::move-pad1] [::move-pad2]]}))


;;{
;; Event to move the ball and reset counter if a player score
;;}
(react/register-event
 ::move-ball ::global-state
 (fn [env]
   (update env ::global-state (fn [global-state]
                                (let [state (assoc global-state :ball-state (update-ball-state (:ball-state global-state)))]
                                  (if (= (:pos (:ball-state init-global-state)) (:pos (:ball-state state)))
                                    (assoc state :counter 0)
                                    state))))))

;;{
;; Event to increment the counter 
;;}
(react/register-event
 ::update-counter ::global-state
 (fn [env]
   (update env ::global-state (fn [global-state]
                                (update-in global-state [:counter] inc)))))


(react/register-event
 ::move-pad1 ::global-state
 (fn [env]
   (let [direction (:pad1-action (::global-state env))]
     (if (= :nil direction) 
       env 
       (update env ::global-state (fn [global-state]
                                    (assoc global-state :pad1-state (move-pad direction (:pad1-state global-state)))))))))                            


(react/register-event
 ::move-pad2 ::global-state
 (fn [env]
   (let [direction (:pad2-action (::global-state env))]
     (if (= :nil direction)
       env
       (update env ::global-state (fn [global-state]
                                    (assoc global-state :pad2-state (move-pad direction (:pad2-state global-state)))))))))                            


;;{
;; Event called when the ball collides with a pad
;; The handler takes 3 arguments:
;; * `side` : what side is the object
;; * `part` : what part of the object (if it's precised)
;;}
(react/register-event
 ::ball-collision ::global-state
 (fn [env side part]
   (update env ::global-state (fn [state]
                                (assoc state :ball-state (modif-ball side part (:ball-state state)))))))
   ;(assoc {} ::global-state 
    ;      (react/update-state ::global-state (fn [state]
     ;                                          (assoc-in state [:ball-state] (modif-ball side part (:ball-state state))))))))


(defn key-check
  [kbd-state [key1 key2]]
  (if (seq (:keysdown kbd-state))
    (let [up (if (key1 (:keysdown kbd-state)) true false)
          down (if (key2 (:keysdown kbd-state)) true false)]
      (cond
        (and up down) :nil
        up :up
        down :down
        :else :nil))
    :nil))

(react/register-event
 :react/key-update ::global-state
 (fn [env kbd-state]
   (let [pad1 (key-check kbd-state [:up :down])
         pad2 (key-check kbd-state [:e :d])]
     (-> env
         (update ::global-state (fn [state] (assoc state :pad1-action pad1 )))
         ;(update ::global-state (fn [state] (assoc state :pad1-state (move-pad pad1 (:pad1-state state)))))
         (update ::global-state (fn [state] (assoc state :pad2-action pad2 )))
         ;(update ::global-state (fn [state] (assoc state :pad2-state (move-pad pad2 (:pad2-state state)))))
         ))))

;;===========
;; Functions
;;===========
(defn move-pad [direction {pos :pos delta :delta}]
  (if (not (= direction :nil))
    (let [op (case direction
               :up +
               :down -)]
      {:pos (mapv op pos delta)
       :delta delta})
    {pos :pos delta :delta}))

(defn modif-ball [side part {pos :pos delta :delta}]
  (let [x (case side
            :right (- (Math/abs (get delta 0)))
            :left (Math/abs (get delta 0)))
        y (case part
            :top (Math/min MAX-Y (+ (get delta 1) A))
            :middle (get delta 1)
            :bottom (Math/max (- MAX-Y) (- (get delta 1) A)))
        z (get delta 2)]
    {:pos pos :delta [x y z]}))


;;============
;; Wall Limit 
;;============
(defn inter? [s1 s2]
  (if (not-empty (set/intersection s1 s2))
    true
    false))

(defn limit-set [comp coord limit key]
  (if (comp coord limit)
    #{key}
    #{}))

(defn bounds-checker [min-x max-x min-y max-y]
  (fn [[x y z]]
    (set/union (limit-set < x min-x :score-right)
               (limit-set > x max-x :score-left)
               (limit-set < y min-y :underflow-wall)
               (limit-set > y max-y :overflow-wall)
               (limit-set not= z -5 :error))))

(def pos-check (bounds-checker (- 8) 8 (+ 1.4 (* 100 (- MAX-Y))) (- (* 100 MAX-Y) 1.4)))


(defn update-ball-state [{pos :pos [dx dy dz] :delta}]
  (let [checks (pos-check pos)]
    (if (empty? checks)
      {:pos (mapv + pos [dx dy dz])
       :delta [dx dy dz]}
      (if (inter? #{:underflow-wall :overflow-wall} checks)
        (let [delta' [dx (- dy) dz]]
          {:pos (mapv + pos delta')
           :delta delta'})
        (if (inter? #{:score-left :error} checks)
          (:ball-state init-global-state)
          (let [pos' (:pos (:ball-state init-global-state))
                [dx' dy' dz'] (:delta (:ball-state init-global-state))
                res {:pos pos' :delta [(- dx') dy' dz']}
                _ (assoc init-global-state :ball-state res)]
            res))))))


;;; =====================
;;; The view part
;;; =====================

(defn mk-pad-kw [prefix id]
  (keyword "test" (str "pad-" prefix "-" id)))

;; (mk-pad-kw "group" 1)
;; => :test/pad-group-1

(defn pad-keywords [id]
  [(mk-pad-kw "group" id)
   (mk-pad-kw "item" id)
   (mk-pad-kw "hitbox-top" id)
   (mk-pad-kw "hitbox-middle" id)
   (mk-pad-kw "hitbox-bottom" id)])

(defn the-pad [id]
  (let [[group item htop hmid hbot] (pad-keywords id)]
    (fn [state]
      [:group group
       {:pos @state
       :rot [0 0 0]
       :scale 1}
       [:item item
        {:mesh :mesh/cuboid
         :pos [0 0 0]
         :rot [0 0 0]
         :mat :red
         :scale 0.3}]
       [:hitbox htop
        {:pos [0 0.6 0]
         :scale 0.6
         :length [1 1 1]}]
       [:hitbox hmid
        {:pos [0 0 0]
         :scale 0.6
         :length [1 1 1]}]
       [:hitbox hbot
        {:pos [0 -0.6 0]
         :scale 0.6
         :length [1 1 1]}]])))

(def the-pad1 (the-pad 1))
(def the-pad2 (the-pad 2))

(defn the-ball
  [state]
  [:group :test/ball {:pos @state
                      :rot [0 0 0]
                      :scale 1}
   [:item :test/box {:mesh :mesh/box
                     :pos [0 0 0]
                     :rot [0 0 0]
                     :mat :yellow
                     :scale 0.2}]
   [:hitbox :test/ball-hitbox {:pos [0 0 0]
                               :scale 0.4
                               :length [1 1 1]}
    [:test/pad-group-1 :test/pad-hitbox-top-1 #(react/dispatch [::ball-collision :right :top])]
    [:test/pad-group-1 :test/pad-hitbox-middle-1 #(react/dispatch [::ball-collision :right :middle])]
    [:test/pad-group-1 :test/pad-hitbox-bottom-1 #(react/dispatch [::ball-collision :right :bottom])]
    [:test/pad-group-2 :test/pad-hitbox-top-2 #(react/dispatch [::ball-collision :left :top])]
    [:test/pad-group-2 :test/pad-hitbox-middle-2 #(react/dispatch [::ball-collision :left :middle])]
    [:test/pad-group-2 :test/pad-hitbox-bottom-2 #(react/dispatch [::ball-collision :left :bottom])]]])



(defn scene []
  [:scene
   [:ambient {:color :white :i 0.7}]
   [:sun {:color :red :i 1 :dir [-1 0 0]}]
   [:light ::light {:color :yellow :pos [0.5 0 -4]}]
   (let [pad1-pos (react/subscribe ::pad1-changed)]
     [the-pad1 pad1-pos])
   (let [pad2-pos (react/subscribe ::pad2-changed)]
     [the-pad2 pad2-pos])
   (let [ball-pos (react/subscribe ::ball-changed)]
     [the-ball ball-pos])])


;;; =====================
;;; The main part
;;; =====================
(defn start-pong! []
  (react/activate! +myctrl+ [scene]))
