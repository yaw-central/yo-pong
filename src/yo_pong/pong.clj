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
   {:events [[::update-counter] [::move-ball] ]}))

;;{
;; Event to move the ball and reset counter if a player score
;;}
(declare update-ball-state)
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

(declare move-pad)
(react/register-event
 ::move-pad1 ::global-state
 (fn [env]
   (if (= :nil (:pad1-action env))
     env
     (let [direction (:pad1-action env)]
       (update env ::global-state (fn [global-state]
                                    (assoc global-state :pad1-state (move-pad direction (:pad1-state global-state)))))))))                            


(react/register-event
 ::move-pad2 ::global-state
 (fn [env]
   (if (= :nil (:pad2-action env))
     env
     (let [direction (:pad2-action env)]
       (update env ::global-state (fn [global-state]
                                    (assoc global-state :pad2-state (move-pad direction (:pad2-state global-state)))))))))                            



(react/register-event
 :react/key-update
 (fn [kbd-state]
   ;; Pad 1 action (right side)
   (cond
     ;; if we want to move up and down at the same time
     (and (:up (:keysdown kbd-state)) (:down (:keysdown kbd-state))) (react/update-state ::global-state (fn [old] (assoc-in old [:pad1-action] :nil)))
     ;; up-arrow
     (:up (:keysdown kbd-state)) (react/update-state ::global-state (fn [old] (assoc-in old [:pad1-action] :up)))
     ;; down-arrow
     (:down (:keysdown kbd-state)) (react/update-state ::global-state (fn [old] (assoc-in old [:pad1-action] :down)))
     ;; no moves
     :else (react/update-state ::global-state (fn [old] (assoc-in old [:pad1-action] :nil))))
   ;;Pad 2 action (left side)
   (cond
     ;; if we want to move up and down at the same time
     (and (:e (:keysdown kbd-state)) (:d (:keysdown kbd-state))) (react/update-state ::global-state (fn [old] (assoc-in old [:pad2-action] :nil)))
     ;; E key
     (:e (:keysdown kbd-state)) (react/update-state ::global-state (fn [old] (assoc-in old [:pad2-action] :up)))
     ;; D key
     (:d (:keysdown kbd-state)) (react/update-state ::global-state (fn [old] (assoc-in old [:pad2-action] :down)))
     ;; no moves
     :else (react/update-state ::global-state (fn [old] (assoc-in old [:pad2-action] :nil))))))



;;===========
;; Functions
;;===========
(defn move-pad [direction {pos :pos delta :delta}]
  (let [op (case direction
             :up +
             :down -)]
    {:pos (mapv op pos delta)
     :delta delta}))

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
      [:item item
       {:mesh :mesh/cuboid
        :pos @state
        :rot [0 0 0]
        :mat :red
        :scale 0.3}])))

(def the-pad1 (the-pad 1))
(def the-pad2 (the-pad 2))

(defn the-ball
  [state]
  [:item :test/box {:mesh :mesh/box
                    :pos @state
                    :rot [0 0 0]
                    :mat :yellow
                    :scale 0.2}])


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
