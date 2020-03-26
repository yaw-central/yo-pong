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

;; global state
(react/register-state ::global-state init-global-state)

;;; =====================
;;; Subscription(s)
;;; =====================

(react/register-subscription
 ::pad1-changed
 (fn [db]
   (::global-state  db))) 

(react/register-subscription
 ::pad2-changed
 (fn [db]
   (::global-state  db))) 

(react/register-subscription
 ::ball-changed
 (fn [db]
   (::global-state  db))) 



;;; ==============
;;; Event handlers
;;; ==============

;;{
;; The :react/frame-update event does 4 actions:
;; * It dispatches an event to move the ball
;; * It dispatches an event to move the right pad
;; * It dispatches an event to move the left pad
;; * It increments ::counter
;; * If the state ::counter is modulo nb-tick, it increases the speed of the ball
;;}
(react/register-event
 :react/frame-update
 (fn [_]
   (let [pad1-action (get (react/read-state ::global-state) :pad1-action)
         pad2-action (get (react/read-state ::global-state) :pad2-action)]
     (do
       (react/update-state ::global-state (fn [old]
                                            (let [c (get-in old [:counter])]
                                              (assoc-in old [:counter] (inc c)))))
       (if (zero? (mod (get (react/read-state ::global-state) :counter) nb-ticks))
        (react/update-state ::global-state (fn [old] 
                                              (let [ball (get-in old [:ball-state])
                                                    x (get-in ball [:delta 0])]
                                                    (if (neg? x)
                                                      (assoc-in old [:ball-state] (assoc-in ball [:delta 0] (Math/max (- MAX-X) (- x S))))
                                                      (assoc-in old [:ball-state] (assoc-in ball [:delta 0] (Math/min MAX-X (+ x S)))))))))
       (react/dispatch [::move-ball])
       (if-not (= pad1-action :nil)
         (react/dispatch [::move-pad1 pad1-action]))
       (if-not (= pad2-action :nil)
         (react/dispatch [::move-pad2 pad2-action]))))))

;;{
;; How to handle the keyboard input:
;; * The pad at the right side move with the up and down arrow
;; * The pad at the left side move with E and D
;;
;; The inputs only change the state "::pad1-action" and "::pad1-action"
;; who can contain :up, :down or :nil
;; When the key to move up and the key to move down are pressed simultanously,
;; the state becomes :nil
;; When pressing the key to move up (resp. move down), the state becomes :up (resp. down)
;; until the key is released
;; The event frame-update move the pads following the states
;;}
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

;;{
;; Event to move the left pad, the handler takes a direction in order
;; to know where to move it
;;}
(react/register-event
 ::move-pad1
 (fn [direction]
   (react/update-state ::global-state (fn [old]
                                        (let [pad1 (get-in old [:pad1-state])]
                                          (assoc-in old [:pad1-state] (move-pad direction pad1)))))))

  ;  ::pad1-state #(move-pad direction %))))

;;{
;; Event to move the right pad, the handler takes a direction in order
;; to know where to move it
;;}
(react/register-event
 ::move-pad2
 (fn [direction]
   (react/update-state ::global-state (fn [old]
                                        (let [pad2 (get-in old [:pad2-state])]
                                         (assoc-in old [:pad2-state] (move-pad direction pad2)))))))

;;{
;; Event to move the ball
;;}
(react/register-event
 ::move-ball
 (fn []
   (react/update-state ::global-state
                       (fn [old]
                        (let [ball (get-in old [:ball-state])]
                         (assoc-in old [:ball-state] (update-ball-state ball)))))
   (let [ball-state (:ball-state (react/read-state ::global-state))]
     (if (= (:pos (:ball-state init-global-state)) (:pos ball-state))
       (react/update-state ::global-state (fn [old] (assoc-in old [:counter] 0)))))))



;;{
;; Event called when the ball collides with a pad
;; The handler takes 3 arguments:
;; * `side` : what side is the object
;; * `part` : what part of the object (if it's precised)
;;}
(react/register-event
 ::ball-collision
 (fn [side part]
  (react/update-state ::global-state (fn [old] 
                                      (let [ball (get-in old [:ball-state])]
                                        (assoc-in old [:ball-state] (modif-ball side part ball)))))))



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


;;================
;; Wall Limit 
;;================
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
    (fn [state id]
      [:group group
       {:pos (:pos (if (= id 1)
                    (:pad1-state @state)
                    (:pad2-state @state)))
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
  [:group :test/ball {:pos (:pos (:ball-state @state))
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


(defn scene [ctrl]
  [:scene
   [:ambient {:color :white :i 0.7}]
   [:sun {:color :red :i 1 :dir [-1 0 0]}]
   [:light ::light {:color :yellow :pos [0.5 0 -4]}]
   (let [pad-state1 (react/subscribe ctrl [::pad1-changed])]
     [the-pad1 pad-state1 1])
   (let [pad-state2 (react/subscribe ctrl [::pad2-changed])]
     [the-pad2 pad-state2 2])
   (let [ball-state (react/subscribe ctrl [::ball-changed])]
     [the-ball ball-state])
   ])

;;; =====================
;;; The main part
;;; =====================

(defn start-pong! []
  (def +myctrl+ (world/start-universe!))
  (react/activate! +myctrl+ [scene +myctrl+]))
