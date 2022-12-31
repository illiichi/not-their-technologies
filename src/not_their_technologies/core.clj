(ns not-their-technologies.core
  (:use [overtone.core]))

(connect-external-server "localhost" 57110)

(defmacro reduce->
  [initial f & colls]
  `(reduce ~f ~initial (apply map list ~(vec colls))))
(defmacro rotate-> [snd pos]
  `(let [[snd1# snd2#] ~snd] (rotate2 snd1# snd2# ~pos)))
(defn n-range [min max num]
  (range min max (/ (- max min) num)))
(defmacro switch [trig a b]
  `(let [t# ~trig]
     (~'+ (~'* t# ~a)
          (~'* (~'- 1 t#) ~b))))
(defmacro switch->
  [in b eff] `(switch ~b (-> ~in ~eff) ~in))
(defmacro play [t body]
  `((synth (out 0 (hold ~body ~t 0 FREE)))))

;; 1. every thursday
(play 85
  (let [xs [1800 2320 3020 5020 30 80 90 400 8000]]
    (-> (map #(-> (white-noise)
                  (rlpf  %
                         (- 1 (* (env-gen (env-perc 1 4)
                                          (impulse %3 %2))
                                 (lf-pulse 1/3 %2))))
                  (* (hold (lf-pulse 1/8 0 (lf-noise0:kr 1/8)) 79 0 0)))
             xs
             (n-range 0 1/2 (count xs))
             (iterate #(* % 4/5) 1/4))
        splay
        (* 8)
        tanh
        (switch-> (lf-noise1 1/3) free-verb )
        (rotate-> (lf-noise0 1/2)))))

;; 2. uncalling bell
(play 37
  (-> (map #(let [freq (+ %1 (lin-lin (clip:kr (lf-tri:kr 1/8 %2))
                                      0 1
                                      0 (select:kr (< (lf-noise1:kr 2) 1/2) [100 -200])))]
              (* (sin-osc freq)
                 (env-gen (env-perc 0.05 4) (hold (impulse (+ 1/8 (* 1/32 %2)) %2) 33 0 0))))
           (reductions * 300 (cycle [3/2 5/4]))
           (reverse (n-range 0 3 12)))
      splay
      (* 16)
      (free-verb 0.8 0.2)
      tanh))

;; 3. dispense
(play 50
      (-> (map #(* (sin-osc %1)
                   (lf-pulse (lin-exp (lf-saw:kr 1/48 %2) -1 1 1/4 2)
                             0 1/16))
               (reductions * 500 (cycle [5/4 4/3 6/5]))
               (range 3/2 2 1/24))
          splay
          (hold 48 0 0)
          (* 32 (env-gen (envelope [1 1 0] [(* 3 48) 0] 0)))
          free-verb
          (rotate-> (sin-osc 1))
          tanh))

;; 4. myuncleneighbor
(play 57
  (-> (map #(sin-osc (latch:ar (lin-exp (sin-osc (/ 1 %3))
                                        -1 1
                                        %1 (* 10 %1))
                               (hold (impulse 8.05 %2) 52 0 0)))
           (iterate #(* % 3/2) 100)
           (n-range 0 1 8)
           (range 1 9))
      splay
      (* 8)
      (hold 55 2 0)
      (switch-> (lin-lin (lf-saw [1/55 1/50] 1) -1 1 0 1) (free-verb 1 1))
      tanh))

;; 5. autocatalyst
(play 40
  (-> (local-in 2)
      (+ (bpf (gray-noise) 10 (lin-exp (lf-noise0 4) 0 1 0.01 4)))
      (clip:ar -1 (lin-exp (sin-osc (lin-lin (lf-noise0 1/4) 0 1 1/4 8))
                           -1 1 -1 4))
      (* (lf-pulse 1 0 7/8))
      (rlpf (lin-exp (lf-noise0 (lin-exp (lf-pulse 1 0 3/4) 0 1 1/4 8))
                     0 1100 10000) 0.8)
      (doto local-out)
      (* (lin-lin (sin-osc 8) 0 1 1 10))
      leak-dc
      (hpf 100)
      distort))

;; 6. aimlessly(day)
(play 55
  (-> (map #(* (sin-osc %1) %2)
           (iterate #(* % 3/2) 300)
           (map #(env-gen (envelope [0 1 1 0] [0.01 %1 0.05] -8) (hold (impulse %2 %3) 50 0 0))
                (reductions * 0.1 (cycle [3/2 4/5]))
                (iterate #(* % 4/5) 1)
                (n-range 0 1 12)))
      splay
      (* 12)
      tanh))

;; 7. candy in deep
(play 132
  (let [snd (* (sin-osc [1000 4040])
               (env-gen (env-perc 0.05 0.5) (impulse 4))
               (env-gen (env-perc 0 4) (hold (impulse 1/6) 120 0 0)))]
    (-> snd
        (switch-> (lf-noise0 1/4)
                    (+ (* 2/3 (delay-c snd 1 (lf-noise1
                                              (lin-exp (sin-osc 0.003) -1 1 1e-2 8))))))
        (reduce-> (fn [acc [x y]]
                      (free-verb acc (lin-lin (sin-osc 0.01 x) -1 1 0 1) y))
                    (n-range 0 3 4)
                    [1 1/3 2/3 0.1])
        (reduce-> (fn [acc x] (+ acc (delay-c acc x x)))
                    [2 2 3])
        (rotate-> (sin-osc (lin-exp (sin-osc 1/60) -1 1 0.01 8)))
        (tanh))))

;; 8. kinseijin
(play 42
  (-> (* (sin-osc (* [1.01 0.99]
                     (switch (lf-pulse 1/4 0 1/3)
                             (lin-exp (sin-osc 32) -1 1 100 800)
                             (lin-lin (sin-osc 1/8) -1 1 1000 8000))))
         (switch (lf-pulse 1/12 0 5/6)
                 (env-gen (env-perc 0.05 2) (hold (impulse 1) 32 0 0))
                 (env-gen (env-perc 0.0 0.2) (hold (impulse 16) 32 0 0))))
      (ringz [10 11] 0.001)
      (rotate-> (sin-osc (lin-lin (lf-pulse 1/8) -1 1 1/64 16)))
      (reduce-> (fn [acc x] (+ acc (* (lf-noise0 1/8) (free-verb (pitch-shift acc :pitch-ratio x) x 1)))) [1/4 1/8 1/2])
      (tanh)))

;; 9. rain in the notebook
(play 62 (-> (map #(* (sin-osc %1)
                   (env-gen (env-perc 0.08 0.8) %2))
               (reductions * 120 (cycle [3/2 4/5 8/5 5/4 5/4]))
               (map #(-> (lf-saw:kr 1/32 %)
                         (lin-lin -1 1 1/16 1)
                         (round 1/16)
                         (impulse %)
                         (hold 60 0 0))
                    (rotate 12 (range 0 2 1/12))))
          splay
          (* 15)
          tanh))

;; 10. towards the purpose
(play 61
  (-> (map #(* (sin-osc %1)
               (lf-pulse 16 0 1/16)
               (lf-pulse 1/4 %2 7/8)
               (sin-osc (env-gen (envelope [80 80 (* 4 %1) 180] [0 0.2 0.01] -8)
                                 (impulse %3 %2))))
           (iterate #(* % 3/2) 200)
           (n-range 0 1 8)
           (n-range 1/4 1/8 8))
      splay
      (hold 60 0.1 0)
      (free-verb 0.5 (lf-noise0 4))
      (ringz 10 0.1)
      (* 2)
      (hpf 100)
      tanh))

;; 11. hero
(play 35
      (let [speed (lin-exp (lf-saw:kr -1/32 1/2) -1 1 1e-1 1)]
    (-> (map #(let [gate (hold (impulse speed) 32 0 0)
                    xs   (take % (cycle [3/2 5/4 2/3 6/7 7/6]))
                    freq (demand:kr gate 0 (dseq (reductions * 500 xs) INF))
                    snd  (* (saw (-> freq
                                     (switch-> (env-gen (env-perc 0 (/ 7/8 speed)) gate)
                                               (* (lin-lin (lf-noise1 8) 0 1 0.9 1.1)))))
                            (env-gen (envelope [0 1 1 0] [0.001 (/ 1 speed) 0.1] -8) gate))]
                snd)
             (range 4 12 2))
        splay
        (rotate-> (sin-osc 0.08))
        tanh)))

;; 12. mutual concessions
(play 50
  (-> (map #(let [th  (lin-lin (sin-osc:kr 1/32) 0 1)
                  env (hold (lf-pulse (switch (< (lf-noise0:kr 1) th) 1/4 2) % 1/16) (+ 45 %) 0 0)
                  freq (* 1000 % (env-gen (envelope [0 2 1 1 0.9] [0 0.01 0.1 0.5]) env))]
              (* (sin-osc freq) (lf-saw:kr 1/8 %) env))
           (n-range 1 2 8))
      splay
      (* 16)
      (free-verb 0.1 0.7)
      tanh))

;; 13. ku-chang
(play 54
      (-> (map #(let [gate (hold (* (impulse 2 %2)
                                    (lf-pulse 1/8 %2 (lin-lin (lf-saw:kr 1/24 %2) -1 1 1/8 1))) 50 0 0)]
                  (* (sin-osc %1) (env-gen (env-perc 0.001 0.3) gate)))
               (interleave (reductions * 300 (cycle [5/4 6/5]))
                           (reductions * 900 (cycle [4/5 5/6])))
               (n-range 0 3/2 12))
          splay (* 18) tanh))

;; 14. aimlessly(night)
(play 62
      (-> (map #(let [gate (hold (lag-ud %2 0.01 0.05) 60 0 0)
                      ratio (demand:kr gate 0 (dseq [1 3/2 2/3] INF))]
                  (* (sin-osc (* % ratio)) gate))
               (interleave (iterate #(* % 3/2) 150)
                           (iterate #(* % 4/3) 300))
               (map #(lf-pulse %1 %2 1/8)
                    (range 1/8 1/2 1/32)
                    (n-range 0 3 12)))
          splay
          (* 8)
          free-verb
          tanh))

;; 15. yyyy.mm.dd
(play 35
      (-> (map #(* (sin-osc (* (lin-lin (lf-pulse (lin-exp (lf-noise2:kr 1/8)
                                                           0 1
                                                           1/10
                                                           10))
                                        0 1
                                        %1 (* 4/5 %1))
                               (env-gen (envelope [11/12 1 1 0] [0.04 %3 10] 12)))))
           (iterate #(* % 3/2) 200)
           (n-range 10 20 8))
      splay
      (rotate-> (sin-osc 0.1))
      (free-verb 0.1 0.3)
      (* 8)
      tanh))

;; 16. cheerful engine
(play 51
  (let [c (sin-osc (lin-lin (sin-osc 3.3) -1 1 100 150))
        w (lin-exp (sin-osc:kr 1/32) -1 1 1/16 1/4)
        p (lin-lin (sin-osc:kr 1/4) -1 1 4 32)]
    (-> (map #(sin-osc (* %1 c %2))
             (iterate #(* % 3/2) 200)
             (map #(env-gen (env-perc 0.001 %2) (* (lf-pulse % 1/2 w) (hold (impulse p) 50 0 0)))
                  (iterate #(* % 4/5) 1)
                  (n-range 0.001 0.2 10)))
        splay
        (* 10)
        (leak-dc)
        tanh)))

;; 17. transparent words
(play 70
  (-> [(white-noise) (sin-osc 1000)]
      (reduce-> (fn [acc [x y]] (switch (lf-noise0 y) acc (sin-osc x)))
                  (iterate #(* % 3/2) 100)
                  (n-range 1 4 8))
      (* 2)
      (* (env-gen (env-perc 0.05 (line:kr 1 2 60))
                  (hold (impulse [1/2 (lin-lin (clip (lf-saw:kr 1/32 1)) 0 1 0.5 0.6)] [0 1/2])
                        64 0 0)))
      (free-verb (lin-lin (lf-noise0 1/4) 0 1 0.2 0.6) 0.7)
      tanh))

;; 18 too much things to do
(play 45
      (-> (blip (* [0.99 1.01] (lin-lin (lf-saw:kr 3.3 0) -1 1 15 30)))
          (ringz (+ (lin-exp (lf-tri (duty:kr 8 0 (dseq [12 1 4 0.1] INF)))
                             -1 1
                             500 1200)
                    (lin-exp (lf-pulse (duty:kr 3 0 (dseq [8 1] INF)))
                             0 1
                             -100 400))
                 0.01)
          (* 1.5)
          (hold 40 5 FREE)
          tanh))

;; 19. kinoko
(play 100
      (-> (map #(let [gate   (hold (impulse (+ 1/16 (* % 1/88)) 1/2) 88 0 0)
                      freq   (+ 10 (demand gate 0
                                           (dseq (concat (repeat % 100) [1 10 20]) INF)))
                      f-env  (env-gen (envelope [2 1 3 8] [0.08 0.5 4] 4) gate)
                      rq     (lin-exp (sin-osc %) -1 1 0.1 0.3)
                      env    (env-gen (envelope [0 1 1 0] [0.05 1 5] -4) gate)]
                  (* (rlpf (saw freq) (* %2 f-env) rq) env))
               (range 1 31 3)
               (iterate #(* % 3/2) 100))
          splay
          (reduce-> #(+ %1 (* (lf-noise2 1/2) (delay-c %1 %2 %2)))
                    [1/6 1/4 1/2])
          (* 8)
          tanh))

;; 20. forgiveness
(play 75
      (-> (map #(let [gate  (hold (impulse (+ %1 1/8) 1/2) 64 0 0)
                      f-env (env-gen (envelope [2 1 3 8] [0.08 0.5 7] 4) gate)]
                  (* (rlpf (saw %2) (* %2 f-env) 0.1)
                     (env-gen (envelope [0 1 1 0] [0.05 1 7] -4) gate)))
               (reverse (concat (n-range 1/8 1/4 8)
                                (n-range 0 1/4 4)))
               (iterate #(* % 3/2) 100))
          splay
          (* 8)
          tanh))
