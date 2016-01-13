(ns figwheel-test.core
  (:require ))

(enable-console-print!)

;;; (defn gp [mv0 mv1 s]
;;;   ;;dst.vals[0] = 	a*b - A0*B0 + a1*b1 + a2*b2 - A*B*s + a0*b0*s - A1*B1*s - A2*B2*s;
;;;   ;;dst.vals[1] =   a0*b - A0*B + a*b0 - A*B0 + A2*b1 + a2*B1 - A1*b2 - a1*B2;
;;;   ;;dst.vals[2] =   a1*b - a2*B0 + a*b1 + A0*b2 - A1*B*s - A2*b0*s - A*B1*s + a0*B2*s;
;;;   ;;dst.vals[3] =   a2*b + a1*B0 - A0*b1 + a*b2 - A2*B*s + A1*b0*s - a0*B1*s - A*B2*s;
;;;   ;;dst.vals[4] =   A0*b + a*B0 - a2*b1 + a1*b2 + a0*B*s + A*b0*s + A2*B1*s - A1*B2*s;
;;;   ;;dst.vals[5] =   A1*b + a1*B + a2*b0 - A2*B0 + A*b1 + a*B1 - a0*b2 + A0*B2;
;;;   ;;dst.vals[6] =   A2*b + a2*B - a1*b0 + A1*B0 + a0*b1 - A0*B1 + A*b2 + a*B2;
;;;   ;;dst.vals[7] =   A*b + a*B + A0*b0 + a0*B0 + A1*b1 + a1*B1 + A2*b2 + a2*B2;
;;;
;;;   (let [ {:0 a :1 [a0 a1 a2] :2 [A0 A1 A2] :3 A} mv0
;;;         {:0 b :1 [b0 b1 b2] :2 [B0 B1 B2] :3 B} mv1
;;;         r (+  (* a b)
;;;               (- (* A0 B0))
;;;               (* a1 b1)
;;;               (* a2 b2)
;;;               (* s (+ (* A B)
;;;                       (* a0 b0)
;;;                       (- (* A1 B1))
;;;                       (- (* A2 B2)))))
;;;         r0 (+  (* a0 b) (- (* A0 B) ) a0*b - A0*B + a*b0 - A*B0 + A2*b1 + a2*B1 - A1*b2 - a1*B2;)
;;;
;;;                ( println s0 s1))

(defn gp
  ([{[a] :0 [a0 a1 a2] :1 [A0 A1 A2] :2 [A] :3}
     {[b] :0 [b0 b1 b2] :1 [B0 B1 B2] :2 [B] :3}
     s]
  {:0 (+ (* a b) (- (* A0 B0)) (* a1 b1) (* a2 b2)                         ;;dst.vals[0]
         (* s (+ (- (* A B)) (* a0 b0) (- (* A1 B1)) (- (* A2 B2)))))
   :1 [(+ (* a0 b) (- (* A0 B)) (* a b0) (- (* A B0))                      ;;dst.vals[1]
          (* A2 b1) (* a2 B1) (- (* A1 b2)) (- (* a1 B2)))
       (+ (* a1 b) (- (* a2 B0)) (* a b1) (* A0 b2)                        ;;dst.vals[2]
          (* s (+ (- (* A1 B)) (- (* A2 b0)) (- (* A B1)) (* a0 B2))))
       (+ (* a2 b) (* a1 B0) (- (* A0 b1)) (* a b2)                        ;;dst.vals[3]
          (* s (+ (- (* A2 B)) (* A1 b0) (- (* a0 B1)) (- (* A B2)))))]
   :2 [(+ (* A0 b) (* a B0) (- (* a2 b1)) (* a1 b2)                        ;;dst.vals[4]
          (* s (+ (* a0 B) (* A b0) (* A2 B1) (- (* A1 B2)))))
       (+ (* A1 b) (* a1 B) (* a2 b0) (- (* A2 B0))                        ;;dst.vals[5]
          (* A b1) (* a B1) (- (* a0 b2)) (* A0 B2))
       (+ (* A2 b) (* a2 B) (- (* a1 b0)) (* A1 B0)                        ;;dst.vals[6]
          (* a0 b1) (- (* A0 B1)) (* A b2) (* a B2))]
   :3 (+ (* A b) (* a B) (* A0 b0) (* a0 B0)                               ;;dst.vals[7]
         (* A1 b1) (* a1 B1) (* A2 b2) (* a2 B2))})
  ([g1 g2] (gp g1 g2 0)))


;; Alternate way of coding the above.  Not sure if this is better but it's
;; more concise and maybe (??) less error-prone?  The idea is to factor the
;; above expressions into differences of sums of products of pairs of values.

;; sp = "sum of products"
;; takes a vec of vecs, and returns the sum of the products of the elements of each vec
;;  e.g. (sp [[2 3] [4 5] [6 7]]) => (+ (* 2 3) (* 4 5) (* 6 7)) => (+ 6 20 42) => 68
(defn sp [as] (apply + (map #(apply * %) as)))

;; sd = "sum difference":
;; if receives 2 vecs of vecs, return the difference of sp of first and sp of second
;; if receives just one vec of vecs, return sp of that vec
(defn sd
  ([as bs] (- (sp as) (sp bs)))
  ([as] (sp as)))

;;
;; here's the geometric product in terms of the sd function:
;;
(defn gp2
  ([{[a] :0 [a0 a1 a2] :1 [A0 A1 A2] :2 [A] :3}
    {[b] :0 [b0 b1 b2] :1 [B0 B1 B2] :2 [B] :3}
    s]
  {
   :0 (+ (sd [[a b] [a1 b1] [a2 b2]] [[A0 B0]])
         (* s (sd [[a0 b0]] [[A B] [A1 B1] [A2 B2]])))

   :1 [(sd [[a0 b] [a b0] [A2 b1] [a2 B1]] [[A0 B] [A B0] [A1 b2] [a1 B2]])
       (+ (sd [[a1 b] [a b1] [A0 b2]] [[a2 B0]])
          (* s (sd [[a0 B2]] [[A1 B] [A2 b0] [A B1]])))
       (+ (sd [[a2 b] [a1 B0] [a b2]] [[A0 b1]])
          (* s (sd [[A1 b0]] [[A2 B] [a0 B1] [A B2]])))]

   :2 [(+ (sd [[A0 b] [a B0] [a1 b2]] [[a2 b1]])
          (* s (sd [[a0 B] [A b0] [A2 B1]] [[A1 B2]])))
       (sd [[A1 b] [a1 B] [a2 b0] [A b1] [a B1] [A0 B2]] [[A2 B0]  [a0 b2]])
       (sd [[A2 b] [a2 B] [A1 B0] [a0 b1] [A b2] [a B2]] [[a1 b0] [A0 B1]])]

   :3 (sd [[A b] [a B] [A0 b0] [a0 B0] [A1 b1] [a1 B1] [A2 b2] [a2 B2]])
   })
( [ g1 g2 ] (gp2 g1 g2 0)))

(defn zeros? [a]
  (every? zero? a))

(defn gradesAreZero [mv]
  [(zeros? (mv :0))
   (zeros? (mv :1))
   (zeros? (mv :2))
   (zeros? (mv :3))]
  )

; Initialize a multivector.
; Notice that the homogeneous coordinate comes first
; in the multivector format although it is the last
; coordinate in the input coordinates of points & lines
(defn multivector [s [a b c] [x y z] p]
  (let [mv      {:0 [s] :1 [c a b] :2 [z x y] :3 [p]}
        gaz     (gradesAreZero mv)
        kvector (= 1 ((frequencies gaz) false))
        k       (when kvector (first (first (filter #(not (% 1)) (map-indexed vector gaz)))))]
    (assoc mv :gradesAreZero gaz
              :kvector? kvector
              :k k))
  )

; Initialize pure k-vectors for different k
(defn point [x y z]
  (multivector 0 [0 0 0] [x y z] 0))

(defn line [a b c]
  (multivector 0 [a b c] [0 0 0] 0))

(defn scalar [s]
  (multivector s [0 0 0] [0 0 0] 0))

(defn pseudoscalar [p]
  (multivector 0 [0 0 0] [0 0 0] p))

; find orthogonal complement (multiply by pseudoscalar)
(defn polarize [mv s]
  (gp mv (pseudoscalar 1) s))

(defn scalar? [mv]
  (= (mv :gradesAreZero) [false true true true])
  )
(defn line? [mv]
  (= (mv :gradesAreZero) [true false true true])
  )
(defn point? [mv]
  (= (mv :gradesAreZero) [true true false true])
  )
(defn pseudoscalar? [mv]
  (= (mv :gradesAreZero) [true true true false])
  )

; generate some global points and lines for testing
(def p0 (point 0 0 1))
(def p1 (point 1 0 0))
(def p2 (point 0 1 0))
(def p3 (point 2 3 1))

(def m0 (line 0 0 1))
(def m1 (line 1 0 0))
(def m2 (line 0 1 0))
(def m3 (line .6 .8 1))

(def mv1 (multivector 1 [0 0 0] [4 5 6] 7))

;;; ;(multivector [1 [1 2 3] [4 5 6] 7])
;;; (defn testfn [] (println "hello"))
