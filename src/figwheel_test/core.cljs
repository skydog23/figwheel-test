(ns figwheel-test.core
  (:require ))

(enable-console-print!)

(defn zeros? [a]
  (every? zero? a))

(defn gradesAreZero [mv]
  [(zeros? (mv :0))
   (zeros? (mv :1))
   (zeros? (mv :2))
   (zeros? (mv :3))]
  )

; Initialize a multivector.
(defn multivector_native [[s] [a b c] [x y z] [p]]
  (let [mv      {:0 [s] :1 [a b c] :2 [x y z] :3 [p]}
        gaz     (gradesAreZero mv)
        kvector (= 1 ((frequencies gaz) false))
        k       (when kvector (first (first (filter #(not (% 1)) (map-indexed vector gaz)))))]
    (assoc mv :gradesAreZero gaz
              :kvector? kvector
              :k k
             ))
  )

; A constructor which accepts (x, y, z) coordinates
; and converts to native (z,x,y) coordinates,
; and scalar and pseudoscalar are not vectors yet.
; Notice that the homogeneous coordinate comes first
; in the native multivector format although it is the last
; coordinate in the input coordinates of points & lines

(defn multivector [s [a b c] [x y z] p]
  (multivector_native [s] [c a b] [z x y] [p]))

; Initialize pure k-vectors for different k
(defn point [x y z]
  (multivector 0 [0 0 0] [x y z] 0))

(defn line [a b c]
  (multivector 0 [a b c] [0 0 0] 0))

(defn scalar [s]
  (multivector s [0 0 0] [0 0 0] 0))

(defn pseudoscalar [p]
  (multivector 0 [0 0 0] [0 0 0] p))

; extract the various grades (in (x,y,z) coordinate order)
; either as real values or as 3-vectors
(defn scalarFrom [mv]
  (get (mv :0) 0))

(defn pseudoscalarFrom [mv]
  (get (mv :3) 0))

(defn lineFrom [mv]
  (let [[c a b] (mv :1)]
    [a b c]))

(defn pointFrom [mv]
  (let [[z x y] (mv :2)]
   [x y z]))

; predicates to identify different k-vectors
(defn scalar? [mv]
  (= (mv :k) 0)
  )
(defn line? [mv]
  (= (mv :k) 1)
  )
(defn point? [mv]
  (= (mv :k) 2)
  )
(defn pseudoscalar? [mv]
  (= (mv :k) 3)
  )
(defn evenSubalg? [mv]
  (and (get (mv :gradesAreZero) 1)
       (get (mv :gradesAreZero) 3)))

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
; calculate the geometric product of two multivectors
(defn gp
  ([{[a] :0 [a0 a1 a2] :1 [A0 A1 A2] :2 [A] :3}
     {[b] :0 [b0 b1 b2] :1 [B0 B1 B2] :2 [B] :3}
     s]
   (multivector_native
     [(+ (* a b) (- (* A0 B0)) (* a1 b1) (* a2 b2)                         ;;dst.vals[0]
         (* s (+ (- (* A B)) (* a0 b0) (- (* A1 B1))
                 (- (* A2 B2)))))]
     [(+ (* a0 b) (- (* A0 B)) (* a b0) (- (* A B0))                      ;;dst.vals[1]
          (* A2 b1) (* a2 B1) (- (* A1 b2)) (- (* a1 B2)))
       (+ (* a1 b) (- (* a2 B0)) (* a b1) (* A0 b2)                        ;;dst.vals[2]
          (* s (+ (- (* A1 B)) (- (* A2 b0)) (- (* A B1)) (* a0 B2))))
       (+ (* a2 b) (* a1 B0) (- (* A0 b1)) (* a b2)                        ;;dst.vals[3]
          (* s (+ (- (* A2 B)) (* A1 b0) (- (* a0 B1)) (- (* A B2)))))]
     [(+ (* A0 b) (* a B0) (- (* a2 b1)) (* a1 b2)                        ;;dst.vals[4]
          (* s (+ (* a0 B) (* A b0) (* A2 B1) (- (* A1 B2)))))
       (+ (* A1 b) (* a1 B) (* a2 b0) (- (* A2 B0))                        ;;dst.vals[5]
          (* A b1) (* a B1) (- (* a0 b2)) (* A0 B2))
       (+ (* A2 b) (* a2 B) (- (* a1 b0)) (* A1 B0)                        ;;dst.vals[6]
          (* a0 b1) (- (* A0 B1)) (* A b2) (* a B2))]
     [(+ (* A b) (* a B) (* A0 b0) (* a0 B0)                               ;;dst.vals[7]
         (* A1 b1) (* a1 B1) (* A2 b2) (* a2 B2))]))
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
   (multivector_native
   [(+ (sd [[a b] [a1 b1] [a2 b2]] [[A0 B0]])
         (* s (sd [[a0 b0]] [[A B] [A1 B1] [A2 B2]])))]

   [(sd [[a0 b] [a b0] [A2 b1] [a2 B1]] [[A0 B] [A B0] [A1 b2] [a1 B2]])
       (+ (sd [[a1 b] [a b1] [A0 b2]] [[a2 B0]])
          (* s (sd [[a0 B2]] [[A1 B] [A2 b0] [A B1]])))
       (+ (sd [[a2 b] [a1 B0] [a b2]] [[A0 b1]])
          (* s (sd [[A1 b0]] [[A2 B] [a0 B1] [A B2]])))]

   [(+ (sd [[A0 b] [a B0] [a1 b2]] [[a2 b1]])
          (* s (sd [[a0 B] [A b0] [A2 B1]] [[A1 B2]])))
       (sd [[A1 b] [a1 B] [a2 b0] [A b1] [a B1] [A0 B2]] [[A2 B0]  [a0 b2]])
       (sd [[A2 b] [a2 B] [A1 B0] [a0 b1] [A b2] [a B2]] [[a1 b0] [A0 B1]])]

   [(sd [[A b] [a B] [A0 b0] [a0 B0] [A1 b1] [a1 B1] [A2 b2] [a2 B2]])]))
( [ g1 g2 ] (gp2 g1 g2 0)))

; calculate highest-grade part of geometric product (incidence only)
(defn wedge [{[a] :0 [a0 a1 a2] :1 [A0 A1 A2] :2 [A] :3}
             {[b] :0 [b0 b1 b2] :1 [B0 B1 B2] :2 [B] :3}]
  ;a*b;
  ;dst.vals[1] = a0*b + a*b0;
  ;dst.vals[2] = a1*b + a*b1;
  ;dst.vals[3] = a2*b + a*b2;
  ;dst.vals[4] = A0*b + a*B0 - a2*b1 + a1*b2;
  ;dst.vals[5] = A1*b + a2*b0 + a*B1 - a0*b2;
  ;dst.vals[6] = A2*b - a1*b0 + a0*b1 + a*B2;
  ;dst.vals[7] = A*b + a*B + A0*b0 + a0*B0 + A1*b1 + a1*B1 + A2*b2 + a2*B2;
  (let [
        r (* a b)
        r0 (+ (* a0 b) (* a b0))
        r1 (+ (* a1 b) (* a b1))
        r2 (+ (* a2 b) (* a b2))
        R0 (+ (* A0 b) (* a B0) (- (* a2 b1)) (* a1 b2))
        R1 (+ (* A1 b) (* a2 b0) (* a B1) (- (* a0 b2)))
        R2 (+ (* A2 b) (- (* a1 b0)) (* a0 b1) (* a B2))
        R (+ (* A b) (* a B) (* A0 b0) (* a0 B0) (* A1 b1) (* A2 b2) (* a2 B2))
        ]
    (multivector_native [r] [r0 r1 r2] [R0 R1 R2] [R]))
  )

; calculate dual coordinates: an entity's coordinates in the dual grassmann alg.
(defn dual [mv]
  (multivector_native (mv :3) (mv :2) (mv :1) (mv :0)))

; calculate join using dual coordinates
(defn join [mv1 mv2]
  (dual (wedge (dual mv1) (dual mv2))))

; add two multivectors
(defn add [{[a] :0 lineA :1 pointA :2 [A] :3}
             {[b] :0 lineB :1 pointB :2 [B] :3}]
  (multivector_native [(+ a b)]
                      (vec (map + lineA lineB))
                      (vec (map + pointA pointB))
                      [(+ A B)]))

; multiply a vector by a scalar (isn't there an easier way?)
(defn smultiplier [s]
  (fn [v] (map #(* s %) v)))

; multiply a multivector by a scalar
(defn times [mv s]
  (let [smul (smultiplier s)]
    (multivector_native (smul (mv :0)) (smul (mv :1))
                        (smul (mv :2)) (smul (mv :3)))))

; this is needed to "reverse" a multivector
(def reversesigns [1,1,1,1,-1,-1,-1,-1])

; reverse the order of all products of 1-vectors in a mv ('reverse' is already taken)
(defn gareverse [{[s] :0 [c a b] :1 [z x y] :2 [p] :3}]
  (multivector s [a b c] (map - [x y z]) (- p)))
;;
;;  Here begins the metric code
;;
;;
; find orthogonal complement (multiply by pseudoscalar)
(defn polarize [mv s]
  (gp mv (pseudoscalar 1) s))

(defn idealInnerPro [v0 v1]  ;; positive definite ip
  (apply + (map * v0 v1)))

;; private function to decide whether the k-vector is ideal (null vector)
;; only allow 1- and 2-vectors
(defn ideal? [mv nv]
  (and (mv :kvector?)
       (and (= nv 0)
            (or (line? mv)
                (point? mv)
                )
            )
       )
  )

(def gradekeys [:0 :1 :2 :3])

; return the norm squared of a k-vector, or else nil
(defn normsquared [mv]
    (when (mv :kvector?)
      (let [n2 (get ((gp2 mv (gareverse mv)) :0) 0)] ;; scalar part of gp
        (if (ideal? mv n2)
          (let [v (mv (get gradekeys (mv :k)))]
            (idealInnerPro v v))
          n2
          )
        )
      )
    )

; normalize a k-vector to have norm 1 (when possible) or else return it unchanged
(defn normalize [mv]
  (let [ n2 (normsquared mv)]
  (when (not= n2 nil)
    (times mv (/ 1.0 (Math.sqrt (Math.abs n2)))))))

; invert a k-vector
(defn invert [mv]
  (when (mv :kvector?)
    (let [n2 (normsquared mv)]
      (when (and (not= nil n2) (not= 0 n2))
        (times mv (/ 1.0 n2))
        )
      )
    )
  )

;;
;; isometry code
;;
(defn reflector [mirror]
  (when (and (mirror :kvector?) (= (mirror :k) 1))
    (fn [X] (gp (gp mirror X) mirror))
    )
  )

; generate some global points and lines for testing
(def p0 (point 0 0 1))
(def p1 (point 1 0 0))
(def p2 (point 0 1 0))
(def v0 (point 2 3 0))

(def m0 (line 0 0 1))
(def p3 (point 2 3 1))
(def m1 (line 1 0 0))
(def m2 (line 0 1 0))
(def m3 (line .6 .8 1))

(def mv1 (multivector 1 [0 0 0] [4 5 6] 7))

