(ns figwheel-test.core
  (:require ))

(enable-console-print!)

(println "Edit to this text should show up in your developer console.")

(println (str "1+1=") (+ 1 1))
;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(defn gp [mv0 mv1 s]
  ;dst.vals[0] = 	a*b - A0*B0 + a1*b1 + a2*b2 - A*B*s + a0*b0*s - A1*B1*s - A2*B2*s;
  ;dst.vals[1] =    a0*b - A0*B + a*b0 - A*B0 + A2*b1 + a2*B1 - A1*b2 - a1*B2;
  ;dst.vals[2] =    a1*b - a2*B0 + a*b1 + A0*b2 - A1*B*s - A2*b0*s - A*B1*s + a0*B2*s;
  ;dst.vals[3] =    a2*b + a1*B0 - A0*b1 + a*b2 - A2*B*s + A1*b0*s - a0*B1*s - A*B2*s;
  ;dst.vals[4] =    A0*b + a*B0 - a2*b1 + a1*b2 + a0*B*s + A*b0*s + A2*B1*s - A1*B2*s;
  ;dst.vals[5] =    A1*b + a1*B + a2*b0 - A2*B0 + A*b1 + a*B1 - a0*b2 + A0*B2;
  ;dst.vals[6] =    A2*b + a2*B - a1*b0 + A1*B0 + a0*b1 - A0*B1 + A*b2 + a*B2;
  ;dst.vals[7] =    A*b + a*B + A0*b0 + a0*B0 + A1*b1 + a1*B1 + A2*b2 + a2*B2;

  (let [ {:0 a :1 [a0 a1 a2] :2 [A0 A1 A2] :3 A} mv0
        {:0 b :1 [b0 b1 b2] :2 [B0 B1 B2] :3 B} mv1
        r (+  (* a b)
              (- (* A0 B0))
              (* a1 b1)
              (* a2 b2)
              (* s (+ (* A B)
                      (* a0 b0)
                      (- (* A1 B1))
                      (- (* A2 B2)))))
        r0 (+  (* a0 b) (- (* A0 B) ) a0*b - A0*B + a*b0 - A*B0 + A2*b1 + a2*B1 - A1*b2 - a1*B2;)

    ( println s0 s1))


(defn multivector [s [a b c] [x y z] p]
  (let [result {:0 s :1 [c a b] :2 [z x y] :3 p}]
  (println "hello")
  result)
  )

;(multivector [1 [1 2 3] [4 5 6] 7])
(defn testfn [] (println "hello"))



