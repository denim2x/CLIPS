;;;
; Tic-tac-toe implementation in CLIPS
;;;

(clear)

(deffunction step ()
  (run 2)
)

(deffunction winner (?p ?game)
  (retract ?game)
  (print "Winner: ")
  (if (eq ?p x)
    then (system "wmic useraccount get fullname | findstr /v \"FullName\" | findstr /rvc:\"   \"")
    else (println "Computer")
  )
)

(deffunction draw (?game)
  (retract ?game)
  (println "Draw")
)

(deffunction label (?value ?index)
  (if (eq ?value _) then ?index else ?value)
)

(deftemplate game
  (slot turn (allowed-values x o))
  ;(multislot board (allowed-values _ x o))
  (multislot board)
)

(deffunction retry ()
  (reset)
  
  (assert (game
    (turn x)
    (board 
      _ _ _
      _ _ _
      _ _ _
    )
  ))
  (set-salience-evaluation when-activated)
  (run 1)
)

(deffunction update (?game $?board)
  (retract ?game)
  (assert (game 
    (turn x) 
    (board $?board)
  ))
)

(deffunction patch (?list ?i ?value)
  (replace$ ?list ?i ?i ?value)
)

(defrule prompt
  ?game <- (game 
    (turn x)
    (board $?board)
  )
=>
  (foreach ?m ?board
    (if (= 1 (mod ?m-index 3))
      then (print crlf " ")
    )
    (print " " (label ?m ?m-index))
  )

  (bind ?i 0)
  (bind ?x nil)

  (while (neq _ ?x)
    (print crlf "Your move: ")
    (bind ?i (- (get-char) 48))
    (bind ?x (nth$ ?i ?board))
  )

  (retract ?game)
  (assert (game
    (turn o)
    (board (patch ?board ?i x))
  ))

  ;(run 1)
)

(defrule winner-1 (declare (salience 2))
?game <- (game
  (turn ?p)
  (board 
     ?m&~_ ?m&~_ ?m&~_
    $?
  )
)
=>
  (winner ?m ?game)
)

(defrule winner-2 (declare (salience 2))
?game <- (game
  (turn ?p)
  (board 
     ?     ?     ?
     ?m&~_ ?m&~_ ?m&~_
    $?
  )
)
=>
  (winner ?m ?game)
)

(defrule winner-3 (declare (salience 2))
?game <- (game
  (turn ?p)
  (board 
    $?
     ?m&~_ ?m&~_ ?m&~_
  )
)
=>
  (winner ?m ?game)
)

(defrule winner-4 (declare (salience 2))
?game <- (game
  (turn ?p)
  (board 
    ?m&~_ ? ?
    ?m&~_ ? ?
    ?m&~_ ? ?
  )
)
=>
  (winner ?m ?game)
)

(defrule winner-5 (declare (salience 2))
?game <- (game
  (turn ?p)
  (board 
    ? ?m&~_ ?
    ? ?m&~_ ?
    ? ?m&~_ ?
  )
)
=>
  (winner ?m ?game)
)

(defrule winner-6 (declare (salience 2))
?game <- (game
  (turn ?p)
  (board 
    ? ? ?m&~_
    ? ? ?m&~_
    ? ? ?m&~_
  )
)
=>
  (winner ?m ?game)
)

(defrule winner-7 (declare (salience 2))
?game <- (game
  (turn ?p)
  (board 
    ?m&~_ ?     ?
    ?     ?m&~_ ?
    ?     ?     ?m&~_
  )
)
=>
  (winner ?m ?game)
)

(defrule winner-8 (declare (salience 2))
?game <- (game
  (turn ?p)
  (board 
    ?     ?     ?m&~_
    ?     ?m&~_ ?
    ?m&~_ ?     ?
  )
)
=>
  (winner ?m ?game)
)

(defrule draw (declare (salience 1))
?game <- (game
  (turn ?p)
  (board 
    ~_ ~_ ~_
    ~_ ~_ ~_
    ~_ ~_ ~_
  )
)
=>
  (draw ?game)
)

(defrule attack-1 (declare (salience -1))
?game <- (game
  (turn o)
  (board
      o o _
    $?rest
  )
)
=>
  (update ?game
      o o o 
    $?rest
  )
)

(defrule block-1 (declare (salience -2))
?game <- (game
  (turn o)
  (board
      x x _
    $?rest
  )
)
=>
  (update ?game
      x x o 
    $?rest
  )
)

(defrule attack-2 (declare (salience -1))
?game <- (game
  (turn o)
  (board
      o _ o
    $?rest
  )
)
=>
  (update ?game
      o o o
    $?rest
  )
)

(defrule block-2 (declare (salience -2))
?game <- (game
  (turn o)
  (board
      x _ x
    $?rest
  )
)
=>
  (update ?game
      x o x
    $?rest
  )
)

(defrule attack-3 (declare (salience -1))
?game <- (game
  (turn o)
  (board
      _ o o
    $?rest
  )
)
=>
  (update ?game
      o o o
    $?rest
  )
)

(defrule block-3 (declare (salience -2))
?game <- (game
  (turn o)
  (board
      _ x x
    $?rest
  )
)
=>
  (update ?game
      o x x
    $?rest
  )
)

(defrule attack-4 (declare (salience -1))
?game <- (game
  (turn o)
  (board
     ?a ?b ?c
      o  o  _
    $?rest
  )
)
=>
  (update ?game
     ?a ?b ?c
      o  o  o 
    $?rest
  )
)

(defrule block-4 (declare (salience -2))
?game <- (game
  (turn o)
  (board
     ?a ?b ?c
      x  x  _
    $?rest
  )
)
=>
  (update ?game
     ?a ?b ?c
      x  x  o 
    $?rest
  )
)

(defrule attack-5 (declare (salience -1))
?game <- (game
  (turn o)
  (board
     ?a ?b ?c
      o  _  o
    $?rest
  )
)
=>
  (update ?game
     ?a ?b ?c
      o  o  o
    $?rest
  )
)

(defrule block-5 (declare (salience -2))
?game <- (game
  (turn o)
  (board
     ?a ?b ?c
      x  _  x
    $?rest
  )
)
=>
  (update ?game
     ?a ?b ?c
      x  o  x
    $?rest
  )
)

(defrule attack-6 (declare (salience -1))
?game <- (game
  (turn o)
  (board
     ?a ?b ?c
      _  o  o
    $?rest
  )
)
=>
  (update ?game
     ?a ?b ?c
      o  o  o
    $?rest
  )
)

(defrule block-6 (declare (salience -2))
?game <- (game
  (turn o)
  (board
     ?a ?b ?c
      _  x  x
    $?rest
  )
)
=>
  (update ?game
     ?a ?b ?c
      o  x  x
    $?rest
  )
)

(defrule attack-7 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    $?rest
      o o _
  )
)
=>
  (update ?game
    $?rest
      o o o 
  )
)

(defrule block-7 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    $?rest
      x x _
  )
)
=>
  (update ?game
    $?rest
      x x o 
  )
)

(defrule attack-8 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    $?rest
      o _ o
  )
)
=>
  (update ?game
    $?rest
      o o o
  )
)

(defrule block-8 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    $?rest
      x _ x
  )
)
=>
  (update ?game
    $?rest
      x o x
  )
)

(defrule attack-9 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    $?rest
      _ o o
  )
)
=>
  (update ?game
    $?rest
      o o o
  )
)

(defrule block-9 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    $?rest
      _ x x
  )
)
=>
  (update ?game
    $?rest
      o x x
  )
)

(defrule attack-10 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    o ?a ?b
    o ?c ?d
    _ ?e ?f
  )
)
=>
  (update ?game
    o ?a ?b
    o ?c ?d
    o ?e ?f
  )
)

(defrule block-10 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    x ?a ?b
    x ?c ?d
    _ ?e ?f
  )
)
=>
  (update ?game
    x ?a ?b
    x ?c ?d
    o ?e ?f
  )
)

(defrule attack-11 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    o ?a ?b
    _ ?c ?d
    o ?e ?f
  )
)
=>
  (update ?game
    o ?a ?b
    o ?c ?d
    o ?e ?f
  )
)

(defrule block-11 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    x ?a ?b
    _ ?c ?d
    x ?e ?f
  )
)
=>
  (update ?game
    x ?a ?b
    o ?c ?d
    x ?e ?f
  )
)

(defrule attack-12 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    _ ?a ?b
    o ?c ?d
    o ?e ?f
  )
)
=>
  (update ?game
    o ?a ?b
    o ?c ?d
    o ?e ?f
  )
)

(defrule block-12 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    _ ?a ?b
    x ?c ?d
    x ?e ?f
  )
)
=>
  (update ?game
    o ?a ?b
    x ?c ?d
    x ?e ?f
  )
)

(defrule attack-13 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    ?a o ?b
    ?c o ?d
    ?e _ ?f
  )
)
=>
  (update ?game
    ?a o ?b
    ?c o ?d
    ?e o ?f
  )
)

(defrule block-13 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    ?a x ?b
    ?c x ?d
    ?e _ ?f
  )
)
=>
  (update ?game
    ?a x ?b
    ?c x ?d
    ?e o ?f
  )
)

(defrule attack-14 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    ?a o ?b
    ?c _ ?d
    ?e o ?f
  )
)
=>
  (update ?game
    ?a o ?b
    ?c o ?d
    ?e o ?f
  )
)

(defrule block-14 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    ?a x ?b
    ?c _ ?d
    ?e x ?f
  )
)
=>
  (update ?game
    ?a x ?b
    ?c o ?d
    ?e x ?f
  )
)

(defrule attack-15 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    ?a _ ?b
    ?c o ?d
    ?e o ?f
  )
)
=>
  (update ?game
    ?a o ?b
    ?c o ?d
    ?e o ?f
  )
)

(defrule block-15 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    ?a _ ?b
    ?c x ?d
    ?e x ?f
  )
)
=>
  (update ?game
    ?a o ?b
    ?c x ?d
    ?e x ?f
  )
)

(defrule attack-16 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    ?a ?b o
    ?c ?d o
    ?e ?f _    
  )
)
=>
  (update ?game
    ?a ?b o
    ?c ?d o
    ?e ?f o 
  )
)

(defrule block-16 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    ?a ?b x
    ?c ?d x
    ?e ?f _    
  )
)
=>
  (update ?game
    ?a ?b x
    ?c ?d x
    ?e ?f o 
  )
)

(defrule attack-17 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    ?a ?b o
    ?c ?d _    
    ?e ?f o
  )
)
=>
  (update ?game
    ?a ?b o
    ?c ?d o 
    ?e ?f o
  )
)

(defrule block-17 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    ?a ?b x
    ?c ?d _    
    ?e ?f x
  )
)
=>
  (update ?game
    ?a ?b x
    ?c ?d o 
    ?e ?f x
  )
)

(defrule attack-18 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    ?a ?b _    
    ?c ?d o
    ?e ?f o
  )
)
=>
  (update ?game
    ?a ?b o 
    ?c ?d o
    ?e ?f o
  )
)

(defrule block-18 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    ?a ?b _    
    ?c ?d x
    ?e ?f x
  )
)
=>
  (update ?game
    ?a ?b o 
    ?c ?d x
    ?e ?f x
  )
)

(defrule attack-19 (declare (salience -1))
?game <- (game
  (turn o)
  (board
     o ?a ?b
    ?c  o ?d    
    ?e ?f  _
  )
)
=>
  (update ?game
     o ?a ?b
    ?c  o ?d
    ?e ?f  o 
  )
)

(defrule block-19 (declare (salience -2))
?game <- (game
  (turn o)
  (board
     x ?a ?b
    ?c  x ?d    
    ?e ?f  _
  )
)
=>
  (update ?game
     x ?a ?b
    ?c  x ?d
    ?e ?f  o 
  )
)

(defrule attack-20 (declare (salience -1))
?game <- (game
  (turn o)
  (board
     o ?a ?b
    ?c  _ ?d    
    ?e ?f  o
  )
)
=>
  (update ?game
     o ?a ?b
    ?c  o ?d
    ?e ?f  o
  )
)

(defrule block-20 (declare (salience -2))
?game <- (game
  (turn o)
  (board
     x ?a ?b
    ?c  _ ?d    
    ?e ?f  x
  )
)
=>
  (update ?game
     x ?a ?b
    ?c  o ?d
    ?e ?f  x
  )
)

(defrule attack-21 (declare (salience -1))
?game <- (game
  (turn o)
  (board
     _ ?a ?b
    ?c  o ?d    
    ?e ?f  o
  )
)
=>
  (update ?game
     o ?a ?b
    ?c  o ?d
    ?e ?f  o
  )
)

(defrule block-21 (declare (salience -2))
?game <- (game
  (turn o)
  (board
     _ ?a ?b
    ?c  x ?d    
    ?e ?f  x
  )
)
=>
  (update ?game
     o ?a ?b
    ?c  x ?d
    ?e ?f  x
  )
)

(defrule attack-22 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    ?a ?b  o
    ?c  o ?d    
     _ ?e ?f
  )
)
=>
  (update ?game
    ?a ?b  o
    ?c  o ?d
     o ?e ?f
  )
)

(defrule block-22 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    ?a ?b  x
    ?c  x ?d    
     _ ?e ?f
  )
)
=>
  (update ?game
    ?a ?b  x
    ?c  x ?d
     o ?e ?f
  )
)

(defrule attack-23 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    ?a ?b  o
    ?c  _ ?d    
     o ?e ?f
  )
)
=>
  (update ?game
    ?a ?b  o
    ?c  o ?d
     o ?e ?f
  )
)

(defrule block-23 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    ?a ?b  x
    ?c  _ ?d    
     x ?e ?f
  )
)
=>
  (update ?game
    ?a ?b  x
    ?c  o ?d
     x ?e ?f
  )
)

(defrule attack-24 (declare (salience -1))
?game <- (game
  (turn o)
  (board
    ?a ?b  _
    ?c  o ?d
     o ?e ?f
  )
)
=>
  (update ?game
    ?a ?b  o
    ?c  o ?d
     o ?e ?f
  )
)

(defrule block-24 (declare (salience -2))
?game <- (game
  (turn o)
  (board
    ?a ?b  _
    ?c  x ?d
     x ?e ?f
  )
)
=>
  (update ?game
    ?a ?b  o 
    ?c  x ?d
     x ?e ?f
  )
)

(defrule attack-25 (declare (salience -3))
?game <- (game
  (turn o)
  (board
     _      ?m&~_ ?a
    ?m&~_  $?rest
  )
)
=>
  (update ?game
     o  ?m ?a 
    ?m $?rest
  )
)

(defrule attack-26 (declare (salience -3))
?game <- (game
  (turn o)
  (board
     ?a ?m&~_  _
     ?b ?c    ?m&~_    
    $?rest
  )
)
=>
  (update ?game
     ?a ?m  o 
     ?b ?c ?m
    $?rest
  )
)

(defrule attack-27 (declare (salience -3))
?game <- (game
  (turn o)
  (board
    $?rest    ?m&~_    
     ?a ?m&~_  _
  )
)
=>
  (update ?game
    $?rest ?m    
     ?a ?m  o
  )
)

(defrule attack-28 (declare (salience -3))
?game <- (game
  (turn o)
  (board
    ?m&~_  $?rest
     _      ?m&~_ ?a
  )
)
=>
  (update ?game
    ?m $?rest
     o  ?m ?a 
  )
)

(defrule attack-29 (declare (salience -4))
?game <- (game
  (turn o)
  (board
     ?m&~_ ?a  _
     ?b    ?c ?m&~_
    $?rest
  )
)
=>
  (update ?game
     ?m ?a  o 
     ?b ?c ?m
    $?rest
  )
)

(defrule attack-30 (declare (salience -4))
?game <- (game
  (turn o)
  (board
     ?m&~_ ?a ?b
      _    ?c ?m&~_
    $?rest
  )
)
=>
  (update ?game
     ?m ?a ?b 
      o ?c ?m
    $?rest
  )
)

(defrule attack-31 (declare (salience -4))
?game <- (game
  (turn o)
  (board
      _    ?a ?m&~_
     ?m&~_ ?b
    $?rest
  )
)
=>
  (update ?game
      o ?a ?m
     ?m ?b
    $?rest
  )
)

(defrule attack-32 (declare (salience -4))
?game <- (game
  (turn o)
  (board
     ?a    ?b ?m&~_
     ?m&~_ ?c  _
    $?rest
  )
)
=>
  (update ?game
     ?a ?b ?m
     ?m ?c  o
    $?rest
  )
)

(defrule attack-33 (declare (salience -4))
?game <- (game
  (turn o)
  (board
    $?rest
     ?m&~_ ?a  _
     ?b    ?c ?m&~_
  )
)
=>
  (update ?game
    $?rest
     ?m ?a  o 
     ?b ?c ?m
  )
)

(defrule attack-34 (declare (salience -4))
?game <- (game
  (turn o)
  (board
    $?rest
     ?m&~_ ?a ?b
      _    ?c ?m&~_
  )
)
=>
  (update ?game
    $?rest
     ?m ?a ?b 
      o ?c ?m
  )
)

(defrule attack-35 (declare (salience -4))
?game <- (game
  (turn o)
  (board
    $?rest
      _    ?a ?m&~_
     ?m&~_ ?b ?c
  )
)
=>
  (update ?game
    $?rest
      o ?a ?m
     ?m ?b ?c 
  )
)

(defrule attack-36 (declare (salience -4))
?game <- (game
  (turn o)
  (board
     ?m&~_ _
    $?rest
          ?m&~_ ?a
  )
)
=>
  (update ?game
     ?m o 
    $?rest
       ?m ?a
  )
)

(defrule attack-37 (declare (salience -4))
?game <- (game
  (turn o)
  (board
     ?m&~_
    $?rest
      _ ?m&~_ ?a
  )
)
=>
  (update ?game
     ?m
    $?rest
      o ?m ?a
  )
)

(defrule attack-38 (declare (salience -4))
?game <- (game
  (turn o)
  (board
      _    ?m&~_
    $?rest
     ?m&~_ ?a    ?b
  )
)
=>
  (update ?game
      o ?m
    $?rest
     ?m ?a ?b
  )
)

(defrule attack-39 (declare (salience -4))
?game <- (game
  (turn o)
  (board
     ?a    ?m&~_
    $?rest
     ?m&~_  _    ?b
  )
)
=>
  (update ?game
     ?a ?m
    $?rest
     ?m  o ?b
  )
)

(defrule attack-40 (declare (salience -4))
?game <- (game
  (turn o)
  (board
     ?a ?m&~_  _
    $?rest
              ?m&~_
  )
)
=>
  (update ?game
     ?a ?m o
    $?rest
          ?m
  )
)

(defrule attack-41 (declare (salience -4))
?game <- (game
  (turn o)
  (board
     ?a ?m&~_
    $?rest
         _    ?m&~_
  )
)
=>
  (update ?game
     ?a ?m
    $?rest
         o ?m
  )
)

(defrule attack-42 (declare (salience -4))
?game <- (game
  (turn o)
  (board
     ?a  _    ?m&~_
    $?rest
        ?m&~_ ?b   
  )
)
=>
  (update ?game
     ?a  o ?m
    $?rest
        ?m ?b
  )
)

(defrule attack-43 (declare (salience -4))
?game <- (game
  (turn o)
  (board
     ?a ?b    ?m&~_
    $?rest
        ?m&~_  _   
  )
)
=>
  (update ?game
     ?a ?b ?m
    $?rest
        ?m  o
  )
)

(defrule attack-44 (declare (salience -5))
?game <- (game
  (turn o)
  (board
    ?a ?b ?c
    ?d  _ ?e
    ?f ?g ?h
  )
)
=>
  (update ?game
    ?a ?b ?c
    ?d  o ?e
    ?f ?g ?h
  )
)

(defrule attack-45 (declare (salience -7))
?game <- (game
  (turn o)
  (board
    ?m&~_ ?a ?b
    ?c     _ ?d
    ?e    ?f  _
  )
)
=>
  (update ?game
    ?m ?a ?b
    ?c  _ ?d
    ?e ?f  o
  )
)

(defrule attack-46 (declare (salience -7))
?game <- (game
  (turn o)
  (board
     _ ?a ?b
    ?c  _ ?d
    ?e ?f ?m&~_
  )
)
=>
  (update ?game
     o ?a ?b
    ?c  _ ?d
    ?e ?f ?m
  )
)

(defrule attack-47 (declare (salience -7))
?game <- (game
  (turn o)
  (board
    ?a ?b ?m&~_
    ?c  _ ?d
     _ ?e ?f
  )
)
=>
  (update ?game
    ?a ?b ?m
    ?c  _ ?d
     o ?e ?f
  )
)

(defrule attack-48 (declare (salience -7))
?game <- (game
  (turn o)
  (board
    ?a    ?b  _
    ?c     _ ?d
    ?m&~_ ?e ?f
  )
)
=>
  (update ?game
    ?a ?b  o
    ?c  _ ?d
    ?m ?e ?f
  )
)

(defrule attack-49 (declare (salience -6))
?game <- (game
  (turn o)
  (board
     ?m&~_ _ _
    $?rest
  )
)
=>
  (update ?game
     ?m _ o
    $?rest
  )
)

(defrule attack-50 (declare (salience -6))
?game <- (game
  (turn o)
  (board
      _ _ ?m&~_
    $?rest
  )
)
=>
  (update ?game
      o _ ?m
    $?rest
  )
)

(defrule attack-51 (declare (salience -6))
?game <- (game
  (turn o)
  (board
     ?a    ?b ?c
     ?m&~_  _  _
    $?rest
  )
)
=>
  (update ?game
     ?a ?b ?c
     ?m  _  o
    $?rest
  )
)

(defrule attack-52 (declare (salience -6))
?game <- (game
  (turn o)
  (board
     ?a ?b ?c
      _  _ ?m&~_
    $?rest
  )
)
=>
  (update ?game
     ?a ?b ?c
      o  _ ?m
    $?rest
  )
)

(defrule attack-53 (declare (salience -6))
?game <- (game
  (turn o)
  (board
    $?rest
     ?m&~_ _ _
  )
)
=>
  (update ?game
    $?rest
     ?m _ o
  )
)

(defrule attack-54 (declare (salience -6))
?game <- (game
  (turn o)
  (board
    $?rest
      _ _ ?m&~_
  )
)
=>
  (update ?game
    $?rest
      o _ ?m
  )
)

(defrule attack-55 (declare (salience -6))
?game <- (game
  (turn o)
  (board
    ?m&~_ ?a ?b
     _    ?c ?d
     _    ?e ?f
  )
)
=>
  (update ?game
    ?m ?a ?b
     _ ?c ?d
     o ?e ?f
  )
)

(defrule attack-56 (declare (salience -6))
?game <- (game
  (turn o)
  (board
     _    ?a ?b
     _    ?c ?d
    ?m&~_ ?e ?f
  )
)
=>
  (update ?game
     o ?a ?b
     _ ?c ?d
    ?m ?e ?f
  )
)

(defrule attack-57 (declare (salience -6))
?game <- (game
  (turn o)
  (board
    ?a ?m&~_ ?b
    ?c  _    ?d
    ?e  _    ?f
  )
)
=>
  (update ?game
    ?a ?m ?b
    ?c  _ ?d
    ?e  o ?f
  )
)

(defrule attack-58 (declare (salience -6))
?game <- (game
  (turn o)
  (board
    ?a  _    ?b
    ?c  _    ?d
    ?e ?m&~_ ?f
  )
)
=>
  (update ?game
    ?a  o ?b
    ?c  _ ?d
    ?e ?m ?f
  )
)

(defrule attack-59 (declare (salience -6))
?game <- (game
  (turn o)
  (board
    ?a ?b ?m&~_
    ?c ?d  _
    ?e ?f  _
  )
)
=>
  (update ?game
    ?a ?b ?m
    ?c ?d _
    ?e ?f o
  )
)

(defrule attack-60 (declare (salience -6))
?game <- (game
  (turn o)
  (board
    ?a ?b  _
    ?c ?d  _
    ?e ?f ?m&~_
  )
)
=>
  (update ?game
    ?a ?b  o
    ?c ?d  _
    ?e ?f ?m
  )
)

(defrule attack-61 (declare (salience -8))
?game <- (game
  (turn o)
  (board
     _ ?a ?b
    ?c ?d ?e
    ?f ?g ?h
  )
)
=>
  (update ?game
     o ?a ?b
    ?c ?d ?e
    ?f ?g ?h
  )
)

(defrule attack-62 (declare (salience -8))
?game <- (game
  (turn o)
  (board
    ?a ?b  _
    ?c ?d ?e
    ?f ?g ?h
  )
)
=>
  (update ?game
    ?a ?b  o
    ?c ?d ?e
    ?f ?g ?h
  )
)

(defrule attack-63 (declare (salience -8))
?game <- (game
  (turn o)
  (board
    ?a ?b ?c
    ?d ?e ?f
     _ ?g ?h
  )
)
=>
  (update ?game
    ?a ?b ?c
    ?d ?e ?f
     o ?g ?h
  )
)

(defrule attack-64 (declare (salience -8))
?game <- (game
  (turn o)
  (board
    ?a ?b ?c
    ?d ?e ?f
    ?g ?h  _
  )
)
=>
  (update ?game
    ?a ?b ?c
    ?d ?e ?f
    ?g ?h  o
  )
)

(defrule attack-65 (declare (salience -9))
?game <- (game
  (turn o)
  (board
      o _
    $?rest
  )
)
=>
  (update ?game
      o o 
    $?rest
  )
)

(defrule attack-66 (declare (salience -9))
?game <- (game
  (turn o)
  (board
      _ o
    $?rest
  )
)
=>
  (update ?game
      o o
    $?rest
  )
)

(defrule attack-67 (declare (salience -9))
?game <- (game
  (turn o)
  (board
     ?a o _
    $?rest
  )
)
=>
  (update ?game
     ?a o o
    $?rest
  )
)

(defrule attack-68 (declare (salience -9))
?game <- (game
  (turn o)
  (board
     ?a _ o
    $?rest
  )
)
=>
  (update ?game
     ?a o o
    $?rest
  )
)

(defrule attack-69 (declare (salience -9))
?game <- (game
  (turn o)
  (board
     ?a ?b ?c
      o  _
    $?rest
  )
)
=>
  (update ?game
     ?a ?b ?c
      o  o
    $?rest
  )
)

(defrule attack-70 (declare (salience -9))
?game <- (game
  (turn o)
  (board
     ?a ?b ?c
      _  o
    $?rest
  )
)
=>
  (update ?game
     ?a ?b ?c
      o  o
    $?rest
  )
)

(defrule attack-71 (declare (salience -9))
?game <- (game
  (turn o)
  (board
     ?a ?b ?c
     ?d  o  _
    $?rest
  )
)
=>
  (update ?game
     ?a ?b ?c
     ?d  o  o
    $?rest
  )
)

(defrule attack-72 (declare (salience -9))
?game <- (game
  (turn o)
  (board
     ?a ?b ?c
     ?d  _  o
    $?rest
  )
)
=>
  (update ?game
     ?a ?b ?c
     ?d  o  o
    $?rest
  )
)

(defrule attack-73 (declare (salience -9))
?game <- (game
  (turn o)
  (board
    $?rest
      o _ ?a
  )
)
=>
  (update ?game
    $?rest
      o o ?a
  )
)

(defrule attack-74 (declare (salience -9))
?game <- (game
  (turn o)
  (board
    $?rest
      _ o ?a
  )
)
=>
  (update ?game
    $?rest
      o o ?a
  )
)

(defrule attack-75 (declare (salience -9))
?game <- (game
  (turn o)
  (board
    $?rest
      o _
  )
)
=>
  (update ?game
    $?rest
      o o
  )
)

(defrule attack-76 (declare (salience -9))
?game <- (game
  (turn o)
  (board
    $?rest
      _ o
  )
)
=>
  (update ?game
    $?rest
      o o
  )
)

(defrule attack-77 (declare (salience -9))
?game <- (game
  (turn o)
  (board
      _ ?a ?b
     ?c  o
    $?rest
  )
)
=>
  (update ?game
      o ?a ?b
     ?c  o
    $?rest
  )
)

(defrule attack-78 (declare (salience -9))
?game <- (game
  (turn o)
  (board
    $?rest
         o ?a
     ?b ?c  _
  )
)
=>
  (update ?game
    $?rest
         o ?a
     ?b ?c  o
  )
)

(defrule attack-79 (declare (salience -9))
?game <- (game
  (turn o)
  (board
     ?a ?b  _
     ?c  o
    $?rest
  )
)
=>
  (update ?game
     ?a ?b  o
     ?c  o
    $?rest
  )
)

(defrule attack-80 (declare (salience -9))
?game <- (game
  (turn o)
  (board
    $?rest
         o ?a
      _ ?b ?c
  )
)
=>
  (update ?game
    $?rest
         o ?a
      o ?b ?c
  )
)

(defrule attack-81 (declare (salience -10))
?game <- (game
  (turn o)
  (board
    ?a  _ ?b
    ?c ?d ?e 
    ?f ?g ?h
  )
)
=>
  (update ?game
    ?a  o ?b
    ?c ?d ?e 
    ?f ?g ?h
  )
)

(defrule attack-82 (declare (salience -10))
?game <- (game
  (turn o)
  (board
    ?a ?b ?c
     _ ?d ?e
    ?f ?g ?h
  )
)
=>
  (update ?game
    ?a ?b ?c
     o ?d ?e
    ?f ?g ?h
  )
)

(defrule attack-83 (declare (salience -10))
?game <- (game
  (turn o)
  (board
    ?a ?b ?c
    ?d ?e  _
    ?f ?g ?h
  )
)
=>
  (update ?game
    ?a ?b ?c
    ?d ?e  o
    ?f ?g ?h
  )
)

(defrule attack-84 (declare (salience -10))
?game <- (game
  (turn o)
  (board
    ?a ?b ?c
    ?d ?e ?f
    ?g  _ ?h
  )
)
=>
  (update ?game
    ?a ?b ?c
    ?d ?e ?f
    ?g  o ?h
  )
)

(defglobal ?*seed* = 0)
(foreach ?m (subseq$ (local-time) 1 6)
  (bind ?*seed* (+ ?m ?*seed*))
)

(seed ?*seed*)
(retry)
