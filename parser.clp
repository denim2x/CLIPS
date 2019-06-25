;;;
; A simple GLR parser implementation in CLIPS
;;;

(clear)

(defglobal ?*graph* = "")
(defglobal ?*id* = nil)

(deffunction @view ()
  (system "start \"\" \"http://chart.googleapis.com/chart?cht=gv&chof=png&chs=700x400&chl=digraph{" ?*graph* "}\"")
)

(deffunction @link (?id)
  (if (eq ?id .)
    then (bind ?id "_dot_")
  )
  (bind ?*graph* (str-cat ?*graph* ?id "->" ?*id* ";"))
)

(deffunction @label (?label)
  (bind ?*graph* (str-cat ?*id* "[label=" ?label "];" ?*graph*))
)

(deffunction sym (?value) "Symbol creation"
  (string-to-field ?value)
)

(deffunction % (?value ?i ?c) "Substring (?i = start index, ?c = count)"
  (if (= ?c 0) 
    then "" 
    else 
      (bind ?len (str-length ?value))
      (if (< ?c 0) then (bind ?c (+ ?c ?len)))
      (if (< ?i 0) 
        then (bind ?i (+ ?i (+ 1 ?len)))
        else 
          (if (= ?i 0) 
            then (bind ?i 1) 
          )
      )
      (sub-string ?i (+ ?i (- ?c 1)) ?value)
  )
)

(deffunction : ($?value) "List creation"
  $?value
)

(deffunction ^ (?value $?list) "Membership test"
  (integerp (member$ ?value $?list))
)

(deftemplate # "Node template"
  (slot $ (default-dynamic (gensym*)))   ; key
  (multislot \)                 ; value
)

(deffunction @ (?id $?value) "Shorthand Node constructor"
  (bind ?*id* ?id)
  (assert (# ($ ?id) (\ $?value)))
  ?id
)

(deffunction @$ ($?value) "Shorthand Node constructor"
  (@ (gensym) $?value)
)

(deffacts lexicon
  (nouns dog fox)
  (verbs jumps)
  (prepositions over)
  (articles a an the)
  (adjectives brown lazy quick)
  (puncts .)
)

(defrule noun (declare (salience -4))
  ?Model <- (model $?model)
  ?Input <- (input ?term $?input)
  (nouns $? ?term $?)
=>
  (retract ?Model ?Input)
  (assert (model $?model (@$ noun ?term)) (input $?input))
  (@link ?term)
  (@label "noun")
)

(defrule verb
  ?Model <- (model $?model)
  ?Input <- (input ?term $?input)
  (verbs $? ?term $?)
=>
  (retract ?Model ?Input)
  (assert (model $?model (@$ verb ?term)) (input $?input))
  (@link ?term)
  (@label "verb")
)

(defrule preposition
  ?Model <- (model $?model)
  ?Input <- (input ?term $?input)
  (prepositions $? ?term $?)
=>
  (retract ?Model ?Input)
  (assert (model $?model (@$ preposition ?term)) (input $?input))
  (@link ?term)
  (@label "preposition")
)

(defrule article
  ?Model <- (model $?model)
  ?Input <- (input ?term $?input)
  (articles $? ?term $?)  
=>
  (retract ?Model ?Input)
  (assert (model $?model (@$ article ?term)) (input $?input))
  (@link ?term)
  (@label "article")
)

(defrule adjective
  ?Model <- (model $?model)
  ?Input <- (input ?term $?input)
  (adjectives $? ?term $?)
=>
  (retract ?Model ?Input)
  (assert (model $?model (@$ adjective ?term)) (input $?input))
  (@link ?term)
  (@label "adjective")
)

(defrule punct
  ?Model <- (model $?model)
  ?Input <- (input ?term $?input)
  (puncts $? ?term $?)
=>
  (retract ?Model ?Input)
  (assert (model $?model (@$ punct ?term)) (input $?input))
  (@link ?term)
  (@label "punct")
)

(defrule adjectives-1 (declare (salience 11))
  ?Model <- (model $?model ?term)
  (# ($ ?term) (\ adjective $?))
=>
  (retract ?Model)
  (assert (model $?model (@$ adjectives ?term)))
  (@link ?term)
  (@label "adjectives")
)

(defrule adjectives-2 (declare (salience 12))
  ?Model <- (model $?model ?list ?term)
  ?List <- (# ($ ?list) (\ adjectives $?terms))
  (# ($ ?term) (\ adjective $?))
=>
  (retract ?Model ?List)
  (assert (model $?model (@ ?list adjectives $?terms ?term)))
  (@link ?term)
)

(defrule attribute-1 (declare (salience -3))
  ?Model <- (model $?model ?term)
  (# ($ ?term) (\ article $?))
=>
  (retract ?Model)
  (assert (model $?model (@$ attribute ?term)))
  (@link ?term)
  (@label "attribute")
)

(defrule attribute-2 (declare (salience -2))
  ?Model <- (model $?model ?terms)
  (# ($ ?terms) (\ adjectives $?))
=>
  (retract ?Model)
  (assert (model $?model (@$ attribute ?terms)))
  (@link ?terms)
  (@label "attribute")
)

(defrule attribute-3 (declare (salience -1))
  ?Model <- (model $?model ?term ?terms)
  (# ($ ?term) (\ article $?))
  (# ($ ?terms) (\ adjectives $?))
=>
  (retract ?Model)
  (assert (model $?model (@$ attribute ?term ?terms)))
  (@link ?term)
  (@link ?terms)
  (@label "attribute")
)

(defrule phrase-1 (declare (salience 6))
  ?Model <- (model $?model ?term)
  (# ($ term) (\ noun $?))
=>
  (retract ?Model)
  (assert (model $?model (@$ phrase ?term)))
  (@link ?term)
  (@label "phrase")
)

(defrule phrase-2 (declare (salience 7))
  ?Model <- (model $?model ?attr ?noun)
  (# ($ ?attr) (\ attribute $?))
  (# ($ ?noun) (\ noun $?))
=>
  (retract ?Model)
  (assert (model $?model (@$ phrase ?attr ?noun)))
  (@link ?attr)
  (@link ?noun)
  (@label "phrase")
)

(defrule object-1 (declare (salience 4))
  ?Model <- (model $?model ?phrase)
  (# ($ ?) (\ subject $?))
  (# ($ ?) (\ verb $?))
  (# ($ ?phrase) (\ phrase $?))
=>
  (retract ?Model)
  (assert (model $?model (@$ object ?phrase)))
  (@link ?phrase)
  (@label "object")
)

(defrule object-2 (declare (salience 5))
  ?Model <- (model $?model ?prep ?phrase)
  (# ($ ?) (\ subject $?))
  (# ($ ?) (\ verb $?))
  (# ($ ?prep) (\ preposition $?))
  (# ($ ?phrase) (\ phrase $?))
=>
  (retract ?Model)
  (assert (model $?model (@$ object ?prep ?phrase)))
  (@link ?prep)
  (@link ?phrase)
  (@label "object")
)

(defrule subject (declare (salience 3))
  ?Model <- (model $?model ?phrase)
  (# ($ ?phrase) (\ phrase $?))
=>
  (retract ?Model)
  (assert (model $?model (@$ subject ?phrase)))
  (@link ?phrase)
  (@label "subject")
)

(defrule sentence-1 (declare (salience 1))
  ?Model <- (model $?model ?sub ?verb ?punct)
  (# ($ ?sub) (\ subject $?))
  (# ($ ?verb) (\ verb $?))
  (# ($ ?punct) (\ punct $?))
=>
  (retract ?Model)
  (assert (model $?model (@$ sentence ?sub ?verb ?punct)))
  (@link ?sub)
  (@link ?verb)
  (@link ?punct)
  (@label "sentence")
)

(defrule sentence-2 (declare (salience 2))
  ?Model <- (model $?model ?sub ?verb ?obj ?punct)
  (# ($ ?sub) (\ subject $?))
  (# ($ ?verb) (\ verb $?))
  (# ($ ?obj) (\ object $?))
  (# ($ ?punct) (\ punct $?))
=>
  (retract ?Model)
  (assert (model $?model (@$ sentence ?sub ?verb ?obj ?punct)))
  (@link ?sub)
  (@link ?verb)
  (@link ?obj)
  (@link ?punct)
  (@label "sentence")
)

(defrule valid (declare (salience -5))
  ?Model <- (model ?)
  ?Input <- (input)
=>
  (retract ?Input)
  (println "Done parsing; opening result graph...")
  (@view)
)

(defrule invalid (declare (salience -6))
  ?Model <- (model $?)
  ?Input <- (input $?)
=>
  (println "Invalid input")
)

(reset)

(print "Your sentence: ")
(defglobal ?*input* = (lowcase (readline)))
(assert (model))
(assert (input (explode$ (% ?*input* 1 -1)) (sym (% ?*input* -1 1))))
(undefglobal input)

(run)
