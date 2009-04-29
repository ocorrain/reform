(in-package #:reform)

(hunchentoot:define-easy-handler (mick :uri "/michaelmcnamara")
    ()
  (with-standard-page (:title "Michael McNamara - Candidate for the European elections")
    ((:div :class "candidate-display")
     ((:div :class "span-16")
      (:h2 "Michael McNamara - Candidate for the European elections")
      (:h3 "Biography")
      (:blockquote "Michael McNamara is a Clare-based farmer and practising barrister."
		   (:br)
		   "He has studied at University College Cork,
     the Catholic University of Louvain and the King's Inns, Dublin
     and previously worked for RT&Eacute;, the OSCE and the UN and has
     also worked as a consultant on EU democratisation programmes in
     the Middle East and South Asia."))
     ((:div :class "span-8 last")
      (:img :src "/images/candidates/mmn.jpg"))
     ((:div :class "span-24 last")
      (:h2 "Policies"))
     ((:div :class "span-11 colborder")
     
      (:h3 "Local government")
      (:ul (:li "Accountability")
	   (:li "Sustainability")))
     ((:div :class "span-12 last")
      (:h3 "European Union")
      (:ul (:li "Institutional reform")
	   (:li "Financial regulation")
	   (:li "Fair application of EU law")
	   (:li "Regionalisation")))
     ((:div :class "span-24 last")
      (:blockquote "The following articles from reform.ie are by Michael McNamara and outline his policy positions in more detail:"
	  (fmt "~{~A~^, ~}." (mapcar (lambda (article)
				       (html ((:a :href (get-url article)) (str (get-headline article)))))
				     (remove-if-not (lambda (article)
						      (string-equal (get-author article) "Michael McNamara"))
						    (get-in-tag-order 'article)))))))))





