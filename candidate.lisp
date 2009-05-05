(in-package #:reform)

(hunchentoot:define-easy-handler (mick :uri "/michaelmcnamara")
    ()
  (with-standard-page (:title "Michael McNamara - Candidate for the European elections")
    ((:div :class "candidate-display")
     
     ((:div :class "span-24 last")
      (:h2 "Michael McNamara - Candidate for the European elections")
      (:h2 "Policies"))

     ((:div :class "span-11 colborder")

      ((:h3 :class "alt") "Agriculture") 
      (:ul (:li "EU Law such as the Nitrates directive and Animal
By-Products Regulations must be applied nationally in a manner that no longer unfairly hampers
Irish farmers."))

      ((:h3 :class "alt") "Institutional Reform") 
      (:ul (:li "The Lisbon Treaty is an important and welcome step in the
reform of the European Union"))
      ((:h3 :class "alt") "Financial Regulation") 
      (:ul (:li "Only pan-national financial regulation in an EU-wide
context can restore the credibility of the Irish financial system"))
      ((:h3 :class "alt") "Immigration and Asylum") 

 

      (:ul (:li "Immigrants and their children are now part of the Irish
nation. It is time for a modern, transparent and humane immigration
system to be put in place in Ireland, as is provided for in EU Law and
the Common European Asylum System."))


      
 
      ;; (:h3 "Local government")
      ;; (:ul (:li "Accountability")
      ;; 	   (:li "Sustainability"))
      )
     ((:div :class "span-12 last")
      ((:h3 :class "alt") "Regionalisation") 

                         
      (:ul (:li "We need to replace our current fragmented, ineffective and
undemocratic local government with strong democratic regional and
municipal government that can provide leadership locally and represent
us in Europe, where Structural Funds are dispersed regionally.")
	   (:li "Support for SMEs is provided through the European Regional
Development Fund (ERDF) and European Social Fund (ESF) and we need our
MEPs and local government representatives actively seeking such
support for Western and Northern firms.")
	   (:li "Broadband access should be guaranteed by EU Law to all Western
and Northern homes and businesses.")
	   (:li "A public service obligation or PSO needs to be put in place to
ensure permanent Shannon connectivity")
	   (:li "All politicians must be accountable to those who elect them and
must govern in their name, especially local politicians, who are not
now accountable for local government as, though elected, they are
powerless.")
	   (:li "The current level of bureaucracy and duplication in local
government is no longer sustainable."))

      ;; (:h3 "European Union")
      ;;       (:ul (:li "Institutional reform")
      ;; 	   (:li "Financial regulation")
      ;; 	   (:li "Fair application of EU law")
      ;; 	   (:li "Regionalisation"))
      )
     ((:div :class "span-16")

      (:h3 "Biography")
      (:blockquote "Michael McNamara is a Clare-based farmer and practising barrister."
		   (:br)
		   "He has studied at University College Cork,
     the Catholic University of Louvain,  Teagasc, and the King's Inns, Dublin
     and previously worked for RT&Eacute;, the OSCE and the UN and has
     also worked as a consultant on EU democratisation programmes in
     the Middle East and South Asia."
		   (:br)
		   "He has been called to the Bar of Ireland and
		   Northern Ireland and is a member of the Irish
		   Martime Law Association and the Irish Society of
		   International Law, as well as the Irish Farmers'
		   Association."))
     ((:div :class "span-8 last")
      (:img :src (get-random-candidate-image)))
     ((:div :class "span-24 last")
      (:blockquote "The following articles from reform.ie are by Michael McNamara and outline his policy positions in more detail:"
		   (fmt "~{~A~^, ~}." (mapcar (lambda (article)
						(html ((:a :href (get-url article)) (str (get-headline article)))))
					      (remove-if-not (lambda (article)
							       (string-equal (get-author article) "Michael McNamara"))
							     (get-in-tag-order 'article)))))))))


(hunchentoot:define-easy-handler (mc :uri "/mcnamara")
    ()
  (mick))



(defun get-random-candidate-image ()
  (format nil "/images/candidates/mcnamara~A.jpg" (1+ (random 5))))

