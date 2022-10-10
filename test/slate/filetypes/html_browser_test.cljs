(ns slate.filetypes.html-browser-test
  (:require-macros [slate.macros :refer [slurp-file]])
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.filetypes.import.html :as html-import]
            [slate.filetypes.export.html :as html-export]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.run :as r :refer [run]]
            [reagent.dom.server :refer [render-to-static-markup]]))

(defn doc=
  "Tests if two docs are equal, disregarding paragraph UUIDs."
  [doc1 doc2]
  (letfn [(strip-uuids [doc]
            (update doc :children
                    (fn [children] (map #(dissoc % :uuid) children))))]
    (= (strip-uuids doc1) (strip-uuids doc2))))

(defn doc-frag=
  [frag1 frag2]
  (= (map #(dissoc % :uuid) (:paragraphs frag1))
     (map #(dissoc % :uuid) (:paragraphs frag2))))

(def test-file1 (slurp-file "test_files/html/the_quiet_universe.html"))
(def test-file2 (slurp-file "test_files/html/conversion_test.html"))
(def paste-tests (let [get-name-and-html (fn [s]
                                           (let [idx (.indexOf s "\n")]
                                             [(keyword (.substr s 0 idx))
                                              (.substr s (inc idx))]))
                       sections (-> (slurp-file "test_files/html/paste_tests.html")
                                    (.split "\n---\n"))]
                   (into {} (map get-name-and-html sections))))

(def test-file1-expected
  (document [(paragraph (random-uuid) :h2 [(run "The Quiet Universe - 2.235674301")])
             (paragraph [(run "Written by Dispatcher 8765, otherwise known as Lamoss, after his exile. This document was a significant factor in the decision to end the Dispatcher program, circa 125112." #{:italic})])
             (paragraph [(run "")])
             (paragraph [(run "My garden is a small world. A few parsecs out from the edge of a hook-shaped cluster of stars too dense for this part of the galaxy, some simple calculations show that its star has spent the last several billion years slowly drifting the opposite direction, towards the outer rim. No doubt that closely-packed cluster was its birthplace. The star would have separated about 1.3 billion years ago, taking with it a fresh planetary nebula, and the first ingredients for life.")])
             (paragraph [(run "\u2003It’s young ground I stand on now, though compared to my own brief flash in the darkness these stones hold an eternity I shudder at. At nighttime, when the orbits line up correctly, the entire dome of the sky will face away from the galaxy, out towards intergalactic space. The Great In Between. Normally I can only look for a few minutes before the dimensions overwhelm me and the vertigo is too great to stand. No species ever encountered by Librarians has dared to bridge ")
                         (run "that" #{:italic})
                         (run " distance.")])
             (paragraph [(run "\u2003Of course, the sky does not look so different than it does any other time. No one on this planet, without my access to an orbital calculator and the Library’s vast starcharts, would know that every point of light in that sky is separate galaxy, and that perhaps somewhere in one of them another observer looks up, looking back at us, a tiny point in the ocean of heaven.")])
             (paragraph [(run "\u2003Besides, there is no one else.")])
             (paragraph [(run "\u2003Here there are no other observers. On this world I am the only thing with a single neuron. But that is not to say this world is empty. At the equator, there are jungles so choked with life that seeds dropped to the forest floor must grow six feet in a day in hopes of reaching a single sunbeam before running out of energy, and those that cannot make it rot, feeding a huge ecosystem of organisms that have never known the light. In the north lie vast tundras so harsh that algae bloom in the snowfall, clinging to the droplets of moisture, living out microscopic lives in a day. In high mountain rivers plants grow beneath thick ice that have not melted for a hundred million years. If real sunlight ever touched them they would die within hours. Everything photosynthesizes, nothing thinks. My thoughts are the only ones that have ever graced this place. Either way, the dance of life is beautiful on this green world, this garden.")])
             (paragraph [(run "\u2003I spend my days mostly in silence, observing. The nearest dispatch beacon is a hundred parsecs away. The vast quantum subnet used to communicate back to the Library is only a distant memory, detectable as a few nodes sending distorted connection requests to any other beacon hiding out in the dark. It’s funny, our attempts to flood the universe with our signals. Information binds societies together, yes (and Librarians ")
                         (run "are" #{:italic})
                         (run " a society, though won’t admit it), but it’s more than that. It’s how we make sense of the universe. Maybe it gives us comfort to fill space with our chatter. But subnets or smoke signals, sooner or later it all fades, entropy returns, information disappears. In the end, this is the mission of the Library: a wall against silence. An attempt to preserve information against all odds. And it is a noble one.")])
             (paragraph [(run "\u2003The question Librarians never dare to ask is: ")
                         (run "Why?" #{:italic})
                         (run " To do so is to ask the stars why they shine. A functioning Librarian knows no other way, any more than the plants around me know how ")
                         (run "not" #{:italic})
                         (run " to collect the sun’s energy. A functioning Librarian cannot see our desperate attempts to guard knowledge are really a pursuit of something different, something far more slippery. ")
                         (run "Meaning." #{:italic})])
             (paragraph [(run "\u2003Librarians do not have to search for meaning. To one of us, the purpose of living is to safeguard Nevernues, the galaxy’s bastion of knowledge. On this unshakeable pillar rests the life of every one of the Library’s servants—save, perhaps, one. I knew from a novice that something in me was unusual. My first assignment was to a team categorizing accounts from natives of a planet incinerated in a supernova. They saw their demise coming; records had survived by way of a probe launched towards the nearest star. Their probe was slow. It could not carry them to safety, only a handful of their thoughts and words, a tiny number of the sum total that would be lost forever.")])
             (paragraph [(run "\u2003I recall the other Librarians never fully understanding the actions of the natives in their last days. They saw only life’s desire for self-preservation, an instinct as natural as it is easily explained. The launch of the probe, with its treasure trove of information, they understood as a last-ditch attempt at survival. Their compulsive desire for new, rare information distracted them from seeing anything else. I took the probe for what it really was: a struggle for meaning in the face of extinction.")])
             (paragraph [(run "\u2003Later I became a Dispatcher, and learned almost all of what there is to know of biological systems. Evolution’s principles are well known to me, and the search for meaning is well explained by them. A primitive life form benefits from intelligence only insofar as the observations of that intelligence have meaning that exceeds the observations of the senses alone. A rustle in the grass might mean the approach of a predator, a familiar smell on the breeze might mean food. It is in our nature—every thinking being from rim to rim—to ask, ")
                         (run "why? " #{:italic})
                         (run "If this is the effect, then what is the cause? By the time metacognition evolves, the pattern of meaning-finding will have been reinforced by a hundred million years of ruthless selection, and the being capable of thinking about itself will naturally seek to find significance in its own existence. It doesn’t matter that the question, ")
                         (run "What is life’s meaning?" #{:italic})
                         (run " is unfounded and senseless. In a conscious mind, it is ")
                         (run "perception" #{:italic})
                         (run "—feeling—that creates the bounds of reality. This property is common to every natural species that has built a civilization.")])
             (paragraph [(run "\u2003The Architects, whoever they truly were (the silence of Nevernues’s own records on the question does not escape me, nor does it that no species is likely to arrive at a position of such incredible power with clean hands), they understood this fact. Created species such as ourselves do not have to grapple with questions of meaning, made as we are to fulfill a specific purpose, outside the confines of natural selection. But I doubt that even the Architects could have fully escaped the trap of designing after themselves. Somewhere deep inside Librarians is a piece of us that desires for something more. In my case, luck (here I remind the reader that luck need not be ")
                         (run "good luck" #{:italic})
                         (run ") must have brought it out.")])
             (paragraph [(run "\u2003With the passing of another billion years, my garden will have moved a further dozen parsecs away from its birth cluster. Another billion after that, it will have drifted into the misty halo of stars outside the edge of the galaxy. Eventually (I have not cared to make the calculations) its star will balloon into a red giant, before cooling and shedding its mass, layer by layer, into space. These are the questions I can answer. Other things about my garden’s future remain uncertain. Will life’s current incarnation continue to thrive? Or will this world be rocked by extinctions, resetting its clock, causing a radically different branch of the tree of life to take root? Maybe animals. Maybe from those animals, intelligence. Or maybe the surface of this planet will remain quiet forever.")])
             (paragraph [(run "\u2003I cannot know. What I can know is that surely more worlds like this exist, untouched by thought. No conscious being will ever step foot on those places, never there will anyone experience the excruciating joy that is to merely ")
                         (run "experience" #{:italic})
                         (run ", to take that small slice of eternity and hold it in one’s mind for a while. Reality is inherently beautiful. But true beauty, like a story, exists only in the presence of an observer—and in a universe so vast, there will never be enough observers.")])
             (paragraph [(run "\u2003Probability tells us that the greatest, most achingly beautiful events will pass by unnoted. Most places—physical and temporal—will never be seen. Consciousness, the universe’s tool for observing itself, is too complex to emerge but rarely. Perhaps that it has emerged at all is the true miracle. Life itself is rare enough. On all but a fraction of the billion trillion worlds that dot the expanse, no eyes will ever rise from the muck. And in all but a sliver of the expanse itself, the universe is nothing but void. Particles separated by miles, dancing against a backdrop of uncaring, neverending blackness.")])
             (paragraph [(run "\u2003I have said that the universe alone is beautiful. Why should it bother me that so few parts of it will ever arrange themselves in such a way as to be capable of recognizing its beauty? Some will—if you are reading these words, you are one of them. Congratulations. But it will never be enough.")])
             (paragraph [(run "\u2003Mostly, the universe is silent.")])
             (paragraph [(run "\u2003At some point, perhaps long after my death, Librarians will find this message. It will be dutifully incorporated into their archives. Generations of Librarians will pour over it, study it, come to think that they understand it. They will say to themselves, ")
                         (run "He should have just come back to Nevernues. What better purpose than to do what we can to preserve what" #{:italic})
                         (run " has ")
                         (run "been seen?" #{:italic})])
             (paragraph [(run "\u2003But my revelation is sealed. I cannot unsee it. In the face of the massive flow of information loss, our tiny damn at Nevernues cannot pretend to stop the current. Staring once again at the thousand galaxies in the night sky, I reflect that here we see a parallel with the search for meaning: it is perception, not logic, that matters in the end.")])]))

(def test-file2-expected
  (document [(paragraph (random-uuid) :h1 [(run "This is an H1")])
             (paragraph (random-uuid) :h2 [(run "This is an H2")])
             (paragraph [(run "")])
             (paragraph [(run "Normal paragraph with a sentence, some ")
                         (run "italics" #{:italic})
                         (run ", ")
                         (run "bold" #{:bold})
                         (run ", and ")
                         (run "strikethrough" #{:strikethrough})
                         (run ".")])
             (paragraph [(run "")])
             (paragraph (random-uuid) :ol [(run "OL 1")])
             (paragraph (random-uuid) :ol [(run "OL 2")])
             (paragraph (random-uuid) :ol [(run "OL 3")])
             (paragraph [(run "")])
             (paragraph (random-uuid) :ul [(run "UL 1")])
             (paragraph (random-uuid) :ul [(run "UL 2")])
             (paragraph (random-uuid) :ul [(run "UL 3")])
             (paragraph [(run "")])
             (paragraph [(run "\u2003And a longer indented paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after.")])]))

(comment
  (html-import/html->droplet test-file2)
  (html-import/html->droplet (:gdocs-complex paste-tests))
  (html-import/html->fragment (:word-online-simple paste-tests))
  (html-import/html->fragment (:word-online-complex paste-tests))
  )

(deftest whole-document-import
  (testing "can import from google docs"
    (is (doc= (html-import/html->doc test-file1) test-file1-expected))
    (is (doc= (html-import/html->doc test-file2) test-file2-expected))))

(deftest paste-import
  (testing "can handle pastes from google docs"
    (is (=
         (html-import/html->fragment (:gdocs-basic-single-style paste-tests))
         (p/fragment [(run "Hello")])))
    (is (=
         (html-import/html->fragment (:gdocs-basic-two-style paste-tests))
         (p/fragment [(run "Hello ") (run "there" #{:bold})])))
    (is (doc-frag=
         (html-import/html->fragment (:gdocs-complex paste-tests))
         (doc/fragment [(paragraph (random-uuid) :h1 [(run "This is an H1")])
                        (paragraph (random-uuid) :h2 [(run "This is an H2")])
                        (paragraph [(run "")])
                        (paragraph [(run "Normal paragraph with a sentence, some ")
                                    (run "italics" #{:italic})
                                    (run ", ")
                                    (run "bold" #{:bold})
                                    (run ", and ")
                                    (run "strikethrough" #{:strikethrough})
                                    (run ".")])
                        (paragraph [(run "")])
                        (paragraph (random-uuid) :ol [(run "OL 1")])
                        (paragraph (random-uuid) :ol [(run "OL 2")])
                        (paragraph (random-uuid) :ol [(run "OL 3")])
                        (paragraph [(run "")])
                        (paragraph (random-uuid) :ul [(run "UL 1")])
                        (paragraph (random-uuid) :ul [(run "UL 2")])
                        (paragraph (random-uuid) :ul [(run "UL 3")])
                        (paragraph [(run "")])
                        (paragraph [(run "\u2003And a longer indented paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after.")])
                        (paragraph [(run "")])]))))
  (testing "can handle pastes from MS Word online"
    (is (= (html-import/html->fragment (:word-online-simple paste-tests))
           (p/fragment [(run "Hello ") (run "there" #{:bold})])))
    #_(is (doc-frag= (html-import/html->fragment (:word-online-complex paste-tests))
                   ;; NOTE: for some reason, MS word online leaves a trailing non-breaking space (nbsp) at the end of each paragraph.
                   ;; It may be worth trimming off the trailing whitespace of any paragraph at some point. For now, this is fine and
                   ;; requires fewer special cases.
                   (doc/fragment [(paragraph (random-uuid) :h1 [(run "This is an H1 ")])
                                  (paragraph (random-uuid) :body [(run "This is an H2 ")])
                                  (paragraph [(run " ")])
                                  (paragraph [(run "Normal paragraph with a sentence, some ")
                                              (run "italics" #{:italic})
                                              (run ", ")
                                              (run "bold" #{:bold})
                                              (run ", and ")
                                              (run "strikethrough" #{:strikethrough})
                                              (run ". ")])
                                  (paragraph [(run " ")])
                                  (paragraph (random-uuid) :ol [(run "OL 1 ")])
                                  (paragraph (random-uuid) :ol [(run "OL 2 ")])
                                  (paragraph (random-uuid) :ol [(run "OL 3 ")])
                                  (paragraph [(run " ")])
                                  (paragraph (random-uuid) :ul [(run "UL 1 ")])
                                  (paragraph (random-uuid) :ul [(run "UL 2 ")])
                                  (paragraph (random-uuid) :ul [(run "UL 3 ")])
                                  (paragraph [(run " ")])
                                  (paragraph [(run "\u2003And a longer indented paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. ")])
                                  (paragraph [(run " ")])])))))

(def export1-expected
  (render-to-static-markup
   [:html
    [:head]
    [:body
     [:h1 {:style html-export/default-p-styles}
      [:span "This is an H1"]]
     [:h2 {:style html-export/default-p-styles}
      [:span "This is an H2"]]
     [:br]
     [:p {:style html-export/default-p-styles}
      [:span "Normal paragraph with a sentence, some "]
      [:span {:style {:font-style "italic"}} "italics"]
      [:span ", "]
      [:span {:style {:font-weight "bold"}} "bold"]
      [:span ", and "]
      [:span {:style {:text-decoration "line-through"}} "strikethrough"]
      [:span "."]]
     [:br]
     [:ol
      [:li {:style html-export/default-p-styles} [:span "OL 1"]]
      [:li {:style html-export/default-p-styles} [:span "OL 2"]]
      [:li {:style html-export/default-p-styles} [:span "OL 3"]]]
     [:br]
     [:ul
      [:li {:style html-export/default-p-styles} [:span "UL 1"]]
      [:li {:style html-export/default-p-styles} [:span "UL 2"]]
      [:li {:style html-export/default-p-styles} [:span "UL 3"]]]
     [:br]
     [:p {:style (merge html-export/default-p-styles {:text-indent "30px"})}
      [:span "And a longer indented paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after."]]]]))

(deftest droplet->html
  (testing "can export whole document"
    (is (= (html-export/doc->html test-file2-expected) export1-expected)))

  (testing "can export fragment"
    (let [fragment1 (p/fragment [(run "foo") (run "bar" #{:italic})])
          fragment1-expected (render-to-static-markup
                              [:<>
                               [:span "foo"]
                               [:span {:style {:font-style "italic"}} "bar"]])
          fragment2 (doc/fragment [(paragraph (random-uuid) :h1 [(run "title")])
                                   (paragraph [(run "foo")])
                                   (paragraph [(run "bar" #{:italic})])])
          fragment2-expected (render-to-static-markup
                              [:<>
                               [:h1 {:style html-export/default-p-styles}
                                [:span "title"]]
                               [:p {:style html-export/default-p-styles}
                                [:span "foo"]]
                               [:p {:style html-export/default-p-styles}
                                [:span {:style {:font-style "italic"}} "bar"]]])]
      (is (= (html-export/fragment->html fragment1) fragment1-expected))
      (is (= (html-export/fragment->html fragment2) fragment2-expected)))))
