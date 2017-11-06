;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                     Carnegie Mellon University                      ;;;
;;;                  and Alan W Black and Kevin Lenzo                   ;;;
;;;                      Copyright (c) 1998-2000                        ;;;
;;;                        All Rights Reserved.                         ;;;
;;;                                                                     ;;;
;;; Permission is hereby granted, free of charge, to use and distribute ;;;
;;; this software and its documentation without restriction, including  ;;;
;;; without limitation the rights to use, copy, modify, merge, publish, ;;;
;;; distribute, sublicense, and/or sell copies of this work, and to     ;;;
;;; permit persons to whom this work is furnished to do so, subject to  ;;;
;;; the following conditions:                                           ;;;
;;;  1. The code must retain the above copyright notice, this list of   ;;;
;;;     conditions and the following disclaimer.                        ;;;
;;;  2. Any modifications must be clearly marked as such.               ;;;
;;;  3. Original authors' names are not deleted.                        ;;;
;;;  4. The authors' names are not used to endorse or promote products  ;;;
;;;     derived from this software without specific prior written       ;;;
;;;     permission.                                                     ;;;
;;;                                                                     ;;;
;;; CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK        ;;;
;;; DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING     ;;;
;;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT  ;;;
;;; SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE     ;;;
;;; FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   ;;;
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  ;;;
;;; AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,         ;;;
;;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      ;;;
;;; THIS SOFTWARE.                                                      ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lexicon, LTS and Postlexical rules for ssn_hts
;;;

;;; Load any necessary files here

(define (ssn_hts_addenda)
  "(ssn_hts_addenda)
Basic lexicon should (must ?) have basic letters, symbols and punctuation."

;;; Pronunciation of letters in the alphabet
;(lex.add.entry '("a" nn (((a) 0))))
;(lex.add.entry '("b" nn (((b e) 0))))
;(lex.add.entry '("c" nn (((th e) 0))))
;(lex.add.entry '("d" nn (((d e) 0))))
;(lex.add.entry '("e" nn (((e) 0))))
; ...

;;; Symbols ...
;(lex.add.entry 
; '("*" n (((a s) 0) ((t e) 0) ((r i1 s) 1)  ((k o) 0))))
;(lex.add.entry 
; '("%" n (((p o r) 0) ((th i e1 n) 1) ((t o) 0))))

;; Basic punctuation must be in with nil pronunciation
(lex.add.entry '("," punc nil))
(lex.add.entry '("." punc nil))
;(lex.add.entry '("." nn (((p u1 n) 1) ((t o) 0))))
(lex.add.entry '("'" punc nil))
(lex.add.entry '(":" punc nil))
(lex.add.entry '(";" punc nil))
;;(lex.add.entry '("," punc nil))
;(lex.add.entry '("," nn (((k o1) 1) ((m a) 0))))
(lex.add.entry '("-" punc nil))
(lex.add.entry '("\"" punc nil))
(lex.add.entry '("`" punc nil))
(lex.add.entry '("?" punc nil))
(lex.add.entry '("!" punc nil))
)

(require 'lts)

;;;  Function called when word not found in lexicon
;;;  and you've trained letter to sound rules
(define (ssn_hts_lts_function word features)
  "(ssn_hts_lts_function WORD FEATURES)
Return pronunciation of word not in lexicon."
  (if (not boundp 'ssn_hts_lts_rules)
      (require 'ssn_hts_lts_rules))
  (let ((dword (downcase word)) (phones) (syls))
    (set! phones (lts_predict dword ssn_hts_lts_rules))
    (set! syls (ssn_hts_lex_syllabify_phstress phones))
    (list word features syls)))

;; utf8 letter based one
;(define (ssn_hts_lts_function word features)
;  "(ssn_hts_lts_function WORD FEATURES)
;Return pronunciation of word not in lexicon."
;  (let ((dword word) (phones) (syls))
;    (set! phones (utf8explode dword))
;    (set! syls (ssn_hts_lex_syllabify_phstress phones))
;    (list word features syls)))

(define (ssn_hts_is_vowel x)
  (string-equal "+" (phone_feature x "vc")))

(define (ssn_hts_contains_vowel l)
  (member_string
   t
   (mapcar (lambda (x) (ssn_hts_is_vowel x)) l)))

(define (ssn_hts_lex_sylbreak currentsyl remainder)
  "(ssn_hts_lex_sylbreak currentsyl remainder)
t if this is a syl break, nil otherwise."
  (cond
   ((not (ssn_hts_contains_vowel remainder))
    nil)
   ((not (ssn_hts_contains_vowel currentsyl))
    nil)
   (t
    ;; overly naive, I mean wrong
    t))
)

(define (ssn_hts_lex_syllabify_phstress phones)
 (let ((syl nil) (syls nil) (p phones) (stress 0))
    (while p
     (set! syl nil)
     (set! stress 0)
     (while (and p (not (ssn_hts_lex_sylbreak syl p)))
       (if (string-matches (car p) "xxxx")
           (begin
             ;; whatever you do to identify stress
             (set! stress 1)
             (set syl (cons (car p-stress) syl)))
           (set! syl (cons (car p) syl)))
       (set! p (cdr p)))
     (set! syls (cons (list (reverse syl) stress) syls)))
    (reverse syls)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR: Hand written letter to sound rules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (ssn_hts_lts_function word features)
   "(ssn_hts_lts_function WORD FEATURES)
 Return pronunciation of word not in lexicon."

   (cond
   ((string-equal "" word )())
   ((string-equal "LSIL" word )(set! wordstruct '( (("LSIL") 0)  ))(list word nil wordstruct))
   ((string-equal "SSIL" word )(set! wordstruct '( (("SSIL") 0)  ))(list word nil wordstruct))
   ((string-equal "mono" word )(set! myfilepointer (fopen "unit_size.sh" "w"))(format myfilepointer "%s" "mono")(fclose myfilepointer))
   ((string-equal word "phone")(set! myfilepointer (fopen "unit_size.sh" "w"))(format myfilepointer "%s" "phone")(fclose myfilepointer))
   (t
   (set! myfilepointer (fopen (path-append ssn_hts_demo::dir "toword") "w"))
(format myfilepointer word)
(fclose myfilepointer)
(system "perl scripts/word_pronunciation.pl lists/phoneset_all lists/phoneset_uyir lists/phoneset_mei lists/phoneset_uyirmei toword .")
                 (load (path-append ssn_hts_demo::dir "wordpronunciation"))                  
                 (load (path-append ssn_hts_demo::dir "morph_tag1"))
                                                                             
 (list word a wordstruct)))

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Postlexical Rules 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ssn_hts::postlex_rule1 utt)
  "(ssn_hts::postlex_rule1 utt)
A postlexical rule form correcting phenomena over word boundaries."
  (mapcar
   (lambda (s)
     ;; do something
     )
   (utt.relation.items utt 'Segment))
   utt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lexicon definition
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lex.create "ssn_hts")
(lex.set.phoneset "ssn_hts")
(lex.set.lts.method 'ssn_hts_lts_function)
(if (probe_file (path-append ssn_hts_demo::dir "festvox/sied_ae.out"))
    (lex.set.compile.file (path-append ssn_hts_demo::dir 
                                       "festvox/sied_ae.out")))
(ssn_hts_addenda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lexicon setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ssn_hts_demo::select_lexicon)
  "(ssn_hts_demo::select_lexicon)
Set up the lexicon for ssn_hts."
  (lex.select "ssn_hts")

  ;; Post lexical rules
  (set! postlex_rules_hooks (list ssn_hts::postlex_rule1))
)

(define (ssn_hts_demo::reset_lexicon)
  "(ssn_hts_demo::reset_lexicon)
Reset lexicon information."
  t
)

(provide 'ssn_hts_demo_lexicon)
