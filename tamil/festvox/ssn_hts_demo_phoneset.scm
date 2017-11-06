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
;;; Phonset for ssn_hts
;;;

;;;  Feeel free to add new feature values, or new features to this
;;;  list to make it more appropriate to your language

;; This is where it'll fall over if you haven't defined a 
;; a phoneset yet, if you have, delete this, if you haven't
;; define one then delete this error message
;;(error "You have not yet defined a phoneset for tamil (and others things ?)\n            Define it in festvox/ssn_hts_demo_phoneset.scm\n")

(defPhoneSet
  ssn_hts
  ;;;  Phone Features
  (;; vowel or consonant
   (vc + - 0)  
   ;; vowel length: short long dipthong schwa
   (vlng s l d a 0)
   ;; vowel height: high mid low
   (vheight 1 2 3 0 -)
   ;; vowel frontness: front mid back
   (vfront 1 2 3 0 -)
   ;; lip rounding
   (vrnd + - 0)
   ;; consonant type: stop fricative affricative nasal liquid
   (ctype s f a n l r 0)
   ;; place of articulation: labial alveolar palatal labio-dental
   ;;                         dental velar
   (cplace l a p b d v g 0)
   ;; consonant voicing
   (cvox + - 0)
   )
  (
 (SIL  0 0 0 0 0 0 0 0)   ;; slience ... 
 (LSIL  0 0 0 0 0 0 0 0)
 (MSIL  0 0 0 0 0 0 0 0)
	(ae  	+   	s   	3   	1   	-   	0   	0   	0)
	(a	+	s	1	1	-	0	0	0)
	(aa	+	l	3	1	-	0	0	0)
	(i	+	s	1	1	-	0	0	0)
	(ii	+	l	1	1	-	0	0	0)
	(iy	+	l	1	1	-	0	0	0)
	(u	+	s	1	3	+	0	0	0)
	(eu	+	s	1	3	+	0	0	0)
	(uu	+	l	1	3	+	0	0	0)
	(e	+	s	2	1	-	0	0	0)
	(ee	+	l	2	1	-	0	0	0)
	(ai	+	d	3	2	-	0	0	0)
	(o	+	s	2	3	+	0	0	0)
	(oo	+	l	3	3	+	0	0	0)
	(au	+	d	2	3	+	0	0	0)
	(k	-	0	0	0	0	s	v	-)
	(ng	-	0	0	0	0	n	a	+)
	(c	-	0	0	0	0	a	a	-)
	(j	-	0	0	0	0	a	a	-)
	(nj	-	0	0	0	0	n	a	+)
	(tx	-	0	0	0	0	s	a	-)
	(t	-	0	0	0	0	s	d	-)
	(nd	-	0	0	0	0	n	a	+)
	(nx	-	0	0	0	0	n	a	+)
	(n	-	0	0	0	0	n	a	+)
	(p	-	0	0	0	0	s	l	-)
	(m	-	0	0	0	0	n	l	+)
	(y	-	0	0	0	0	r	p	+)
	(r	-	0	0	0	0	r	a	+)
	(rx	-	0	0	0	0	r	a	+)
	(l	-	0	0	0	0	l	a	+)
	(lx	-	0	0	0	0	l	a	+)
	(zh	-	0	0	0	0	l	p	+)
	(w	-	0	0	0	0	r	l	+)
	(v	-	0	0	0	0	r	l	+)
	(sx	-	0	0	0	0	f	p	-)
	(s	-	0	0	0	0	a	a	-)
	(h	-	0	0	0	0	f	g	-)
	(f	-	0	0	0	0	f       b       -)
	(g	-	0	0	0	0	s       v       +)
	(b	-	0	0	0	0	s       l       +)
	(d	-	0	0	0	0	s       d       +)
	(dx 	-	0	0	0	0	s       a       +)
	(uh  +   s   2   3   -   0   0    0)
   (@@  +   l   2   2   -   0   0    0)
   (ei  +   d   2   1   -   0   0    0)
   (oi  +   d   3   3   +   0   0    0)
   (ou  +   d   2   2   -   0   0    0)
   (e@  +   d   2   1   -   0   0    0)
   (i@  +   d   1   1   -   0   0    0)
   (u@  +   d   3   1   +   0   0    0)
   (@   +   a   2   2   -   0   0    0)
   (z   -   0   0   0   0   f   a   +)
   (sh  -   0   0   0   0   f   p   -)
   (th  -   0   0   0   0   f   d   -)
   (dh  -   0   0   0   0   f   d   +)
   (ch  -   0   0   0   0   a   p   -)
   (jh  -   0   0   0   0   a   p   +)

   ;; insert the phones here, see examples in 
   ;; festival/lib/*_phones.scm

  )
)

(PhoneSet.silences '(SIL MSIL LSIL))

(define (ssn_hts_demo::select_phoneset)
  "(ssn_hts_demo::select_phoneset)
Set up phone set for ssn_hts."
  (Parameter.set 'PhoneSet 'ssn_hts)
  (PhoneSet.select 'ssn_hts)
)

(define (ssn_hts_demo::reset_phoneset)
  "(ssn_hts_demo::reset_phoneset)
Reset phone set for ssn_hts."
  t
)

(provide 'ssn_hts_demo_phoneset)
