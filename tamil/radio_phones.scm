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
;;; Phonset for utts_for
;;;

;;;  Feeel free to add new feature values, or new features to this
;;;  list to make it more appropriate to your language

;; This is where it'll fall over if you haven't defined a 
;; a phoneset yet, if you have, delete this, if you haven't
;; define one then delete this error message
;(error "You have not yet defined a phoneset for tamaarthi (and others things ?)\n            Define it in festvox/utts_for_hts_phoneset.scm\n")

(defPhoneSet
  utts_for
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
   (SIL  0 0 0 0 0 0 0 0)  ;; slience ... 
   (MSIL  0 0 0 0 0 0 0 0)
   (LSIL  0 0 0 0 0 0 0 0)

	 (ae  +   s   3   1   -   0   0   0) ;; fat
   (ah  +   s   2   2   -   0   0   0) ;; but
   (ao  +   l   3   3   +   0   0   0) ;; lawn
   (aw  +   d   3   2   -   0   0   0) ;; how
   (ax  +   a   2   2   -   0   0   0) 
   (qr  +   a   2   2   -   r   0   0)
   (ow  +   d   2   3   +   0   0   0) ;; lone
   (oy  +   d   2   3   +   0   0   0) 
   (v   -   0   0   0   0   f   b   +)
   (zx  -   0   0   0   0   f   p   +)
   (z   -   0   0   0   0   f   a   +)
   (eh  +   s   2   1   -   0   0   0)
   
(aa	+	l	3	2	-	0	0	0)
(ai +	d	2	1	-	0	0	0)
(a +	s	2	2	-	0	0	0)
(au	+	d	1	3	+	0	0	0)
(b	-	0	0	0	0	s	l	+)
(c	-	0	0	0	0	a	p	-)
(dx	-	0	0	0	0	s	a	+)
(d	-	0	0	0	0	s	d	+)
(eu	+	s	1	3	-	0	0	0)
(ee	+	l	2	1	-	0	0	0)
(e	+	s	2	1	-	0	0	0)
(g	-	0	0	0	0	s	v	+)
(h	-	0	0	0	0	f	v	-)
(ii	+	l	1	1	-	0	0	0)
(i	+	s	1	1	-	0	0	0)
(j	-	0	0	0	0	a	p	+)
(k	-	0	0	0	0	s	v	-)
(l	-	0	0	0	0	l	d	+)
(lx	-	0	0	0	0	l	p	+)
(m	-	0	0	0	0	n	l	+)
(nx	-	0	0	0	0	n	a	+)
(n	-	0	0	0	0	n	d	+)
(nd	-	0	0	0	0	n	d	+)
(ng	-	0	0	0	0	n	v	+)
(nj	-	0	0	0	0	n	p	+)
(oo	+	l	2	3	+	0	0	0)
(o	+	s	2	3	+	0	0	0)
(p	-	0	0	0	0	s	l	-)
(f	-	0	0	0	0	s	l	-)
(r	-	0	0	0	0	l	p	+)
(rx	+	s	2	2	-	l	p	+)
(s	-	0	0	0	0	f	d	-)
(sx	-	0	0	0	0	f	p	+)
(tx	-	0	0	0	0	s	a	-)
(t	-	0	0	0	0	s	d	-)
(u	+	s	1	3	+	0	0	0)
(uu	+	l	1	3	+	0	0	0)
(w	-	0	0	0	0	l	d	+)
(y	-	0	0	0	0	l	v	+)
(zh	-	0	0	0	0	0	p	+)

   ;; insert the phones here, see examples in 
   ;; festival/lib/*_phones.scm

  )
)

(PhoneSet.silences '(SIL MSIL LSIL))

(define (utts_for_hts::select_phoneset)
  "(utts_for_hts::select_phoneset)
Set up phone set for utts_for."
  (Parameter.set 'PhoneSet 'utts_for)
  (PhoneSet.select 'utts_for)
)

(define (utts_for_hts::reset_phoneset)
  "(utts_for_hts::reset_phoneset)
Reset phone set for utts_for."
  t
)

(provide 'utts_for_hts_phoneset)
