;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                             ;;
;;;                Rules to map tokens to words    	                        ;;
;;;                                                                             ;;
;;;  Copyright (c) 2011, vinodh <vinodh@lantana.tenet.res.in>   		;;
;;;                                                                             ;;
;;;  This program is a part of Tamil voice for Festival TTS.			;;
;;;  										;;
;;;  festival tamil is free software; you can redistribute it and/or modify     ;;
;;;  it under the terms of the GNU General Public License as published by	;;
;;;  the Free Software Foundation; either version 2 of the License, or		;;
;;;  (at your option) any later version.					;;
;;;										;;
;;;  This program is distributed in the hope that it will be useful,		;;
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of		;;
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the		;;
;;;  GNU General Public License for more details.				;;
;;;										;;
;;;  You should have received a copy of the GNU General Public License		;;
;;;  along with this program; if not, write to the Free Software		;;
;;;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA  ;;
;;;										;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tamil_dotted_abbr_list
  '(
;;Time
    ("ma" "மணி")
;;Designations
    ("txaa" "டாக்டர்")
    ("si" "சிரஞ்ஜீவி")
    ("sau" "சௌபாக்கியவதி")
    ("ku" "குமாரி")
    ;; Morning, Afternoon, Evening, Night
    ("mu.pa" "முற்பகல்")
    ("pi.pa" "பிற்பகல்")
;;AM and PM
    ("kaa" "காலை")    
    ("maa" "மாலை") 
;;Miscellaneous
    ("ndi" "நிமிடம்")
    	("௺" "நம்பர்")
    ("nde" "நம்பர்")
    ("ki" "கிலோ")
    ("mii" "மீட்டர்")
    ("ruu" "ரூபாய்")
))

(defvar tamil_abbr_list
  '(
;;Weekdays (as defined by the POSIX te_IN locale
   ;; ("ஞாயிறு" "ஞாயிற்றுக் கிழமை")
   ;; ("திங்கள்" "திங்கட் கிழமை")    
   ;; ("செவ்வாய்" "செவ்வாய்க் கிழமை")    
   ;; ("புதன்" "புதன் கிழமை")    
   ;; ("வியாழன்" "வியாழக் கிழமை")    
   ;; ("வெள்ளி" "வெள்ளிக் கிழமை")    
   ;; ("சனி" "சனிக் கிழமை")    
    ("1/2"  "அரை")
    ("1/4" "கால்")
  ;;  ("எ" "என்கிண்ற")
    ("e.txu." "எடுத்துக்காட்டு")
    ("u.m." "உதாரணம்")
))

(defvar tamil_denominations_list
  '(
   ("billionlu") ("billionla")  ("பில்லியன்")   
  ("millionlu")   ("millionla")  ("மில்லியன்")
   ("kotlu")   ("கோடியே")     ("கோடி")
    ("lakshalu")     ("லட்சத்து")     ("லட்சம்")
    ("velu")     ("ஆயிரத்து")     ("ஆயிரம்")
     ("நூற்றி")     ("நூற்று")     ("நூறு")
))

(defvar tamil_common_symbols_table
  '(
      ;("\"" "quote")
      ;("'" "tick")
  ;  ("#" "number")
   ; ("$" "dollar")
   ; ("%" "percent")
   ; ("&" "and")
   ; ("*" "star")
   ; ("+" "plus")
    ;("," "comma")
    ;("-" "dash")
    ;("." "dot")
    ;("/" "slash")
    ;(":" "colon")
    ;(";" "semi")
   ; ("<" ("less"))
   ; ("=" ("equals"))
   ; (">" ("greater"))
    ;("?" "question")
   ; ("@" "at")
   ; ("^" "caret")
   ; ("_" ("underline"))
    ;("!" ("bang"))
    ;("`" ("graav"))
   ; ("~" "tilda")
   ; ("|" "bar")
    ("©" "காபிரைட்") ;copyright sign
    ("®" "ரெஜிஸ்டர்டு") ;registered sign
    ("£" "பவுண்டு")
    ("€" "யூரோ")
    ("¥" "என்")
    ("¢" "செண்ட்")
  ;  ("ruu" "ரூபாய்")
    ("।" "")	;Devanagari Danda
    ("॥" "")	;Devanagari Double Danda
    ;;ensure digits are always at the end of the table
    ;;so that they will be used as separators after other symbols have been considered
    ("0" "பூஜ்ஜியம்")
    ("1" "ஒன்று")
    ("2" "இரண்டு")
    ("3" "மூன்று")
    ("4" "நான்கு")
    ("5" "ஐந்து")
    ("6" "ஆறு")
    ("7" "ஏழு")
    ("8" "எட்டு")
    ("9" "ஒன்பது")
))

(defvar tamil_supplementary_char_list
  '("£" "€" "¥" "©" "®" "¢" "।" "॥"))

(defvar tamil_currency_list
  '("$" "£" "€" "¥" "¢"))

(defvar tamil_abbr_markers_table
   '( ("." "") ("।" "") ("॥" "") ))
 
(defvar tamil_numbers_table
  '(
    ("௦" "0")
    ("௧" "1")
    ("௨" "2")
    ("௩" "3")
    ("௪" "4")
    ("௫" "5")
    ("௬" "6")
    ("௭" "7")
    ("௮" "8")
    ("௯" "9")
))


(define (tamil_token_to_words token name)
(flatten (tamil_token_to_words_ token name))
)

(defvar ascii_char_hash (cons-array 256));
(defvar defined_ascii_char_hash)
(define (initialize_ascii_char_hash)
(let ((char 0))
      (while (<= char 255)
        (hset ascii_char_hash (string-append (format nil "%c" char)) char)
        (set! char (+ char 1)))
(set! defined_ascii_char_hash t)
))

(define (ascii_of char)
(if (not defined_ascii_char_hash) (initialize_ascii_char_hash))
      (href ascii_char_hash char)
)

(define (tamil_match_string_start_with_list input_string string_list)
  ;;checks is any of the strings in string_list match the start of input_string
  ;;returns the matched string from string_list
  (let (substr (matched_substr ""))
  (while (and (not (null? string_list)) (string-equal matched_substr ""))
  	(set! substr (car string_list))
	(set! string_list (cdr string_list))
	(if (string-equal substr "$") 
	  	(set! match_string (string-append "\\" substr ".*"))
	  	(set! match_string (string-append substr ".*")))
  	(if (string-matches input_string match_string) (set! matched_substr substr)))
  matched_substr
)) 

(define (tamil_string_cleanup input_string)
  ;;ensures the string has characters with are either in the tamil unicode range,
  ;;ascii punctuation or represent a currency symbol
( let (curr_char next_char next_next_char (clean_string "") matched_string)
  ;;convert tamil unicode digits to ascii
  (while (and (set! matched_string (tamil_match_string_with_table input_string tamil_numbers_table))
	      (not (string-equal matched_string "")))
	(set! input_string (string-append (string-before input_string matched_string)
  	    (car (tamil_table_lookup matched_string tamil_numbers_table)) (string-after input_string matched_string))))
    (while (not (equal? input_string ""))
	(set! curr_char (ascii_of (substring input_string 0 1)))
	(set! next_char (ascii_of (substring input_string 1 1)))
	(set! next_next_char (ascii_of (substring input_string 2 1)))
	(cond
      	     ;;basic ascii
	     ((and (>= curr_char 33) (<= curr_char 126))
	          (set! num_chars_valid 1))
	     ;;tamil unicode block (of the form \340 {\256 \257} [\200 .. \277]) - In Octal representation
	     ((and (equal? curr_char 224)
		           (or (equal? next_char 174) (equal? next_char 175))
		          (and (>= next_next_char 128) (<= next_next_char 191)))
	          	  (set! num_chars_valid 3))
	     ;;Supplementary chars like yen, pound, cent, copyright, registered mark, Euro
	     ((not (string-equal (set! matched_string 
		   (tamil_match_string_start_with_list input_string tamil_supplementary_char_list)) ""))
  	           (set! num_chars_valid (string-length matched_string)))
	     ;;does not match anything, throw away the char
	     (t (set! num_chars_valid 0))
    	)
	(set! clean_string (string-append clean_string (substring input_string 0 num_chars_valid)))
	;;does not match anything, throw away the char
        (if (equal? num_chars_valid 0) (set! num_chars_valid 1))
	(set! input_string (substring input_string num_chars_valid (- (string-length input_string) num_chars_valid))))
    clean_string
))

(define (tamil_match_string_with_table input_string string_table)
  ;;checks is any of the strings in string_table match a substring of input_string
  ;;returns the matched string from string_table
  (let (substr (matched_substr ""))
  (while (and (not (null? string_table)) (string-equal matched_substr ""))
  	(set! substr (car (car string_table)))
	(set! string_table (cdr string_table))
	(if (or (string-equal substr "$") (string-equal substr "^") (string-equal substr "?")
		(string-equal substr "*") (string-equal substr "+") (string-equal substr "."))
	  	(set! match_string (string-append ".*\\" substr ".*"))
	  	(set! match_string (string-append ".*" substr ".*")))
  	(if (string-matches input_string match_string) (set! matched_substr substr)))
  matched_substr
)) 

(define (tamil_token_to_words_ token name)
  (set! name (tamil_string_cleanup name))
  (let ((number_regex "[0-9,]+\\(\\.[0-9]+\\)?") matched_substr matched_start_string prev_token_matched_start_string
						 currency_name time_segments date_segments)
  (set! matched_start_string (tamil_match_string_start_with_list name tamil_currency_list))
  (cond
    ;;currencies
   ((and (not (string-equal matched_start_string ""))
	 (string-matches (string-after name matched_start_string) number_regex))
      (set! currency_name (string-append (car (tamil_table_lookup matched_start_string tamil_common_symbols_table)) "கள்"))
      (if (tamil_list_lookup (item.feat token "n.name") tamil_denominations_list)
	  (tamil_token_to_words_ token (substring name 1  (- (string-length name) 1)))
	  (list (tamil_token_to_words_ token (substring name 1  (- (string-length name) 1))) currency_name)))
   ((and (not (string-equal (set! prev_token_matched_start_string (tamil_match_string_start_with_list 
			(item.feat token "p.name") tamil_currency_list)) ""))
       (string-matches (string-after (item.feat token "p.name") prev_token_matched_start_string) number_regex)
       (tamil_list_lookup name tamil_denominations_list)) 
           (list name (string-append 
			(car (tamil_table_lookup prev_token_matched_start_string tamil_common_symbols_table)) "கள்")))
   ;;rupees
   ((string-matches name (string-append "௹\\.[" number_regex))
        (if (tamil_list_lookup (item.feat token "n.name") tamil_denominations_list)
	  (tamil_token_to_words_ token (string-after name "௹."))
	  (list (tamil_token_to_words_ token (string-after name "௹.")) "ரூபாய்")))
    ((and (string-matches (item.feat token "p.name") (string-append "௹\\.[" number_regex))
	          (tamil_list_lookup name tamil_denominations_list) ) (list name "ரூபாய்"))
   ;;cents
   ((string-matches name (string-append number_regex "¢"))
        (list (tamil_token_to_words_ token (string-before name "¢")) "செண்டுகள்"))
    ((string-matches name "[0-9]+") (tamil_number_to_words name))
    ((string-matches name "[0-9]*\\.[0-9]+") 
        (list (tamil_number_to_words (string-before name ".")) '("புள்ளி") 
		    (mapcar tamil_number_to_words (mapcar string-append (symbolexplode (string-after name "."))))))
    ((string-matches name "\\([0-9]+,[0-9]*\\)+\\(\.[0-9]+\\)?") 
		     (tamil_token_to_words_ token (tamil_removechar name ","))) ;dd,dd,ddd.dd form
   ((string-matches name (string-append "-" number_regex)) (list '("மைனஸ்") 
							    (tamil_token_to_words_ token (string-after name "-"))))
   ((string-matches name (string-append "\\+" number_regex)) (list '("பிளஸ்") 
							    (tamil_token_to_words_ token (string-after name "+")))) 
    ;;line of characters
    ((string-matches name "_____+") (list '"அண்டர்" "ஸ்கோர்களின்" "வரிசை"))
    ((string-matches name "=====+") (list "ஈக்குவல்" "டூவின்" "வரிசை"))
    ((string-matches name "-----+") (list "ஹைஃபன்களின்" "வரிசை"))
    ((string-matches name "\\*\\*\\*\\*\\*+") (list "ஸ்டார்களின்" "வரிசை"))
    ;;time and date
    ((set! time_segments (tamil_string_matches_time name)) (mapcar tamil_token_to_words_ 
						  	  (list token token token token token token token) (mapcar string-append time_segments)))
    ((set! date_segments (tamil_string_matches_date name)) (mapcar tamil_token_to_words_ 
						  	  (list token token token) (mapcar string-append date_segments)))
    ;;abbreviations
    ;;dotted abbreviations when followed by space ex: "dr. hello"
    ((and (tamil_table_lookup name tamil_dotted_abbr_list)  (string-equal (item.feat token "punc") "."))
		(tamil_table_lookup name tamil_dotted_abbr_list))
    ;;abbreviation follwed by markers .,:,devanagari danda, double danda 
    ((and (set! matched_substr (tamil_match_string_with_table name tamil_abbr_markers_table))
	  (not (string-equal matched_substr ""))
     	  (tamil_table_lookup (string-before name matched_substr) tamil_dotted_abbr_list))
       	     (list (tamil_table_lookup (string-before name matched_substr) tamil_dotted_abbr_list)
	           (tamil_token_to_words_ token (string-after name matched_substr))))   
    ;;abbreviations not followed by .
    ((tamil_table_lookup name tamil_abbr_list) (tamil_table_lookup name tamil_abbr_list))
    ;;special characters
    ((string-matches name (string-append number_regex "%")) 
     		(list (tamil_token_to_words_ token (string-before name "%")) '("சதவிகிதம்")))
    ;;separators and connectors {#,$,%,&,*,+,-,/,<,=,>,@,\,^,_,`,copyright,registered trademark} 
    ((and (set! matched_substr (tamil_match_string_with_table name tamil_common_symbols_table) )
	  (not (string-equal matched_substr "")))
     	(list (tamil_token_to_words_ token (string-before name matched_substr))
	      (tamil_table_lookup matched_substr tamil_common_symbols_table)
	      (tamil_token_to_words_ token (string-after name matched_substr))))   
    (t 
;; (if (lts.in.alphabet (tamil_string_cleanup name) 'iitm_tam) 
	  (list (tamil_string_cleanup name)) 
;;       (list '("")))
    )
)))

;; TIME Handling
(define (tamil_string_matches_time input_string)
  ;;returns input string is in HH:MM(:SS) format
  (let ((hrs (parse-number (string-before input_string ":"))) mins secs)
  (cond 
    ((not (string-matches input_string "[0-9][0-9]?:[0-9][0-9]\\(:[0-9][0-9]\\)?")) nil)
    (t
	(set! Time "நேரம்")
	(set! Hours "மணி")
	(set! Minutes "நிமிடம்")
	(set! Seconds "வினாடி")
    (set! input_string (string-after input_string ":"))
    (set! mins (string-before input_string ":"))
    (if (string-equal mins "") (set! mins (parse-number input_string)) (set! mins (parse-number mins)))
    (set! secs (string-after input_string ":"))

    ;;checking for HH,MM,SS to be in valid 24 hour format
    (if (and (>= hrs 0) (<= hrs 23) (>= mins 0) (<= mins 59) (>= (parse-number secs) 0) (<= (parse-number secs) 59))
      		(list Time hrs Hours mins Minutes secs Seconds) nil)
    )
)))

;; DATE Handling
(defvar tamil_no_of_days_in_month
  '( (1 31) (2 29)(3 31) (4 30)
     (5 31) (6 30) (7 31) (8 31)
     (9 30) (10 31) (11 30) (12 31)))

(defvar tamil_month
	'( 
	   ("1" "ஜனவரி")
	   ("2" "பிப்ரவரி")
	   ("3" "மார்ச்")			
	   ("4" "ஏப்ரல்")
	   ("5" "மே")
	   ("6" "ஜுன்")
	   ("7" "ஜூலை")
	   ("8" "ஆகஸ்ட்")
	   ("9" "செப்டம்பர்")
	   ("10" "அக்டோபர்")
	   ("11" "நவம்பர்")
	   ("12" "டிசம்பர்"))
)

(define (tamil_string_matches_date input_string)
  ;;returns true, if the input string is in DD/MM(/YY/YYYY) format,
  (let ((date_segment1 (parse-number (string-before input_string "/"))) date_segment2 date_segment3 days_in_month)
  (cond
    ((not (string-matches input_string "[0-9][0-9]?/[0-9][0-9]?\\(/[0-9][0-9]\\([0-9][0-9]\\)?\\)?")) nil)
    (t
    (set! input_string (string-after input_string "/"))
    (set! date_segment2 (string-before input_string "/"))
    (if (string-equal date_segment2 "") (set! date_segment2 (parse-number input_string))
      					(set! date_segment2 (parse-number date_segment2)))
    (set! date_segment3 (string-after input_string "/"))
    ;;checking for DD,MM to be in valid 
    (set! days_in_month (car (tamil_table_lookup date_segment2 tamil_no_of_days_in_month)))
	(set! date_segment_month (car (tamil_table_lookup date_segment2 tamil_month)))
    (if (and days_in_month (> date_segment1 0) (<= date_segment1 days_in_month)) 
      				(list date_segment_month date_segment1 date_segment3) nil))
)))

;; NUMBER handling 
(define (tamil_number_to_words number)
  (let (input_string)
    (flatten (tamil_number_to_words_rec input_string number))
))

(define (tamil_number_to_words_rec token_list name)
  (set! name (tamil_strip_leading_zeros name)) ;remove leading zeros
  (let ((number_length (string-length name)))
  (cond
;; ;; ;; For Numbers with 8 or More Digits
    ((>= number_length 8)
     (if (string-equal name "10000000") (append token_list '("ஒருகோடி"))
       (if (string-matches name "[0-9]+0000000") ; For Numbers 2 crores ... 10 crores ... 50 crores ... 99 crores
;; Condition TRUE 
	 (append (tamil_number_to_words_rec token_list (substring name 0 (- number_length 7))) '("கோடி")) ;or kootlu
;; Condition FALSE
	 (if (string-matches name "1[0-9][0-9][0-9][0-9][0-9][0-9][0-9]") ;1 followed by other digits
      ;; Condition TRUE
	     (append  token_list '("ஒருகோடியே") (tamil_number_to_words_rec token_list (substring name (- number_length 7) 7)))
      ;; Condition FALSE
	     (append (tamil_number_to_words_rec token_list (substring name 0 (- number_length 7))) '("கோடியே") 
	           (tamil_number_to_words_rec token_list (substring name (- number_length 7) 7)))
	 )
	)
      )
    )
;; ;; ;; For 6 and 7 Digit Numbers
    ((and (<= number_length 7) (>= number_length 6))
     (if (string-equal name "100000") (append token_list '("ஒருலட்சம்"))
       (if (string-matches name "[0-9]+00000") ; For Numbers 200000 ... 1000000 ... 4000000 .. 9900000
;; Condition TRUE
	 	(append token_list (tamil_two_digit_number_to_words 
				   (substring name 0 (- number_length 5))) '("லட்சம்"));or laskalu 
;; Condition FALSE
		(if (string-matches name "1[0-9][0-9][0-9][0-9][0-9]")
	  ;; Condition TRUE
        	   (tamil_number_to_words_rec (append token_list '("ஒருலட்சத்து")) (substring name (- number_length 5) 5))
	  ;; Condition FALSE
	           (tamil_number_to_words_rec (append token_list (tamil_two_digit_number_to_words 
			(substring name 0 (- number_length 5))) '("லட்சத்து")) (substring name (- number_length 5) 5))
		)
	)
      )
    )
;; ;; ;; For 4 and 5 Digit Numbers
    ((and (<= number_length 5) (>= number_length 4))
     (if (string-equal name "1000") (append token_list '("ஆயிரம்"))
       (if (string-matches name "[0-9]+000") ; For Numbers 2000 ... 10,000 ... 40,000 .. 99,000
;; 	Condition TRUE
	 	;;(append token_list (tamil_two_digit_number_to_words (substring name 0 (- number_length 3))) '("ஆயிரம்"))
		  (append token_list (tamil_table_lookup name tamil_four_digit_numbers_table))
;; 	Condition FALSE
	    (if (string-matches name "1[0-9][0-9][0-9]") ; For Numbers 1001 - 1999
      ;; Condition TRUE
		(tamil_number_to_words_rec (append token_list '("ஆயிரத்து")) (substring name (- number_length 3) 3))
      ;; Condition FALSE
		(tamil_number_to_words_rec (append token_list (tamil_table_lookup (substring name 0 (- number_length 3) )tamil_four_digit_numbers_table))  (substring name (- number_length 3) 3))
	;;	(tamil_number_to_words_rec (append token_list (tamil_two_digit_number_to_words 
	;;	(substring name 0 (- number_length 3))) '("ஆயிரத்தி")) (substring name (- number_length 3) 3))
	   )
       )
     )
    )
;; ;; ;; For 3 Digit Numbers
    ((eq number_length 3) 
     (if (string-equal name "100") 
;; Condition TRUE 
	 (append token_list '("நூறு"))	;only 100
;; Condition FALSE  - else if
      (if (string-matches name "[2-8]00") ; For 200,300,400 ...
  ;; Condition TRUE 
	    (append token_list (list (string-append (car (tamil_table_lookup (substring name 0 (- number_length 2)) tamil_three_digit_numbers_table)) '"நூறு"))) ;or vandala
  ;; Condition FALSE  - else if
	(if (string-equal name "900")  ;only 900
      ;; Condition TRUE 
	    (append token_list '("தொள்ளாயிரம்")) 
      ;; Condition FALSE  - else if
	  (if (string-matches name "9[0-9][0-9]") ; For 901-999
	;; Condition TRUE 
	    (append token_list '("தொள்ளாயிரத்து")	
	    (tamil_two_digit_number_to_words (substring name 1 2)))
	;; Condition FALSE  - else if
	    (if (string-matches name "1[0-9][0-9]") ; For 101-199
	  ;; Condition TRUE 
		(append token_list '("நூற்றி")	
		(tamil_two_digit_number_to_words (substring name 1 2)))
	  ;; Condition FALSE  - else if
		(tamil_number_to_words_rec  (append token_list (list (string-append  (car (tamil_table_lookup (substring name 0 (- number_length 2)) tamil_three_digit_numbers_table)) "நூற்றி"))) (substring name (- number_length 2) 2))
	    )
	  )
	)	
       )
      )
     )
;; ;; ;; For 2 Digit Numbers
    ((<= number_length 2) 
      (append token_list (tamil_two_digit_number_to_words name))
    )
)))

(defvar tamil_basic_number_table
  '(
    ("0" "பூஜ்ஜியம்")
    ("1" "ஒன்று")
    ("2" "இரண்டு")
    ("3" "மூன்று")
    ("4" "நான்கு")
    ("5" "ஐந்து")
    ("6" "ஆறு")
    ("7" "ஏழு")
    ("8" "எட்டு")
    ("9" "ஒன்பது")
))

(defvar tamil_two_digit_numbers_table
  '(
    ("10" "பத்து")
    ("11" "பதினொன்று")
    ("12" "பன்னிரெண்டு")
    ("13" "பதிமூன்று")
    ("14" "பதிநான்கு")
    ("15" "பதினைந்து")
    ("16" "பதினாறு")
    ("17" "பதினேழு")
    ("18" "பதினெட்டு")
    ("19" "பத்தொன்பது")
    ("20" "இருபது")
    ("30" "முப்பது")
    ("40" "நாற்பது")
    ("50" "ஐம்பது")
    ("60" "அறுபது")
    ("70" "எழுவது")
    ("80" "எண்பது")
    ("90" "தொண்ணூறு")
    ("2" "இருபத்து")
    ("3" "முப்பத்து")
    ("4" "நாற்பத்து")
    ("5" "ஐம்பத்து")
    ("6" "அறுபத்து")
    ("7" "எழுபத்து")
    ("8" "எண்பத்து")
    ("9" "தொண்ணூற்றி")
))

(defvar tamil_three_digit_numbers_table
  '(
    ("2" "இரு")
    ("3" "முன்")
    ("4" "நா")
    ("5" "ஐ")
    ("6" "அறு")
    ("7" "எழு")
    ("8" "எண்")
 ))

(defvar tamil_four_digit_numbers_table
 '(
    ("2000" "இரண்டாயிரம்")
    ("2" "இரண்டாயிரத்து")
    ("3000" "மூவாயிரம்")
    ("3" "மூவாயிரத்து")
    ("4000" "நான்காயிரம்")
    ("4" "நான்காயிரத்து")
    ("5000" "ஐந்தாயிரம்")
    ("5" "ஐந்தாயிரத்து")
    ("6000" "ஆறாயிரம்")
    ("6" "ஆறாயிரத்து")
    ("7000" "ஏழாயிரம்")
    ("7" "ஏழாயிரத்து")
    ("8000" "எட்டாயிரம்")
    ("8" "எட்டாயிரத்து")
    ("9000" "ஒன்பதாயிரம்")
    ("9" "ஒன்பதாயிரத்து")
    ("10000" "பத்தாயிரம்")
    ("10" "பத்தாயிரத்து")
    ("11000" "பதினொன்றாயிரம்")
    ("11" "பதினொன்றாயிரத்து")
    ("12000" "பன்னிரெண்டாயிரம்")
    ("12" "பன்னிரெண்டாயிரத்து")
    ("13000" "பதிமூன்றாயிரம்")
    ("13" "பதிமூன்றாயிரத்து")
    ("14000" "பதினாங்காயிரம்")
    ("14" "பதினாங்காயிரத்து")
    ("15000" "பதினைந்தாயிரம்")
    ("15" "பதினைந்தாயிரத்து")
    ("16000" "பதினாறாயிரம்")
    ("16" "பதினாறாயிரத்து")
    ("17000" "பதினேழாயிரம்")
    ("17" "பதினேழாயிரத்து")
    ("18000" "பதினெட்டாயிரம்")
    ("18" "பதினெட்டாயிரத்து")
    ("19000" "பத்தொன்பதாயிரம்")
    ("19" "பத்தொன்பதாயிரத்து")
    ("20000" "இருபதாயிரம்")
    ("20" "இருபதாயிரத்து")
    ("21000" "இருபத்தொன்றாயிரம்")
    ("21" "இருபத்தொன்றாயிரத்து")
    ("22000" "இருபத்திரண்டாயிரம்")
    ("22" "இருபத்திரண்டாயிரத்து")
    ("23000" "இருபத்திமூன்றாயிரம்")
    ("23" "இருபத்திமூன்றாயிரத்து")
    ("24000" "இருபத்திநான்காயிரம்")
    ("24" "இருபத்திநான்காயிரத்து")
    ("25000" "இருபத்தைந்தாயிரம்")
    ("25" "இருபத்தைந்தாயிரத்து")
    ("26000" "இருபத்தாறாயிரம்")
    ("26" "இருபத்தாறாயிரத்து")
    ("27000" "இருபத்தேழாயிரம்")
    ("27" "இருபத்தேழாயிரத்து")
    ("28000" "இருபத்தெட்டாயிரம்")
    ("28" "இருபத்தெட்டாயிரத்து")
    ("29000" "இருபத்தொன்பதாயிரம்")
    ("29" "இருபத்தொன்பதாயிரத்து")
    ("30000" "முப்பதாயிரம்")
    ("30" "முப்பதாயிரத்து")
    ("31000" "முப்பத்தொன்றாயிரம்")
    ("31" "முப்பத்தொன்றாயிரத்து")
    ("32000" "முப்பத்திரண்டாயிரம்")
    ("32" "முப்பத்திரண்டாயிரத்து")
    ("33000" "முப்பத்திமூன்றாயிரம்")
    ("33" "முப்பத்திமூன்றாயிரத்து")
    ("34000" "முப்பத்திநான்காயிரம்")
    ("34" "முப்பத்திநான்காயிரத்து")
    ("35000" "முப்பத்தைந்தாயிரம்")
    ("35" "முப்பத்தைந்தாயிரத்து")
    ("36000" "முப்பத்தாறாயிரம்")
    ("36" "முப்பத்தாறாயிரத்து")
    ("37000" "முப்பத்தேழாயிரம்")
    ("37" "முப்பத்தேழாயிரத்து")
    ("38000" "முப்பத்தெட்டாயிரம்")
    ("38" "முப்பத்தெட்டாயிரத்து")
    ("39000" "முப்பத்தொன்பதாயிரம்")
    ("39" "முப்பத்தொன்பதாயிரத்து")
    ("40000" "நாற்பதாயிரம்")
    ("40" "நாற்பதாயிரத்து")
    ("41000" "நாற்பத்தொன்றாயிரம்")
    ("41" "நாற்பத்தொன்றாயிரத்து")
    ("42000" "நாற்பத்திரண்டாயிரம்")
    ("42" "நாற்பத்திரண்டாயிரத்து")
    ("43000" "நாற்பத்திமூன்றாயிரம்")
    ("43" "நாற்பத்திமூன்றாயிரத்து")
    ("44000" "நாற்பத்திநான்காயிரம்")
    ("44" "நாற்பத்திநான்காயிரத்து")
    ("45000" "நாற்பத்தைந்தாயிரம்")
    ("45" "நாற்பத்தைந்தாயிரத்து")
    ("46000" "நாற்பத்தாறாயிரம்")
    ("46" "நாற்பத்தாறாயிரத்து")
    ("47000" "நாற்பத்தேழாயிரம்")
    ("47" "நாற்பத்தேழாயிரத்து")
    ("48000" "நாற்பத்தெட்டாயிரம்")
    ("48" "நாற்பத்தெட்டாயிரத்து")
    ("49000" "நாற்பத்தொன்பதாயிரம்")
    ("49" "நாற்பத்தொன்பதாயிரத்து")
    ("50000" "ஐம்பதாயிரம்")
    ("50" "ஐம்பதாயிரத்து")
    ("51000" "ஐம்பத்தொன்றாயிரம்")
    ("51" "ஐம்பத்தொன்றாயிரத்து")
    ("52000" "ஐம்பத்திரண்டாயிரம்")
    ("52" "ஐம்பத்திரண்டாயிரத்து")
    ("53000" "ஐம்பத்திமூன்றாயிரம்")
    ("53" "ஐம்பத்திமூன்றாயிரத்து")
    ("54000" "ஐம்பத்திநான்காயிரம்")
    ("54" "ஐம்பத்திநான்காயிரத்து")
    ("55000" "ஐம்பத்தைந்தாயிரம்")
    ("55" "ஐம்பத்தைந்தாயிரத்து")
    ("56000" "ஐம்பத்தாறாயிரம்")
    ("56" "ஐம்பத்தாறாயிரத்து")
    ("57000" "ஐம்பத்தேழாயிரம்")
    ("57" "ஐம்பத்தேழாயிரத்து")
    ("58000" "ஐம்பத்தெட்டாயிரம்")
    ("58" "ஐம்பத்தெட்டாயிரத்து")
    ("59000" "ஐம்பத்தொன்பதாயிரம்")
    ("59" "ஐம்பத்தொன்பதாயிரத்து")
    ("60000" "அறுபதாயிரம்")
    ("60" "அறுபதாயிரத்து")
    ("61000" "அறுபத்தொன்றாயிரம்")
    ("61" "அறுபத்தொன்றாயிரத்து")
    ("62000" "அறுபத்திரண்டாயிரம்")
    ("62" "அறுபத்திரண்டாயிரத்து")
    ("63000" "அறுபத்திமூன்றாயிரம்")
    ("63" "அறுபத்திமூன்றாயிரத்து")
    ("64000" "அறுபத்திநான்காயிரம்")
    ("64" "அறுபத்திநான்காயிரத்து")
    ("65000" "அறுபத்தைந்தாயிரம்")
    ("65" "அறுபத்தைந்தாயிரத்து")
    ("66000" "அறுபத்தாறாயிரம்")
    ("66" "அறுபத்தாறாயிரத்து")
    ("67000" "அறுபத்தேழாயிரம்")
    ("67" "அறுபத்தேழாயிரத்து")
    ("68000" "அறுபத்தெட்டாயிரம்")
    ("68" "அறுபத்தெட்டாயிரத்து")
    ("69000" "அறுபத்தொன்பதாயிரம்")
    ("69" "அறுபத்தொன்பதாயிரத்து")
    ("70000" "எழுபதாயிரம்")
    ("70" "எழுபதாயிரத்து")
    ("71000" "எழுபத்தொன்றாயிரம்")
    ("71" "எழுபத்தொன்றாயிரத்து")
    ("72000" "எழுபத்திரண்டாயிரம்")
    ("72" "எழுபத்திரண்டாயிரத்து")
    ("73000" "எழுபத்திமூன்றாயிரம்")
    ("73" "எழுபத்திமூன்றாயிரத்து")
    ("74000" "எழுபத்திநான்காயிரம்")
    ("74" "எழுபத்திநான்காயிரத்து")
    ("75000" "எழுபத்தைந்தாயிரம்")
    ("75" "எழுபத்தைந்தாயிரத்து")
    ("76000" "எழுபத்தாறாயிரம்")
    ("76" "எழுபத்தாறாயிரத்து")
    ("77000" "எழுபத்தேழாயிரம்")
    ("77" "எழுபத்தேழாயிரத்து")
    ("78000" "எழுபத்தெட்டாயிரம்")
    ("78" "எழுபத்தெட்டாயிரத்து")
    ("79000" "எழுபத்தொன்பதாயிரம்")
    ("79" "எழுபத்தொன்பதாயிரத்தி")
    ("80000" "எண்பதாயிரம்")
    ("80" "எண்பதாயிரத்து")
    ("81000" "எண்பத்தொன்றாயிரம்")
    ("81" "எண்பத்தொன்றாயிரத்து")
    ("82000" "எண்பத்திரண்டாயிரம்")
    ("82" "எண்பத்திரண்டாயிரத்து")
    ("83000" "எண்பத்திமூன்றாயிரம்")
    ("83" "எண்பத்திமூன்றாயிரத்து")
    ("84000" "எண்பத்திநான்காயிரம்")
    ("84" "எண்பத்திநான்காயிரத்து")
    ("85000" "எண்பத்தைந்தாயிரம்")
    ("85" "எண்பத்தைந்தாயிரத்து")
    ("86000" "எண்பத்தாறாயிரம்")
    ("86" "எண்பத்தாறாயிரத்து")
    ("87000" "எண்பத்தேழாயிரம்")
    ("87" "எண்பத்தேழாயிரத்து")
    ("88000" "எண்பத்தெட்டாயிரம்")
    ("88" "எண்பத்தெட்டாயிரத்து")
    ("89000" "எண்பத்தொன்பதாயிரம்")
    ("89" "எண்பத்தொன்பதாயிரத்து")
    ("90000" "தொண்ணூறாயிரம்")
    ("90" "தொண்ணூறாயிரத்து")
    ("91000" "தொண்ணூத்தொன்றாயிரம்")
    ("91" "தொண்ணூத்தொன்றாயிரத்து")
    ("92000" "தொண்ணூத்திரண்டாயிரம்")
    ("92" "தொண்ணூத்திரண்டாயிரத்து")
    ("93000" "தொண்ணூத்திமூன்றாயிரம்")
    ("93" "தொண்ணூத்திமூன்றாயிரத்து")
    ("94000" "தொண்ணூத்திநான்காயிரம்")
    ("94" "தொண்ணூத்திநான்காயிரத்து")
    ("95000" "தொண்ணூத்தைந்தாயிரம்")
    ("95" "தொண்ணூத்தைந்தாயிரத்து")
    ("96000" "தொண்ணூத்தாறாயிரம்")
    ("96" "தொண்ணூத்தாறாயிரத்து")
    ("97000" "தொண்ணூத்தேழாயிரம்")
    ("97" "தொண்ணூத்தேழாயிரத்து")
    ("98000" "தொண்ணூத்தெட்டாயிரம்")
    ("98" "தொண்ணூத்தெட்டாயிரத்து")
    ("99000" "தொண்ணூத்தொன்பதாயிரம்")
    ("99" "தொண்ணூத்தொன்பதாயிரத்து")
))


(define (tamil_list_lookup abbr lst)
 (assoc_string abbr lst))

(define (tamil_table_lookup abbr table)
  (cdr (assoc_string abbr table)))

(define (tamil_two_digit_number_to_words name)
  ;number to words for 0-99
  (let (lst units tens) 
    (set! lst (reverse (symbolexplode name)))
;; (display lst)
    (set! units (car lst))
;; (display units)
    (set! tens (car (cdr lst)))
;; (display tens)
    ;compress the units digit if it is 0
;;     (if (and (string-equal units "0") (not (or (eq tens nil) (string-equal tens "1")))) (set! units "")) 
    ;remove leading zero
    (if (string-equal tens "0") (set! tens "")) 
    (if (or (string-equal tens "1") (and (string-matches tens "[2-9]") (string-equal units "0"))) (tamil_table_lookup name tamil_two_digit_numbers_table)
        (cons (tamil_table_lookup tens tamil_two_digit_numbers_table) (tamil_table_lookup units tamil_basic_number_table))
)))

(define (tamil_strip_leading_zeros number)
  ;removes leading zeros for a number, if single zero, leave it as it is
  (if (string-matches number "0+") (set! number "0")
       (while (string-matches number "0[0-9]*") (set! number (string-after number "0"))))
  number
)

(define (tamil_removechar input_string char)
  ;removes all occurences of char from input_string
  (let ((has_matched 1) match_string)
  (if (string-equal char "\\") (set! match_string (string-append ".*\\\\.*"))
      				 (set! match_string (string-append ".*" char ".*")))
  (while has_matched
    (if (string-matches input_string match_string)
	 (set! input_string (string-append (string-before input_string char) (string-after input_string char)))
	 (set! has_matched nil)))
    input_string))

(provide 'tamil_token)
