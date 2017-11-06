PROCEDURE TO RUN THE DEMO

Step 1: Configure the folder
	./configure --with-fest-search-path=/$FESTDIR/examples/ --with-sptk-search-path=/usr/local/SPTK/bin/ --with-hts-search-path=/usr/local/HTS-2.2beta/bin/ --with-hts-engine-search-path=/PATH TO hts_engine_API-1.06/bin/


Step 2: Copy the radio_phones.scm into the $FESTDIR/lib/

Step 3: Copy the file Slurp.pm to /usr/share/perl5/File/

Step 4 : Compile the c file "scripts/tamil_trans.c"
		gcc scripts/tamil_trans.c -o scripts/tamil_trans
		
Step 5 : To run the project from the command line, run the command
         java -jar hts_demo.jar


Note: If the Tamil fonts is not been displayed in the GUI,
	rename the .ttf in /usr/share/fonts/lohit-tamil/ as 
	lohit_ta.ttf
	
