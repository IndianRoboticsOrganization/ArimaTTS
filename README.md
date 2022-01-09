@Vasanth Kumar A

ArimaTTS - Tamil Speech Synthesizer by Indian Robotics
======================================================

Minimum System Requirements :
=============================

* Processor	Intel/AMD : Dual Core : 2GHz+ 
* RAM		1 GB	
* Disk Space	2 GB
* OS		Linux (Ubuntu 16.04 Recommended)

Tested On 
=========

* Linux Ubuntu 16.04 x86 32Bit
* AMD A4 PRO-3340B with Radeon HD Graphics × 4 
* 2 GB RAM

********************************************************************

1. Download & Extract ArimaTTS anywhere in your computer (ex: /home/indianrobotics/ArimaTTS)

Directory Structure
===================

	ArimaTTS
		|
		|-festival
			|-.
		|-festvox
			|-.
		|- ..

2. Run "install.sh" as super user (root)

	$ chmod +x ./install.sh
        
	$ sudo ./install.sh

Compilation & Installation may take 10 to 20 Minutes.. So go & have a coffee!

3. When you come back, if everything went good. Congrats! Your Tamil Speech Synthesizer Is READY!

4. To Test Whether Your Synthesizer Works! Enter the following command.

	$ sudo ./vanakkam.sh

 Now, you should hear the synthesized speech "வணக்கம்"! :)

5. Lets Start! To Synthesize Your First Speech Output. Run the commands as shown below.

	$ cd tamil
        
	$ ./scripts/complete "வணக்கம்" linux

 Viola! All Done, checkout your synthesized speech in wav/1.wav

 You can synthesize any tamil sentence, just replace "வணக்கம்" with your text.
 

 To Play The Synthesized Speech
 ==============================

	$ aplay wav/1.wav


 ArimaTTS Is The Best OFFLINE Natural Sounding Speech Synthesizer For Tamil.

*******************************************************************************

Contact Us : indianroboticsorganization@gmail.com

For more information - Visit : www.indianrobotics.org

Like Us On Facebook : https://facebook.com/indianrobotics/

*******************************************************************************

Credits :
=========

Our sincere thanks to IIT Madras & The development team of Indic TTS Project.

This software can't be made possible without their effort & years of work.

* IIT Madras [Donlab - Indic TTS] 
* HTK [HTS]
* CMU [Festival]

Special thanks to Prof.Hema A Murthy, IIT Madras, Department of Computer Science.

Future :
========

* Graphical User Interface (Coming Soon)
* Support for other indian languages (Malayalam, Telugu, Kannada, Hindi)
* Debian package for easy installation. (apt-get install arimatts)

**************************************************************************************

If you got any errors, feel free to contact us on indianroboticsorganization@gmail.com

**************************************************************************************

### Author : Vasanth Kumar (@jarvisvasu)
### Founder Of Indian Robotics Organization
### 
### Contact : indianroboticsorganization@gmail.com
###
### For more info : http://www.indianrobotics.org
###
### If You Like The Software, Hit A LIKE On Our Facebook Page
###
### Like Us On : http://facebook.com/indianrobotics/
#
### For any queries, feel free to contact us!
### Please post your feedbacks on facebook, It really means so much for us!


