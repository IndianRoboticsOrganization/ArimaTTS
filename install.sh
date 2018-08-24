#!/bin/bash

# ArimaTTS Tamil - Indian Robotics [Powered By IIT Madras Donlab's Indic TTS]
# =============================================================================
# This is an automated source code compilation and installation script, which 
# downloads the dependencies for running ArimaTTS on Linux Computer (Ubuntu).
# 
# Most of the required tools are third-party softwares, Indian Robotics has no 
# control or rights over the license and safety of these softwares.
# Individual users are responsible for using these tools. 
# =============================================================================
#
# Author : Vasanth Kumar (@jarvisvasu)
# Founder Of Indian Robotics Organization
# 
# Contact : indianroboticsorganization@gmail.com
#
# For more info : http://www.indianrobotics.org
#
# If You Like The Software, Hit A LIKE On Our Facebook Page
#
# Like Us On : http://facebook.com/indianrobotics/
#
# For any queries, feel free to contact us!
# Please post your feedbacks on facebook, It really means so much for us!
#
# License : GPL

ARIMATTS_PATH=`pwd`


#Dependies
sudo apt-get install flex libncurses5-dev wget festival libx11-dev perl build-essential g++-4.7 csh gawk bc sox tcsh default-jre lame alsa aplay -y
sudo ln -s /lib/libncurses.so.5 /lib/libcurses.so
sudo ln -s /usr/lib/libstdc++.so.6 /lib/libstdc++.so

#Tools Required
cd speech_tools
chmod +x ./configure
./configure
make info
make
export ESTDIR=`pwd`
cd ..

cd festvox
chmod +x ./configure
./configure
make info
make
export FESTVOXDIR=`pwd`
cd ..

cd festival
chmod +x ./configure
./configure
make info
make
export FESTDIR=`pwd`
cd ..

sudo ln -s $ARIMATTS_PATH/festival/bin/festival /usr/bin/festival


cd htk

patch -p1 -d . < HTS-2.2_for_HTK-3.4.1.patch
chmod +x ./configure
./configure -I
make
sudo make install
sudo make hlmtools install-hlmtools
sudo make hdecode install-hdecode

cd ..

cd hts_engine_api
chmod +x ./configure
./configure
make
sudo make install

cd ..


cd sptk
chmod +x ./configure
./configure
make
sudo make install

perl -MCPAN -e 'install Parallel::ForkManager'

cd ..

#Configuring To Synthesize Tamil Speech
cd /usr/share/doc/festival/examples/
sudo  gunzip dumpfeats.gz

sudo gunzip dumpfeats.sh.gz
sudo chmod a+rx /usr/share/doc/festival/examples/dumpfeats

cd $ARIMATTS_PATH

#Voice & Language Data
cd tamil
chmod +x ./configure
./configure --with-fest-search-path=/usr/share/doc/festival/examples --with-sptk-search-path=$ARIMATTS_PATH/sptk/bin/ --with-hts-search-path=$ARIMATTS_PATH/htk/bin/ --with-hts-engine-search-path=$ARIMATTS_PATH/hts_engine_api/bin/

sudo mv /usr/share/festival/radio_phones.scm /usr/share/festival/radio_phones.scm-old

sudo cp $ARIMATTS_PATH/tamil/radio_phones.scm /usr/share/festival/

sudo cp $ARIMATTS_PATH/tamil/Slurp.pm /usr/share/perl5/File/

gcc scripts/tamil_trans.c -o scripts/tamil_trans

perl -i -pe 's/\$FESTDIR/\/usr/g' $ARIMATTS_PATH/tamil/scripts/complete

chmod +x $ARIMATTS_PATH/tamil/scripts/complete
chmod +x $ARIMATTS_PATH/vanakkam.sh
$ARIMATTS_PATH/vanakkam.sh
