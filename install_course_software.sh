#######################################
### Install and setup R and Rstudio ###
#######################################

## Rstudio is stupidly enough not out with an official repository, so doing dirty install

echo "Preparing temporary folder for install files"
mkdir rstudio_tmp
cd rstudio_tmp

echo "Downloading the latest Rstudio file (Link must be updated manually!)" 
wget -c https://download1.rstudio.org/electron/jammy/amd64/rstudio-2023.09.1-494-amd64.deb

echo "Installing R from official repository and Rstudio from downloaded file"
sudo apt-get install --yes r-base-dev libxml2-dev libcurl4-openssl-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev vim ./rstudio-2023.09.1-494-amd64.deb ## Requires password

echo "Cleaning up"
cd ..
rm -r rstudio_tmp

echo "Installing required packages from script"
Rscript "R_packages.R" > installR.log

echo "Ensuring no critical errors occured."
grep "error" -i installR.log

echo "Press Q to quit!" >> installR.log

#############################
### Installing micromamba ###
#############################

echo "Fetching installer"
wget micro.mamba.pm/install.sh

echo "Providing default values for - and executing -installer"
echo -e "~/.local/bin\nY\nY\n" | bash install.sh

echo "Adding mamba alias"
echo alias mamba=micromamba >> ~/.bashrc

echo "Removing installer"
rm install.sh

echo "Initiating changes"
source .bashrc


#########################################
### Create the CGEfinders environment ###
#########################################

echo "Installing CGEfinders environment"
mamba env create --file cge_packages.yaml --name CGEfinders --yes

echo "Making directories for Databases"
mamba activate CGEfinders
echo alias resfinder.py=run_resfinder.py >> ~/.bashrc
mkdir -p DB
cd DB

echo "Downloading Virulence and Resfinder Databases"
download-virulence-db.sh
download-db.sh

echo "Installing Plasmidfinder last, as its database download script overwrites the resfinder script"
mamba install -c conda-forge -c bioconda plasmidfinder --yes
download-db.sh

echo "Linking Resfinder-, pointfinder-, and plasmidfinder-DBs to local folder"
ln -sf /home/gebt/micromamba/envs/CGEfinders/share/plasmidfinder-2.1.6/database plasmidfinder_db
ln -sf /home/gebt/micromamba/envs/CGEfinders/share/resfinder-4.1.11/db/db_resfinder/ resfinder_db
ln -sf /home/gebt/micromamba/envs/CGEfinders/share/resfinder-4.1.11/db/db_pointfinder/ pointfinder_db

echo "Downloading spatyper Database"
git clone https://bitbucket.org/genomicepidemiology/spatyper_db.git

echo "Installing spatyper"
cd /home/gebt/micromamba/envs/CGEfinders/share
git clone https://bitbucket.org/genomicepidemiology/spatyper.git
echo alias spatyper.py='python /home/gebt/micromamba/envs/CGEfinders/share/spatyper/spatyper.py' >> ~/.bashrc


echo "Wrapping up finders"
cd ~
mamba deactivate

##########################################
### Create the NCBItypers environment ###
##########################################

echo "Installing NCBItypers environment"
mamba env create --file ncbi_packages.yaml --name NCBItypers --yes

echo "Preparing MLST databases"
mamba activate NCBItypers
mlst-make_blast_db

ln -sf /home/gebt/micromamba/envs/NCBItypers/db/blast/mlst.fa DB/mlst.fa
#############
### Done! ###
#############
echo Done!

######################################################
### Inspecting potential R packages install errors ###
######################################################

echo "Inspecting `installR.log` manually after finished setup!"
less installR.log
