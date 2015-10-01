# Heartsteps data exporter

## Setting up

Install the Google App Engine SDK and its dependencies.

https://cloud.google.com/appengine/downloads

```shell
sudo apt-get install libreadline-gplv2-dev libncurses-dev libncursesw5-dev libssl-dev libsqlite3-dev tk-dev libgdbm-dev libc6-dev libbz2-dev
wget http://python.org/ftp/python/2.7.5/Python-2.7.5.tgz
tar -xvf Python-2.7.5.tgz
cd Python-2.7.5
./configure
make
sudo make install
wget https://storage.googleapis.com/appengine-sdks/featured/google_appengine_1.9.26.zip
unzip /Data/google_appengine_1.9.26.zip
export PATH=$PATH:/Data/google_appengine/
export GAE_SDK_ROOT=/Data/google_appengine
/usr/bin/env python -V
./Heartsteps_Data_Exporter/appcfg.py
```

## Exporting data

```shell
cd Heartsteps_Data_Exporter
./export.sh
```

When prompted, enter the Heartsteps app credentials.
