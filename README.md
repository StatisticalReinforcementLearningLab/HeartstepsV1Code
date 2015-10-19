# HeartSteps pilot data analysis

- [Mounting M+Box](##mouting-m+box)
- [Exporting data](##exporting-data)
- [Preparing data for analysis](##preparing-data-for-analysis)

## Mounting M+Box

The project's remote M+Box share can be accessed as though it were part of your local file system using the WebDAV protocol.

1. From [M+Box settings](https://umich.app.box.com/settings), set up an external password. You will use this password and your primary email address (also found in M+Box setting) as your credentials to access or "mount" M+Box content.
2. Follow the system-specific instructions below.

### Mac

### Windows

### Ubuntu

The following terminal commands install davfs2 and creates a mount point called `mbox` in your home directory.
```shell
~$ sudo pico /etc/davfs2/davfs2.conf
~$ sudo dpkg-reconfigure davfs2
~$ sudo usermod -a -G davfs2 USERNAME
~$ mkdir ~/mbox
~$ sudo pico /etc/fstab
```
To `/etc/fstab` add the line
```
https://dav.box.com/dav /home/USERNAME/mbox davfs rw,user,noauto 0 0
```
and save. Back in the terminal run:
```shell
chmod 600 /home/USERNAME/.davfs2/secrets
su - $USER
```
Now M+Box can be mounted with the following command.
```shell
~$ mount mbox
```

## Exporting data

### Jawbone and Google Fit

Download using the browser interface at <http://jitai-api.appspot.com>.

### HeartSteps

Ensure your system has the following installed.

- [Google App Engine Python SDK](https://cloud.google.com/appengine/downloads)
- [Python 2.7+](https://www.python.org/downloads/)

From the command line, navigate to the `heartstepsdata/exporter` folder in your local copy of this repository. Run the export script specific to your system. When prompted, enter the HeartSteps account credentials.

## Preparing data for analysis
