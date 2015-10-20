# HeartSteps pilot data analysis

- [Mounting M+Box](#mouting-m+box)
- [Exporting data](#exporting-data)
- [Preparing data for analysis](#preparing-data-for-analysis)
- [Running data summaries](#running-data-summaries)

## Mounting M+Box

Mounting via [WebDAV](http://community.box.com/t5/Managing-Your-Content/Does-Box-support-WebDAV/ta-p/310) allows you to access remote content stored on M+Box as though it were part of your local filesystem.

1. From your [M+Box account settings](https://umich.app.box.com/settings/account), set up an external password. You will use this password and your primary M+Box email address (also found in settings) as the credentials to mount access M+Box content.
2. Follow the system-specific instructions below.

### Mac

### Windows

### Ubuntu

- Install [davfs2](http://savannah.nongnu.org/projects/davfs2) and create a mount point called `mbox` in your home directory with the following terminal commands. Here `USERNAME` should be replace with your own system login name.
```shell
sudo pico /etc/davfs2/davfs2.conf
sudo dpkg-reconfigure davfs2
sudo usermod -a -G davfs2 USERNAME
mkdir ~/mbox
echo "https://dav.box.com/dav /home/USERNAME/mbox davfs rw,user,noauto 0 0" | sudo tee -a /etc/fstab
chmod 600 /home/USERNAME/.davfs2/secrets
```
- Logout and log back in.
- M+Box can now be mounted with the command `mount ~/mbox`.

## Exporting data

### Jawbone and Google Fit

Download using the browser interface at <http://jitai-api.appspot.com>.

### HeartSteps

Ensure your system has [M+Box mounted](#mounting-m+box) and the following software installed.

- [Google App Engine Python SDK](https://cloud.google.com/appengine/downloads)
- [Python 2.7+](https://www.python.org/downloads/)

From the command line, navigate to the `heartstepsdata/exporter` folder in your local copy of this repository. Run the export script specific to your system. When prompted, enter the HeartSteps account credentials.

## Preparing data for analysis

## Running data summaries
