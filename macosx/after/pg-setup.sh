#!/bin/sh

PGVER=83

sudo launchctl load -w /Library/LaunchDaemons/org.macports.postgresql${PGVER}-server.plist
sudo mkdir -p /opt/local/var/db/postgresql${PGVER}/defaultdb
sudo chown postgres:postgres /opt/local/var/db/postgresql${PGVER}/defaultdb
sudo ln -nfs /opt/local/lib/postgresql${PGVER} /opt/local/lib/postgresql   

cd /
sudo su postgres -c "/opt/local/lib/postgresql${PGVER}/bin/initdb -D /opt/local/var/db/postgresql${PGVER}/defaultdb"
sudo su postgres -c "/opt/local/lib/postgresql${PGVER}/bin/createuser -s $USER"
