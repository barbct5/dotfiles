# Update
sudo xbps-install -Syu

# Install X. Swap glib-devel with musl-devel if using musl distro
sudo xbps-install -Sy xorg-server xorg-fonts xinit libX11-devel libXft-devel libXinerama-devel glib-devel

sudo xbps-install -Sy dbus

sudo ln -s /etc/sv/dbus /var/service/

# Only available on glib distro
sudo xbps-install -Sy virtualbox-ose-guest
sudo ln -s /etc/sv/vboxservice /var/service/

sudo xbps-install -Sy git make gcc pkg-config
