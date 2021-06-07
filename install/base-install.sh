#!/bin/bash

set -e

# Format the disk.
parted -s /dev/sda -- mklabel gpt \
       mkpart ESP fat32 1MiB 513MiB \
       mkpart primary linux-swap 513MiB 4609MiB \
       mkpart primary ext4 4609MiB 100%

mkfs.fat -F32 /dev/sda1
mkfs.ext4 -F /dev/sda3

mount /dev/sda3 /mnt

mkswap /dev/sda2
swapon /dev/sda2

# Install core
pacman -Syy

pacstrap -i /mnt base base-devel linux linux-firmware --noconfirm

genfstab -U /mnt > /mnt/etc/fstab

# Configuration
arch-chroot /mnt

# Timezone
timedatectl set-timezone Europe/London

# Locale
locale-gen
echo LANG=en_GB.UTF-8 > /etc/locale.conf
export LANG=en_GB.UTF-8

# Bootloader
pacman -S grub efibootmgr --noconfirm --needed

mkdir /boot/efi
mount /dev/sda1 /boot/efi

grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=grub

grub-mkconfig -o /boot/grub/grub.cfg

# Install Xserver

pacman -S xorg-server xorg-xinit rxvt-unicode --noconfirm --needed

pacman -S virtualbox-guest-utils --noconfirm --needed

# Install i3

pacman -S i3-gaps ttf-jetbrains-mono ttf-material-design-icons --noconfirm --needed

# Install zsh and oh-my-zsh

pacman -S zsh --noconfirm

sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# Copy .zshrc

# Create user

useradd -m -g users -G wheel,storage,power -s /usr/bin/zsh tucker

passwd tucker

# Reset root password

passwd

# Install dhcpcd

pacman -S dhcpcd --noconfirm

systemctl enable dhcpcd

# Install yay
pacman -S --needed --noconfirm git base-devel
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si

# Install polybar
yay -S polybar

# Install spotify-tui

yay -S spotify-tui

# Install lightdm

yay -S lightdm lightdm-gtk-greeter

# Insttall Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
