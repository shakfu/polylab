from base/archlinux:latest

# Update Packages & Upgrade System
RUN pacman -Syyu --noconfirm

# Install Salt Dependencies
RUN pacman -Sy --noconfirm \
  python2 \
  python2-yaml
