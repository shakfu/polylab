from base/arch

# Create a pacman .d directory so we can just override salt's included configuration if needed
RUN [ -d /etc/pacman.d ] || mkdir -p /etc/pacman.d

# Update Packages & Upgrade System
RUN pacman -Syyu --noconfirm

# Install Salt Dependencies
RUN pacman -Sy --noconfirm \
  python2 \
  python2-yaml
