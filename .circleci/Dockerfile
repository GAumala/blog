# Set base image
FROM base/archlinux:latest

RUN pacman-db-upgrade
RUN pacman -Syyu --noconfirm

# circle ci dependencies
RUN pacman -S --noconfirm git openssh tar gzip ca-certificates

# my dependencies
# stack needs make & gcc during setup
RUN pacman -S --noconfirm make gcc nodejs yarn stack 

# Add utf-8 support so that hakyll can compile files with unicode chars
RUN echo 'en_US.UTF-8 UTF-8' > /etc/locale.gen \
    && locale-gen \
    && echo 'LANG=en_US.UTF-8' > /etc/locale.conf

ENV LANG en_US.UTF-8  


