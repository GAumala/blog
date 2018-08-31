# Set base image
FROM base/archlinux:latest

RUN pacman-db-upgrade
RUN pacman -Syyu --noconfirm

# circle ci dependencies
RUN pacman -S --noconfirm git openssh tar gzip ca-certificates

# my dependencies
# stack needs make & gcc during setup
RUN pacman -S --noconfirm make gcc nodejs yarn stack 

ENTRYPOINT bash

