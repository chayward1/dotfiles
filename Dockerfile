# This file is controlled by /etc/dotfiles/README.org

# Derive from the official image.
FROM nixos/nix

# Setup the default environment.
WORKDIR /etc/dotfiles
COPY . .

# Load the default system shell.
RUN nix-shell /etc/dotfiles/shell.nix
