# Derive from the official image.
FROM nixos/nix

# Add the unstable channel.
RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update

# Setup the default environment.
WORKDIR /etc/dotfiles
COPY . .

# Load the default system shell.
RUN nix-shell -p
