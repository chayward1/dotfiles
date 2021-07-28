# <<file-warning>>
{ config, pkgs, ... }:

let
  # Keep garbage out of the home directory.
  mySteamDir = "$XDG_DATA_HOME/steam";

  # Custom shim for running steam.
  mySteam = pkgs.writeScriptBin "steam" ''
    HOME="${mySteamDir}" exec ${pkgs.steam}/bin/steam "$@"
  '';

  # Run applications using the steam libraries.
  mySteamRun = pkgs.writeScriptBin "steam-run" ''
    HOME="${mySteamDir}" exec ${pkgs.steam-run-native}/bin/steam-run "$@"
  '';

in {
  # Install custom shims.
  environment.systemPackages = [
    mySteam
    mySteamRun
  ];

  # Create the steam directory.
  system.userActivationScripts.setupSteamDir = ''
    mkdir -p ${mySteamDir}
  '';

  # Required hardware configuration(s).
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.steam-hardware.enable = true;

  # Increase performance for proton games.
  systemd.extraConfig = "DefaultLimitNOFILE=1048576";
}
