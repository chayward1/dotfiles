{ config, pkgs, ... }:

let
  myIntelBusId = "PCI:0:2:0";
  myNvidiaBusId = "PCI:1:0:0";
  myNvidiaOffload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';

in {
  # Add the offload script to the $PATH.
  environment.systemPackages = [ myNvidiaOffload ];

  # Configure XDG compliance.
  environment.variables = {
    __GL_SHADER_DISK_CACHE_PATH = "$XDG_CACHE_HOME/nv";
    CUDA_CACHE_PATH = "$XDG_CACHE_HOME/nv";
  };

  # Enable the NVIDIA drivers.
  services.xserver.videoDrivers = [ "nvidia" ];

  # Fix screen tearing.
  services.xserver.screenSection = ''
    Option "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
    Option "AllowIndirectGLXProtocol" "off"
    Option "TripleBuffer" "on"
  '';

  # Configure `offload-mode'.
  hardware.nvidia.prime = {
    offload.enable = true;
    intelBusId = myIntelBusId;
    nvidiaBusId = myNvidiaBusId;
  };

  # Add OpenGL support.
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages32 = with pkgs; [
      pkgsi686Linux.libva
    ];
  };

  # Add user to video group.
  users.users.chris = {
    extraGroups = [ "video" ];
  };
}
