{ config, pkgs, ... }:

let
  myIntelBusId = "PCI:0:2:0";
  myNvidiaBusId = "PCI:1:0:0";
  myNvidiaOffload = pkgs.writeShellScriptBin "dotfiles-nvidia-offload" ''
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
  # NOTE: You may need to use either of the commands below:
  services.xserver.videoDrivers = [ "nvidia" ];
  # services.xserver.videoDrivers = [ "modesetting" "nvidia" ];

  # Configure `offload-mode'.
  hardware.nvidia.prime = {
    offload.enable = true;
    intelBusId = myIntelBusId;
    nvidiaBusId = myNvidiaBusId;
  };
}
