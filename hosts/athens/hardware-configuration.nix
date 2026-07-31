# Placeholder. Overwritten by:
#   nixos-anywhere --generate-hardware-config nixos-generate-config \
#     ./hosts/athens/hardware-configuration.nix
# Contains no fileSystems: disko owns those (see ./disko.nix).
{modulesPath, ...}: {
  imports = [(modulesPath + "/installer/scan/not-detected.nix")];
  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usbhid" "sd_mod"];
  boot.kernelModules = ["kvm-intel"];
}
