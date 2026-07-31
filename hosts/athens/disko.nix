{
  disko.devices.disk.main = {
    type = "disk";
    # Stable by-id path for the target disk; disko uses the whole device.
    device = "/dev/disk/by-id/nvme-eui.002538550143eda3";
    content = {
      type = "gpt";
      partitions = {
        ESP = {
          type = "EF00";
          size = "1G";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = ["umask=0077"];
          };
        };
        root = {
          size = "100%";
          content = {
            type = "filesystem";
            format = "ext4";
            mountpoint = "/";
          };
        };
      };
    };
  };
}
