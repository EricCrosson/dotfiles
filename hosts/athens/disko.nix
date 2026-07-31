{
  disko.devices.disk.main = {
    type = "disk";
    # REPLACE before step 7: run `ls -l /dev/disk/by-id` on athens and use the
    # stable by-id path for the target disk. Do not commit /dev/nvme0n1.
    device = "/dev/disk/by-id/REPLACE-ME";
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
