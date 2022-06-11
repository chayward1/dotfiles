#!/usr/bin/env pwsh
qemu-system-x86_64 -accel whpx,kernel-irqchip=off \
		   -hda .\nixos.qcow2 \
		   -m 4096 \
		   -net nic,model=virtio \
		   -net user \
		   -vga virtio \
		   -boot strict=on
