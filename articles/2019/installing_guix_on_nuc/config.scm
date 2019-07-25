(use-modules (gnu) (nongnu packages linux))
(use-service-modules desktop networking ssh xorg)

(operating-system
 (kernel linux)
 (locale "en_US.utf8")
 (host-name "intelnuc")
 (timezone "America/New_York")

 (keyboard-layout (keyboard-layout "us" "altgr-intl"))

 ;; This will be what is used on the target machine
 ;;  (bootloader (bootloader-configuration
 ;;              (bootloader grub-efi-bootloader)
 ;;              (timeout 1)
 ;;              (target "/boot/efi")))

 ;; This is needed to create a bootable USB
 (bootloader (bootloader-configuration
              (bootloader grub-bootloader)
              (target "/dev/sda")))
 
 (firmware (append (list iwlwifi-firmware)
		   %base-firmware))
 
 (users (cons* (user-account
		(name "wschenk")
		(group "users")
		(supplementary-groups '("wheel" "netdev" "audio" "lp" "video"))
		;; TODO: Default to name?
		(home-directory "/home/wschenk"))
	       %base-user-accounts))
 
 (packages
  (append
   (list (specification->package "i3-wm")
	 (specification->package "i3status")
	 (specification->package "dmenu")
	 (specification->package "st")
	 (specification->package "nss-certs"))
   %base-packages))
  
 
 (services
  (append
   (list (service gnome-desktop-service-type)
	 (service openssh-service-type)
	 (set-xorg-configuration
	  (xorg-configuration
	   (keyboard-layout keyboard-layout))))
   %desktop-services))
 
 (file-systems (cons* (file-system
		       (device (file-system-label "guix"))
		       (mount-point "/")
		       (type "ext4"))
		      (file-system
		       (mount-point "/tmp")
		       (device "none")
		       (type "tmpfs")
		       (check? #f))
		      %base-file-systems)))

