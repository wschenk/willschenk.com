(use-modules (gnu) (nongnu packages linux))
(use-service-modules desktop networking ssh xorg)

(define this-file
  (local-file (basename (assoc-ref (current-source-location) 'filename))
	      "config.scm"))

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
   (list
    (specification->package "nss-certs"))
   %base-packages))
  
 
 (services
  (append
   (list
    ;; Copy current config to /etc/config.scm
    (simple-service 'config-file etc-service-type
		    `(("config.scm" ,this-file)))
    (service gnome-desktop-service-type)
    (service openssh-service-type)
    (set-xorg-configuration
     (xorg-configuration
      (keyboard-layout keyboard-layout))))
   %desktop-services))
 
 (file-systems (cons* (file-system
		       (device (file-system-label "guix"))
		       (mount-point "/")
		       (type "ext4"))
;; Not needed for bootable usb but needed for final system
;;		      (file-system
;;		       (device "/dev/nvme0n1p1")
;;		       (type "vfat")
;;		       (mount-point "/boot/efi"))
		      (file-system
		       (mount-point "/tmp")
		       (device "none")
		       (type "tmpfs")
		       (check? #f))
		      %base-file-systems)))

