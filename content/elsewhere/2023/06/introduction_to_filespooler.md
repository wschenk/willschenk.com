---
title: Introduction to Filespooler
date: 2023-06-03
origin: https://www.complete.org/introduction-to-filespooler/
type: link
author: John Goerzen
---

Filespooler is a tool for enforcing ordered execution of jobs that may arrive out of order, particularly useful for handling backups and synchronizing git repositories. It is designed to cooperate easily with transports that can be written to as a filesystem or piped to, and can use tools such as S3, Dropbox, Syncthing, NNCP, ssh, UUCP, USB drives, CDs, etc. as transport. Filespooler is written in Rust and is particularly suited for distributed and Asynchronous Communication.
