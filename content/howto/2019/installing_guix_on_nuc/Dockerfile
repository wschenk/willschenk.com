FROM debian:stretch-slim

RUN apt-get update && apt-get install -y wget gpg xz-utils make netbase

RUN groupadd guixbuild; groupadd kxm; \
  for i in `seq -w 1 10`; \
  do \
    useradd -g guixbuild -G guixbuild,kxm       \
            -d /var/empty -s `which nologin`    \
            -c "Guix build user $i" --system    \
            guixbuilder$i; \
  done

RUN wget https://sv.gnu.org/people/viewgpg.php?user_id=15145 -qO - | gpg --import -
RUN wget https://ftp.gnu.org/gnu/guix/guix-binary-1.0.1.x86_64-linux.tar.xz -q

RUN cd /tmp && tar --warning=no-timestamp -xf \
	/guix-binary-1.0.1.x86_64-linux.tar.xz && \
	mv var/guix /var/ && mv gnu /

RUN mkdir -p ~root/.config/guix && ln -sf /var/guix/profiles/per-user/root/current-guix \
	~root/.config/guix/current

RUN mkdir -p /usr/local/bin; cd /usr/local/bin; ln -s /var/guix/profiles/per-user/root/current-guix/bin/guix

RUN bash -c "source /root/.config/guix/current/etc/profile && guix archive --authorize < \
     ~root/.config/guix/current/share/guix/ci.guix.gnu.org.pub"

CMD bash
