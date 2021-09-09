#!/bin/bash

albatross-client-local create signer signer.hvt \
	--net=service:br1 \
	--block=certificate:certificate-signer \
	--arg="--ipv4=192.168.1.5/24" \
	--arg="--ipv4-gateway=192.168.1.2" \
	--arg="--dns-server=192.168.1.3" \
        --arg="--dns-key=personal._update.osau.re:SHA256:cdVGTIJq+Ht7nzr2ZG0fKxE4FAqOjXmsLR1srKCKh8E=" \
	--arg="--selector=ptt" \
	--arg="--domain=osau.re" \
	--arg="--private-key=tPIv8iEGGl1BBUgzdsWv1eJV6nd8vIuHiG0ccsNYdxs=" \
	--arg="--destination 192.168.1.6" \
	--arg="--postmaster hostmaster@osau.re" \
	--arg="--cert-fingerprint osau.re:SHA256:xD70Gj2fkgkyJZFA9RywcWGnH2e/d/eiP/fSlanCe3Y=" \
	--arg="-l debug"
