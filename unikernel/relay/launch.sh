#!/bin/bash

albatross-client-local create relay relay.hvt \
	--net=service:br1 \
	--block=certificate:certificate-relay \
	--arg="--ipv4=192.168.1.6/24" \
	--arg="--ipv4-gateway=192.168.1.2" \
	--arg="--remote git@192.168.1.2:smtp.git" \
	--arg="--ssh-seed Wg1CrQ4eb4UB4K4uN1avrUQMstI41drvmxBg1NhG" \
	--arg="--resolver tcp://192.168.1.7:53/" \
	--arg="--domain osau.re" \
	--arg="--postmaster hostmaster@osau.re" \
	--arg="-l debug"
