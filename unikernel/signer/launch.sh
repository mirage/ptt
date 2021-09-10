#!/bin/bash

CERT_FGP="osau.re:SHA256:C3wx1vJSWjE/4P6HeDc/qFwFws95GVo9iWdvM7zbtXM="
CERT_KEY="+ZNBSdekm6J4e79mStLlc4YE1TjDUt1YLeGGTJX3nRg="

CERT_DER=""
CERT_DER+="MIIDhTCCAm2gAwIBAgIBADANBgkqhkiG9w0BAQsFADAtMQwwCgYDVQQDDANQVFQxHTAbBgNVBAMMFEVw"
CERT_DER+="aGVtZXJhbCBDQSBmb3IgUFRUMB4XDTIxMDkwOTE3MjkzN1oXDTIyMDkwOTE3MjkzN1owLTEMMAoGA1UE"
CERT_DER+="AwwDUFRUMR0wGwYDVQQDDBRFcGhlbWVyYWwgQ0EgZm9yIFBUVDCCASIwDQYJKoZIhvcNAQEBBQADggEP"
CERT_DER+="ADCCAQoCggEBAL+d1qT3RTjMwtFMlsHQc1D4SauZXsnk31/XNiARAgzCUJQYjdk4LwQr4OFQw0e3pw78"
CERT_DER+="c9zMKYfthWGktHdvnfiCaXHKyeN5wSwvBOThiFTQw74O/awKEZ2GtzauFEsg8JbtzmGHsBaQtMiShMhO"
CERT_DER+="eHFf93ONpQ9qiXBtY+VjDcnAtz3WUDhUNlqQDWcXcKROCca51VuwktYqs3zePdsv4Y9iEc/pmeZD+WaJ"
CERT_DER+="p+9xodI/da4dtkNwSrcgqHhXnK1NfO+93Y+FM5vTGWXy4SmlAYOTNFrsToLK8wutoNYyBHLF4rTl+MNe"
CERT_DER+="Fo08wvHIpq/k00Ojz29DJXb92e9WOnh7WkkCAwEAAaOBrzCBrDAdBgNVHQ4EFgQUQ8JepmC263x3HsS/"
CERT_DER+="LUeSrQWgnlIwDwYDVR0PAQH/BAUDAwfgADAVBgNVHREBAf8ECzAJggdvc2F1LnJlMAwGA1UdEwEB/wQC"
CERT_DER+="MAAwVQYDVR0jBE4wTIAUQ8JepmC263x3HsS/LUeSrQWgnlKhMaQvMC0xDDAKBgNVBAMMA1BUVDEdMBsG"
CERT_DER+="A1UEAwwURXBoZW1lcmFsIENBIGZvciBQVFSCAQAwDQYJKoZIhvcNAQELBQADggEBAEA3jDypZUMQcowx"
CERT_DER+="cO5xkBU4ovORX1eNs0FY4uSbScJ0IV3xXEnQSjec/pAc5Pqcg8lwD8Nv/sIBOzYr974z/zoRZ8eVi3Uw"
CERT_DER+="t2HT0AE0LfUwbA/JFkA36j35XLJPPusX75ylweZmr5my/QSW7oHXIO+W/D4QTlan5ZG1n0CkKNbo3PdE"
CERT_DER+="eVRzXJuWutmJiDggNvjgEWLcjcQmuIADjhYT1EhCL4kQ/OB3n9iVO4OeVMPObos1CTU8zvIMLjgLOluj"
CERT_DER+="yUXPNKasw64QFVuCy1Y7BKq1BCHILMP8oMzaTIFjTP8TkdzUWKzcg3SKasBz09JPnXWKWGg42xpD9Pvf"
CERT_DER+="zRAPzMo="

albatross-client-local create signer signer.hvt \
	--net=service:br1 \
	--arg="--ipv4=192.168.1.5/24" \
	--arg="--ipv4-gateway=192.168.1.2" \
	--arg="--dns-server=192.168.1.3" \
        --arg="--dns-key=personal._update.osau.re:SHA256:cdVGTIJq+Ht7nzr2ZG0fKxE4FAqOjXmsLR1srKCKh8E=" \
	--arg="--selector=ptt" \
	--arg="--domain=osau.re" \
	--arg="--private-key=tPIv8iEGGl1BBUgzdsWv1eJV6nd8vIuHiG0ccsNYdxs=" \
	--arg="--destination 192.168.1.6" \
	--arg="--postmaster hostmaster@osau.re" \
	--arg="--cert-fingerprint=osau.re:SHA256:MKXWSBC/CWPg2jsAatdBfZzf0gRSpEVGu997udkJa9A=" \
	--arg="--cert-key=$CERT_KEY" \
	--arg="--cert-der=$CERT_DER" \
	--arg="-l debug"
