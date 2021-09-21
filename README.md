# PTT - Poste, Télégraphe et Téléphone

`ptt` is a suit of tools to deploy a full SMTP service. The current available
design is:

```
                                         [ Primary DNS server ]
					           |
                                                   |
 *:465 [ Submission server ] -- TLS --> *:25 [ DKIM signer ] -.
                |                                             |
                ----------------------- [ SMTP relay ] *:25 <-'
       [ Git Database ]                    |  |
			 [ DNS Resolver ] -'  `-> *:25 [ Internet ]
```

3 _unikernels_ (with [MirageOS][mirage]) exist:
- a submission server (LIPAP) which must be synchronized with a database
  (such as [irmin][irmin] x [git][ocaml-git]) to authentify users
- a DKIM signer (NEC) which requires a private RSA key and update the
  primary DNS server of the domain-name with the public RSA key. It signs
  incoming emails and pass them then to the SMTP relay
- the SMTP relay (MTI-GF) which will resolves recipients _via_ a specific
  DNS resolver and send incoming emails to destinations.

Even if we use the `*:25` to communicate between each _unikernels_, all of them
start a TLS session. Only yhe SMTP relay requires a DNS resolution process and
the DKIM private key is never shared to anyone - only the public key is sended
to the primary DNS server to let recipients to verify outcoming emails.

This is the usual design of an SMTP service **to be able to send an email**.
The reception of an email does not exists - and it's not the purpose of `ptt`
which does not (yet) implement IMAP.

## How to use?

`ptt` provides these unikernels and they can be deployed as a simple MirageOS:
- as a simple executable (see `mirage configure -t unix`)
- as a full operating system

Currently, `ptt` is used as a full operating system with the `hvt` target. We
deploy these _unikernels_ via KVM & Solo5. DNS resolver and primary DNS server
are _unikernels_ too (see [robur.io][roburio] -- but, as far as we can say,
this SMTP stack is usable with a simple DNS server (as far as it implements
`nsupdate`).

## Experimental

`ptt` is not yet stable or reliable but we currently try to deploy/use it
and improve it. Don't use it!

## Credits

`ptt` has received funding from the Next Generation Internet Initiative (NGI)
within the framework of the DAPSI Project.
