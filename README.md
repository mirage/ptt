# Postes, Télégraphes et Téléphones

[PTT][ptt] is a project to play with mails. The goal is to provide a UNIX tool
which use [mrmime][mrmime] to parse, extract informations, check & verify them
and provide some ways to send mails.

## [MTI-PF][mti-pf]

MTI-PF verify our mails from a `maildir` database. It will launch
[mrmime][mrmime] on your mails and see if we can compute them or not. It's a PoC
to validate implementation of [mrmime][mrmime] mostly.

[mrmime]: https://github.com/mirage/mrmime.git
