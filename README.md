# Postes, Télégraphes et Téléphones

[PTT][ptt] is a project to play with mails. The goal is to provide a UNIX tool
which use [mrmime][mrmime] to parse, extract informations, check & verify them
and provide some ways to send mails.

## [MTI-PF][mti-pf]

MTI-PF verify our mails from a `maildir` database. It will launch
[mrmime][mrmime] on your mails and see if we can compute them or not. It's a PoC
to validate implementation of [mrmime][mrmime] mostly.

[ptt]: https://en.wikipedia.org/wiki/Postes,_t%C3%A9l%C3%A9graphes_et_t%C3%A9l%C3%A9phones_(France)
[mrmime]: https://github.com/mirage/mrmime.git
[mti-pf]: https://www.docaufutur.fr/wp-content/uploads/2017/02/IMG_1176-MTIPF.jpg
