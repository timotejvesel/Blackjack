## Blackjack
Blackjack je ena izmed najbolj popularnih igralniških iger. Gre za igro s kartami, pri kateri igralec oziroma igralci tekmujejo proti delivcu (ang. dealer), pri čemer igralci ne tekmujejo med seboj. Pri igri se uporabi en ali več paketov 52 igralnih kart. Čeprav navzgor ni omejtive, se ponavadi uporabi od 6 do 8 paketov. Igralec stavi, da je njegova vsota kart višja kot dealerjeva, pri čemer vsota ne sme preseči 21 ("bust"). 

### Osnovna pravila
Igralec in dealer dobita dobita vsak dve karti, pri čemer sta obe igralčevi obrnjeni navzgor, dealerjevi pa ena navzgor ter ena navzdol. Vrednost kart od 2 do 10 je kar njihova vrednost, fant, dama in kralj so vredni 10, as pa je lahko vreden 1 ali pa 11. Vrednost roke (ang. hand value) je vsota vseh kart v roki. Igralec lahko vzame nove karte, da s tem izboljša vrednost roke. Roka z asom, vrednim 11, se imenuje "soft" (mehka), kar pomeni, da če igralec vzame novo karto in vrednost roke preseže 21, se bo vrednost asa spremenila iz 11 v 1. Sicer se roka imenjue "hard" (trda). 

Ko igralec zaključi, je na vrsti dealer, ki jemlje nove karte (razen, če igralec preseže 21), dokler vrednost njegove roke ni vsaj 17. Rezultat igre je sledeč:
* če igralec preseže 21, izgubi (tudi, če bi dealer presegel 21),
* če dealer preseže 21 in igralec ne, igralec zmaga,
* če je vrednost igralčeve roke večja od vrednosti dealerjeve roke (in igralec ne preseže 21), igralec zmaga,
* če je vrednost igralčeve roke večja od vrednosti dealerjeve roke (in dealer ne preseže 21), igralec izgubi,
* če imata njuni roki enaki vrednosti (manjša od 21), nihče ne zmaga.

### Izbira pravil
Vsak kazino izbere svoja pravila igre, tako da obstaja več kot 100 variacij igre.
Kot pri vseh igralniških igrah, tudi pri Blackjacku igralec na dolgi rok proti igralnici izgubi ("house edge"). Izbira pravila je za igralnico zelo pomembna, saj je višina house edga odvisna predvsem od njih. Prednost dealerja je v tem, da če vrednost igralčeve roke preseže 21, bo igralec v vsakem primeru izgubil (tudi če bi dealer nato presegel 21).

Tako kot kazinoji sem moral za obravnavo igre tudi sam najprej določiti pravila. Trenutna pravila:
 * osnovna hit & stand,
 * igralec lahko na začetku podvoji stavo in vzame natanko 1 karto (ang. double),
 * dealer ne vzame nove karte pri soft 17,
 * blackjack (natural 21; prvi dve karti imata vsoto 21) lahko izplača 3:2, 6:5 ali 1:1,
 * 4-8 paketov kart.
 
Včasih so vsi kazinoji za blackjack izplačevali 3:2, v zadnjem času pa nekateri izplačujejo 6:5 ali pa celo 1:1 kar je za igralca veliko slabše.

### "Optimalna" strategija
Vsaka igra Blackjacka ima *"optimalno"* oziroma *osnovno strategijo*, ki določi optimalen način igranja igralca za vsako roko proti poljubni navzgor obrnjeni karti dealerja. S tem igralec minimizira (dolgoročno) prednost hiše (ang. house edge) oziroma svojo pričakovano izgubo.
Izraz optimalna strategija pa je lahko nekoliko zavajajoč, saj ta strategija minimizira prednost hiše le, če igralec igra "pošteno" (ne šteje kart,...). 
Strategijo lahko dobimo na več načinov, sam pa sem jo dobil s pomočjo Monte Carlo simulacij. Za vsako kombinacijo vrednosti igralčeve roke (hard in soft) in odkrito dealerjevo karto sem tako določil najboljšo potezo, ki jo lahko naredi igralec. 

### House edge
House edge je prednost hiše oziroma izguba igralca na dolgi rok. House edge oziroma njegov približek lahko izračunamo s pomočjo MC simulacij kot 
*HE = VsotaIzgubljenegaDenarja / VsotaZačetnihStav*

Pri tem je potrebno paziti, da je v imenovalcu res vsota začetnih stav in ne vsota vseh stav (npr. pri "double" začetno stavo podvojimo).

Višina house edga je precej odvisna od pravil igre.

### Štetje kart
Štetje kart je (v kombinaciji z optimalno strategijo) strategija s katero lahko igralec zmanjša house edge. Obstaja več načinov štetja kart. Vsem je skupno, da igralec vsaki karti določi neko vrednost. Ko je neka karta izvlečena števec poveča oziroma zmanjša za njeno vrednost. Ko je vrednost števca nizka, stavi manj, ko pa je vrednost večja, pa več denarja. Igralec, ki šteje karte je še v večji prednosti, če se igra z manj paketi kart, saj je v največji prednosti, ko so karte pri koncu. 

#### Hi-Lo
Pri tej metodi štetja se kartam določi sledeče vrednosti:
 * 2, 3, 4, 5, 6: +1
 * 7, 8, 9: 0
 * 10, J, Q, K, A: -1
 
S tem dobimo tako imenovani *running count*. Ker pa se ponavadi igra z več paketi kart, pri tej metodi poterbujemo še t.i. *true count*. Dobimo ga tako, da running count delimo s številom paketov, ki so še ostali. 

Hi-Lo je uravnotežen sistem ("balanced system") kar pomeni, da začnemo štetje z 0, ko preštejemo vse karte iz enega paketa pa mora running count spet imeti vrednost 0.

#### Hi-Opt II
Ta metoda je nekoliko bolj zahtevna, vendar tudi učinkovitejša. Kartam se določi naslednje vrednosti:

 * 2, 3, 6, 7: +1
 * 4, 5: +2
 * 10, J, Q, or K: -2
 * A, 8, 9: 0

Prav tako je to uravnotežen sistem. Poleg tega moramo tudi tu izračunati true count. 