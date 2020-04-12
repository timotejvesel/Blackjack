## House edge
House edge (HE) je prednost hiše oziroma izguba igralca na dolgi rok. House edge oziroma njegov približek lahko izračunamo s pomočjo Monte Carlo simulacij kot 
*HE = VsotaIzgubljenegaDenarja / VsotaZačetnihStav*

Pri tem je potrebno paziti, da je v imenovalcu res vsota začetnih stav in ne vsota vseh stav (npr. pri "double" začetno stavo podvojimo).

### Skupne nastavitve pravil & opombe
 * Natural 21: koliko bo izplačal Blackjack oziroma Natural 21, to je, ko je vrednost začetne igralčeve roke 21.
 * Stevilo paketov kart: igra se lahko igra s 4, 6 ali pa 8 paketi. 
 * Stevilo iteracij: izračun z 100 000 iteracijami igre traja približno 10-15 sekund, vendar približek HE ni najboljši. Izračun z 1 milijonom iteracijami pa traja ustrezno dlje, vendar je približek, ki ga dobimo precej dober. Simulacija z 10 000 iteracijami igre pa nam da preslab približek za HE in je v nekaterih primerih uporabna samo za okvirne primerjave house edgov različnih strategij ter pravil.
 * Pozitiven house edge pomeni, da je (dolgoročno) v prednosti igralec.

#### 'Slaba' strategija
Pri tej strategiji igralec igra podobno kot dealer, tj. nove karte jemlje dokler vrednost njegove roke ne doseže neke meje. Posledično je njegova strategija tudi neodvisna od odkrite karte dealerja. 
Izberete lahko mejo do katere igralec jemlje nove karte.

#### 'Optimalna' strategija
Igralec igra (osnovo) optimalno strategijo. Glede na vrednost svoje roke ter odkrito dealerjevo karto naredi potezo, ki je optimalna.

#### Štetje kart
Igralec še vedno igra (osnovno) optimalno strategijo, poleg tega pa še šteje karte. Pri tem lahko izberete še način štetja.