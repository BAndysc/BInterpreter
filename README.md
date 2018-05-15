# Język BK

## Opis rozwiązania

Interpreter opiera się głównie na dwóch monadach: `State` i `Reader`. `Reader` trzyma "środowisko" - mapę nazw zmiennych w ich lokację. `State` trzyma mapę lokacji w wartości "w pamięci" oraz następną wolną "lokację". Nieużywane lokacje nie sa zwalniane.

## Opis plików

 * `Types.hs` - wszystkie wykorzystane typy (poza type checkerem), opis w pliku
 * `Memory.hs` - pomocnicze funkcje do zarządzania stanem pamięci
 * `TypeChecker.hs` - moduł odpowiedzialny za statyczne typowanie
 * `Interpreter.hs` - właściwy interpreter
 * `main.hs` - plik początkowy interpretera

## Wymagania i kompilacja

Interpreter wymaga biblioteki `mtl`, został przetestowany na wersjach ghc 7.6.3 oraz 8.0.2.

```
    make
    ./interpreter
```

## Opis jęzka

### Program:

Program jest po prostu listą instrukcji oddzielonych średnikiem.

### Typy:

Dostępne są trzy typy proste: `int` (liczba całkowita), `bool` oraz `string`. Stałe liczbowe to po prostu `\-?[0-9]+`, stałe typu `bool` to `true` i `false`. Literały napisowe zapisujemy w cudzysłowach. Dodatkowo funkcje mogą zwracać typ `void`, czyli tak naprawdę nic nie zwracać.
Listy są typu `List<T>`, gdzie T jest dowolnym innym typem.
Funkcje anonimowe są typu: `Fun<[Typ zwracany]([lista typów przyjmowanych argumentów])>`.

(W kolejnej wersji dostępne będą struktury, ich typ to `struct [IDENTYFIKATOR]`)

Typowanie jest statyczne.

### Arytmetyka

Operatory arytmetyczne: +, -, *, /, %, minus unarny
Operatory porównania: <, <=, >, >=, ==, !=
Operatory logiczne: and, or, not


### if:

```
if [WARUNEK] then
{
	[Stmt]
}	
else
{
	[Stmt]
}
```

else jest opcjonalny. Warunek jest wyrażeniem, musi być typu `bool`. Nawiasy klamrowe są wymagane (w celu uniknięcia niejednoznaczności przy if _ then if _ then _ else _)

### while

```
while [WARUNEK] do
	Stmt
```

Warunek jest sprawdzany *przed* każdym obrotem pętli. Warunek jest wyrażeniem, musi być typu `bool`.

### for

```
for [ZMIENNA] from [WYRAŻENIE [OD]] to [WYRAŻENIE [DO]] do
	Stmt
```

Przed uruchomieniem pętli wyliczana jest wartość wyrażeń [od] i [do], muszą być typu int, instrukcja jest wykonywana ([DO] - [OD] + 1) razy, przed wykonaniem instrukcji na [zmienną] przypisana jest wartość wyrażenia. Gdy [DO] < [OD], to pętla nie jest wykonywana ani razu.

### for in
```
for [ZMIENNA] in [WYRAŻENIE]
	Stmt
```

[WYRAŻENIE] musi być typu `List<T>`, a [ZMIENNA] typu `T`. Instrukcja jest uruchamiane dla każdego elementu listy, przypisawszy wcześniej wartość tego elementu na [ZMIENNĄ].

### Funkcje
```
function NAZWA([ARGUMENT]) -> TYP_ZWRACANY
{
	[Stmt]
}
// ARGUMENT ::= (ref)? T nazwa
``` 
Funkcje mają nazwę, listę argumentów oddzielonych przecinkami i typ zwracany. Funkcje mogą przyjmować typy `int`, `bool`, `string`, `List<T>`, `struct [Ident]` oraz `Fun<T([S])>`. Takie też typy mogą zwrócić, dodatkowo mogą zwrócić typ `void`, co oznacza, że funkcja nic nie zwraca (jest "procedurą"). Dodatkowo argumenty fukcji mogą być poprzedzone modyfikatorem `ref` oznaczjącym, że argument przekazywany jest przez referencję. Domyślnie argumenty są przykazywane przez *wartość*.

##### Wołanie funkcji
```
	nazwa_funkcji([lista argumentów])
```

Wywołujemy funkcję podając jej identyfikator i w nawiasie listę argumentów. Jeśli argument w definicji funkcji poprzedzony jest modyfikatorem `ref`, to także podczas wołania funkcji należy przekazać *zmienną* z modyfikatorem `ref` (i naturalnie nie może to być wtedy wyrażenie, tylko identyfikator!). 

##### Funkcje anonimowe
```
	Fun<T([S])> = [C] ([S]) -> T { Stmt; }
```

`[S]` to lista argumentów przyjmowanych przez funkcję anonimową, analogicznie jak w funkcjach.
`T` to typ zwracany analogicznie jak w funkcjach.
`C` to lista identyfikatorów, tzw. "Capture Group", których *wartość* będzie widoczna w ciele lambdy. UWAGA: wartość tych zmiennych jest wyliczana w momencie deklaracji lambdy. Zatem kod:
```
	int x = 10;
	Fun<void()> f = [x] () -> void { print x };
	x = 20;
	f();
```
Spowoduje wypisanie wartości `10`. W ciele lambdy nie można się odwoływać do innych zmiennych niż te z `capture groupy` oraz argumentów.

##### Funkcje zagnieżdżone
Funkcja można zagnieżdżać, tzn. można deklarować funkcje w funkcji. Funkcja zagnieżdżona ma dostęp do `wszystkich` identyfikatorów widocznych w zewnętrzenej funkcji. Funkcje zagnieżdżone naturalnie nie są widoczne poza funkcją zewnętrzną.

### Print, format
Dwie instrukcje wbudowane slużące do wypisywania na ekran. 

##### print
```
	print [WYRAŻENIE]
```
Wylicza wartość wyrażenia i drukuje na standardowe wyjście.

##### format
```
	format [FORMAT_STRING] [WYRAŻENIA...]
```
Pierwszym argumentem jest `string` oznaczjący format. Specjalny znak `_` oznacza wzięcie kolejnego wyrażenia z listy wyrażeń i wypisania właśnie niego. Gdy podane jest więcej wyrażeń niż `_`, nic się nie dzieje, gdy jest mniej - na ekran wypisuje się znak `_`.

```
	format "_ + _ = _\n" 1, 2, 1 + 2
	// 1 + 2 = 3

	format "_ + _ = _\n" 1, 2
	// 1 + 2 = _
```

### Obsługa błędów
Występują dwa typy wyjątków:
 
  * typecheck exception - taki wyjątek jest zgłaszany podczas statycznego typowania

  * runtime exception - taki wyjątek jest zgłaszany podczas interpretowania programu (np. dzielenie przez 0)

### Listy
```
	List<T> lista;
	T t;
	lista.push t (-> void);
	lista::at [Integer] (-> T);
	lista::length (-> int);
```
Listy mogą przechowywać dowolny typ (także inne listy). Dostępne są trzy funkcje wbudowane: `push`, `at`, `length`. `Push` wkłada element na koniec listy, `at` zwraca element pod danym indeksem (gdy indeks jest poza zakresem to zgłaszany jest błąd wykonania), `length` zwraca długość listy.

### Struktury
[NIEDOSTĘPNE W TEJ ITERACJI]
```
	struct NAZWA
	{
		TYP_POLA [NAZWA_POLA];
		TYP_POLA_2 [NAZWA_POLA_2];
		.
		.
	}
```
Definicja struktury składa się z listy nazwy pól wraz z ich typem. 


### Cechy języka

```
                                           15 maja       12 czerwca
1 (dwa typy) int, bool                        +       
2 (arytmetyka, porównania)                    +           
3 (while, if)                                 +
4 (procedury lub funkcje, rekurencja)         +                     
5 (print)                                     +
6 a) (przez zmienną i wartość)                +
  b) (pętla for)                              +
7 (statyczne typowanie)                       +       
8 (przesłanianie i statyczne wiązanie)        +                     
9 (obsługa błędów wykonania)                  +           
10 (funkcje zwracające wartość)               +                
   b) (listy)                                 +                
   c) (rekordy)                                               +
   f) (funkcje jako parametry)                +                
   h) (funkcje anonimowe)                     +                
12 (funkcje zagnieżdżone                      +                
	ze statycznym wiązaniem)                         
```
