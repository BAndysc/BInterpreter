//     c) (rekordy)

//struct Point {
//	int x;
//	int y;
//};

//struct Player {
//	struct Point position;
//	int health;
//};



function Move(ref struct Player player, int offsetX, int offsetY) -> void
{
	player.position.x = (player).position.x + offsetX;
	player.position.y = (player).position.y + offsetY;
};

struct Player player;

player.health = 10;
player.position = new Struct;
player.position.x = 15;
player.position.y = 3;

struct Point p;
p.x = 3;
p.y = 10;

//player.position = p;

format "start: (_, _)\n" (player).position.x, (player).position.y;

Move(ref player, -1, 1);

format "after move (-1, 1): (_, _)\n" (player).position.x, (player).position.y;

int i = 0;

for player.position.x from 0 to (player).position.x do
	i = i + 1;

format "_\n_\n" (player).position.x, i;

function test() -> int
{
	return 1;
};

int x = test();
format "Oto wartosc zwrocona: _\n\n\n" x;

function GenStruct() -> struct St
{
	struct St ss;
	ss.x = 1;
	return ss;	
};

format "anon struct: _\n" (GenStruct()).x;

function stworz_zwierzaka(string gatunek) -> struct Zwierz
{
	struct Zwierz z;
	z.gatunek = gatunek;
	z.odglos = [gatunek]() -> void { format "Generyczny odglos _\n" gatunek;};
	return z;
};

function stworz_kota() -> struct Zwierz
{
	struct Zwierz z = stworz_zwierzaka("Kot");
	z.odglos = [z]() -> void { print "Miau!\n"; };
	return z;
};

struct Zwierz z = stworz_zwierzaka("Pies");
(z).odglos();

z = stworz_kota();
(z).odglos();