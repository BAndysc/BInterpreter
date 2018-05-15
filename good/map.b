function pusta() -> Fun<int(int)>
{
    return [ ] (int i) -> int { return -1; };
};

function mapa_dodaj(Fun<int(int)> map, int klucz, int wartosc) -> Fun<int(int)>
{
    return [map, klucz, wartosc] (int i) -> int
    {
        if i == klucz then 
        {
            return wartosc;
        }
        else
        {
            return map(i);
        }
    };
};

Fun<int(int)> mapa_kwadratow = pusta();
int i;
for i from 1 to 50 do
    mapa_kwadratow = mapa_dodaj(mapa_kwadratow, i, i*i);

int x = mapa_kwadratow(21);

print x;
