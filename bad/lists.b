List<int> lista;

//lista.push true;   // <-- error

List<List<bool>> lista2;

// lista2.push int[];      // <-- error

lista2.push bool[];

// List<int> inner = lista2.at 0;     // <-- error

List<bool> inner = lista2.at 0;

//inner.push 1;     // <-- error

int e;
for e in inner do
    format "Element: _\n" e;
