typedef unsigned int tuint;
typedef int tint;
typedef unsigned hyper tuhyper;
typedef hyper thyper;
typedef float tfloat;
typedef double tdouble;
typedef bool tbool;

typedef int tarray[1];
typedef int tuarray<>;
typedef int tvarray<1>;

typedef opaque topaque[1];
typedef opaque tuopaque<>;
typedef opaque tvopaque<1>;
typedef string tustring<>;
typedef string tvstring<1>;
typedef int *toptional;

typedef struct {
  int cint;
} tstruct;

typedef union switch(bool x) {
case TRUE: int cint;
default: void;
} teunion;

typedef union switch(int x) {
case 1:
case 2: int cint;
default: void;
} tiunion;

typedef tint talias;
