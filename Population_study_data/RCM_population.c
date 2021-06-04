#include "RCM.h"
#include "RCM_population.h"

static int population_counts[32][32][5] =
{ { /* Male stars */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {32, 8, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {21, 0, 19, 0, 0},
    {0, 15, 26, 0, 0},
    {17, 4, 19, 0, 0},
    {0, 0, 0, 0, 0},
    {26, 0, 0, 14, 0},
    {0, 21, 0, 19, 0},
    {23, 5, 0, 12, 0},
    {0, 0, 21, 19, 0},
    {16, 0, 15, 9, 0},
    {0, 8, 25, 7, 0},
    {11, 4, 18, 7, 0},
    {0, 0, 0, 0, 0},
    {31, 0, 0, 0, 9},
    {0, 24, 0, 0, 16},
    {20, 12, 0, 0, 8},
    {0, 0, 32, 0, 8},
    {17, 0, 16, 0, 7},
    {0, 8, 27, 0, 5},
    {15, 6, 16, 0, 4},
    {0, 0, 0, 28, 12},
    {20, 0, 0, 13, 7},
    {0, 13, 0, 21, 6},
    {20, 5, 0, 11, 4},
    {0, 0, 21, 12, 7},
    {14, 0, 12, 9, 5},
    {0, 6, 25, 6, 3},
    {10, 4, 10, 13, 3} },
  { /* Female stars */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {22, 18, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {26, 0, 14, 0, 0},
    {0, 22, 18, 0, 0},
    {17, 17, 6, 0, 0},
    {0, 0, 0, 0, 0},
    {25, 0, 0, 15, 0},
    {0, 24, 0, 16, 0},
    {16, 16, 0, 8, 0},
    {0, 0, 24, 16, 0},
    {15, 0, 10, 15, 0},
    {0, 17, 8, 15, 0},
    {17, 11, 6, 6, 0},
    {0, 0, 0, 0, 0},
    {22, 0, 0, 0, 18},
    {0, 19, 0, 0, 21},
    {16, 15, 0, 0, 10},
    {0, 0, 21, 0, 19},
    {14, 0, 7, 0, 19},
    {0, 18, 11, 0, 12},
    {15, 9, 9, 0, 7},
    {0, 0, 0, 15, 25},
    {23, 0, 0, 10, 7},
    {0, 18, 0, 10, 12},
    {12, 15, 0, 6, 7},
    {0, 0, 8, 12, 20},
    {21, 0, 1, 15, 3},
    {0, 15, 9, 11, 5},
    {11, 15, 4, 6, 4} },
  { /* Films */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {17, 23, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {27, 0, 13, 0, 0},
    {0, 23, 17, 0, 0},
    {17, 17, 6, 0, 0},
    {0, 0, 0, 0, 0},
    {27, 0, 0, 13, 0},
    {0, 29, 0, 11, 0},
    {11, 27, 0, 2, 0},
    {0, 0, 21, 19, 0},
    {20, 0, 13, 7, 0},
    {0, 15, 15, 10, 0},
    {15, 14, 8, 3, 0},
    {0, 0, 0, 0, 0},
    {21, 0, 0, 0, 19},
    {0, 19, 0, 0, 21},
    {12, 13, 0, 0, 15},
    {0, 0, 27, 0, 13},
    {15, 0, 10, 0, 15},
    {0, 16, 10, 0, 14},
    {7, 14, 12, 0, 7},
    {0, 0, 0, 5, 36},
    {16, 0, 0, 4, 21},
    {0, 21, 0, 5, 14},
    {8, 17, 0, 4, 11},
    {0, 0, 15, 6, 19},
    {12, 0, 9, 5, 14},
    {0, 12, 8, 5, 15},
    {7, 11, 5, 3, 14} },
  { /* Star pairs */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {25, 15, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {16, 0, 24, 0, 0},
    {0, 17, 23, 0, 0},
    {13, 11, 16, 0, 0},
    {0, 0, 0, 0, 0},
    {27, 0, 0, 13, 0},
    {0, 22, 0, 18, 0},
    {24, 9, 0, 7, 0},
    {0, 0, 19, 21, 0},
    {11, 0, 19, 10, 0},
    {0, 9, 22, 10, 0},
    {17, 5, 15, 3, 0},
    {0, 0, 0, 0, 0},
    {22, 0, 0, 0, 18},
    {0, 20, 0, 0, 20},
    {21, 13, 0, 0, 6},
    {0, 0, 27, 0, 13},
    {18, 0, 16, 0, 6},
    {0, 19, 11, 0, 10},
    {13, 11, 8, 0, 8},
    {0, 0, 0, 16, 24},
    {17, 0, 0, 10, 13},
    {0, 14, 0, 8, 18},
    {17, 11, 0, 8, 5},
    {0, 0, 18, 14, 8},
    {14, 0, 9, 12, 5},
    {0, 6, 19, 6, 9},
    {11, 5, 13, 6, 5} },
  { /* Pizzas */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {15, 26, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {23, 0, 17, 0, 0},
    {0, 22, 18, 0, 0},
    {6, 18, 16, 0, 0},
    {0, 0, 0, 0, 0},
    {27, 0, 0, 13, 0},
    {0, 21, 0, 19, 0},
    {6, 23, 0, 11, 0},
    {0, 0, 17, 23, 0},
    {14, 0, 13, 13, 0},
    {0, 21, 13, 6, 0},
    {5, 19, 6, 10, 0},
    {0, 0, 0, 0, 0},
    {17, 0, 0, 0, 23},
    {0, 29, 0, 0, 11},
    {8, 25, 0, 0, 7},
    {0, 0, 21, 0, 19},
    {7, 0, 15, 0, 18},
    {0, 26, 6, 0, 8},
    {7, 19, 12, 0, 3},
    {0, 0, 0, 16, 24},
    {8, 0, 0, 18, 14},
    {0, 18, 0, 10, 12},
    {4, 15, 0, 12, 9},
    {0, 0, 6, 18, 16},
    {9, 0, 10, 9, 12},
    {0, 23, 5, 9, 3},
    {4, 15, 8, 8, 5} },
  { /* Juices */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {15, 25, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {22, 0, 18, 0, 0},
    {0, 30, 10, 0, 0},
    {15, 14, 11, 0, 0},
    {0, 0, 0, 0, 0},
    {26, 0, 0, 14, 0},
    {0, 25, 0, 15, 0},
    {21, 15, 0, 4, 0},
    {0, 0, 29, 11, 0},
    {19, 0, 13, 8, 0},
    {0, 28, 7, 5, 0},
    {10, 20, 5, 5, 0},
    {0, 0, 0, 0, 0},
    {24, 0, 0, 0, 16},
    {0, 24, 0, 0, 16},
    {11, 18, 0, 0, 11},
    {0, 0, 21, 0, 19},
    {13, 0, 19, 0, 8},
    {0, 16, 13, 0, 12},
    {11, 11, 7, 0, 11},
    {0, 0, 0, 15, 25},
    {13, 0, 0, 7, 20},
    {0, 22, 0, 8, 11},
    {14, 14, 0, 4, 8},
    {0, 0, 22, 6, 12},
    {9, 0, 10, 11, 10},
    {0, 16, 7, 6, 11},
    {5, 12, 6, 6, 11} },
  { /* Colours */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {21, 19, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {28, 0, 12, 0, 0},
    {0, 37, 3, 0, 0},
    {19, 15, 6, 0, 0},
    {0, 0, 0, 0, 0},
    {8, 0, 0, 32, 0},
    {0, 15, 0, 25, 0},
    {7, 8, 0, 25, 0},
    {0, 0, 6, 34, 0},
    {10, 0, 3, 27, 0},
    {0, 5, 7, 29, 0},
    {6, 4, 3, 27, 0},
    {0, 0, 0, 0, 0},
    {22, 0, 0, 0, 18},
    {0, 16, 0, 0, 24},
    {16, 12, 0, 0, 12},
    {0, 0, 11, 0, 29},
    {12, 0, 9, 0, 19},
    {0, 16, 8, 0, 16},
    {14, 11, 3, 0, 12},
    {0, 0, 0, 33, 7},
    {6, 0, 0, 31, 3},
    {0, 10, 0, 20, 10},
    {4, 9, 0, 24, 4},
    {0, 0, 4, 30, 6},
    {7, 0, 1, 21, 11},
    {0, 11, 2, 21, 6},
    {3, 11, 3, 17, 6} },
  { /* Colour Combinations */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {18, 22, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {21, 0, 19, 0, 0},
    {0, 19, 21, 0, 0},
    {18, 7, 15, 0, 0},
    {0, 0, 0, 0, 0},
    {25, 0, 0, 15, 0},
    {0, 19, 0, 21, 0},
    {22, 7, 0, 11, 0},
    {0, 0, 21, 19, 0},
    {20, 0, 11, 9, 0},
    {0, 6, 17, 17, 0},
    {13, 7, 11, 9, 0},
    {0, 0, 0, 0, 0},
    {21, 0, 0, 0, 19},
    {0, 19, 0, 0, 21},
    {12, 12, 0, 0, 16},
    {0, 0, 19, 0, 21},
    {15, 0, 15, 0, 10},
    {0, 14, 14, 0, 12},
    {12, 5, 13, 0, 11},
    {0, 0, 0, 21, 19},
    {19, 0, 0, 13, 8},
    {0, 17, 0, 15, 8},
    {10, 8, 0, 16, 7},
    {0, 0, 14, 11, 15},
    {14, 0, 5, 10, 11},
    {0, 9, 14, 7, 10},
    {13, 7, 2, 6, 12} },
  { /* Events */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {32, 8, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {32, 0, 8, 0, 0},
    {0, 15, 25, 0, 0},
    {20, 15, 6, 0, 0},
    {0, 0, 0, 0, 0},
    {36, 0, 0, 4, 0},
    {0, 19, 0, 21, 0},
    {28, 11, 0, 1, 0},
    {0, 0, 18, 22, 0},
    {24, 0, 5, 11, 0},
    {0, 11, 20, 9, 0},
    {22, 8, 3, 7, 0},
    {0, 0, 0, 0, 0},
    {22, 0, 0, 0, 18},
    {0, 9, 0, 0, 31},
    {19, 8, 0, 0, 13},
    {0, 0, 17, 0, 23},
    {24, 0, 4, 0, 12},
    {0, 4, 18, 0, 18},
    {25, 5, 7, 0, 4},
    {0, 0, 0, 4, 36},
    {24, 0, 0, 2, 14},
    {0, 7, 0, 8, 25},
    {29, 2, 0, 2, 7},
    {0, 0, 18, 3, 19},
    {21, 0, 6, 4, 9},
    {0, 2, 12, 5, 21},
    {20, 3, 8, 0, 9} },
  { /* Radio formats */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {10, 30, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {11, 0, 29, 0, 0},
    {0, 13, 27, 0, 0},
    {5, 4, 31, 0, 0},
    {0, 0, 0, 0, 0},
    {22, 0, 0, 18, 0},
    {0, 28, 0, 12, 0},
    {9, 22, 0, 9, 0},
    {0, 0, 39, 1, 0},
    {3, 0, 32, 5, 0},
    {0, 7, 26, 7, 0},
    {3, 8, 24, 5, 0},
    {0, 0, 0, 0, 0},
    {15, 0, 0, 0, 25},
    {0, 16, 0, 0, 24},
    {10, 12, 0, 0, 18},
    {0, 0, 34, 0, 6},
    {4, 0, 25, 0, 11},
    {0, 7, 26, 0, 7},
    {2, 10, 21, 0, 7},
    {0, 0, 0, 9, 31},
    {8, 0, 0, 9, 24},
    {0, 12, 0, 9, 20},
    {8, 12, 0, 6, 14},
    {0, 0, 22, 6, 12},
    {7, 0, 19, 11, 3},
    {0, 9, 19, 4, 8},
    {3, 5, 22, 2, 8} },
  { /* Musical artists */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {29, 11, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {26, 0, 14, 0, 0},
    {0, 23, 17, 0, 0},
    {19, 8, 13, 0, 0},
    {0, 0, 0, 0, 0},
    {26, 0, 0, 14, 0},
    {0, 27, 0, 13, 0},
    {22, 13, 0, 5, 0},
    {0, 0, 26, 14, 0},
    {25, 0, 7, 8, 0},
    {0, 21, 11, 8, 0},
    {19, 8, 11, 2, 0},
    {0, 0, 0, 0, 0},
    {25, 0, 0, 0, 15},
    {0, 22, 0, 0, 18},
    {15, 11, 0, 0, 14},
    {0, 0, 22, 0, 18},
    {24, 0, 8, 0, 8},
    {0, 9, 14, 0, 18},
    {15, 5, 14, 0, 6},
    {0, 0, 0, 13, 27},
    {20, 0, 0, 7, 13},
    {0, 15, 0, 9, 16},
    {20, 5, 0, 5, 10},
    {0, 0, 17, 4, 19},
    {14, 0, 10, 4, 12},
    {0, 12, 11, 3, 15},
    {12, 9, 12, 2, 5} },
  { /* Aboriginal art */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {28, 12, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {23, 0, 17, 0, 0},
    {0, 11, 29, 0, 0},
    {18, 9, 13, 0, 0},
    {0, 0, 0, 0, 0},
    {21, 0, 0, 19, 0},
    {0, 12, 0, 28, 0},
    {19, 6, 0, 15, 0},
    {0, 0, 15, 25, 0},
    {19, 0, 14, 7, 0},
    {0, 11, 18, 11, 0},
    {14, 8, 12, 7, 0},
    {0, 0, 0, 0, 0},
    {30, 0, 0, 0, 10},
    {0, 15, 0, 0, 25},
    {19, 6, 0, 0, 15},
    {0, 0, 23, 0, 17},
    {23, 0, 10, 0, 7},
    {0, 8, 19, 0, 13},
    {15, 6, 11, 0, 8},
    {0, 0, 0, 23, 17},
    {14, 0, 0, 11, 15},
    {0, 5, 0, 21, 14},
    {17, 9, 0, 7, 7},
    {0, 0, 15, 13, 12},
    {18, 0, 12, 6, 5},
    {0, 9, 8, 9, 14},
    {25, 2, 6, 2, 5} },
  { /* Impressionist art */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {26, 14, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {26, 0, 14, 0, 0},
    {0, 20, 20, 0, 0},
    {17, 14, 9, 0, 0},
    {0, 0, 0, 0, 0},
    {24, 0, 0, 17, 0},
    {0, 20, 0, 20, 0},
    {16, 11, 0, 13, 0},
    {0, 0, 12, 28, 0},
    {22, 0, 8, 10, 0},
    {0, 15, 6, 19, 0},
    {21, 7, 7, 5, 0},
    {0, 0, 0, 0, 0},
    {23, 0, 0, 0, 17},
    {0, 13, 0, 0, 27},
    {17, 9, 0, 0, 14},
    {0, 0, 8, 0, 32},
    {21, 0, 3, 0, 16},
    {0, 11, 4, 0, 25},
    {17, 6, 5, 0, 12},
    {0, 0, 0, 18, 23},
    {14, 0, 0, 5, 21},
    {0, 11, 0, 7, 22},
    {19, 9, 0, 3, 9},
    {0, 0, 3, 11, 26},
    {16, 0, 8, 4, 12},
    {0, 12, 2, 12, 14},
    {10, 6, 6, 4, 14} },
  { /* Sentences */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {22, 19, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {13, 0, 27, 0, 0},
    {0, 16, 24, 0, 0},
    {20, 3, 17, 0, 0},
    {0, 0, 0, 0, 0},
    {28, 0, 0, 13, 0},
    {0, 30, 0, 10, 0},
    {21, 15, 0, 4, 0},
    {0, 0, 30, 10, 0},
    {16, 0, 19, 5, 0},
    {0, 12, 22, 6, 0},
    {10, 8, 18, 4, 0},
    {0, 0, 0, 0, 0},
    {19, 0, 0, 0, 21},
    {0, 19, 0, 0, 21},
    {17, 12, 0, 0, 11},
    {0, 0, 26, 0, 14},
    {11, 0, 17, 0, 12},
    {0, 8, 24, 0, 8},
    {8, 9, 15, 0, 8},
    {0, 0, 0, 6, 34},
    {20, 0, 0, 2, 18},
    {0, 18, 0, 4, 18},
    {13, 10, 0, 2, 15},
    {0, 0, 28, 1, 11},
    {13, 0, 14, 5, 8},
    {0, 10, 17, 3, 10},
    {15, 7, 16, 0, 2} },
  { /* Travel */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {27, 13, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {29, 0, 11, 0, 0},
    {0, 20, 20, 0, 0},
    {19, 10, 11, 0, 0},
    {0, 0, 0, 0, 0},
    {30, 0, 0, 10, 0},
    {0, 24, 0, 16, 0},
    {20, 14, 0, 6, 0},
    {0, 0, 30, 10, 0},
    {24, 0, 12, 4, 0},
    {0, 16, 17, 7, 0},
    {15, 7, 10, 8, 0},
    {0, 0, 0, 0, 0},
    {13, 0, 0, 0, 28},
    {0, 9, 0, 0, 32},
    {11, 5, 0, 0, 24},
    {0, 0, 10, 0, 30},
    {15, 0, 8, 0, 17},
    {0, 7, 4, 0, 29},
    {13, 2, 2, 0, 23},
    {0, 0, 0, 8, 32},
    {13, 0, 0, 3, 24},
    {0, 9, 0, 4, 27},
    {8, 6, 0, 8, 18},
    {0, 0, 8, 5, 27},
    {10, 0, 3, 2, 25},
    {0, 5, 9, 2, 24},
    {10, 4, 4, 1, 21} },
  { /* Marijuana */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {30, 10, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {30, 0, 10, 0, 0},
    {0, 29, 11, 0, 0},
    {27, 8, 5, 0, 0},
    {0, 0, 0, 0, 0},
    {27, 0, 0, 13, 0},
    {0, 23, 0, 17, 0},
    {22, 8, 0, 10, 0},
    {0, 0, 15, 26, 0},
    {27, 0, 4, 9, 0},
    {0, 23, 5, 12, 0},
    {22, 4, 5, 9, 0},
    {0, 0, 0, 0, 0},
    {35, 0, 0, 0, 6},
    {0, 22, 0, 0, 18},
    {20, 9, 0, 0, 11},
    {0, 0, 32, 0, 8},
    {28, 0, 8, 0, 4},
    {0, 24, 11, 0, 5},
    {26, 6, 2, 0, 6},
    {0, 0, 0, 34, 6},
    {23, 0, 0, 11, 6},
    {0, 22, 0, 14, 4},
    {25, 2, 0, 10, 3},
    {0, 0, 18, 14, 8},
    {21, 0, 8, 6, 5},
    {0, 14, 6, 15, 5},
    {21, 4, 2, 8, 5} },
  { /* Latitude */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {22, 18, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {31, 0, 9, 0, 0},
    {0, 29, 11, 0, 0},
    {17, 16, 7, 0, 0},
    {0, 0, 0, 0, 0},
    {25, 0, 0, 16, 0},
    {0, 32, 0, 8, 0},
    {24, 14, 0, 2, 0},
    {0, 0, 17, 23, 0},
    {30, 0, 6, 4, 0},
    {0, 24, 14, 2, 0},
    {23, 4, 12, 1, 0},
    {0, 0, 0, 0, 0},
    {30, 0, 0, 0, 10},
    {0, 31, 0, 0, 9},
    {19, 15, 0, 0, 6},
    {0, 0, 36, 0, 4},
    {30, 0, 9, 0, 2},
    {0, 22, 15, 0, 3},
    {9, 15, 14, 0, 2},
    {0, 0, 0, 26, 14},
    {26, 0, 0, 5, 9},
    {0, 25, 0, 8, 7},
    {16, 18, 0, 3, 3},
    {0, 0, 26, 14, 0},
    {33, 0, 4, 1, 2},
    {0, 24, 13, 3, 0},
    {19, 9, 10, 2, 0} },
  { /* Dots */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {31, 9, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {32, 0, 9, 0, 0},
    {0, 24, 16, 0, 0},
    {28, 7, 5, 0, 0},
    {0, 0, 0, 0, 0},
    {27, 0, 0, 13, 0},
    {0, 18, 0, 22, 0},
    {29, 5, 0, 6, 0},
    {0, 0, 18, 22, 0},
    {24, 0, 6, 10, 0},
    {0, 12, 12, 16, 0},
    {22, 2, 4, 12, 0},
    {0, 0, 0, 0, 0},
    {30, 0, 0, 0, 10},
    {0, 21, 0, 0, 19},
    {24, 10, 0, 0, 6},
    {0, 0, 18, 0, 22},
    {26, 0, 9, 0, 6},
    {0, 15, 9, 0, 16},
    {26, 5, 4, 0, 5},
    {0, 0, 0, 27, 13},
    {26, 0, 0, 10, 4},
    {0, 14, 0, 20, 6},
    {20, 7, 0, 12, 1},
    {0, 0, 11, 20, 9},
    {20, 0, 0, 12, 8},
    {0, 11, 3, 19, 7},
    {23, 4, 2, 6, 5} },
  { /* Triangles */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {20, 20, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {23, 0, 17, 0, 0},
    {0, 30, 10, 0, 0},
    {18, 18, 4, 0, 0},
    {0, 0, 0, 0, 0},
    {27, 0, 0, 13, 0},
    {0, 31, 0, 9, 0},
    {13, 22, 0, 6, 0},
    {0, 0, 26, 14, 0},
    {22, 0, 7, 11, 0},
    {0, 29, 9, 2, 0},
    {18, 16, 3, 3, 0},
    {0, 0, 0, 0, 0},
    {23, 0, 0, 0, 17},
    {0, 17, 0, 0, 23},
    {19, 12, 0, 0, 9},
    {0, 0, 12, 0, 28},
    {19, 0, 7, 0, 14},
    {0, 14, 6, 0, 20},
    {18, 12, 2, 0, 8},
    {0, 0, 0, 16, 24},
    {13, 0, 0, 3, 24},
    {0, 13, 0, 4, 23},
    {14, 11, 0, 5, 11},
    {0, 0, 5, 5, 30},
    {16, 0, 4, 0, 20},
    {0, 18, 5, 2, 15},
    {16, 17, 0, 0, 7} },
  { /* Population */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {25, 15, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {21, 0, 19, 0, 0},
    {0, 19, 21, 0, 0},
    {12, 8, 20, 0, 0},
    {0, 0, 0, 0, 0},
    {25, 0, 0, 15, 0},
    {0, 27, 0, 13, 0},
    {20, 14, 0, 6, 0},
    {0, 0, 30, 10, 0},
    {13, 0, 17, 10, 0},
    {0, 14, 17, 10, 0},
    {13, 11, 14, 2, 0},
    {0, 0, 0, 0, 0},
    {18, 0, 0, 0, 22},
    {0, 19, 0, 0, 21},
    {11, 8, 0, 0, 22},
    {0, 0, 19, 0, 21},
    {11, 0, 12, 0, 17},
    {0, 15, 12, 0, 13},
    {11, 11, 9, 0, 9},
    {0, 0, 0, 13, 27},
    {15, 0, 0, 4, 21},
    {0, 15, 0, 4, 21},
    {15, 7, 0, 3, 15},
    {0, 0, 19, 9, 12},
    {12, 0, 17, 3, 8},
    {0, 13, 11, 5, 11},
    {11, 10, 10, 2, 7} },
  { /* Surface area */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {37, 3, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {38, 0, 2, 0, 0},
    {0, 24, 16, 0, 0},
    {31, 5, 4, 0, 0},
    {0, 0, 0, 0, 0},
    {40, 0, 0, 1, 0},
    {0, 30, 0, 11, 0},
    {31, 9, 0, 0, 0},
    {0, 0, 23, 17, 0},
    {34, 0, 5, 1, 0},
    {0, 23, 13, 4, 0},
    {30, 3, 5, 2, 0},
    {0, 0, 0, 0, 0},
    {38, 0, 0, 0, 2},
    {0, 24, 0, 0, 16},
    {35, 3, 0, 0, 2},
    {0, 0, 23, 0, 17},
    {34, 0, 3, 0, 3},
    {0, 20, 12, 0, 8},
    {27, 7, 4, 0, 2},
    {0, 0, 0, 14, 26},
    {36, 0, 0, 0, 4},
    {0, 21, 0, 4, 15},
    {27, 5, 0, 2, 6},
    {0, 0, 25, 3, 12},
    {34, 0, 5, 0, 1},
    {0, 12, 17, 3, 8},
    {26, 5, 6, 0, 3} },
  { /* Beer */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {16, 24, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {19, 0, 21, 0, 0},
    {0, 36, 4, 0, 0},
    {7, 31, 2, 0, 0},
    {0, 0, 0, 0, 0},
    {22, 0, 0, 18, 0},
    {0, 19, 0, 21, 0},
    {13, 17, 0, 10, 0},
    {0, 0, 5, 35, 0},
    {13, 0, 2, 25, 0},
    {0, 17, 2, 21, 0},
    {14, 9, 1, 16, 0},
    {0, 0, 0, 0, 0},
    {17, 0, 0, 0, 23},
    {0, 24, 0, 0, 17},
    {13, 14, 0, 0, 13},
    {0, 0, 3, 0, 37},
    {8, 0, 6, 0, 26},
    {0, 10, 1, 0, 29},
    {12, 13, 2, 0, 13},
    {0, 0, 0, 19, 21},
    {11, 0, 0, 12, 17},
    {0, 12, 0, 13, 16},
    {12, 7, 0, 4, 17},
    {0, 0, 0, 14, 26},
    {10, 0, 0, 12, 18},
    {0, 9, 1, 10, 20},
    {4, 12, 1, 11, 12} },
  { /* Cars */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {11, 29, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {12, 0, 28, 0, 0},
    {0, 21, 19, 0, 0},
    {8, 21, 11, 0, 0},
    {0, 0, 0, 0, 0},
    {19, 0, 0, 21, 0},
    {0, 21, 0, 19, 0},
    {4, 27, 0, 10, 0},
    {0, 0, 29, 11, 0},
    {11, 0, 17, 12, 0},
    {0, 12, 15, 13, 0},
    {2, 20, 10, 8, 0},
    {0, 0, 0, 0, 0},
    {21, 0, 0, 0, 20},
    {0, 24, 0, 0, 16},
    {7, 19, 0, 0, 14},
    {0, 0, 28, 0, 12},
    {6, 0, 20, 0, 14},
    {0, 16, 13, 0, 11},
    {6, 14, 13, 0, 7},
    {0, 0, 0, 14, 26},
    {10, 0, 0, 10, 20},
    {0, 6, 0, 16, 18},
    {3, 17, 0, 4, 16},
    {0, 0, 9, 12, 19},
    {8, 0, 16, 6, 10},
    {0, 9, 10, 8, 13},
    {4, 11, 7, 14, 4} },
  { /* Restaurants */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {15, 25, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {18, 0, 22, 0, 0},
    {0, 21, 19, 0, 0},
    {10, 19, 11, 0, 0},
    {0, 0, 0, 0, 0},
    {17, 0, 0, 23, 0},
    {0, 19, 0, 21, 0},
    {8, 19, 0, 13, 0},
    {0, 0, 13, 27, 0},
    {10, 0, 12, 18, 0},
    {0, 18, 11, 11, 0},
    {10, 11, 8, 11, 0},
    {0, 0, 0, 0, 0},
    {20, 0, 0, 0, 20},
    {0, 35, 0, 0, 5},
    {15, 21, 0, 0, 4},
    {0, 0, 36, 0, 4},
    {16, 0, 19, 0, 5},
    {0, 27, 12, 0, 1},
    {17, 11, 11, 0, 2},
    {0, 0, 0, 30, 10},
    {8, 0, 0, 14, 18},
    {0, 23, 0, 14, 4},
    {11, 21, 0, 8, 0},
    {0, 0, 21, 16, 3},
    {12, 0, 16, 11, 1},
    {0, 15, 13, 12, 0},
    {11, 9, 14, 6, 0} },
  { /* Flight layovers */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {23, 17, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {28, 0, 12, 0, 0},
    {0, 24, 16, 0, 0},
    {22, 10, 8, 0, 0},
    {0, 0, 0, 0, 0},
    {25, 0, 0, 15, 0},
    {0, 32, 0, 8, 0},
    {20, 6, 0, 14, 0},
    {0, 0, 24, 16, 0},
    {29, 0, 6, 6, 0},
    {0, 23, 4, 13, 0},
    {23, 6, 8, 3, 0},
    {0, 0, 0, 0, 0},
    {29, 0, 0, 0, 11},
    {0, 28, 0, 0, 12},
    {18, 13, 0, 0, 9},
    {0, 0, 31, 0, 9},
    {22, 0, 9, 0, 9},
    {0, 24, 11, 0, 5},
    {19, 4, 6, 0, 11},
    {0, 0, 0, 29, 12},
    {26, 0, 0, 7, 7},
    {0, 20, 0, 10, 10},
    {21, 7, 0, 5, 7},
    {0, 0, 25, 7, 8},
    {19, 0, 13, 2, 6},
    {0, 23, 7, 3, 7},
    {18, 7, 2, 5, 8} },
  { /* Future payments */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {22, 18, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {22, 0, 18, 0, 0},
    {0, 27, 13, 0, 0},
    {20, 11, 9, 0, 0},
    {0, 0, 0, 0, 0},
    {29, 0, 0, 11, 0},
    {0, 28, 0, 12, 0},
    {25, 8, 0, 7, 0},
    {0, 0, 21, 19, 0},
    {21, 0, 4, 15, 0},
    {0, 31, 3, 6, 0},
    {25, 8, 0, 7, 0},
    {0, 0, 0, 0, 0},
    {28, 0, 0, 0, 12},
    {0, 29, 0, 0, 11},
    {21, 7, 0, 0, 12},
    {0, 0, 29, 0, 12},
    {24, 0, 9, 0, 7},
    {0, 28, 3, 0, 10},
    {23, 9, 2, 0, 6},
    {0, 0, 0, 26, 14},
    {25, 0, 0, 2, 13},
    {0, 24, 0, 3, 13},
    {21, 12, 0, 0, 7},
    {0, 0, 22, 6, 12},
    {26, 0, 4, 2, 8},
    {0, 24, 2, 5, 9},
    {19, 13, 1, 1, 6} },
  { /* Phone plans */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {20, 20, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {21, 0, 19, 0, 0},
    {0, 8, 32, 0, 0},
    {12, 6, 22, 0, 0},
    {0, 0, 0, 0, 0},
    {19, 0, 0, 21, 0},
    {0, 20, 0, 20, 0},
    {11, 8, 0, 21, 0},
    {0, 0, 16, 24, 0},
    {14, 0, 14, 12, 0},
    {0, 16, 14, 10, 0},
    {18, 4, 9, 9, 0},
    {0, 0, 0, 0, 0},
    {20, 0, 0, 0, 20},
    {0, 15, 0, 0, 25},
    {13, 9, 0, 0, 18},
    {0, 0, 18, 0, 22},
    {15, 0, 10, 0, 16},
    {0, 8, 18, 0, 14},
    {8, 10, 8, 0, 14},
    {0, 0, 0, 23, 17},
    {20, 0, 0, 9, 11},
    {0, 13, 0, 8, 19},
    {19, 8, 0, 7, 6},
    {0, 0, 17, 10, 13},
    {14, 0, 11, 6, 9},
    {0, 9, 19, 5, 7},
    {10, 8, 10, 2, 11} },
  { /* Hotel rooms */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {32, 8, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {31, 0, 9, 0, 0},
    {0, 35, 6, 0, 0},
    {25, 9, 6, 0, 0},
    {0, 0, 0, 0, 0},
    {33, 0, 0, 7, 0},
    {0, 35, 0, 5, 0},
    {22, 12, 0, 6, 0},
    {0, 0, 28, 12, 0},
    {24, 0, 14, 2, 0},
    {0, 25, 11, 5, 0},
    {22, 7, 5, 6, 0},
    {0, 0, 0, 0, 0},
    {33, 0, 0, 0, 7},
    {0, 34, 0, 0, 6},
    {19, 19, 0, 0, 2},
    {0, 0, 34, 0, 6},
    {25, 0, 11, 0, 4},
    {0, 25, 11, 0, 4},
    {24, 9, 6, 0, 1},
    {0, 0, 0, 30, 10},
    {27, 0, 0, 7, 6},
    {0, 28, 0, 9, 3},
    {16, 19, 0, 3, 2},
    {0, 0, 27, 11, 2},
    {23, 0, 9, 3, 5},
    {0, 21, 13, 4, 2},
    {21, 8, 7, 1, 3} },
  { /* Two-flight itineraries */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {18, 22, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {13, 0, 27, 0, 0},
    {0, 19, 21, 0, 0},
    {8, 16, 16, 0, 0},
    {0, 0, 0, 0, 0},
    {18, 0, 0, 23, 0},
    {0, 19, 0, 21, 0},
    {10, 14, 0, 16, 0},
    {0, 0, 21, 19, 0},
    {12, 0, 19, 9, 0},
    {0, 15, 10, 15, 0},
    {8, 11, 16, 5, 0},
    {0, 0, 0, 0, 0},
    {21, 0, 0, 0, 19},
    {0, 16, 0, 0, 24},
    {6, 18, 0, 0, 16},
    {0, 0, 17, 0, 23},
    {15, 0, 12, 0, 14},
    {0, 20, 11, 0, 9},
    {8, 10, 14, 0, 8},
    {0, 0, 0, 14, 26},
    {12, 0, 0, 10, 18},
    {0, 20, 0, 6, 14},
    {8, 17, 0, 7, 8},
    {0, 0, 16, 7, 17},
    {10, 0, 12, 0, 18},
    {0, 15, 15, 4, 6},
    {8, 8, 10, 5, 9} },
  { /* Televisions */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {28, 12, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {30, 0, 10, 0, 0},
    {0, 20, 20, 0, 0},
    {25, 7, 8, 0, 0},
    {0, 0, 0, 0, 0},
    {12, 0, 0, 28, 0},
    {0, 1, 0, 39, 0},
    {13, 4, 0, 23, 0},
    {0, 0, 3, 37, 0},
    {12, 0, 3, 25, 0},
    {0, 5, 7, 28, 0},
    {9, 1, 4, 27, 0},
    {0, 0, 0, 0, 0},
    {22, 0, 0, 0, 18},
    {0, 5, 0, 0, 35},
    {20, 2, 0, 0, 18},
    {0, 0, 15, 0, 25},
    {22, 0, 4, 0, 14},
    {0, 4, 9, 0, 27},
    {15, 1, 5, 0, 19},
    {0, 0, 0, 27, 14},
    {7, 0, 0, 21, 12},
    {0, 3, 0, 24, 13},
    {8, 3, 0, 19, 10},
    {0, 0, 7, 27, 6},
    {6, 0, 3, 25, 6},
    {0, 1, 6, 21, 12},
    {6, 0, 5, 21, 8} },
  { /* Coffee */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {8, 32, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {7, 0, 33, 0, 0},
    {0, 23, 17, 0, 0},
    {1, 14, 25, 0, 0},
    {0, 0, 0, 0, 0},
    {16, 0, 0, 24, 0},
    {0, 30, 0, 10, 0},
    {5, 25, 0, 10, 0},
    {0, 0, 27, 14, 0},
    {6, 0, 26, 8, 0},
    {0, 23, 12, 5, 0},
    {2, 18, 15, 5, 0},
    {0, 0, 0, 0, 0},
    {19, 0, 0, 0, 21},
    {0, 34, 0, 0, 6},
    {7, 28, 0, 0, 5},
    {0, 0, 33, 0, 7},
    {9, 0, 20, 0, 11},
    {0, 17, 19, 0, 4},
    {3, 17, 18, 0, 2},
    {0, 0, 0, 19, 21},
    {8, 0, 0, 18, 14},
    {0, 32, 0, 7, 2},
    {8, 23, 0, 6, 3},
    {0, 0, 25, 7, 8},
    {4, 0, 21, 9, 6},
    {0, 20, 14, 6, 0},
    {4, 17, 14, 2, 3} },
  { /* Charity */
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {16, 24, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {13, 0, 27, 0, 0},
    {0, 6, 35, 0, 0},
    {15, 5, 20, 0, 0},
    {0, 0, 0, 0, 0},
    {10, 0, 0, 30, 0},
    {0, 27, 0, 13, 0},
    {20, 10, 0, 10, 0},
    {0, 0, 21, 19, 0},
    {13, 0, 13, 14, 0},
    {0, 6, 26, 8, 0},
    {10, 3, 16, 11, 0},
    {0, 0, 0, 0, 0},
    {11, 0, 0, 0, 29},
    {0, 17, 0, 0, 23},
    {11, 15, 0, 0, 14},
    {0, 0, 25, 0, 15},
    {15, 0, 12, 0, 13},
    {0, 6, 14, 0, 20},
    {10, 5, 8, 0, 17},
    {0, 0, 0, 15, 25},
    {8, 0, 0, 9, 24},
    {0, 11, 0, 5, 24},
    {9, 9, 0, 5, 17},
    {0, 0, 21, 8, 11},
    {13, 0, 12, 6, 9},
    {0, 6, 12, 5, 17},
    {9, 2, 9, 8, 12} } };

void set_population_data(Universe *univ, RCM *rcm, int domain)
{
    unsigned i, A;
    for (A=0; A<univ->nSets; A++)
        if (univ->cardinality[A] > 1) {
            rcm->N_total[A] = 0;
            for (i=0; i<univ->cardinality[A]; i++) {
                unsigned x = univ->element[A][i];
                rcm->N[A][x] = population_counts[domain][A][x];
                rcm->N_total[A] += rcm->N[A][x];
            }
        }
}