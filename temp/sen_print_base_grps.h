#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "sen_init_base_grps.h"

struct snt_Piece;
struct snt_Board;
struct snt_Line;
struct snt_Loop;
struct snt_Hex;
struct snt_Rect;

char*  snt_Piece_snt___repr__(struct snt_Piece *this) {
  return "<Group Piece instance>";
}

char*  snt_Board_snt___repr__(struct snt_Board *this) {
  return "<Group Board instance>";
}

char*  snt_Line_snt___repr__(struct snt_Line *this) {
  return "<Group Line instance>";
}

char*  snt_Loop_snt___repr__(struct snt_Loop *this) {
  return "<Group Loop instance>";
}

char*  snt_Hex_snt___repr__(struct snt_Hex *this) {
  return "<Group Hex instance>";
}

char*  snt_Rect_snt___repr__(struct snt_Rect *this) {
  return "<Group Rect instance>";
}
