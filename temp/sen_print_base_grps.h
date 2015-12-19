#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "sen_init_base_grps.h"

#ifndef SEN_PRINT_BASE_GRPS
#define SEN_PRINT_BASE_GRPS

char*  snt_Piece_snt___repr__(struct snt_Piece *this) {
  return this->snt_s;
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

#endif
