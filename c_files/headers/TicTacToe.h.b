struct GameBoard
{
	int length;
	int width;
	int *board;
	int game_step;
};
typedef struct GameBoard GameBoard;

//Functions
GameBoard* init_board( int length, int width);
void display(GameBoard);
int board_evaluate(GameBoard);
int valid_turn(GameBoard,int x,int y);
void turn(GameBoard* game_board);
void reset_game(GameBoard* game_board);

