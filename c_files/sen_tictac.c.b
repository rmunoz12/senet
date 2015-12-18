// ConsoleApplication11.cpp : Defines the entry point for the console application.
//

#include <stdio.h>
#include "TicTacToe.h";

GameBoard* init_board(int length,int width)
{
	GameBoard *game_board = malloc(sizeof(GameBoard));
	game_board->length = length;
	game_board->width = width;
	game_board->board = malloc(sizeof(int)*length*width);
	game_board->game_step =1;
		for (int i = 0; i < length*width; i++)
		{
			game_board->board[i] = 0;
		}
	return game_board;
}

void display(GameBoard game_board)
{
	for (int i = 0; i < game_board.length*game_board.width; i++)
	{
		if (i % game_board.width == 0)
		{
			printf("\n");

		}
		if (game_board.board[i] == 0)
		{
			printf(" - ");
		}
		else if (game_board.board[i] == 1)
		{
			printf(" X ");
		}
		else
		{
			printf(" O ");
		}

	}
	printf("\n");
}
int board_evaluate(GameBoard game_board)
{
	if ((game_board.board[0] + game_board.board[1] + game_board.board[2] == 3) || (game_board.board[0] + game_board.board[1] + game_board.board[2] == -3) || (game_board.board[3] + game_board.board[4] + game_board.board[5] == 3) || (game_board.board[3] + game_board.board[4] + game_board.board[5] == -3) || (game_board.board[6] + game_board.board[7] + game_board.board[8] == 3) || (game_board.board[6] + game_board.board[7] + game_board.board[8] == -3) || (game_board.board[0] + game_board.board[4] + game_board.board[8] == 3) || (game_board.board[0] + game_board.board[4] + game_board.board[8] == -3) || (game_board.board[2] + game_board.board[4] + game_board.board[6] == 3) || (game_board.board[2] + game_board.board[4] + game_board.board[6] == -3) || (game_board.board[0] + game_board.board[3] + game_board.board[6] == 3) || (game_board.board[0] + game_board.board[3] + game_board.board[6] == -3) || (game_board.board[1] + game_board.board[4] + game_board.board[7] == 3) || (game_board.board[1] + game_board.board[4] + game_board.board[7] == -3) || (game_board.board[2] + game_board.board[5] + game_board.board[8] == 3) || (game_board.board[2] + game_board.board[5] + game_board.board[8] == -3))
	{
		if (((game_board.game_step-1)%2)!=0)
		{
			printf("Player 1 wins! \n");

		}
		else
		{
			printf("Player 2 wins! \n");
		}
		return 1;
	}
	else if (game_board.game_step > 9)
	{
		printf("Game Drawn!! \n");
		return 1;
	}

	else
	{
		return 0;

	}
}

int valid_turn(GameBoard game_board,int x,int y)
{
	int index = x*game_board.length + y - (game_board.length + 1);
	if (x > game_board.length || x< 1||y>game_board.width||y<1)
	{
		printf("Input out of Range.  \n");
		return 0;
	}
	
	else if (game_board.board[index] != 0)
	{
		printf("Tile already occupied. \n");
		return 0;

	}

	return 1;

}

void turn(GameBoard* game_board)
{
	if (((game_board->game_step)%2)!=0)
	{
		printf("Player 1's Turn: \n");
	}
	else
	{
		printf("Player 2's Turn: \n");

	}
	int x;
	int y;
	printf("Please enter a valid input in the range of 1 and 3 \n");
	scanf_s("%d", &x);
	printf("Please enter a valid input in the range of 1 and 3 \n");
	scanf_s("%d", &y);
	while (!valid_turn(*game_board,x,y))
	{
		printf("Please enter a valid input in the range of 1 and 3 \n");
		scanf_s("%d", &x);
		printf("Please enter a valid input in the range of 1 and 3 \n");
		scanf_s("%d", &y);

	}
	int index= x*game_board->length + y - ((game_board->length) + 1);
	if (((game_board->game_step)%2)!=0)
	{
		game_board->board[index] = 1;
	}

	else
	{
		game_board->board[index] = -1;
	}
	game_board->game_step++;
	printf("Game Step from call: %d", game_board->game_step);
}

void reset_game(GameBoard* game_board)
{
	printf("Would you like to continue? Type y to continue playing and n to Exit.\n");
	char c = ' ';
	scanf_s(" %c", &c);
	printf("%c", c);
	while ((c != 'n') && (c != 'y'))
	{
		printf("Enter valid input. Either y or n. \n");
		scanf_s(" %c", &c);
	}

	if (c == 'y')
	{
		int length = game_board->length;
		int width = game_board->width;
		free(game_board->board);
		free(game_board);
		game_board=init_board(length,width);
		display(*game_board);
	}
	else
	{
		free(game_board->board);
		free(game_board);
		exit(0);
	}



}

int main()
{
	int length = 3;
	int width = 3;
	GameBoard* game_board=init_board(length,width);
	display(*game_board);
	do
	{
		turn(game_board);
		printf("Game Step from caller: %d", game_board->game_step);
		display(*game_board);
		if (board_evaluate(*game_board))
		{
			reset_game(game_board);
		}
	} while (game_board->game_step <10);

	return 0;
}

