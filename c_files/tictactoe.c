// ConsoleApplication11.cpp : Defines the entry point for the console application.
//

#include <stdio.h>


int *board; 
int game_step = 0;
int player_1 = 1;


void init_board(int*board, int length)
{
	for (int i = 0; i < length; i++)
	{
		board[i] = 0;
	}
}

void display(int*board, int length)
{
	for (int i = 0; i < length; i++)
	{
		if (i % 3 == 0)
		{
			printf( "\n");

		}
		if (board[i] == 0)
		{
			printf(" - ");
		}
		else if (board[i] == 1)
		{
			printf( " X ");
		}
		else
		{
			printf(" O ");
		}

	}
	printf( "\n");
}
int board_evaluate(int*board)
{
	if ((board[0] + board[1] + board[2] == 3) || (board[0] + board[1] + board[2] == -3) || (board[3] + board[4] + board[5] == 3) || (board[3] + board[4] + board[5] == -3) || (board[6] + board[7] + board[8] == 3) || (board[6] + board[7] + board[8] == -3) || (board[0] + board[4] + board[8] == 3) || (board[0] + board[4] + board[8] == -3) || (board[2] + board[4] + board[6] == 3) || (board[2] + board[4] + board[6] == -3) || (board[0] + board[3] + board[6] == 3) || (board[0] + board[3] + board[6] == -3) || (board[1] + board[4] + board[7] == 3) || (board[1] + board[4] + board[7] == -3) || (board[2] + board[5] + board[8] == 3) || (board[2] + board[5] + board[8] == -3))
	{
		if (player_1)
		{
			printf("Player 1 wins! \n");

		}
		else
		{
			printf("Player 2 wins! \n");
		}
		return 1;
	}
	else if (game_step > 8)
	{
		printf("Game Drawn!! \n");
		return 1;
	}

	else
	{
		return 0;

	}
}

int valid_turn(int input)
{
	if (input > 8 || input < 0)
	{
		printf("Input out of Range.  \n");
		return 0;
	}

	else if (board[input] != 0)
	{
		printf("Tile already occupied. \n");
		return 0;

	}

	return 1;

}

void turn()
{
	if (player_1)
	{
		printf("Player 1's Turn: \n");
	}
	else
	{
		printf("Player 2's Turn: \n");

	}
	int input;
	scanf_s("%d", &input);
	while (!valid_turn(input))
	{
		printf("Please enter a valid input in the range of 0 and 8 \n");
		scanf_s("%d", &input);

	}

	if ((player_1))
	{
		board[input] = 1;
	}

	else
	{
		board[input] = -1;
	}
	game_step++;
}

void reset_game(int*board, int length)
{
	printf("Would you  like to continue? Type y to continue playing and n to Exit.\n");
	char c=' ';
	scanf_s(" %c", &c);
	printf("%c", c);
	while ((c!= 'n') && (c!='y'))
	{
		printf("Enter valid input. Either y or n. \n");
		scanf_s(" %c", &c);
	}

	if (c == 'y')
	{
		init_board(board, length);
		game_step = 0;
		display(board, length);
	}
	else
	{
		exit(0);
	}



}

int main()
{
	int length = 9;
	board=(int*)malloc(9 * sizeof(int));
	init_board(board, length);
	display(board, length);
	do
	{

		if ((game_step % 2) == 0)
		{
			player_1 = 1;
		}
		else
		{
			player_1 = 0;
		}
		turn();
		display(board, length);
		if (board_evaluate(board))
		{
			reset_game(board, length);
		}
	} while (game_step <9);

	return 0;
}

