#include "AStar.h"
#include <stdio.h>
#include <stdlib.h>

int main ()
{
	int grid[] = {1, 1, 1, -1, -1, 1, 1, 1, 1};
	int solLength = 0;
	int* solution = astar (grid, &solLength, 3, 3, 0, 6);
	for (int y = 0; y < 3; y++) {
		for (int x = 0; x < 3; x++) {
			int wasInSolution = 0;
			for (int i = 0; i < solLength; i++) {
				if ((x + 3 * y) == solution[i]) {
					printf ("%i", i);
					wasInSolution = 1;
				}					
			}
			if (!wasInSolution)
				printf ("*");
		}
		printf ("\n");
	}
	free (solution);
}
