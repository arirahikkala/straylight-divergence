#include "AStar.h"
#include "IndexPriorityQueue.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static int max (int a, int b)
{
	if (a > b)
		return a;
	else 
		return b;
}

static int getIndex (struct coord bounds, struct coord c)
{
	return c.x + c.y * bounds.x;
}

static int getCost (int *grid, struct coord bounds, struct coord c)
{
	return grid[getIndex (bounds, c)];
}

static struct coord getCoord (struct coord bounds, int c)
{
	struct coord rv = { c % bounds.x, c / bounds.x };
	return rv;
}

static int contained (struct coord bounds, struct coord c)
{
	return c.x >= 0 && c.y >= 0 && c.x < bounds.x && c.y < bounds.y;
}

static int getNeighbours (int *grid, struct coord bounds, struct coord c, struct coord *neighbours)
{
	struct coord neighbours_1 [] = { {c.x + 1, c.y},
					 {c.x - 1, c.y},
					 {c.x, c.y + 1},
					 {c.x, c.y - 1},
					 {c.x + 1, c.y + 1},
					 {c.x + 1, c.y - 1},
					 {c.x - 1, c.y + 1},
					 {c.x - 1, c.y - 1} };

	int j = 0;
	for (int i = 0; i < 8; i++)
		if (contained (bounds, neighbours_1[i]) && -1 != getCost (grid, bounds, neighbours_1[i]))
			neighbours[j++] = neighbours_1[i];

	return j;
}

static int estimateDistance (struct coord start, struct coord end)
{
//	return 10 * max (abs (start.x - end.x), abs (start.y - end.y));
	return 10 * round (sqrt ((start.x - end.x) * (start.x - end.x) + (start.y - end.y) * (start.y - end.y)));
}

// only ever called for adjacent tiles
// implements a local vaguely Euclidean distance metric, multiplying the cost-to-enter of a tile
static int preciseDistance (int *grid, struct coord bounds, struct coord start, struct coord end)
{
	if (start.x - end.x != 0 && start.y - end.y != 0)
		return 14 * grid[getIndex (bounds, end)];
	else
		return 10 * grid[getIndex (bounds, end)];
}

void printSolution (int *cameFrom, struct coord bounds, int begin, int end)
{
	for (int i = end; i != begin; i = cameFrom[i]) {
		printf ("(%i %i)\n", getCoord(bounds, i).x, getCoord(bounds, i).y);
	}
}

int *recordSolution (int *cameFrom, int *solLen, int begin, int end)
{
	int rvLen = 1;
	*solLen = 0;
	int *rv = malloc (rvLen * sizeof (int));
	for (int i = end; ; i = cameFrom[i]) {
		rv[*solLen] = i;
		(*solLen)++;
		if (*solLen >= rvLen) {
			rvLen *= 2;
			rv = realloc (rv, rvLen * sizeof (int));
			if (!rv)
				return NULL;
		}
		if (i == begin)
			break;
	}

	return rv;
}

void debugDump (int *grid, queue *open, char *closed, int *cameFrom, struct coord bounds, int node, int start, int end)
{
	FILE *out = fopen ("astar_log.txt", "a");
	for (int i = 0; i < 10; i++) {
		fputs ("\n", out);
		for (int j = 0; j < bounds.x; j++) {
			int isSol = 0;
			int thisNode = i * bounds.x + j;

			if (thisNode == start) {
				fprintf (out, "1"); 
				continue;
			}

			if (thisNode == end) {
				fprintf (out, "2");
				continue;
			}
			
			if (thisNode == node) {
				fprintf (out, "*");
				continue;
			}

			if (exists (open, thisNode)) {
				fprintf (out, "o");
				continue;
			}

			if (closed [thisNode]) {
				fprintf (out, "c");
				continue;
			}

			for (int k = node; k != -1; k = cameFrom[k]) {
				if (thisNode == k) {
					isSol = 1;
					fprintf (out, "+");
					continue;
				}
			}

			if (isSol)
				continue;

			if (grid[thisNode] >= 0)
				fprintf (out, " ");
			else
				fprintf (out, "#");
		}
	}
	fputs ("\n\n", out);
	fclose (out);
}

int *astar (int *grid, int *solLength, int boundX, int boundY, int start, int end)
{
	struct coord bounds = {boundX, boundY};
	int size = bounds.x * bounds.y;

	struct coord startCoord = getCoord (bounds, start);
	struct coord endCoord = getCoord (bounds, end);

	queue *open = createQueue();
	char closed [size];
	int gScores [size];
	int cameFrom [size];
	struct coord neighbours[8];

	memset (closed, 0, sizeof(closed));

	*solLength = -1;

	gScores[start] = 0;
	cameFrom[start] = -1;
	if (contained (bounds, startCoord) && -1 != getCost (grid, bounds, startCoord))
		insert (open, start, estimateDistance (startCoord, endCoord));
	
	while (open->size) {
		int node = findMin (open)->value; 
//		debugDump (grid, open, closed, cameFrom, bounds, node, start, end);
		struct coord nodeCoord = getCoord (bounds, node);
		if (nodeCoord.x == endCoord.x && nodeCoord.y == endCoord.y) {
			freeQueue (open);
			return recordSolution (cameFrom, solLength, start, node);
		}

		deleteMin (open);
		closed[node] = 1;

		for (int n = getNeighbours (grid, bounds, nodeCoord, neighbours); n-- > 0; )
		{
			int newNode = getIndex (bounds, neighbours[n]);
			struct coord newCoord = neighbours[n];

			if (closed[newNode])
				continue;
			
			if (!exists (open, newNode)) {
				cameFrom[newNode] = node;
				gScores[newNode] = gScores[node] + preciseDistance (grid, bounds, nodeCoord, newCoord);
				insert (open, newNode, gScores[newNode] + estimateDistance (newCoord, endCoord));
			}
			else if (gScores[newNode] > gScores[node] + preciseDistance (grid, bounds, nodeCoord, newCoord)) {
				cameFrom[newNode] = node;
				int oldGScore = gScores[newNode];
				gScores[newNode] = gScores[node] + preciseDistance (grid, bounds, nodeCoord, newCoord);
				changePriority (open, newNode, priorityOf (open, newNode) - oldGScore + gScores[newNode]);
			}
			// otherwise: The new node is already open and the previous path to it is better than this one
		}
	}
	freeQueue (open);
	return NULL;
}
