#ifndef ASTAR_H_
#define ASTAR_H_

struct coord {
	int x;
	int y;
};

int *astar (int *grid, int *solLength, int boundX, int boundY, int start, int end);


#endif
