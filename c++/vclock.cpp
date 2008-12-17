#include <iostream>
#include "VClock.h"
#include "Solver.h"
using namespace std;

int main(int argc, char** argv)
{
	// Ignore first argument (program name)
	argc--;
	argv++;

	if(argc < 3)
	{
		cerr << "Usage:\n"
		     << "vclock numhours starthour endhour"
		     << " movement1 [movements...]\n"
			 << endl;
		exit(1);
	}

	// Grab clock size, start value, and end value
	int size, start, end;

	size = atoi(argv[0]);
	start = atoi(argv[1]);
	end = atoi(argv[2]);

	argc -= 3;
	argv += 3;


	if(!argc)
	{
		cerr << "You must supply at least one movement." << endl;
		exit(1);
	}

	vector<int> movements;

	while(argc)
	{
		stringstream s(argv[0]);
		int movement;
		s >> movement;
		if(s.fail())
		{
			cerr << "Invalid argument: " << argv[0] << endl;
			exit(1);
		}

		movements.push_back(movement);

		argv++;
		argc--;
	}

	assert(!movements.empty());

	VClock c(size, start, end, movements);
	solver::solve(c);
	return 0;
}
