#include <iostream>
#include "Water.h"
#include "Solver.h"
using namespace std;

int main(int argc, char** argv)
{
	// Ignore first argument (program name)
	argc--;
	argv++;

	if(argc < 2)
	{
		cerr << "Usage:\n"
		     << "water <desired amount> <bucket1> [buckets...]\n"
			 << endl;
		exit(1);
	}

	// Grab desired amount
	int amt = atoi(argv[0]);

	argc --;
	argv ++;

	// Grab bucket sizes
	vector<int> buckets;

	while(argc)
	{
		stringstream s(argv[0]);
		int bucket(0);
		s >> bucket;
		if(s.fail() || bucket < 1)
		{
			cerr << "Invalid argument: " << argv[0] << endl;
			exit(1);
		}

		buckets.push_back(bucket);

		argv++;
		argc--;
	}

	assert(!buckets.empty());

	Water w(amt, buckets);
	solver::solve(w);
	return 0;
}
