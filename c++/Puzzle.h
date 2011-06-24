/*
 * Puzzle.h
 *
 * This header contains the declaration of the Puzzle class,
 * which is the base class of all other puzzles.
 *
 * Author: Noah Richards
 *
 * $Id: Puzzle.h,v 1.4 2006/04/24 13:42:57 noah Exp $
 */


#ifndef PUZZLE_H
#define PUZZLE_H

#include <vector>
#include <iostream>
using namespace std;

template <typename T>
class Puzzle
{
public:
/*
	static void T_REQUIREMENTS(T a, T b)
	{
		a == b != a;
		a < b <= a;
	};
*/

	typedef T config_type;

	virtual vector<config_type> getNeighbors(const config_type&) const = 0;
	virtual bool isSolution(const config_type&) const = 0;
	virtual config_type getStartConfig() const = 0;
	virtual string stringFromConfig(const config_type&) const = 0;
	virtual ~Puzzle() {};
};

#endif

/* Change log:
 *
 * $Log: Puzzle.h,v $
 * Revision 1.4  2006/04/24 13:42:57  noah
 *
 *
 * Added water start.
 *
 * Revision 1.3  2006/04/10 15:50:42  noah
 *
 * Replaced some map logic in solver.
 *
 * Original: if(!map.find) insert ...
 * Now: if(!insert) ...
 *
 * Revision 1.2  2006/03/27 17:45:33  noah
 *
 * Added constraints checking to Puzzle.h.
 *
 * Revision 1.1  2006/03/27 05:00:19  noah
 * Forgot to add Puzzle.h
 */
