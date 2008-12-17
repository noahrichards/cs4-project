/*
 * Jam.h
 *
 * This header contains the declaration of the Jam puzzle class.
 *
 * Author: Noah Richards
 *
 * $Id: Jam.h,v 1.4 2006/04/10 15:50:42 noah Exp $
 *
 * $Log: Jam.h,v $
 * Revision 1.4  2006/04/10 15:50:42  noah
 *
 * Replaced some map logic in solver.
 *
 * Original: if(!map.find) insert ...
 * Now: if(!insert) ...
 *
 * Revision 1.3  2006/04/03 15:13:27  noah
 *
 *
 * Modified lloyd to work as per specs, like slide (first argument is
 * either filename or "-" for stdin, second argument is either filename
 * or "-" for stdout).  Other cosmetic changes.
 *
 * Revision 1.2  2006/03/27 04:59:04  noah
 * Replaced Solver class with single templated function.
 * Created a templated Puzzle base class for the templated function
 * to use, which formalizes requirements on the puzzle types and
 * allows the templated solve function to guess its arguments.
 *
 * Revision 1.1.1.1  2006/03/27 00:52:30  noah
 * CS4 Project
 *
 *
 */


#ifndef JAM_H
#define JAM_H

#include <vector>
#include <iostream>
#include <sstream>
#include <list>
#include "Puzzle.h"
using namespace std;

class Jam : public Puzzle<vector<int> >
{
public:

	enum DIRECTION {HORIZONTAL = 0, VERTICAL = 1};

	/* Block info is a pair:
	 * first - block size 
	 * second - DIRECTION (enum)
	 */
	typedef std::pair<unsigned int, DIRECTION> block_info;
	typedef vector<int> config_type;

	// Exceptions
	class StreamException {};
	class InvalidStreamException : public std::exception, public StreamException
	{
		const char* what() const throw()
		{
			return "Input stream passed to constructor is not valid.\n";
		}
	};

	class InvalidDataException : public std::exception, public StreamException
	{
		const char* what() const throw()
		{
			return "Input stream data is invalid.\n";
		}
	};

	Jam(istream& in);

	vector<config_type> getNeighbors(const config_type&) const;
	bool isSolution(const config_type&) const;
	config_type getStartConfig() const;
	string stringFromConfig(const config_type&) const;

private:

	inline unsigned int intFromRC(int r, int c) const
	{
		return r * _cols + c;
	}

	inline bool moveBlock(config_type& cfg, int row, int col,
		   int targetrow, int targetcol)  const
	{
		if(row < 0 || targetrow < 0 || col < 0 || targetcol < 0)
			return false;
		if(row >= (int)_rows || targetrow >= (int)_rows
				|| col >= (int)_cols || targetcol >= (int)_cols)
			return false;

		unsigned int startindex = intFromRC(row, col);
		unsigned int endindex = intFromRC(targetrow, targetcol);

		if(cfg[endindex] != -1)
			return false;

		cfg[endindex] = cfg[startindex];
		cfg[startindex] = -1;

		return true;
	}

	unsigned int _rows, _cols, _numBlocks;
	unsigned int _targetrow;
	config_type _start;
	vector<block_info> _blockinfo;
};

#endif

/* Change log:
 *
 * $Log: Jam.h,v $
 * Revision 1.4  2006/04/10 15:50:42  noah
 *
 * Replaced some map logic in solver.
 *
 * Original: if(!map.find) insert ...
 * Now: if(!insert) ...
 *
 * Revision 1.2  2006/03/27 04:59:04  noah
 * Replaced Solver class with single templated function.
 * Created a templated Puzzle base class for the templated function
 * to use, which formalizes requirements on the puzzle types and
 * allows the templated solve function to guess its arguments.
 *
 * Revision 1.1.1.1  2006/03/27 00:52:30  noah
 */
