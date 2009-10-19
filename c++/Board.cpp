/*
 * Board.cpp
 *
 * This header contains the definition of the Board class, which is the
 * the configuration type of the Slide puzzle.
 *
 * Author: Noah Richards
 *
 * $Id: Board.cpp,v 1.3 2006/04/20 21:46:56 noah Exp $
 */

#include "Slide.h"
#include <vector>
#include <map>
#include <iostream>
using namespace std;

ostream& operator<<(ostream& os, const vector<int>& vec)
{
	for(unsigned int i = 0; i < vec.size(); i++)
	{
		cout << vec[i];
		if((i + 1) % 4 == 0)
			cout << endl;
	}
	return os;
}

bool
Board::operator<(const Board& rhs) const
{
	if(!_beenCanonicalized) canonicalize();
	if(!rhs._beenCanonicalized) rhs.canonicalize();

	return _canon < rhs._canon;
}

bool
Board::operator==(const Board& rhs) const
{
	return !operator!=(rhs);
}

bool
Board::operator!=(const Board& rhs) const
{
	return operator<(rhs) || rhs < *this;
}

void
Board::canonicalize() const
{
	vector<bool> beenSeen(_puzzle->_numBlocks, false);

	_canon.resize(_puzzle->_numBlocks * 2);
	unsigned int ix = 0;

	for(unsigned int i = 0; i < _board.size(); i++)
	{
		if(_board[i] != -1 && !beenSeen[_board[i]])
		{
			_canon[ix++] = i;
			_canon[ix++] = _puzzle->_blockids[_board[i]];
			beenSeen[_board[i]] = true;
		}
	}
	_beenCanonicalized = true;
}



/*
 * Change log:
 *
 * $Log: Board.cpp,v $
 * Revision 1.3  2006/04/20 21:46:56  noah
 *
 * Minor cleanups.
 *
 * Revision 1.2  2006/04/10 15:50:42  noah
 *
 * Replaced some map logic in solver.
 *
 * Original: if(!map.find) insert ...
 * Now: if(!insert) ...
 *
 * Revision 1.1.1.1  2006/03/27 00:52:30  noah
 * CS4 Project
 *
 * Revision 1.4  2006/02/18 22:35:12  noah
 * Modified canonization to create a smaller vector
 * Removed unnecessary information (full board)
 *
 * Revision 1.3  2006/02/08 06:06:42  noah
 * *** empty log message ***
 *
 * Revision 1.2  2006/02/08 05:24:18  noah
 * Added remembered canonized representation.
 * operator< now checks to see if board has been canonized, if
 * it hasn't, it canonizes the board.
 *
 * Revision 1.1  2006/01/28 05:12:49  noah
 * Initial revision
 *
 */
