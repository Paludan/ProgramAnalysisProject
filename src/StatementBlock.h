//
// Created by lee on 9/24/18.
//

#ifndef PROGRAMANALYSIS_STATEMENTBLOCK_H
#define PROGRAMANALYSIS_STATEMENTBLOCK_H

#include "Statement.h"
#include <list>

class StatementBlock : Statement{
  std::list<Statement> statements;
};


#endif //PROGRAMANALYSIS_STATEMENTBLOCK_H
