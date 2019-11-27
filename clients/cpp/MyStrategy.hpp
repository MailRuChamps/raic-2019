#ifndef _MY_STRATEGY_HPP_
#define _MY_STRATEGY_HPP_

#include "Debug.hpp"
#include "model/CustomData.hpp"
#include "model/Game.hpp"
#include "model/Unit.hpp"
#include "model/UnitAction.hpp"

class MyStrategy {
public:
  MyStrategy();
  UnitAction getAction(const Unit &unit, const Game &game, Debug &debug);
};

#endif