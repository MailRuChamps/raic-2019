#ifndef _MODEL_MINE_STATE_HPP_
#define _MODEL_MINE_STATE_HPP_

#include "../Stream.hpp"

enum MineState {
    PREPARING = 0,
    IDLE = 1,
    TRIGGERED = 2,
    EXPLODED = 3
};

#endif
