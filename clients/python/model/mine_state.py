from enum import IntEnum

class MineState(IntEnum):
    PREPARING = 0
    IDLE = 1
    TRIGGERED = 2
    EXPLODED = 3
