package model

import "io"
import . "aicup2019/stream"

type MineState int32
const (
    MineStatePreparing MineState = 0
    MineStateIdle MineState = 1
    MineStateTriggered MineState = 2
    MineStateExploded MineState = 3
)
func ReadMineState(reader io.Reader) MineState {
    switch ReadInt32(reader) {
    case 0:
        return MineStatePreparing
    case 1:
        return MineStateIdle
    case 2:
        return MineStateTriggered
    case 3:
        return MineStateExploded
    }
    panic("Unexpected discriminant value")
}
