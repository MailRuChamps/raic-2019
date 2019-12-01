package model

import "io"
import . "aicup2019/stream"

type TextAlignment int32
const (
    TextAlignmentLeft TextAlignment = 0
    TextAlignmentCenter TextAlignment = 1
    TextAlignmentRight TextAlignment = 2
)
func ReadTextAlignment(reader io.Reader) TextAlignment {
    switch ReadInt32(reader) {
    case 0:
        return TextAlignmentLeft
    case 1:
        return TextAlignmentCenter
    case 2:
        return TextAlignmentRight
    }
    panic("Unexpected discriminant value")
}
