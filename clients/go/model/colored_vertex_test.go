package model

/*
	Test for colored_vertex
*/

import (
	mTest "testing"
)

var (
	pos *Vec2Float32
	col *ColorFloat32
)

func TestColoredVertex(test *mTest.T) {
	_Positive := func() {
		_NewVertex := func() {
			vert := NewColoredVertex(pos, col)
			if vert == nil {
				test.Errorf("_NewVertex(): ERROR vert==nil")
			}
			if vert.Color == nil {
				test.Errorf("_NewVertex(): ERROR vert.Color==nil")
			}
			if vert.Position == nil {
				test.Errorf("_NewVertex(): ERROR vert.Position==nil")
			}
		}
		_ReadVert := func() {
			vert := ReadColoredVertex(reader)
			if vert.Color == nil {
				test.Errorf("_ReadVert(): ERROR vert.Color==nil")
			}
			if vert.Position == nil {
				test.Errorf("_ReadVert(): ERROR vert.Position==nil")
			}
		}
		_Write := func() {
			vert := ReadColoredVertex(reader)
			vert.Write(writer)
		}
		_NewVertex()
		_ReadVert()
		_Write()
	}
	_Negative := func() {
		_BadPos := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadPos(): ERROR in panic")
				}
			}()
			_ = NewColoredVertex(nil, col)
		}
		_BadCol := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadCol(): ERROR in panic")
				}
			}()
			_ = NewColoredVertex(pos, nil)
		}
		_BadPos()
		_BadCol()
	}
	pos = NewVec2Float32(1, 1)
	col = NewColorFloat32(1, 1, 1, 1)
	_Positive()
	_Negative()
}
