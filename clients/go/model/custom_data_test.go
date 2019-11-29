package model

/*
	Testing for custom_data
*/

import (
	mTest "testing"
)

var (
	pos1   *Vec2Float32
	size1  *Vec2Float32
	color1 *ColorFloat32
	vert   []*ColoredVertex
)

func TestCustomDataPolygon(test *mTest.T) {
	_Positive := func() {
		_NewDp := func() {
			dp := NewCustomDataPolygon(vert)
			if dp == nil {
				test.Errorf("_NewDp(): ERROR dp==nil")
			}
			if dp.Vertices == nil {
				test.Errorf("_NewDp(): ERROR dp.Vertices==nil")
			}
		}
		_ReadDp := func() {
			dp := ReadCustomDataPolygon(reader)
			if dp == nil {
				test.Errorf("_ReadDp(): ERROR dp==nil")
			}
			if dp.Vertices == nil {
				test.Errorf("_ReadDp(): ERROR dp.Vertices==nil")
			}
		}
		_WriteDp := func() {
			dp := NewCustomDataPolygon(vert)
			dp.Write(writer) //TODO: CRITICAL ERROR!!!!!
		}
		_NewDp()
		_ReadDp()
		_WriteDp()
	}
	_Negative := func() {
		_BadVert := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadColor(): ERROR in panic")
				}
			}()
			_ = NewCustomDataPolygon(nil)
		}
		_BadVert()
	}
	_Positive()
	_Negative()
}

func TestCustomDataLine(test *mTest.T) {
	_Positive := func() {
		_NewDl := func() {
			dl := NewCustomDataLine(pos1, size1, 10, color1)
			if dl == nil {
				test.Errorf("_NewDl(): ERROR dl==nil")
			}
		}
		_ReadDl := func() {
			dl := ReadCustomDataLine(reader)
			if dl == nil {
				test.Errorf("_ReadDl(): ERROR dl==nil")
			}
		}
		_WriteDl := func() {
			dl := ReadCustomDataLine(reader)
			dl.Write(writer)
		}
		_NewDl()
		_ReadDl()
		_WriteDl()
	}
	_Negative := func() {
		_BadP1 := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadP1(): ERROR in panic")
				}
			}()
			_ = NewCustomDataLine(nil, size1, 10, color1)
		}
		_BadP2 := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadP2(): ERROR in panic")
				}
			}()
			_ = NewCustomDataLine(pos1, nil, 10, color1)
		}
		_BadWidth := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadWidth(): ERROR in panic")
				}
			}()
			_ = NewCustomDataLine(pos1, size1, -1, color1)
		}
		_BadColor := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadColor(): ERROR in panic")
				}
			}()
			_ = NewCustomDataLine(pos1, size1, 10, nil)
		}
		_BadP1()
		_BadP2()
		_BadWidth()
		_BadColor()
	}
	_Positive()
	_Negative()
}

func TestCustomData(test *mTest.T) {
	_Positiv := func() {
		_Read := func() {
			cd := ReadCustomData(reader)
			if cd == nil {
				test.Errorf("_Read(): ERROR cd==nil")
			}
		}
		_Read()
	}
	_Positiv()
}

func TestCustomDataLog(test *mTest.T) {
	_Positiv := func() {
		_NewDl := func() {
			dl := NewCustomDataLog("123")
			if dl == nil {
				test.Errorf("_NewDl(): dl==nil")
			}
			if dl.Text != "123" {
				test.Errorf("_NewDl(): dl.Text(%v)!\"123\"", dl.Text)
			}
		}
		_ReadDl := func() {
			cd := ReadCustomDataLog(reader)
			if cd == nil {
				test.Errorf("_ReadDl(): ERROR cd==nil")
			}
		}
		_WriteDl := func() {
			dl := NewCustomDataLog("123")
			dl.Write(writer)
		}
		_NewDl()
		_ReadDl()
		_WriteDl()
	}
	_Positiv()
}

func TestCustomDataRect(test *mTest.T) {
	_Positiv := func() {
		_NewDr := func() {
			dl := NewCustomDataRect(pos1, size1, color1)
			if dl == nil {
				test.Errorf("_NewDl(): dl==nil")
			}
			if dl.Pos == nil {
				test.Errorf("_NewDl(): dl.Pos==nil")
			}
			if dl.Size == nil {
				test.Errorf("_NewDl(): dl.Size==nil")
			}
			if dl.Color == nil {
				test.Errorf("_NewDl(): dl.Color==nil")
			}
		}
		_ReadDr := func() {
			cd := ReadCustomDataRect(reader)
			if cd == nil {
				test.Errorf("_Read(): ERROR cd==nil")
			}
		}
		_WriteDr := func() {
			dl := NewCustomDataRect(pos1, size1, color1)
			dl.Write(writer)
		}
		_NewDr()
		_ReadDr()
		_WriteDr()
	}
	_Negativ := func() {
		_BadPos := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadPos(): ERROR in panic")
				}
			}()
			_ = NewCustomDataRect(nil, size1, color1)
		}
		_BadSize := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadSize(): ERROR in panic")
				}
			}()
			_ = NewCustomDataRect(pos1, nil, color1)
		}
		_BadColor := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadColor(): ERROR in panic")
				}
			}()
			_ = NewCustomDataRect(pos1, size1, nil)
		}
		_BadPos()
		_BadSize()
		_BadColor()
	}
	_Positiv()
	_Negativ()
}

func init() {
	pos1 = NewVec2Float32(1, 1)
	size1 = NewVec2Float32(1, 1)
	color1 = NewColorFloat32(1, 1, 1, 1)
	vert = make([]*ColoredVertex, 5)
}
