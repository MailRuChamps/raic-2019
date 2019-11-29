package model

/*
	This module testing of bullets params
*/

import(
	mTest "testing"
)

func TestBulletParams(test *mTest.T){
	_Posiiv:=func(){
		_NewBulParam:=func(){
			if bp:=NewBulletParams(1,2,3);bp==nil{
				test.Errorf("_NewBulParam(): ERROR bp have nil")
			}
		}
		_NewBulParam()
	}
	_Posiiv()
}
