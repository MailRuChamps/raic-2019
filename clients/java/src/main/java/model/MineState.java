package model;

import util.StreamUtil;

public enum MineState {
    PREPARING(0),
    IDLE(1),
    TRIGGERED(2),
    EXPLODED(3);
    public int discriminant;
    MineState(int discriminant) {
      this.discriminant = discriminant;
    }
}
