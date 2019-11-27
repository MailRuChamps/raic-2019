package model;

import util.StreamUtil;

public enum Tile {
    EMPTY(0),
    WALL(1),
    PLATFORM(2),
    LADDER(3),
    JUMP_PAD(4);
    public int discriminant;
    Tile(int discriminant) {
      this.discriminant = discriminant;
    }
}
